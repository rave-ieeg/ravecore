digest <- ravepipeline::digest
prepare_subject_raw_voltage_with_epoch_legacy <- function(
    subject, electrodes, epoch_name, time_windows, stitch_events = NULL,
    ..., quiet = TRUE, repository_id = NULL) {
  re <- list()
  subject <- as_rave_subject(subject)

  # ----- project -----
  re$project <- subject$project

  # ----- subject -----
  re$subject <- subject

  if(missing(electrodes)){
    electrodes <- subject$get_default(
      "electrodes", default_if_missing = subject$electrodes)
    message("No electrodes specified, loading all electrodes: ", deparse_svec(electrodes))
  }
  if(length(electrodes) == 1 && is.character(electrodes)) {
    electrodes <- sort(parse_svec(electrodes))
  }

  # ----- epoch -----
  if(missing(time_windows)){
    time_windows <- subject$get_default("time_windows", default_if_missing = list(c(0, 2)))
    message("No time_windows specified, using default: ", deparse(time_windows))
    missing_time_windows <- TRUE
  }
  time_windows <- validate_time_window(time_windows)
  re$time_windows <- time_windows

  if(missing(epoch_name)){
    epoch_names <- subject$epoch_names
    if(!length(epoch_names)){
      stop("No epoch file found in this subject. Please check meta folder.")
    }
    epoch_name <- subject$get_default('epoch_name') %OF% epoch_names
    message("No epoch_name specified, using epoch `", epoch_name, "`.")
    epoch <- subject$get_epoch(
      epoch_name = epoch_name,
      trial_starts = min(unlist(time_windows)),
      as_table = FALSE
    )
  } else {
    if(inherits(epoch_name, "RAVEEpoch")){
      epoch <- epoch_name
      epoch_name <- epoch$name
    } else {
      epoch <- subject$get_epoch(
        epoch_name = epoch_name,
        trial_starts = min(unlist(time_windows)),
        as_table = FALSE
      )
    }
  }
  re$epoch <- epoch
  re$epoch_name <- epoch_name
  epoch_table <- epoch$table
  epoch_table <- epoch_table[order(epoch_table$Trial), ]
  re$epoch_table <- epoch_table

  # ----- stitch_events -----
  if(length(stitch_events)) {
    # check if the events are in epochs
    available_events <- epoch$available_events
    if(length(stitch_events) == 1) {
      stitch_events <- c(stitch_events, stitch_events)
    } else {
      stitch_events <- stitch_events[c(1, 2)]
    }
    stitch_events[tolower(stitch_events) %in% c("trial onset")] <- ""
    if(!all(stitch_events %in% available_events)) {

      warning(
        "Cannot find events to stitch: ",
        paste(sQuote(stitch_events[!stitch_events %in% available_events]), collapse = ", "),
        ". Available events: ",
        paste(sQuote(available_events), collapse = ", ")
      )
    }
    stitch_events_start <- stitch_events[[1]] %OF% available_events
    stitch_events_end <- stitch_events[[2]] %OF% available_events
    stitch_events <- c(stitch_events_start, stitch_events_end)
  } else {
    stitch_events <- NULL
  }
  re$stitch_events <- stitch_events

  # ----- electrode_list -----
  re$electrode_list <- electrodes

  # ----- electrode_table -----
  electrode_table <- subject$get_electrode_table()
  re$electrode_table <- electrode_table

  # ----- electrode_signal_types -----
  sel <- subject$electrodes %in% electrodes
  electrode_signal_types <- subject$electrode_types[sel]
  re$electrode_signal_types <- electrode_signal_types

  # ----- sample_rate -----
  sample_rate <- unique(subject$raw_sample_rates[sel])
  if(length(sample_rate) > 1) {
    stop(sprintf("Found more than different sample rates from the requested electrode channels [%s]. Please choose electrodes with the same sample rate.", paste(sprintf("%.0fHz", sample_rate), collapse = ", ")))
  }
  re$sample_rate <- sample_rate

  # ----- electrode_instances -----
  electrode_instances <- structure(lapply(seq_along(electrodes), function(ii){
    e <- electrodes[[ii]]
    signal_type <- electrode_signal_types[[ii]]

    el <- new_electrode(subject = subject, number = e, signal_type = signal_type, quiet = quiet)
    el$set_epoch(epoch, stitch_events = stitch_events)
    el$trial_intervals <- time_windows
    el
  }), names = sprintf("e_%d", electrodes))
  re$electrode_instances <- electrode_instances

  # ----- load_data -----
  data_list <- lapply(
    re$electrode_instances,
    function(inst) {
      inst$load_data(type = "raw-voltage")
    }
  )
  names(data_list) <- names(electrode_instances)
  dim <- dim(data_list[[1]])
  dim[[3]] <- length(data_list)

  dimnames <- dimnames(data_list[[1]])
  dimnames[[3]] <- electrodes

  digest_key <- list(
    subject_id = subject$subject_id,
    epoch_table = epoch_table,
    electrodes = electrodes,
    rave_data_type = "raw-voltage",
    electrode_signal_types = electrode_signal_types,
    sample_rate = sample_rate,
    time_windows = time_windows,
    stitch_events = stitch_events
  )
  digest_string <- digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))
  if(!length(repository_id)) {
    repository_id <- rand_string(4)
  }
  re$repository_id <- repository_id

  re$raw_voltage <- list_to_fastmap2(list(
    dim = dim,
    data_list = data_list,
    dimnames = dimnames,
    signature = re$signature
  ))

  class(re) <- c("rave_prepare_subject_raw_voltage_with_epoch", "rave_repository", "list")
  re
}
prepare_subject_voltage_with_epoch_lagacy <- function(
    subject, electrodes, epoch_name, time_windows, reference_name,
    stitch_events = NULL, ..., quiet = TRUE, repository_id = NULL) {

  # ----- DIPSAUS: DEBUG START--------
  # devtools::load_all()
  # subject <- "devel/subHUP064"
  # electrodes <- c(14,15)
  # epoch_name <- "subHUP064_seizure"
  # time_windows <- c(-1,2)
  # quiet = TRUE
  # repository_id <- NULL
  # re <- fastmap2()
  # subject <- as_rave_subject(subject)
  # re$project <- subject$project
  # re$subject <- subject

  re <- list()
  subject <- as_rave_subject(subject)

  if(!all(subject$notch_filtered)) {
    message("No Notch filter has been applied (though highly recommended), raw signals will be loaded.")
  }

  # ----- project -----
  re$project <- subject$project

  # ----- subject -----
  re$subject <- subject

  if(missing(electrodes)){
    electrodes <- subject$get_default(
      "electrodes", default_if_missing = subject$electrodes)
    message("No electrodes specified, loading all electrodes: ", deparse_svec(electrodes))
  }
  if(length(electrodes) == 1 && is.character(electrodes)) {
    electrodes <- sort(parse_svec(electrodes))
  }

  # ----- reference -----
  if(missing(reference_name) || !length(reference_name) || !all(reference_name %in% subject$reference_names)){
    if(!length(subject$reference_names)){
      warning("No reference file found in this subject. Please check meta folder! Preparing table with no reference.")
      safe_write_csv(
        data.frame(
          Electrode = subject$electrodes,
          Group = "default",
          Reference = "noref",
          Type = "No Reference"
        ), file = file.path(subject$meta_path, "reference_noref.csv"),
        row.names = FALSE
      )
      reference_name <- "noref"
    } else {
      reference_name <- subject$get_default('reference_name', default_if_missing = subject$reference_names[[1]])
      if(!reference_name %in% subject$reference_names){
        reference_name <- subject$reference_names[[1]]
      }
      if(reference_name != "noref") {
        message("No reference_name specified, using reference `", reference_name, "`.")
      }
    }
  } else {
    reference_name <- reference_name[reference_name %in% subject$reference_names]
    reference_name <- reference_name[[1]]
  }
  reference_table <- subject$get_reference(reference_name)

  if("Reference" %in% names(reference_table)){
    old_electrodes <- electrodes
    electrodes <- as.integer(reference_table$Electrode[reference_table$Reference != ''])
    electrodes <- old_electrodes[old_electrodes %in% electrodes]
    if(!setequal(electrodes, old_electrodes)){
      old_electrodes <- deparse_svec(old_electrodes[!old_electrodes %in% electrodes])
      message("The following electrodes are removed because they are either missing or marked as `excluded`: ", old_electrodes)
    }
  }
  re$reference_table <- reference_table
  re$reference_name <- reference_name

  # ----- epoch -----
  if(missing(time_windows)){
    time_windows <- subject$get_default("time_windows", default_if_missing = list(c(0, 2)))
    message("No time_windows specified, using default: ", deparse(time_windows))
    missing_time_windows <- TRUE
  }
  time_windows <- validate_time_window(time_windows)
  re$time_windows <- time_windows

  if(missing(epoch_name)){
    epoch_names <- subject$epoch_names
    if(!length(epoch_names)){
      stop("No epoch file found in this subject. Please check meta folder.")
    }
    epoch_name <- subject$get_default('epoch_name') %OF% epoch_names
    message("No epoch_name specified, using epoch `", epoch_name, "`.")
    epoch <- subject$get_epoch(
      epoch_name = epoch_name,
      trial_starts = min(unlist(time_windows)),
      as_table = FALSE
    )
  } else {
    if(inherits(epoch_name, "RAVEEpoch")){
      epoch <- epoch_name
      epoch_name <- epoch$name
    } else {
      epoch <- subject$get_epoch(
        epoch_name = epoch_name,
        trial_starts = min(unlist(time_windows)),
        as_table = FALSE
      )
    }
  }
  re$epoch <- epoch
  re$epoch_name <- epoch_name
  epoch_table <- epoch$table
  epoch_table <- epoch_table[order(epoch_table$Trial), ]
  re$epoch_table <- epoch_table

  # ----- electrode_list -----
  re$electrode_list <- electrodes

  # ----- electrode_table -----
  electrode_table <- subject$get_electrode_table()
  re$electrode_table <- electrode_table

  # ----- electrode_signal_types -----
  sel <- subject$electrodes %in% electrodes
  electrode_signal_types <- subject$electrode_types[sel]
  re$electrode_signal_types <- electrode_signal_types

  # ----- sample_rate -----
  sample_rate <- unique(subject$raw_sample_rates[sel])
  if(length(sample_rate) > 1) {
    stop(sprintf("Found more than different sample rates from the requested electrode channels [%s]. Please choose electrodes with the same sample rate.", paste(sprintf("%.0fHz", sample_rate), collapse = ", ")))
  }
  re$sample_rate <- sample_rate

  # ----- stitch_events -----
  if(length(stitch_events)) {
    # check if the events are in epochs
    available_events <- epoch$available_events
    if(length(stitch_events) == 1) {
      stitch_events <- c(stitch_events, stitch_events)
    } else {
      stitch_events <- stitch_events[c(1, 2)]
    }
    stitch_events[tolower(stitch_events) %in% c("trial onset")] <- ""
    if(!all(stitch_events %in% available_events)) {

      warning(
        "Cannot find events to stitch: ",
        paste(sQuote(stitch_events[!stitch_events %in% available_events]), collapse = ", "),
        ". Available events: ",
        paste(sQuote(available_events), collapse = ", ")
      )
    }
    stitch_events_start <- stitch_events[[1]] %OF% available_events
    stitch_events_end <- stitch_events[[2]] %OF% available_events
    stitch_events <- c(stitch_events_start, stitch_events_end)
  } else {
    stitch_events <- NULL
  }
  re$stitch_events <- stitch_events

  # ----- reference_instances -----
  ref_table <- reference_table[reference_table$Electrode %in% electrodes, ]
  references_list <- unique(ref_table$Reference)
  re$references_list <- references_list
  # load reference electrodes
  ref_mat <- unique(cbind(
    ref_table$Reference,
    electrode_signal_types
  ))
  reference_instances <- structure(
    lapply(seq_len(nrow(ref_mat)), function(ii){
      y <- ref_mat[ii, ]
      new_reference(subject = subject, number = y[[1]], signal_type = y[[2]], quiet = quiet)
    }),
    names = sprintf("%s_%s", ref_mat[, 1], ref_mat[, 2])
  )
  re$reference_instances <- drop_nulls(reference_instances)

  # ----- electrode_instances -----
  electrode_instances <- structure(lapply(seq_along(electrodes), function(ii){
    e <- electrodes[[ii]]
    signal_type <- electrode_signal_types[[ii]]

    el <- new_electrode(subject = subject, number = e, signal_type = signal_type, quiet = quiet)

    # set reference
    ref_name <- reference_table$Reference[reference_table$Electrode == e][[1]]
    ref_name <- sprintf("%s_%s", ref_name, signal_type)
    ref <- reference_instances[[ref_name]]
    el$set_reference(ref)

    el$set_epoch(epoch, stitch_events = stitch_events)
    el$trial_intervals <- time_windows

    el
  }), names = sprintf("e_%d", electrodes))
  re$electrode_instances <- electrode_instances

  # ----- load_data -----
  ref_mat <- unique(sprintf("%s_%s", re$reference_table[re$reference_table$Electrode %in% re$electrode_list, "Reference"], electrode_signal_types))
  ref_instances <- drop_nulls(re$reference_instances[ref_mat])
  if(length(ref_instances) < 4) {
    refs <- lapply(ref_instances, function(ref){
      ref$load_data(type = "voltage")
    })
  } else {
    refs <- lapply(ref_instances, function(ref){
      ref$load_data(type = "voltage")
    })
  }

  data_list <- lapply(
    re$electrode_instances,
    function(inst) {
      inst$load_data(type = "voltage")
    }
  )
  names(data_list) <- names(electrode_instances)
  dim <- dim(data_list[[1]])
  dim[[3]] <- length(data_list)

  dimnames <- dimnames(data_list[[1]])
  dimnames[[3]] <- electrodes

  digest_key <- list(
    subject_id = subject$subject_id,
    epoch_table = epoch_table,
    electrodes = electrodes,
    rave_data_type = "voltage",
    electrode_signal_types = electrode_signal_types,
    sample_rate = sample_rate,
    time_windows = time_windows,
    stitch_events = stitch_events
  )
  digest_string <- digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))
  if(!length(repository_id)) {
    repository_id <- rand_string(4)
  }
  re$repository_id <- repository_id

  re$voltage <- list_to_fastmap2(list(
    dim = dim,
    data_list = data_list,
    dimnames = dimnames,
    signature = re$signature
  ))

  class(re) <- c("rave_prepare_subject_voltage_with_epoch", "rave_repository", "list")
  re
}

test_that("RAVESubjectEpochRawVoltageRepository", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    dir.exists(as_rave_subject("demo/DemoSubject", strict = FALSE)$path)
  })

  repository_id <- rand_string(4)
  repo_old <- prepare_subject_raw_voltage_with_epoch_legacy(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )
  repo_new0 <- RAVESubjectEpochRawVoltageRepository$new(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )
  raw <- serialize(repo_new0, NULL, refhook = ravepipeline::rave_serialize_refhook)
  repo_new <- unserialize(raw, refhook = ravepipeline::rave_unserialize_refhook)

  testthat::expect_true(inherits(repo_new, "rave_prepare_subject_raw_voltage_with_epoch"))

  names_old <- names(repo_old)
  names_old <- names_old[!names_old %in% names(repo_new)]
  testthat::expect_equal(names_old, character(0L))

  testthat::expect_equal(repo_new$repository_id, repo_old$repository_id)

  testthat::expect_equal(repo_new$project, repo_old$project)
  testthat::expect_equal(repo_new$subject, repo_old$subject)

  testthat::expect_equal(repo_new$electrode_list, repo_old$electrode_list)

  nms <- names(repo_old$electrode_table)
  testthat::expect_equal(repo_new$electrode_table[, nms], repo_old$electrode_table)
  testthat::expect_equal(unname(repo_new$electrode_signal_types),
                         unname(repo_old$electrode_signal_types))

  einst_new <- lapply(repo_new$electrode_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$electrode_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  testthat::expect_equal(repo_new$reference_name, "noref")
  testthat::expect_equal(unique(repo_new$reference_table$Reference), "noref")

  testthat::expect_equal(repo_new$epoch_name, repo_old$epoch_name)
  testthat::expect_equal(repo_new$epoch$`@marshal`(), repo_old$epoch$`@marshal`())
  testthat::expect_equal(repo_new$epoch_table, repo_old$epoch_table)
  testthat::expect_equal(repo_new$stitch_events, repo_old$stitch_events)
  testthat::expect_equal(repo_new$time_windows, repo_old$time_windows)

  # New attributes
  testthat::expect_equal(repo_new$sample_rate, repo_old$sample_rate)

  testthat::expect_true(!is.null(repo_new$raw_voltage))

  testthat::expect_equal(unname(repo_new$raw_voltage$dim), unname(repo_old$raw_voltage$dim))
  testthat::expect_equal(repo_new$raw_voltage$dimnames, repo_old$raw_voltage$dimnames)

  new_datalist <- repo_new$raw_voltage$data_list
  old_datalist <- repo_old$raw_voltage$data_list
  testthat::expect_equal(names(new_datalist), names(old_datalist))
  testthat::expect_equal(lapply(new_datalist, "["), lapply(old_datalist, "["))
})

test_that("RAVESubjectEpochVoltageRepository", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    dir.exists(as_rave_subject("demo/DemoSubject", strict = FALSE)$path)
  })

  repository_id <- rand_string(4)
  repo_old <- prepare_subject_voltage_with_epoch_lagacy(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    reference_name = "default",
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )
  repo_new0 <- RAVESubjectEpochVoltageRepository$new(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    reference_name = "default",
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )
  raw <- serialize(repo_new0, NULL, refhook = ravepipeline::rave_serialize_refhook)
  repo_new <- unserialize(raw, refhook = ravepipeline::rave_unserialize_refhook)

  testthat::expect_true(inherits(repo_new, "rave_prepare_subject_voltage_with_epoch"))

  names_old <- names(repo_old)
  names_old <- names_old[!names_old %in% names(repo_new)]
  testthat::expect_equal(names_old, character(0L))

  testthat::expect_equal(repo_new$repository_id, repo_old$repository_id)

  testthat::expect_equal(repo_new$project, repo_old$project)
  testthat::expect_equal(repo_new$subject, repo_old$subject)

  testthat::expect_equal(repo_new$electrode_list, repo_old$electrode_list)

  nms <- names(repo_old$electrode_table)
  testthat::expect_equal(repo_new$electrode_table[, nms], repo_old$electrode_table)
  testthat::expect_equal(unname(repo_new$electrode_signal_types),
                         unname(repo_old$electrode_signal_types))

  einst_new <- lapply(repo_new$electrode_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$electrode_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  testthat::expect_equal(repo_new$reference_name, repo_old$reference_name)
  testthat::expect_equal(repo_new$reference_table, repo_old$reference_table)
  testthat::expect_equal(repo_new$references_list, repo_old$references_list)

  refinst_new <- lapply(repo_new$reference_instances, function(x) { x$`@marshal`() })
  refinst_old <- lapply(repo_old$reference_instances, function(x) { x$`@marshal`() })
  testthat::expect_equal(refinst_new, refinst_old)

  testthat::expect_equal(repo_new$epoch_name, repo_old$epoch_name)
  testthat::expect_equal(repo_new$epoch$`@marshal`(), repo_old$epoch$`@marshal`())
  testthat::expect_equal(repo_new$epoch_table, repo_old$epoch_table)
  testthat::expect_equal(repo_new$stitch_events, repo_old$stitch_events)
  testthat::expect_equal(repo_new$time_windows, repo_old$time_windows)

  # New attributes
  testthat::expect_equal(repo_new$sample_rate, repo_old$sample_rate)

  testthat::expect_true(!is.null(repo_new$voltage))

  testthat::expect_equal(unname(repo_new$voltage$dim), unname(repo_old$voltage$dim))
  testthat::expect_equal(repo_new$voltage$dimnames, repo_old$voltage$dimnames)

  new_datalist <- repo_new$voltage$data_list
  old_datalist <- repo_old$voltage$data_list
  testthat::expect_equal(names(new_datalist), names(old_datalist))
  testthat::expect_equal(lapply(new_datalist, "["), lapply(old_datalist, "["))
})



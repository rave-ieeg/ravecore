digest <- ravepipeline::digest
prepare_subject_bare_legacy <- function(subject, electrodes, reference_name, ...,
                                        repository_id = NULL) {

  # electrode_list, reference_name, reference_table, electrode_table, subject, references_list, electrode_signal_types, electrode_instances
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


  if(missing(reference_name) || !length(reference_name) || !all(reference_name %in% subject$reference_names)) {
    reference_name <- "noref"
  }

  if(!all(reference_name %in% subject$reference_names)){
    if( !identical(reference_name, "noref") ) {
      warning("No reference file found in this subject. Please check meta folder! Preparing table with no reference.")
    }
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
    # if(!length(subject$reference_names)){
    #   safe_write_csv(
    #     data.frame(
    #       Electrode = subject$electrodes,
    #       Group = "default",
    #       Reference = "noref",
    #       Type = "No Reference"
    #     ), file = file.path(subject$meta_path, "reference_noref.csv"),
    #     row.names = FALSE
    #   )
    #   reference_name <- "noref"
    # } else {
    #   reference_name <- subject$get_default('reference_name', default_if_missing = subject$reference_names[[1]])
    #   if(!reference_name %in% subject$reference_names){
    #     reference_name <- subject$reference_names[[1]]
    #   }
    #   if(reference_name != "noref") {
    #     message("No reference_name specified, using reference `", reference_name, "`.")
    #   }
    # }
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

  # ----- reference_name -----
  re$reference_name <- reference_name

  # ----- reference_table -----
  re$reference_table <- reference_table

  # ----- references_list -----
  ref_table <- reference_table[reference_table$Electrode %in% electrodes, ]
  references_list <- unique(ref_table$Reference)
  re$references_list <- references_list

  # ----- electrode_list -----
  electrode_list <- electrodes
  re$electrode_list <- electrode_list

  # ----- electrode_table -----
  electrode_table <- subject$get_electrode_table(
    electrodes = electrodes,
    reference_name = reference_name,
    subset = FALSE,
    simplify = FALSE)
  re$electrode_table <- electrode_table

  # ----- electrode_signal_types -----
  sel <- subject$electrodes %in% electrodes
  electrode_signal_types <- subject$electrode_types[sel]
  re$electrode_signal_types <- electrode_signal_types


  # ----- reference_instances -----
  # load reference electrodes
  ref_mat <- unique(cbind(
    ref_table$Reference,
    electrode_signal_types
  ))
  reference_instances <- structure(
    lapply(seq_len(nrow(ref_mat)), function(ii){
      y <- ref_mat[ii, ]
      new_reference(subject = subject, number = y[[1]], signal_type = y[[2]])
    }),
    names = sprintf("%s_%s", ref_mat[, 1], ref_mat[, 2])
  )
  re$reference_instances <- drop_nulls(reference_instances)

  # ----- electrode_instances -----
  electrode_instances <- structure(lapply(seq_along(electrode_list), function(ii){
    e <- electrode_list[[ii]]
    signal_type <- electrode_signal_types[[ii]]
    ref_name <- reference_table$Reference[reference_table$Electrode == e][[1]]
    ref_name <- sprintf("%s_%s", ref_name, signal_type)
    ref <- reference_instances[[ref_name]]

    el <- new_electrode(subject = subject, number = e, signal_type = signal_type)
    el$set_reference(ref)
    el
  }), names = sprintf("e_%d", electrode_list))
  re$electrode_instances <- electrode_instances

  digest_key <- list(
    subject_id = re$subject$subject_id,
    reference_table = re$reference_table,
    electrodes = re$electrode_list,
    electrode_signal_types = re$electrode_signal_types
  )
  digest_string <- digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))
  if(!length(repository_id)) {
    repository_id <- rand_string(4)
  }
  re$repository_id <- repository_id

  class(re) <- c("rave_prepare_subject", "rave_repository", "list")
  re

}

prepare_subject_with_epoch_legacy <- function(subject, electrodes, reference_name, epoch_name, time_windows, stitch_events = NULL, env = parent.frame(), ...){

  call <- as.list(match.call())
  call[["env"]] <- NULL
  call[["time_windows"]] <- NULL
  call[["epoch_name"]] <- NULL
  call[[1]] <- quote(prepare_subject_bare_legacy)
  call <- as.call(call)
  re <- eval(call, envir = env)

  if(missing(time_windows)){
    time_windows <- re$subject$get_default("time_windows", default_if_missing = list(c(0, 2)))
    message("No time_windows specified, using default: ", deparse(time_windows))
    missing_time_windows <- TRUE
  }
  time_windows <- validate_time_window(time_windows)
  re$time_windows <- time_windows

  if(missing(epoch_name)){
    if(!length(re$subject$epoch_names)){
      stop("No epoch file found in this subject. Please check meta folder.")
    }
    epoch_name <- re$subject$get_default('epoch_name', default_if_missing = re$subject$epoch_names[[1]])
    if(!epoch_name %in% re$subject$epoch_names){
      epoch_name <- re$subject$epoch_names[[1]]
    }
    message("No epoch_name specified, using epoch `", epoch_name, "`.")
    epoch <- re$subject$get_epoch(
      epoch_name = epoch_name,
      trial_starts = min(unlist(time_windows)),
      as_table = FALSE
    )
  } else {
    if(inherits(epoch_name, "RAVEEpoch")){
      epoch <- epoch_name
      epoch_name <- epoch$name
    } else {
      epoch <- re$subject$get_epoch(
        epoch_name = epoch_name,
        trial_starts = min(unlist(time_windows)),
        as_table = FALSE
      )
    }
  }
  re$epoch_name <- epoch_name
  re$epoch <- epoch

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

  # set epoch and time_windows
  lapply(re$reference_instances, function(e){
    e$set_epoch(epoch, stitch_events = stitch_events)
    e$trial_intervals <- time_windows
    NULL
  })
  lapply(re$electrode_instances, function(e){
    e$set_epoch(epoch, stitch_events = stitch_events)
    e$trial_intervals <- time_windows
    NULL
  })



  digest_key <- list(
    subject_id = re$subject$subject_id,
    epoch_table = re$epoch$table,
    reference_table = re$reference_table,
    electrode_list = re$electrode_list,
    electrode_signal_types = re$electrode_signal_types,
    time_windows = re$time_windows,
    stitch_events = re$stitch_events
  )
  digest_string <- digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))

  class(re) <- c(
    "rave_prepare_with_epoch",
    "rave_prepare_subject", "rave_repository",
    "list"
  )
  re
}

prepare_subject_wavelet_legacy <- function(subject, electrodes, reference_name, epoch_name, time_windows,
         stitch_events = NULL,
         signal_type = c("LFP"), env = parent.frame(), verbose = TRUE, ...) {
  call <- match.call()
  call[[1]] <- quote(prepare_subject_with_epoch_legacy)

  if(length(signal_type) > 1) {
    stop("`prepare_subject_wavelet`: you can only load one signal type each time")
  }

  re <- eval(call, envir = env)

  # DIPSAUS DEBUG START
  # subject = "demo/YAB"
  # re = prepare_subject_with_epoch(subject)
  # signal_type <- "LFP"
  # verbose <- TRUE
  re$signal_type <- signal_type


  frequency_table <- re$subject$get_frequency(simplify = FALSE)
  frequency <- frequency_table$Frequency
  re$frequency <- frequency

  match_signal_types <- re$electrode_signal_types %in% signal_type
  re$electrode_list <- re$electrode_list[match_signal_types]
  re$electrode_instances <- re$electrode_instances[match_signal_types]
  re$electrode_signal_types <- re$electrode_signal_types[match_signal_types]

  electrode_signal_types <- re$electrode_signal_types

  # load references first
  ref_mat <- unique(sprintf("%s_%s", re$reference_table[re$reference_table$Electrode %in% re$electrode_list, "Reference"], electrode_signal_types))

  ref_instances <- drop_nulls(re$reference_instances[ref_mat])
  refs <- lapply(ref_instances, function(ref){
    ref$load_data(type = "wavelet-coefficient")
  })


  # load actual wavelet-coefficient, reference on the fly
  wavelet_list <- lapply(re$electrode_instances, function(el){
      el$load_data(type = "wavelet-coefficient")
    })


  # re$wavelet_list <- wavelet_list
  wavelet_dimnames <- dimnames(wavelet_list[[1]])
  wavelet_dimnames$Electrode <- re$electrode_list
  re$time_points <- wavelet_dimnames$Time
  # re$wavelet_dimnames <- wavelet_dimnames
  wavelet_dim <- vapply(wavelet_dimnames, length, 0L)
  # re$wavelet_dim <- wavelet_dim

  digest_key <- list(
    subject_id = re$subject$subject_id,
    epoch_table = re$epoch$table[c("Block", "Time", "Trial")],
    reference_table = re$reference_table,
    time_points = wavelet_dimnames$Time,
    electrode_list = re$electrode_list,
    frequency_table = frequency_table,
    electrode_signal_types = re$electrode_signal_types,
    signal_data_type = "wavelet-coefficient",
    stitch_events = re$stitch_events
  )
  digest_string <- digest(digest_key)

  re$signature <- structure(digest_string, contents = names(digest_key))

  re$wavelet <- fastmap2()
  re$wavelet$dimnames <- wavelet_dimnames
  re$wavelet$dim <- wavelet_dim
  re$wavelet$data_list <- wavelet_list
  re$wavelet$signature <- digest(c(digest_string, re$electrode_list))

  class(re) <- c(
    sprintf("rave_prepare_wavelet-%s", signal_type),
    "rave_prepare_wavelet", class(re)
  )
  re

}

prepare_subject_power_legacy <- function(subject, electrodes, reference_name, epoch_name, time_windows,
         stitch_events = NULL,
         signal_type = c("LFP"), env = parent.frame(), verbose = TRUE, ...) {
  call <- match.call()
  # DIPSAUS DEBUG START
  # call <- quote(prepare_subject_power(subject = "test/DemoSubject", electrodes = '1-20', reference_name = "default"))
  # list2env(list(signal_type = c("LFP"), env = parent.frame(), verbose = TRUE), envir=.GlobalEnv)
  call[[1]] <- quote(prepare_subject_with_epoch_legacy)

  if(length(signal_type) > 1) {
    stop("`prepare_subject_power`: you can only load one signal type each time")
  }

  re <- eval(call, envir = env)

  re$signal_type <- signal_type


  frequency_table <- re$subject$get_frequency(simplify = FALSE)
  frequency <- frequency_table$Frequency
  re$frequency <- frequency

  match_signal_types <- re$electrode_signal_types %in% signal_type
  re$electrode_list <- re$electrode_list[match_signal_types]
  re$electrode_instances <- re$electrode_instances[match_signal_types]
  re$electrode_signal_types <- re$electrode_signal_types[match_signal_types]

  electrode_signal_types <- re$electrode_signal_types

  # load references first
  ref_mat <- unique(sprintf("%s_%s", re$reference_table[re$reference_table$Electrode %in% re$electrode_list, "Reference"], electrode_signal_types))

  ref_instances <- drop_nulls(re$reference_instances[ref_mat])
  refs <- lapply(ref_instances, function(ref){
    ref$load_data(type = "power")
  })

  # load actual power, reference on the fly
  power_list <- lapply(re$electrode_instances, function(el){
    el$load_data(type = "power")
  })

  # re$power_list <- power_list
  power_dimnames <- dimnames(power_list[[1]])
  power_dimnames$Electrode <- re$electrode_list
  re$time_points <- power_dimnames$Time
  # re$power_dimnames <- power_dimnames
  power_dim <- vapply(power_dimnames, length, 0L)
  # re$power_dim <- power_dim

  digest_key <- list(
    subject_id = re$subject$subject_id,
    epoch_table = re$epoch$table[c("Block", "Time", "Trial")],
    reference_table = re$reference_table,
    time_points = power_dimnames$Time,
    electrode_list = re$electrode_list,
    frequency_table = frequency_table,
    electrode_signal_types = re$electrode_signal_types,
    stitch_events = re$stitch_events
  )
  digest_string <- digest(digest_key)

  re$signature <- structure(digest_string, contents = names(digest_key))

  re$power <- fastmap2()
  re$power$dimnames <- power_dimnames
  re$power$dim <- power_dim
  re$power$data_list <- power_list
  re$power$signature <- digest(c(digest_string, re$electrode_list))

  class(re) <- c(
    sprintf("rave_prepare_power-%s", signal_type),
    "rave_prepare_power", class(re)
  )
  re

}

prepare_subject_phase_legacy <- function(subject, electrodes, reference_name, epoch_name, time_windows,
         stitch_events = NULL,
         signal_type = c("LFP"), env = parent.frame(), verbose = TRUE, ...) {
  call <- match.call()
  call[[1]] <- quote(prepare_subject_with_epoch_legacy)

  if(length(signal_type) > 1) {
    stop("`prepare_subject_phase`: you can only load one signal type each time")
  }

  re <- eval(call, envir = env)

  # DIPSAUS DEBUG START
  # subject = "demo/YAB"
  # re = prepare_subject_with_epoch(subject)
  # signal_type <- "LFP"
  # verbose <- TRUE
  re$signal_type <- signal_type


  frequency_table <- re$subject$get_frequency(simplify = FALSE)
  frequency <- frequency_table$Frequency
  re$frequency <- frequency

  match_signal_types <- re$electrode_signal_types %in% signal_type
  re$electrode_list <- re$electrode_list[match_signal_types]
  re$electrode_instances <- re$electrode_instances[match_signal_types]
  re$electrode_signal_types <- re$electrode_signal_types[match_signal_types]

  electrode_signal_types <- re$electrode_signal_types

  # load references first
  ref_mat <- unique(sprintf("%s_%s", re$reference_table[re$reference_table$Electrode %in% re$electrode_list, "Reference"], electrode_signal_types))

  ref_instances <- drop_nulls(re$reference_instances[ref_mat])
  refs <- lapply(ref_instances, function(ref){
    ref$load_data(type = "phase")
  })

  # load actual phase, reference on the fly
  phase_list <- lapply(re$electrode_instances, function(el){
    el$load_data(type = "phase")
  })


  # re$phase_list <- phase_list
  phase_dimnames <- dimnames(phase_list[[1]])
  phase_dimnames$Electrode <- re$electrode_list
  re$time_points <- phase_dimnames$Time
  # re$phase_dimnames <- phase_dimnames
  phase_dim <- vapply(phase_dimnames, length, 0L)
  # re$phase_dim <- phase_dim

  digest_key <- list(
    subject_id = re$subject$subject_id,
    epoch_table = re$epoch$table[c("Block", "Time", "Trial")],
    reference_table = re$reference_table,
    time_points = phase_dimnames$Time,
    electrode_list = re$electrode_list,
    frequency_table = frequency_table,
    electrode_signal_types = re$electrode_signal_types,
    signal_data_type = "phase",
    stitch_events = re$stitch_events
  )
  digest_string <- digest(digest_key)

  re$signature <- structure(digest_string, contents = names(digest_key))

  re$phase <- fastmap2()
  re$phase$dimnames <- phase_dimnames
  re$phase$dim <- phase_dim
  re$phase$data_list <- phase_list
  re$phase$signature <- digest(c(digest_string, re$electrode_list))

  class(re) <- c(
    sprintf("rave_prepare_phase-%s", signal_type),
    "rave_prepare_phase", class(re)
  )
  re

}

test_that("RAVESubjectEpochTimeFreqCoefRepository", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    dir.exists(as_rave_subject("demo/DemoSubject", strict = FALSE)$path)
  })

  repository_id <- rand_string(4)
  repo_old <- prepare_subject_wavelet_legacy(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    reference_name = "default",
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )
  repo_new0 <- RAVESubjectEpochTimeFreqCoefRepository$new(
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

  testthat::expect_true(inherits(repo_new, "rave_prepare_time_frequency_coefficients"))

  names_old <- names(repo_old)
  names_old <- names_old[!names_old %in% names(repo_new)]
  testthat::expect_equal(names_old, character(0L))

  testthat::expect_equal(repo_new$repository_id, repo_old$repository_id)

  testthat::expect_equal(repo_new$project, repo_old$project)
  testthat::expect_equal(repo_new$subject, repo_old$subject)

  testthat::expect_equal(repo_new$electrode_list, repo_old$electrode_list)

  testthat::expect_equal(repo_new$reference_name, repo_old$reference_name)
  testthat::expect_equal(repo_new$reference_table, repo_old$reference_table)

  nms <- names(repo_old$electrode_table)
  testthat::expect_equal(repo_new$electrode_table[, nms], repo_old$electrode_table)
  testthat::expect_equal(unname(repo_new$electrode_signal_types),
                         unname(repo_old$electrode_signal_types))

  testthat::expect_equal(repo_new$references_list, repo_old$references_list)

  einst_new <- lapply(repo_new$reference_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$reference_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  einst_new <- lapply(repo_new$electrode_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$electrode_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  testthat::expect_equal(repo_new$epoch_name, repo_old$epoch_name)
  testthat::expect_equal(repo_new$epoch$`@marshal`(), repo_old$epoch$`@marshal`())
  testthat::expect_equal(repo_new$time_windows, repo_old$time_windows)

  # testthat::expect_equal(repo_new$sample_rate, repo_old$sample_rate)

  # New attributes
  testthat::expect_equal(repo_new$time, repo_old$time_points)

  testthat::expect_true(!is.null(repo_new$coefficients))

  testthat::expect_equal(repo_new$coefficients$dim, repo_old$wavelet$dim)
  testthat::expect_equal(repo_new$coefficients$dimnames, repo_old$wavelet$dimnames)

  new_datalist <- repo_new$coefficients$data_list
  old_datalist <- repo_old$wavelet$data_list
  testthat::expect_equal(names(new_datalist), names(old_datalist))
  testthat::expect_equal(lapply(new_datalist, "["), lapply(old_datalist, "["))
})


test_that("RAVESubjectEpochPowerRepository", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    dir.exists(as_rave_subject("demo/DemoSubject", strict = FALSE)$path)
  })

  repository_id <- rand_string(4)
  repo_old <- prepare_subject_power(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    reference_name = "default",
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )
  repo_new0 <- RAVESubjectEpochPowerRepository$new(
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

  testthat::expect_true(inherits(repo_new, "rave_prepare_power"))

  names_old <- names(repo_old)
  names_old <- names_old[!names_old %in% names(repo_new)]
  testthat::expect_equal(names_old, character(0L))

  testthat::expect_equal(repo_new$repository_id, repo_old$repository_id)

  testthat::expect_equal(repo_new$project, repo_old$project)
  testthat::expect_equal(repo_new$subject, repo_old$subject)

  testthat::expect_equal(repo_new$electrode_list, repo_old$electrode_list)

  testthat::expect_equal(repo_new$reference_name, repo_old$reference_name)
  testthat::expect_equal(repo_new$reference_table, repo_old$reference_table)

  nms <- names(repo_old$electrode_table)
  testthat::expect_equal(repo_new$electrode_table[, nms], repo_old$electrode_table)
  testthat::expect_equal(unname(repo_new$electrode_signal_types),
                         unname(repo_old$electrode_signal_types))

  testthat::expect_equal(repo_new$references_list, repo_old$references_list)

  einst_new <- lapply(repo_new$reference_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$reference_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  einst_new <- lapply(repo_new$electrode_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$electrode_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  testthat::expect_equal(repo_new$epoch_name, repo_old$epoch_name)
  testthat::expect_equal(repo_new$epoch$`@marshal`(), repo_old$epoch$`@marshal`())
  testthat::expect_equal(repo_new$time_windows, repo_old$time_windows)

  # testthat::expect_equal(repo_new$sample_rate, repo_old$sample_rate)

  # New attributes
  testthat::expect_equal(repo_new$time, repo_old$time_points)

  testthat::expect_true(!is.null(repo_new$power))

  testthat::expect_equal(repo_new$power$dim, repo_old$power$dim)
  testthat::expect_equal(repo_new$power$dimnames, repo_old$power$dimnames)

  new_datalist <- repo_new$power$data_list
  old_datalist <- repo_old$power$data_list
  testthat::expect_equal(names(new_datalist), names(old_datalist))
  testthat::expect_equal(lapply(new_datalist, "["), lapply(old_datalist, "["))
})


test_that("RAVESubjectEpochPhaseRepository", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    dir.exists(as_rave_subject("demo/DemoSubject", strict = FALSE)$path)
  })

  repository_id <- rand_string(4)
  repo_old <- prepare_subject_phase(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    reference_name = "default",
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )
  repo_new0 <- RAVESubjectEpochPhaseRepository$new(
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

  testthat::expect_true(inherits(repo_new, "rave_prepare_phase"))

  names_old <- names(repo_old)
  names_old <- names_old[!names_old %in% names(repo_new)]
  testthat::expect_equal(names_old, character(0L))

  testthat::expect_equal(repo_new$repository_id, repo_old$repository_id)

  testthat::expect_equal(repo_new$project, repo_old$project)
  testthat::expect_equal(repo_new$subject, repo_old$subject)

  testthat::expect_equal(repo_new$electrode_list, repo_old$electrode_list)

  testthat::expect_equal(repo_new$reference_name, repo_old$reference_name)
  testthat::expect_equal(repo_new$reference_table, repo_old$reference_table)

  nms <- names(repo_old$electrode_table)
  testthat::expect_equal(repo_new$electrode_table[, nms], repo_old$electrode_table)
  testthat::expect_equal(unname(repo_new$electrode_signal_types),
                         unname(repo_old$electrode_signal_types))

  testthat::expect_equal(repo_new$references_list, repo_old$references_list)

  einst_new <- lapply(repo_new$reference_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$reference_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  einst_new <- lapply(repo_new$electrode_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$electrode_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  testthat::expect_equal(repo_new$epoch_name, repo_old$epoch_name)
  testthat::expect_equal(repo_new$epoch$`@marshal`(), repo_old$epoch$`@marshal`())
  testthat::expect_equal(repo_new$time_windows, repo_old$time_windows)

  # testthat::expect_equal(repo_new$sample_rate, repo_old$sample_rate)

  # New attributes
  testthat::expect_equal(repo_new$time, repo_old$time_points)

  testthat::expect_true(!is.null(repo_new$phase))

  testthat::expect_equal(repo_new$phase$dim, repo_old$phase$dim)
  testthat::expect_equal(repo_new$phase$dimnames, repo_old$phase$dimnames)

  new_datalist <- repo_new$phase$data_list
  old_datalist <- repo_old$phase$data_list
  testthat::expect_equal(names(new_datalist), names(old_datalist))
  testthat::expect_equal(lapply(new_datalist, "["), lapply(old_datalist, "["))
})









test_that("RAVESubjectEpochTimeFreqCoefRepository - parallel", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    dir.exists(as_rave_subject("demo/DemoSubject", strict = FALSE)$path)
  })
  ravepipeline::raveio_setopt("max_worker", asNamespace('parallel')$detectCores())

  repository_id <- rand_string(4)
  repo_old <- prepare_subject_wavelet_legacy(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    reference_name = "default",
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )
  ravepipeline::with_mirai_parallel(
    workers = 2,
    {
      repo_new0 <- RAVESubjectEpochTimeFreqCoefRepository$new(
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
    }
  )

  testthat::expect_true(inherits(repo_new, "rave_prepare_time_frequency_coefficients"))

  names_old <- names(repo_old)
  names_old <- names_old[!names_old %in% names(repo_new)]
  testthat::expect_equal(names_old, character(0L))

  testthat::expect_equal(repo_new$repository_id, repo_old$repository_id)

  testthat::expect_equal(repo_new$project, repo_old$project)
  testthat::expect_equal(repo_new$subject, repo_old$subject)

  testthat::expect_equal(repo_new$electrode_list, repo_old$electrode_list)

  testthat::expect_equal(repo_new$reference_name, repo_old$reference_name)
  testthat::expect_equal(repo_new$reference_table, repo_old$reference_table)

  nms <- names(repo_old$electrode_table)
  testthat::expect_equal(repo_new$electrode_table[, nms], repo_old$electrode_table)
  testthat::expect_equal(unname(repo_new$electrode_signal_types),
                         unname(repo_old$electrode_signal_types))

  testthat::expect_equal(repo_new$references_list, repo_old$references_list)

  einst_new <- lapply(repo_new$reference_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$reference_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  einst_new <- lapply(repo_new$electrode_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$electrode_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  testthat::expect_equal(repo_new$epoch_name, repo_old$epoch_name)
  testthat::expect_equal(repo_new$epoch$`@marshal`(), repo_old$epoch$`@marshal`())
  testthat::expect_equal(repo_new$time_windows, repo_old$time_windows)

  # testthat::expect_equal(repo_new$sample_rate, repo_old$sample_rate)

  # New attributes
  testthat::expect_equal(repo_new$time, repo_old$time_points)

  testthat::expect_true(!is.null(repo_new$coefficients))

  testthat::expect_equal(repo_new$coefficients$dim, repo_old$wavelet$dim)
  testthat::expect_equal(repo_new$coefficients$dimnames, repo_old$wavelet$dimnames)

  new_datalist <- repo_new$coefficients$data_list
  old_datalist <- repo_old$wavelet$data_list
  testthat::expect_equal(names(new_datalist), names(old_datalist))
  testthat::expect_equal(lapply(new_datalist, "["), lapply(old_datalist, "["))
})


test_that("RAVESubjectEpochPowerRepository - parallel", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    dir.exists(as_rave_subject("demo/DemoSubject", strict = FALSE)$path)
  })
  ravepipeline::raveio_setopt("max_worker", asNamespace('parallel')$detectCores())

  repository_id <- rand_string(4)
  repo_old <- prepare_subject_power(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    reference_name = "default",
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )

  ravepipeline::with_mirai_parallel(
    workers = 2,
    {
      repo_new0 <- RAVESubjectEpochPowerRepository$new(
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
    }
  )

  testthat::expect_true(inherits(repo_new, "rave_prepare_power"))

  names_old <- names(repo_old)
  names_old <- names_old[!names_old %in% names(repo_new)]
  testthat::expect_equal(names_old, character(0L))

  testthat::expect_equal(repo_new$repository_id, repo_old$repository_id)

  testthat::expect_equal(repo_new$project, repo_old$project)
  testthat::expect_equal(repo_new$subject, repo_old$subject)

  testthat::expect_equal(repo_new$electrode_list, repo_old$electrode_list)

  testthat::expect_equal(repo_new$reference_name, repo_old$reference_name)
  testthat::expect_equal(repo_new$reference_table, repo_old$reference_table)

  nms <- names(repo_old$electrode_table)
  testthat::expect_equal(repo_new$electrode_table[, nms], repo_old$electrode_table)
  testthat::expect_equal(unname(repo_new$electrode_signal_types),
                         unname(repo_old$electrode_signal_types))

  testthat::expect_equal(repo_new$references_list, repo_old$references_list)

  einst_new <- lapply(repo_new$reference_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$reference_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  einst_new <- lapply(repo_new$electrode_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$electrode_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  testthat::expect_equal(repo_new$epoch_name, repo_old$epoch_name)
  testthat::expect_equal(repo_new$epoch$`@marshal`(), repo_old$epoch$`@marshal`())
  testthat::expect_equal(repo_new$time_windows, repo_old$time_windows)

  # testthat::expect_equal(repo_new$sample_rate, repo_old$sample_rate)

  # New attributes
  testthat::expect_equal(repo_new$time, repo_old$time_points)

  testthat::expect_true(!is.null(repo_new$power))

  testthat::expect_equal(repo_new$power$dim, repo_old$power$dim)
  testthat::expect_equal(repo_new$power$dimnames, repo_old$power$dimnames)

  new_datalist <- repo_new$power$data_list
  old_datalist <- repo_old$power$data_list
  testthat::expect_equal(names(new_datalist), names(old_datalist))
  testthat::expect_equal(lapply(new_datalist, "["), lapply(old_datalist, "["))
})


test_that("RAVESubjectEpochPhaseRepository - parallel", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    dir.exists(as_rave_subject("demo/DemoSubject", strict = FALSE)$path)
  })
  ravepipeline::raveio_setopt("max_worker", asNamespace('parallel')$detectCores())

  repository_id <- rand_string(4)
  repo_old <- prepare_subject_phase(
    subject = "demo/DemoSubject",
    electrodes = 13:16,
    reference_name = "default",
    epoch_name = "auditory_onset",
    time_windows = c(-1, 2),
    stitch_events = NULL,
    repository_id = repository_id
  )

  ravepipeline::with_mirai_parallel(
    workers = 2,
    {
      repo_new0 <- RAVESubjectEpochPhaseRepository$new(
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
    }
  )

  testthat::expect_true(inherits(repo_new, "rave_prepare_phase"))

  names_old <- names(repo_old)
  names_old <- names_old[!names_old %in% names(repo_new)]
  testthat::expect_equal(names_old, character(0L))

  testthat::expect_equal(repo_new$repository_id, repo_old$repository_id)

  testthat::expect_equal(repo_new$project, repo_old$project)
  testthat::expect_equal(repo_new$subject, repo_old$subject)

  testthat::expect_equal(repo_new$electrode_list, repo_old$electrode_list)

  testthat::expect_equal(repo_new$reference_name, repo_old$reference_name)
  testthat::expect_equal(repo_new$reference_table, repo_old$reference_table)

  nms <- names(repo_old$electrode_table)
  testthat::expect_equal(repo_new$electrode_table[, nms], repo_old$electrode_table)
  testthat::expect_equal(unname(repo_new$electrode_signal_types),
                         unname(repo_old$electrode_signal_types))

  testthat::expect_equal(repo_new$references_list, repo_old$references_list)

  einst_new <- lapply(repo_new$reference_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$reference_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  einst_new <- lapply(repo_new$electrode_instances, function(x) { x$`@marshal`() })
  einst_old <- lapply(repo_old$electrode_instances, function(x) { x$`@marshal`() })

  testthat::expect_equal(einst_new, einst_old)

  testthat::expect_equal(repo_new$epoch_name, repo_old$epoch_name)
  testthat::expect_equal(repo_new$epoch$`@marshal`(), repo_old$epoch$`@marshal`())
  testthat::expect_equal(repo_new$time_windows, repo_old$time_windows)

  # testthat::expect_equal(repo_new$sample_rate, repo_old$sample_rate)

  # New attributes
  testthat::expect_equal(repo_new$time, repo_old$time_points)

  testthat::expect_true(!is.null(repo_new$phase))

  testthat::expect_equal(repo_new$phase$dim, repo_old$phase$dim)
  testthat::expect_equal(repo_new$phase$dimnames, repo_old$phase$dimnames)

  new_datalist <- repo_new$phase$data_list
  old_datalist <- repo_old$phase$data_list
  testthat::expect_equal(names(new_datalist), names(old_datalist))
  testthat::expect_equal(lapply(new_datalist, "["), lapply(old_datalist, "["))
})

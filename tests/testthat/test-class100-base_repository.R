prepare_subject_bare0_legcy <- function(subject, electrodes, reference_name, ..., quiet = TRUE, repository_id = NULL) {
  re <- dipsaus::fastmap2()
  subject <- as_rave_subject(subject)

  # ----- project -----
  re$project <- subject$project

  # ----- subject -----
  re$subject <- subject

  if(missing(electrodes)){
    electrodes <- subject$get_default(
      "electrodes", default_if_missing = subject$electrodes)
    message("No electrodes specified, loading all electrodes: ", dipsaus::deparse_svec(electrodes))
  }
  if(length(electrodes) == 1 && is.character(electrodes)) {
    electrodes <- sort(dipsaus::parse_svec(electrodes))
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
      old_electrodes <- dipsaus::deparse_svec(old_electrodes[!old_electrodes %in% electrodes])
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
  electrode_table <- subject$get_electrode_table()
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
      new_reference(subject = subject, number = y[[1]], signal_type = y[[2]], quiet = quiet)
    }),
    names = sprintf("%s_%s", ref_mat[, 1], ref_mat[, 2])
  )
  re$reference_instances <- dipsaus::drop_nulls(reference_instances)

  # ----- electrode_instances -----
  electrode_instances <- structure(lapply(seq_along(electrode_list), function(ii){
    e <- electrode_list[[ii]]
    signal_type <- electrode_signal_types[[ii]]
    ref_name <- reference_table$Reference[reference_table$Electrode == e][[1]]
    ref_name <- sprintf("%s_%s", ref_name, signal_type)
    ref <- reference_instances[[ref_name]]

    el <- new_electrode(subject = subject, number = e, signal_type = signal_type, quiet = quiet)
    el$set_reference(ref)
    el
  }), names = sprintf("e_%d", electrode_list))
  re$electrode_instances <- electrode_instances

  digest_key <- list(
    subject_id = re$subject$subject_id,
    reference_table = re$reference_table,
    electrodes = as.integer(re$electrode_list),
    electrode_signal_types = re$electrode_signal_types
  )
  re$digest_key <- digest_key
  digest_string <- dipsaus::digest(digest_key)
  re$signature <- structure(digest_string, contents = names(digest_key))
  if(!length(repository_id)) {
    repository_id <- rand_string(4)
  }
  re$repository_id <- repository_id

  class(re) <- c("rave_prepare_subject_bare0", "rave_repository", "fastmap2", "list")
  re
}
test_that("RAVESubjectBaseRepository", {
  testthat::skip_on_cran()
  testthat::skip_if_not({
    subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)
    dir.exists(subject$path)
  })


  repository_id <- rand_string(4)

  utils::capture.output(type = "message", {
    repo_old <- prepare_subject_bare0_legcy("demo/DemoSubject", reference_name = "default", repository_id = repository_id)
    repo_new0 <- RAVESubjectBaseRepository$new(subject = "demo/DemoSubject", reference_name = "default", repository_id = repository_id)
  })
  raw <- serialize(repo_new0, NULL, refhook = ravepipeline::rave_serialize_refhook)
  repo_new <- unserialize(raw, refhook = ravepipeline::rave_unserialize_refhook)

  testthat::expect_true(inherits(repo_new, "rave_repository"))

  testthat::expect_true(all(names(repo_old) %in% names(repo_new)))

  testthat::expect_equal(repo_new$repository_id, repo_old$repository_id)
  testthat::expect_equal(repo_new$digest_key, repo_old$digest_key)
  testthat::expect_equal(repo_new$signature, repo_old$signature)

  testthat::expect_equal(repo_new$project, repo_old$project)
  testthat::expect_equal(repo_new$subject, repo_old$subject)

  testthat::expect_equal(repo_new$electrode_list, repo_old$electrode_list)
  testthat::expect_equal(repo_new$electrode_table, repo_old$electrode_table)
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

})

test_that("RAVEProject - native", {
  project <- RAVEProject$new(project_name = "demo", strict = FALSE)

  extected_path <- file.path(ravepipeline::raveio_getopt("data_dir"), "demo")
  actual_path <- project$path

  testthat::expect_true(path_rel(extected_path, actual_path) == ".")
  testthat::expect_equal(format(project), "demo")
  reconstructed <- unserialize(
    serialize(
      project,
      connection = NULL,
      refhook = ravepipeline::rave_serialize_refhook
    ),
    refhook = ravepipeline::rave_unserialize_refhook
  )

  testthat::expect_true(reconstructed == project)


  # With parent path
  tdir <- normalizePath(tempdir(check = TRUE))
  project <- as_rave_project("demo@native", parent_path = tdir, strict = FALSE)

  extected_path <- normalizePath(file.path(tdir, "demo"), mustWork = FALSE)
  actual_path <- normalizePath(project$path, mustWork = FALSE)

  testthat::expect_true(path_rel(extected_path, actual_path) == ".")
  testthat::expect_equal(format(project), "demo")
})


test_that("RAVEProject - BIDS", {
  project <- RAVEProject$new(project_name = "demo@bids:ds001", strict = FALSE)

  extected_path <- file.path(ravepipeline::raveio_getopt("bids_data_dir"),
                             "ds001", "derivatives", "rave", "data_dir", "demo")
  actual_path <- project$path

  testthat::expect_true(path_rel(extected_path, actual_path) == ".")
  testthat::expect_equal(format(project), "demo@bids:ds001")

  # With parent path
  tdir <- normalizePath(tempdir(check = TRUE))
  project <- as_rave_project("demo@bids", parent_path = file.path(tdir, "ds001"), strict = FALSE)

  extected_path <- normalizePath(
    file.path(tdir, "ds001", "derivatives", "rave", "data_dir", "demo"),
    mustWork = FALSE
  )
  actual_path <- normalizePath(project$path, mustWork = FALSE)

  testthat::expect_true(path_rel(extected_path, actual_path) == ".")
  testthat::expect_equal(format(project), "demo@bids:ds001")

  # With parent path and dataset indicator
  tdir <- normalizePath(tempdir(check = TRUE))
  utils::capture.output({
    project <- as_rave_project("demo@bids:ds001",
                               parent_path = file.path(tdir, "ds002"),
                               strict = FALSE)
  }, type = "message")

  extected_path <- normalizePath(
    file.path(tdir, "ds002", "derivatives", "rave", "data_dir", "demo"),
    mustWork = FALSE
  )
  actual_path <- normalizePath(project$path, mustWork = FALSE)

  testthat::expect_true(path_rel(extected_path, actual_path) == ".")
  testthat::expect_equal(format(project), "demo@bids:ds002")
})

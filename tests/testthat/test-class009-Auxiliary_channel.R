test_that("Auxiliary_electrode", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    subject <- as_rave_subject('demo/DemoSubject', strict = FALSE)
    file.exists(subject$path)
  })

  self <- Auxiliary_electrode$new(subject = 'demo/DemoSubject', number = 14)

  private <- self$.__enclos_env__$private
  active <- self$.__enclos_env__$.__active__

  testthat::expect_equal(self$h5_fname, "14.h5")

  # testthat::skip_if_not(self$valid)
  # self$type
  # active$valid

  testthat::expect_equal(self$type, "Auxiliary")
  testthat::expect_equal(self$raw_sample_rate, NA)

  # Dummy function
  self$set_reference("13")
  testthat::expect_equal(self$reference_name, "noref")

  self$set_epoch(epoch = "auditory_onset")
  self$trial_intervals <- c(-1, 2)

  raw <- serialize(self, NULL, refhook = ravepipeline::rave_serialize_refhook)
  e14 <- unserialize(raw, refhook = ravepipeline::rave_unserialize_refhook)

  testthat::expect_equal(self, e14)
  testthat::expect_equal(e14$h5_fname, "14.h5")
  testthat::expect_equal(e14$type, "Auxiliary")
  testthat::expect_equal(e14$raw_sample_rate, NA)
  testthat::expect_equal(e14$reference_name, "noref")

  #
  # data <- self$load_data("voltage")
  # testthat::expect_equal(data[], e14$load_data("voltage")[])
  #
  # data <- self$load_data("raw-voltage")
  # testthat::expect_equal(data[], e14$load_data("raw-voltage")[])
  #
  # data <- self$load_data_with_blocks("008", type = "power")
  # testthat::expect_equal(data, e14$load_data_with_blocks("008", type = "power"))
  #
  # data <- self$load_data_with_blocks("008", type = "phase")
  # testthat::expect_equal(data, e14$load_data_with_blocks("008", type = "phase"))
  #
  # data <- self$load_data_with_blocks("008", type = "wavelet-coefficient")
  # testthat::expect_equal(data, e14$load_data_with_blocks("008", type = "wavelet-coefficient"))
  #
  # data <- self$load_data_with_blocks("008", type = "voltage")
  # testthat::expect_equal(data, e14$load_data_with_blocks("008", type = "voltage"))
  #
  # data <- self$load_data_with_blocks("008", type = "raw-voltage")
  # testthat::expect_equal(data, e14$load_data_with_blocks("008", type = "raw-voltage"))
  #
  #
  # self$clear_cache()

})

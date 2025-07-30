test_that("LFP_electrode", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    subject <- as_rave_subject('demo/DemoSubject', strict = FALSE)
    file.exists(subject$path)
  })

  self <- LFP_electrode$new(subject = 'demo/DemoSubject', number = 14)
  private <- self$.__enclos_env__$private

  testthat::expect_equal(self$h5_fname, "14.h5")

  testthat::skip_if_not(self$valid)

  testthat::expect_equal(self$type, "LFP")
  testthat::expect_equal(self$raw_sample_rate, 2000)
  testthat::expect_equal(self$power_sample_rate, 100)


  self$set_reference("13")
  self$set_epoch(epoch = "auditory_onset")
  self$trial_intervals <- c(-1, 2)

  private$check_dimensions("wavelet-coefficient")

  testthat::expect_no_failure({
    self$load_data("power")
  })

  testthat::expect_no_failure({
    self$load_data("phase")
  })

  testthat::expect_no_failure({
    self$load_data("wavelet-coefficient")
  })

  testthat::expect_no_failure({
    self$load_data("voltage")
  })

  testthat::expect_no_failure({
    self$load_data("raw-voltage")
  })

  testthat::expect_no_failure({
    self$load_blocks("008", type = "power")
    self$load_blocks("008", type = "phase")
    self$load_blocks("008", type = "wavelet-coefficient")
  })

  testthat::expect_no_failure({
    self$load_blocks("008", type = "voltage")
    self$load_blocks("008", type = "raw-voltage")
  })

  self$clear_cache()

})

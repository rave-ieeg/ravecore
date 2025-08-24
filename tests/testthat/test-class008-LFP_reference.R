test_that("LFP_reference - single channel", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    subject <- as_rave_subject('demo/DemoSubject', strict = FALSE)
    file.exists(subject$path)
  })

  self <- LFP_reference$new(subject = 'demo/DemoSubject', number = 14)
  e14 <- LFP_electrode$new(subject = 'demo/DemoSubject', number = 14)
  private <- self$.__enclos_env__$private

  testthat::expect_equal(self$h5_fname, "14.h5")

  testthat::skip_if_not(self$valid)

  testthat::expect_equal(self$type, "LFP")
  testthat::expect_equal(self$raw_sample_rate, 2000)
  testthat::expect_equal(self$power_sample_rate, 100)


  testthat::expect_error({
    self$set_reference("13")
  })
  self$set_epoch(epoch = "auditory_onset")
  self$trial_intervals <- c(-1, 2)
  e14$set_epoch(epoch = "auditory_onset")
  e14$trial_intervals <- c(-1, 2)

  data <- self$load_data_with_epochs("power")
  testthat::expect_equal(data[], e14$load_data_with_epochs("power")[])

  data <- self$load_data_with_epochs("phase")
  testthat::expect_equal(data[], e14$load_data_with_epochs("phase")[])

  data <- self$load_data_with_epochs("wavelet-coefficient")
  testthat::expect_equal(data[], e14$load_data_with_epochs("wavelet-coefficient")[])

  data <- self$load_data_with_epochs("voltage")
  testthat::expect_equal(data[], e14$load_data_with_epochs("voltage")[])

  data <- self$load_data_with_blocks("008", type = "power")
  testthat::expect_equal(data, e14$load_data_with_blocks("008", type = "power"))

  data <- self$load_data_with_blocks("008", type = "phase")
  testthat::expect_equal(data, e14$load_data_with_blocks("008", type = "phase"))

  data <- self$load_data_with_blocks("008", type = "wavelet-coefficient")
  testthat::expect_equal(data, e14$load_data_with_blocks("008", type = "wavelet-coefficient"))

  data <- self$load_data_with_blocks("008", type = "voltage")
  testthat::expect_equal(data, e14$load_data_with_blocks("008", type = "voltage"))

  e14$clear_cache()
  self$clear_cache()

})


test_that("LFP_reference - multi-channel", {

  testthat::skip_on_cran()
  testthat::skip_if_not({
    subject <- as_rave_subject('demo/DemoSubject', strict = FALSE)
    file.exists(subject$path)
  })

  self <- LFP_reference$new(subject = 'demo/DemoSubject', number = "ref_13-16,24")
  testthat::skip_if_not(self$valid)

  private <- self$.__enclos_env__$private

  testthat::expect_equal(self$h5_fname, "ref_13-16,24.h5")


  testthat::expect_equal(self$type, "LFP")
  testthat::expect_equal(self$raw_sample_rate, 2000)
  testthat::expect_equal(self$power_sample_rate, 100)


  testthat::expect_error({
    self$set_reference("13")
  })
  self$set_epoch(epoch = "auditory_onset")
  self$trial_intervals <- c(-1, 2)

  testthat::expect_true({
    data <- self$load_data_with_epochs("power")
    data <- self$load_data_with_epochs("phase")
    data <- self$load_data_with_epochs("wavelet-coefficient")
    data <- self$load_data_with_epochs("voltage")
    data <- self$load_data_with_blocks("008", type = "power")
    data <- self$load_data_with_blocks("008", type = "phase")
    data <- self$load_data_with_blocks("008", type = "wavelet-coefficient")
    data <- self$load_data_with_blocks("008", type = "voltage")
    TRUE
  })

  self$clear_cache()

})

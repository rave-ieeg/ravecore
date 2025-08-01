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

  raw <- serialize(self, NULL, refhook = ravepipeline::rave_serialize_refhook)
  e14 <- unserialize(raw, refhook = ravepipeline::rave_unserialize_refhook)

  data <- self$load_data("power")
  testthat::expect_equal(data[], e14$load_data("power")[])

  data <- self$load_data("phase")
  testthat::expect_equal(data[], e14$load_data("phase")[])

  data <- self$load_data("wavelet-coefficient")
  testthat::expect_equal(data[], e14$load_data("wavelet-coefficient")[])

  data <- self$load_data("voltage")
  testthat::expect_equal(data[], e14$load_data("voltage")[])

  data <- self$load_data("raw-voltage")
  testthat::expect_equal(data[], e14$load_data("raw-voltage")[])

  data <- self$load_blocks("008", type = "power")
  testthat::expect_equal(data, e14$load_blocks("008", type = "power"))

  data <- self$load_blocks("008", type = "phase")
  testthat::expect_equal(data, e14$load_blocks("008", type = "phase"))

  data <- self$load_blocks("008", type = "wavelet-coefficient")
  testthat::expect_equal(data, e14$load_blocks("008", type = "wavelet-coefficient"))

  data <- self$load_blocks("008", type = "voltage")
  testthat::expect_equal(data, e14$load_blocks("008", type = "voltage"))

  data <- self$load_blocks("008", type = "raw-voltage")
  testthat::expect_equal(data, e14$load_blocks("008", type = "raw-voltage"))


  self$clear_cache()

})

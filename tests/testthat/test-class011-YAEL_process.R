test_that("YAEL_process", {
  self <- YAELProcess$new(subject_code = "demo@bids:ds005953/ecog03")

  raw <- serialize(self, NULL, refhook = ravepipeline::rave_serialize_refhook)
  self2 <- unserialize(raw, refhook = ravepipeline::rave_unserialize_refhook)
  self3 <- as_yael_process("demo@bids:ds005953/sub-ecog03")

  testthat::expect_equal(self2$subject_code, self$subject_code)
  testthat::expect_equal(self2$image_types, self$image_types)
  testthat::expect_equal(self2$work_path, self$work_path)

  testthat::expect_equal(self3$subject_code, self$subject_code)
  testthat::expect_equal(self3$image_types, self$image_types)
  testthat::expect_equal(self3$work_path, self$work_path)

  testthat::expect_true(
    self$get_subject() ==
      as_rave_subject("demo@bids:ds005953/sub-ecog03", strict = FALSE)
  )

  testthat::skip_on_cran()

  mr_path <- "~/rave_data/raw_dir/yael_demo_001/rave-imaging/coregistration/MRI_reference.nii.gz"
  ct_path <- "~/rave_data/raw_dir/yael_demo_001/rave-imaging/coregistration/CT_RAW.nii.gz"

  testthat::skip_if_not(
    identical(Sys.getenv("RAVE_TEST_ALL", ''), "true")
  )

  self$set_input_image(path = mr_path, type = "T1w", overwrite = TRUE)

  self$set_input_image(path = ct_path, type = "CT", overwrite = TRUE)


  self$register_to_T1w()

  self$get_native_mapping(relative = TRUE)


  self$map_to_template(template_name = "mni_icbm152_nlin_asym_09a")


  self$get_template_mapping(relative = TRUE)

  native_ras <- data.frame(
    x = rnorm(10),
    y = rnorm(10),
    z = rnorm(10)
  )
  template_ras <- self$transform_points_to_template(native_ras)

  native_ras2 <- self$transform_points_from_template(template_ras)

  native_ras2 - as.matrix(native_ras)

  self$construct_ants_folder_from_template()

  testthat::expect_true(inherits(self$get_brain(), "rave-brain"))


})

test_that("YAEL_process", {
  self <- YAELProcess$new(subject = "demo@bids:ds005953/ecog03")

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
    identical(Sys.getenv("RAVE_TEST_ALL", ""), "true")
  )
  testthat::skip_if_not_installed("rpyANTs")

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

  # talairach.xfm maps native scanner RAS to MNI305
  ants_dir <- file.path(self$work_path, "ants")
  xfm <- threeBrain::threeBrain(path = ants_dir, subject_code = self$subject_code)$xfm

  # cross-check with `ants1.mat`, which is written as the inverse of the
  # registration affine (native LPS -> template LPS)
  mapping <- self$get_template_mapping(relative = FALSE)
  t2n <- unlist(mapping$template_to_native$transformlist)
  native_to_template_lps <- rpyANTs::as_ANTsTransform(t2n[grepl("\\.mat$", t2n)][[1]])[]
  lps_to_ras <- diag(c(-1, -1, 1, 1))
  expected <- solve(MNI305_to_MNI152) %*% lps_to_ras %*% native_to_template_lps %*% lps_to_ras
  testthat::expect_equal(unname(xfm), unname(expected), tolerance = 1e-3)

  # the affine part roughly agrees with the non-linear mapping inside the brain
  # (typically < 5 mm; the warp may absorb more for some registrations, while
  # the formula before 0.1.1.21 was about 30 mm off for this subject)
  mask <- ieegio::read_volume(file.path(ants_dir, "mri", "brainmask.nii.gz"))
  ijk <- which(array(mask[drop = FALSE] > 0, dim = dim(mask)[1:3]), arr.ind = TRUE)
  ijk <- ijk[round(seq(1, nrow(ijk), length.out = 200)), , drop = FALSE] - 1
  native_ras <- t(mask$transforms$vox2ras %*% rbind(t(ijk), 1))[, 1:3]
  mni305_affine <- t(xfm %*% rbind(t(native_ras), 1))[, 1:3]
  mni152 <- self$transform_points_to_template(native_ras, verbose = FALSE)
  mni305_nonlinear <- t(solve(MNI305_to_MNI152) %*% rbind(t(mni152), 1))[, 1:3]
  testthat::expect_lt(median(sqrt(rowSums((mni305_affine - mni305_nonlinear)^2))), 20)


})

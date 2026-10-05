test_that("ants_affine_to_talairach_xfm() inverts the ANTs affine and converts LPS to RAS", {
  rotation <- function(axis, degree) {
    a <- degree * pi / 180
    m <- diag(4)
    i <- setdiff(1:3, axis)
    m[i, i] <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2)
    m
  }
  # native RAS -> template RAS; pitch, roll and x/y shifts alone are unchanged
  # by the old formula, hence yaw, scaling and z shift
  native_to_template <- rotation(3, 12) %*% rotation(1, -9) %*% diag(c(0.9, 1.1, 0.85, 1))
  native_to_template[1:3, 4] <- c(2, -18, 25)
  lps_to_ras <- diag(c(-1, -1, 1, 1))
  # ANTs (fixed = template, moving = native) stores template LPS -> native LPS
  ants_affine <- lps_to_ras %*% solve(native_to_template) %*% lps_to_ras

  expect_equal(ants_affine_to_talairach_xfm(ants_affine, "MNI305"), native_to_template)
  expect_equal(ants_affine_to_talairach_xfm(ants_affine, "NMTv2ACPC"), native_to_template)
  xfm <- ants_affine_to_talairach_xfm(ants_affine, "MNI152")
  expect_equal(xfm, solve(MNI305_to_MNI152) %*% native_to_template)
  # the formula used up to 0.1.1.20 is about 5 cm off in z
  expect_gt(max(abs(solve(MNI305_to_MNI152) %*% ants_affine - xfm)), 10)

  # legacy layout: transformlist [ants2 (warp), ants1.mat, ants0.mat] has
  # affine part ants0 %*% ants1
  m1 <- rotation(2, 3)
  m1[1:3, 4] <- c(1, 2, -1)
  m0 <- ants_affine %*% solve(m1)
  expect_equal(ants_affine_to_talairach_xfm(list(m1, m0), "MNI305"), native_to_template)
})

test_that("ants_mapping_affine_paths() selects the registration affines in list order", {
  prefix <- "normalization/transformations/sub-S01"
  # current layout, as returned by `get_template_mapping` (list or character)
  mapping <- list(
    native_to_template = list(
      transformlist = list(
        sprintf("%s_from-T1w_to-T_desc-affine+SyN_ants1.nii.gz", prefix),
        sprintf("%s_from-T1w_to-T_desc-affine+SyN_ants0.mat", prefix)
      )
    ),
    template_to_native = list(
      transformlist = c(
        sprintf("%s_from-T_to-T1w_desc-affine+SyN_ants1.mat", prefix),
        sprintf("%s_from-T_to-T1w_desc-affine+SyN_ants0.nii.gz", prefix)
      )
    )
  )
  expect_equal(
    ants_mapping_affine_paths(mapping),
    sprintf("%s_from-T1w_to-T_desc-affine+SyN_ants0.mat", prefix)
  )

  # legacy layout [ants2 (warp), ants1.mat, ants0.mat]
  mapping$native_to_template$transformlist <- sprintf(
    "%s_from-T1w_to-T_desc-affine+SyN_ants%s", prefix,
    c("2.nii.gz", "1.mat", "0.mat")
  )
  expect_equal(
    ants_mapping_affine_paths(mapping),
    sprintf("%s_from-T1w_to-T_desc-affine+SyN_ants%s", prefix, c("1.mat", "0.mat"))
  )

  # missing mapping
  expect_length(ants_mapping_affine_paths(NULL), 0)
})

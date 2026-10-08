# Surfaces generated for atlas volumes by
# YAELProcess$generate_atlas_from_template (see generate_atlas_surface)

write_cube_volume <- function(path) {
  vol <- array(0, dim = rep(20, 3))
  vol[6:15, 6:15, 6:15] <- 1
  ieegio::write_volume(ieegio::as_ieegio_volume(vol, vox2ras = diag(1, 4)), path)
  path
}

test_that("generate_atlas_surface writes the surface next to its volume", {
  path <- write_cube_volume(tempfile(fileext = ".nii.gz"))
  gii <- sub("\\.nii\\.gz$", ".gii", path)
  on.exit(unlink(c(path, gii)), add = TRUE)

  res <- generate_atlas_surface(path, overwrite = TRUE, lambda = 0.2,
                                degree = 2, threshold_lb = 0.5,
                                threshold_ub = NA)
  expect_identical(res, path)
  expect_true(file.exists(gii))
})

test_that("generate_atlas_surface reports a surface it cannot make", {
  bad <- tempfile(fileext = ".nii.gz")
  writeLines("not a volume", bad)
  on.exit(unlink(bad), add = TRUE)

  expect_warning(
    res <- generate_atlas_surface(bad, overwrite = TRUE, lambda = 0.2,
                                  degree = 2, threshold_lb = 0.5,
                                  threshold_ub = NA),
    basename(bad), fixed = TRUE
  )
  expect_identical(res, bad)
})

test_that("generate_atlas_surface passes the newer smoothing arguments on", {
  skip_if_not("max_vertices" %in% names(formals(ieegio::volume_to_surface)))
  path <- write_cube_volume(tempfile(fileext = ".nii.gz"))
  gii <- sub("\\.nii\\.gz$", ".gii", path)
  on.exit(unlink(c(path, gii)), add = TRUE)

  generate_atlas_surface(path, overwrite = TRUE, lambda = 0.2, degree = 2,
                         threshold_lb = 0.5, threshold_ub = NA,
                         max_vertices = 100)
  surf <- ieegio::read_surface(gii)
  expect_lte(ncol(surf$geometry$vertices), 110)
})


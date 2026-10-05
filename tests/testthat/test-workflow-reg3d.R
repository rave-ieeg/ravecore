# reg3d_rigid(): a synthetic CT/MR pair with a known rigid transform

# MR: 48^3 at 1 mm with asymmetric blobs and a bright landmark at `p` (MR RAS).
# CT: the MR moved by the rigid transform `A` (MR RAS -> CT RAS), sampled on a
# finer 100 x 90 x 60 grid and stored as integers like Hounsfield units
make_reg3d_phantom <- function(root) {
  mr_dim <- c(48L, 48L, 48L)
  mr_v2r <- diag(4)
  mr_v2r[1:3, 4] <- -23.5
  ijk <- as.matrix(expand.grid(0:47, 0:47, 0:47))
  ras <- t(mr_v2r %*% rbind(t(ijk), 1))[, 1:3]
  inside <- function(centre, radius) {
    rowSums(sweep(ras, 2, centre)^2 / matrix(radius^2, nrow(ras), 3, byrow = TRUE)) <= 1
  }
  mr <- array(0, mr_dim)
  mr[inside(c(0, 0, 0), c(18, 14, 11))] <- 100
  mr[inside(c(-7, 4, 3), c(5, 5, 5))] <- 200
  mr[inside(c(9, -5, -4), c(4, 3, 6))] <- 60
  p <- c(8, -6, 5)
  mr[inside(p, c(1.6, 1.6, 1.6))] <- 400

  rotation <- function(axis, degree) {
    a <- degree * pi / 180
    m <- diag(3)
    i <- setdiff(1:3, axis)
    m[i, i] <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2)
    m
  }
  A <- diag(4)
  A[1:3, 1:3] <- rotation(3, 8) %*% rotation(1, 5)
  A[1:3, 4] <- c(3, -2, 4)

  ct_dim <- c(100L, 90L, 60L)
  ct_v2r <- diag(c(0.5, 0.5, 0.8, 1))
  ct_v2r[1:3, 4] <- -(ct_dim - 1) * c(0.5, 0.5, 0.8) / 2 + A[1:3, 4]
  ct <- ravetools::apply_transform3d(
    mr, mr_v2r, transform = solve(A), reference_dim = ct_dim,
    reference_vox2ras = ct_v2r, interpolation = "trilinear")
  ct <- round(ct * 10) - 1000
  storage.mode(ct) <- "integer"

  ct_path <- file.path(root, "ct.nii.gz")
  mr_path <- file.path(root, "mr.nii.gz")
  ieegio::write_volume(ieegio::as_ieegio_volume(ct, vox2ras = ct_v2r), ct_path)
  ieegio::write_volume(ieegio::as_ieegio_volume(mr, vox2ras = mr_v2r), mr_path)

  list(ct_path = ct_path, mr_path = mr_path, A = A, p = p,
       brain = rbind(t(ras[mr > 0, , drop = FALSE]), 1))
}

read_matrix <- function(path) {
  unname(as.matrix(utils::read.table(path, header = FALSE)))
}

image_vox2ras <- function(path) {
  vol <- ieegio::read_volume(path)
  matrix(as.numeric(vol$transforms$vox2ras)[1:16], nrow = 4L)
}

test_that("reg3d_rigid() presets", {
  expect_equal(reg3d_preset("Rigid"), list(
    shrink_factors = c(4, 2, 1), smoothing_sigmas = c(2, 1, 0),
    iterations = c(1000, 500, 250), sampling_rate = 0.2))
  expect_equal(reg3d_preset("DenseRigid")$sampling_rate, 1)
  expect_equal(reg3d_preset("DenseRigid")$shrink_factors, c(4, 2, 1))
  expect_equal(reg3d_preset("FastRigid"), list(
    shrink_factors = c(8, 4, 2), smoothing_sigmas = c(3, 2, 1),
    iterations = c(200, 150, 100), sampling_rate = 0.05))
})

test_that("reg3d_rigid() recovers a known transform and saves viewer copies", {
  skip_on_cran()
  root <- tempfile(pattern = "ravecore_reg3d_test_")
  dir.create(root, recursive = TRUE)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  ph <- make_reg3d_phantom(root)
  out <- file.path(root, "coregistration")
  re <- reg3d_rigid(ph$ct_path, ph$mr_path, coreg_path = out, max_side = 64,
                    verbose = FALSE, sampling_rate = 0.4,
                    iterations = c(300, 200, 100))

  expect_setequal(re$files, c(
    "CT_RAW.nii.gz", "CT_ORIG.nii.gz", "MRI_reference.nii.gz", "ct_in_t1.nii.gz",
    "CT_RAS_to_MR_RAS.txt", "CT_IJK_to_MR_RAS.txt",
    "reg3d/reg3d.dcf", "reg3d/reg3d0GenericAffine.mat"))
  expect_equal(unname(re$resampled), c(TRUE, FALSE))

  ct_ras_to_mr_ras <- read_matrix(file.path(out, "CT_RAS_to_MR_RAS.txt"))
  ct_ijk_to_mr_ras <- read_matrix(file.path(out, "CT_IJK_to_MR_RAS.txt"))

  # direction: CT RAS -> MR RAS is the inverse of the MR -> CT transform
  expect_lt(max(abs(ct_ras_to_mr_ras - solve(ph$A))),
            max(abs(ct_ras_to_mr_ras - ph$A)) / 5)

  # accuracy over the phantom: under 1 degree and 1 mm
  rot <- ct_ras_to_mr_ras[1:3, 1:3] %*% ph$A[1:3, 1:3]
  angle <- acos(min(1, (sum(diag(rot)) - 1) / 2)) * 180 / pi
  shift <- sqrt(colSums(((ct_ras_to_mr_ras %*% ph$A - diag(4)) %*% ph$brain)[1:3, ]^2))
  expect_lt(angle, 1)
  expect_lt(sqrt(mean(shift^2)), 1)

  # the viewer copy: sides capped at 64, same field of view, integers kept
  ct_raw <- ieegio::read_volume(file.path(out, "CT_RAW.nii.gz"))
  ct_orig <- ieegio::read_volume(file.path(out, "CT_ORIG.nii.gz"))
  raw_v2r <- image_vox2ras(file.path(out, "CT_RAW.nii.gz"))
  orig_v2r <- image_vox2ras(file.path(out, "CT_ORIG.nii.gz"))
  expect_equal(dim(ct_raw[])[1:3], c(64L, 64L, 60L))
  expect_equal(dim(ct_orig[])[1:3], c(100L, 90L, 60L))
  expect_true(is.integer(ct_raw[]))
  expect_equal(raw_v2r %*% c(-0.5, -0.5, -0.5, 1), orig_v2r %*% c(-0.5, -0.5, -0.5, 1),
               tolerance = 1e-6)
  expect_equal(raw_v2r %*% c(c(64, 64, 60) - 0.5, 1),
               orig_v2r %*% c(c(100, 90, 60) - 0.5, 1), tolerance = 1e-6)

  # the voxel transform refers to the viewer copy
  expect_equal(ct_ijk_to_mr_ras, ct_ras_to_mr_ras %*% raw_v2r, tolerance = 1e-6)

  # the landmark found in the viewer copy lands on its MR position
  voxels <- which(ct_raw[] > 0.6 * 4000 - 1000, arr.ind = TRUE) - 1
  landmark <- (ct_ijk_to_mr_ras %*% c(colMeans(voxels), 1))[1:3]
  expect_lt(sqrt(sum((landmark - ph$p)^2)), 1.5)

  # registered at full resolution: the record keeps the original CT grid
  record <- ravetools::load_registration(file.path(out, "reg3d", "reg3d.dcf"))
  expect_equal(unname(record$geometry$source_vox2ras), orig_v2r, tolerance = 1e-6)
  expect_equal(unname(record$transform), solve(ct_ras_to_mr_ras), tolerance = 1e-6)

  # every image is NIfTI-1, which the electrode localization can read
  for (f in c("CT_RAW", "CT_ORIG", "MRI_reference", "ct_in_t1")) {
    expect_equal(RNifti::niftiHeader(file.path(out, sprintf("%s.nii.gz", f)))$sizeof_hdr, 348)
  }

  # a new run backs up the previous result and leaves no temporary folder
  reg3d_rigid(ph$ct_path, ph$mr_path, coreg_path = out, reg_type = "FastRigid",
              max_side = 384, verbose = FALSE)
  siblings <- list.files(root, all.files = TRUE, no.. = TRUE)
  expect_length(grep("^coregistration_\\[backup_", siblings), 1)
  expect_length(grep("reg3d-", siblings), 0)
  expect_false(file.exists(file.path(out, "CT_ORIG.nii.gz")))
})

test_that("reg3d_rigid() keeps an existing result when it fails", {
  skip_on_cran()
  root <- tempfile(pattern = "ravecore_reg3d_test_")
  dir.create(root, recursive = TRUE)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  ph <- make_reg3d_phantom(root)
  out <- file.path(root, "coregistration")
  dir.create(out)
  writeLines("previous", file.path(out, "marker.txt"))

  # `type` is fixed to rigid: passing it again fails before anything is written
  expect_error(reg3d_rigid(ph$ct_path, ph$mr_path, coreg_path = out,
                           verbose = FALSE, type = "affine"))
  expect_equal(list.files(out), "marker.txt")
  expect_equal(list.files(root, all.files = TRUE, no.. = TRUE),
               c("coregistration", "ct.nii.gz", "mr.nii.gz"))
})

test_that("cmd_run_reg3d_rigid() builds the script without running it", {
  root <- tempfile(pattern = "ravecore_reg3d_cmd_")
  dir.create(file.path(root, "raw"), recursive = TRUE)
  dir.create(file.path(root, "data"), recursive = TRUE)
  old_raw <- ravepipeline::raveio_getopt("raw_data_dir")
  old_data <- ravepipeline::raveio_getopt("data_dir")
  ravepipeline::raveio_setopt("raw_data_dir", file.path(root, "raw"), .save = FALSE)
  ravepipeline::raveio_setopt("data_dir", file.path(root, "data"), .save = FALSE)
  on.exit({
    ravepipeline::raveio_setopt("raw_data_dir", old_raw, .save = FALSE)
    ravepipeline::raveio_setopt("data_dir", old_data, .save = FALSE)
    unlink(root, recursive = TRUE)
  }, add = TRUE)

  ct_path <- file.path(root, "ct.nii.gz")
  mr_path <- file.path(root, "mr.nii.gz")
  ieegio::write_volume(ieegio::as_ieegio_volume(array(0, c(4, 4, 4)), vox2ras = diag(4)), ct_path)
  ieegio::write_volume(ieegio::as_ieegio_volume(array(0, c(4, 4, 4)), vox2ras = diag(4)), mr_path)

  re <- cmd_run_reg3d_rigid(
    subject = "reg3dtest/sub01", ct_path = ct_path, mri_path = mr_path,
    reg_type = "DenseRigid", cost = "cc", interp = "nearest",
    verbose = FALSE, dry_run = TRUE)

  expect_false(grepl("{{", re$script, fixed = TRUE))
  expect_false(grepl("}}", re$script, fixed = TRUE))
  expect_false(file.exists(re$script_path))
  expect_true(grepl(basename(root), re$script_path, fixed = TRUE))
  expect_equal(basename(re$script_path), "cmd-reg3d-rigid.R")
  expect_equal(basename(re$log_file), "log-rave-reg3d-rigid.R.log")

  # the template's call to reg3d_rigid() matches its formal arguments
  exprs <- as.list(parse(text = re$script))
  env <- new.env(parent = baseenv())
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) &&
        identical(deparse(e[[3]][[1]]), "ravecore::reg3d_rigid")) {
      call <- e[[3]]
      call[[1]] <- as.name("reg3d_rigid")
      expect_no_error(match.call(reg3d_rigid, call))
      break
    }
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.character(e[[3]])) {
      assign(as.character(e[[2]]), e[[3]], envir = env)
    }
  }
  expect_equal(env$reg_type, "DenseRigid")
  expect_equal(env$cost, "cc")
  expect_equal(env$interp, "nearest")
})

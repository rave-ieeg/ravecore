# Tests of `yael_brain_extract`: the brain mask of a 'T1' image, from the
# brain mask of `mni_icbm152_nlin_asym_09a` brought by a registration, or from
# a given mask, adjusted to the subject. The toy template stands in for the
# template, and nothing reads real images

test_that("yael_brain_extract maps the template brain mask and adjusts it", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  info <- local_yael_toy_template(template$root)
  requested <- character()
  local_mocked_bindings(
    yael_native_template = function(template_name = rpyants_builtin_templates()) {
      requested <<- c(requested, template_name)
      info
    })
  spy <- local_yael_registration_spy()
  work <- tempfile(pattern = "ravecore-yael-brain-extract-")
  on.exit({ unlink(work, recursive = TRUE) }, add = TRUE)

  mask <- yael_brain_extract(native$t1, work_path = work, verbose = FALSE)

  # always the asymmetric 2009a template, registered as in the `SyNabp` stage
  # of the normalization: affine, then deformable, without masks
  expect_identical(requested, "mni_icbm152_nlin_asym_09a")
  calls <- spy()
  expect_equal(vapply(calls, `[[`, "", "type"), c("affine", "syn_only"))
  expect_equal(vapply(calls, `[[`, FALSE, "source_masked"), c(FALSE, FALSE))
  expect_equal(vapply(calls, `[[`, FALSE, "target_masked"), c(FALSE, FALSE))
  expect_equal(calls[[2]]$init_transform, calls[[1]]$transform)

  # a binary volume on the grid of the image, close to the true brain
  expect_s3_class(mask, "ieegio_volume")
  values <- mask[]
  expect_equal(dim(values)[1:3], native$dim)
  expect_equal(unname(mask$transforms$vox2ras), native$vox2ras, tolerance = 1e-6,
               ignore_attr = TRUE)
  expect_setequal(unique(as.vector(values)), c(0, 1))
  truth <- as.vector(native$truth > 0)
  brain <- as.vector(values > 0)
  expect_gt(2 * sum(brain & truth) / (sum(brain) + sum(truth)), 0.9)

  # the working files stay, named as the stages of the normalization, and the
  # registration reads as the `SyNabp` stage
  expect_setequal(list.files(work), c(
    "nu.nii.gz", "initial_mask.nii.gz", "brainmask.nii.gz", "SyNabp.dcf",
    "SyNabp0GenericAffine.mat", "SyNabp1Warp.nii.gz", "SyNabp1InverseWarp.nii.gz",
    "SyNabp_warped_mov.nii.gz"))
  abp <- yael_native_stage_read("SyNabp", "registration", work)$registration
  expect_equal(abp$transform, calls[[2]]$transform, tolerance = 1e-6)
  expect_equal(as.vector(yael_native_read_volume(file.path(work, "brainmask.nii.gz"))),
               as.vector(values))
  # the initial mask is the template mask (on the template grid) through that
  # registration, which the adjustment changes
  template_mask <- yael_native_resample(yael_native_read_mask(template$mask),
                                        template$dim, template$vox2ras,
                                        interpolation = "nearest")
  mapped <- ravetools::apply_transform3d_volume(
    template_mask, abp, vox2ras = template$vox2ras, reference_dim = native$dim,
    reference_vox2ras = native$vox2ras, direction = "inverse",
    interpolation = "nearest")
  initial <- as.vector(yael_native_read_volume(file.path(work, "initial_mask.nii.gz")) > 0)
  expect_equal(initial, as.vector(mapped > 0))
  expect_gt(sum(initial != brain), 0)
})

test_that("yael_brain_extract adjusts a given mask without a registration", {
  skip_on_cran()
  skip_if_no_native_yael()
  skip_if_old_testthat()
  local_yael_sandbox()
  native <- yael_toy_dataset("native")
  local_mocked_bindings(
    yael_native_template = function(...) { stop("the template was accessed") })
  spy <- local_yael_registration_spy()
  # the true brain grown by 2 voxels, which reaches into the skull
  truth <- native$truth > 0
  grown <- ravetools::morph_volume(truth, "dilate", 2) > 0
  initial <- ieegio::as_ieegio_volume(array(grown * 1, native$dim),
                                      vox2ras = native$vox2ras)
  leftover <- function() { list.files(tempdir(), pattern = "^yael-brain-extract-") }
  before <- leftover()

  mask <- yael_brain_extract(native$t1, mask = initial, verbose = FALSE)

  expect_length(spy(), 0)
  # without `work_path`, the working files are removed
  expect_setequal(leftover(), before)
  expect_s3_class(mask, "ieegio_volume")
  brain <- as.vector(mask[] > 0)
  dice <- function(x) { 2 * sum(x & truth) / (sum(x) + sum(truth)) }
  expect_gte(dice(brain), dice(as.vector(grown)) - 0.01)
})

test_that("yael_brain_extract takes a mask on another grid", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  native <- yael_toy_dataset("native")
  # the true brain on a grid with voxels twice as large
  truth <- array((native$truth > 0) * 1, native$dim)
  attr(truth, "vox2ras") <- native$vox2ras
  coarse <- yael_native_coarsen(truth, 2)
  coarse_vox2ras <- attr(coarse, "vox2ras")
  initial <- ieegio::as_ieegio_volume(array((coarse > 0.5) * 1, dim(coarse)),
                                      vox2ras = coarse_vox2ras)
  expect_false(isTRUE(all(dim(initial[])[1:3] == native$dim)))

  mask <- yael_brain_extract(native$t1, mask = initial, verbose = FALSE)

  expect_equal(dim(mask[])[1:3], native$dim)
  expect_equal(unname(mask$transforms$vox2ras), native$vox2ras, tolerance = 1e-6,
               ignore_attr = TRUE)
  brain <- as.vector(mask[] > 0)
  inside <- as.vector(native$truth > 0)
  expect_gt(2 * sum(brain & inside) / (sum(brain) + sum(inside)), 0.85)
})

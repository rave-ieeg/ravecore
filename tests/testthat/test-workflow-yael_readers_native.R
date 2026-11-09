# The workflows that read a normalization (`transform_point_to_template`,
# `get_available_morph_to_template`, `generate_atlases_from_template` and
# `generate_atlas_YBA`) run on the native engine: `local_yael_rpyants_guard()`
# makes any path that would start 'Python' fail. The subjects have a known
# deformation (see `yael_toy_warped_subject`), so the expected values are
# worked out by hand. Nothing reads real images

# ---- Helpers ---------------------------------------------------------------------

# Atlas on the grid of `yael_toy_warped_subject`: label 7 at x from 10 to 12 mm
# (voxels 31 to 33 of the 0 to 39 mm range starting at -20 mm)
yael_toy_write_atlas <- function(toy, path) {
  labels <- array(0, toy$dim)
  labels[31:33, 18:22, 18:22] <- 7
  yael_native_write_volume(labels, path, vox2ras = toy$vox2ras)
}

# First-axis voxels of a label
label_columns <- function(volume, label = 7) {
  sort(unique(which(volume == label, arr.ind = TRUE)[, 1]))
}

# ---- Points ----------------------------------------------------------------------

test_that("transform_point_to_template applies the warp without 'Python'", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  local_yael_rpyants_guard()
  toy <- yael_toy_warped_subject("YAELReaderPoints", "mni_icbm152_nlin_asym_09b")
  positions <- rbind(c(1, 2, 3), c(-4, 5, -6))

  for (method in c("auto", "nonlinear")) {
    mapped <- transform_point_to_template(toy$subject, positions = positions,
                                          volumetric_transform = method, verbose = FALSE)
    expect_equal(mapped$MNI152_x, c(6, 1), tolerance = 1e-5)
    expect_equal(mapped$MNI152_y, c(2, 5), tolerance = 1e-5)
    expect_equal(mapped$MNI152_z, c(3, -6), tolerance = 1e-5)
  }
})

test_that("transform_point_to_template keeps missing points and takes a single point", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  local_yael_rpyants_guard()
  toy <- yael_toy_warped_subject("YAELReaderMissing", "mni_icbm152_nlin_asym_09b")

  mapped <- transform_point_to_template(toy$subject, positions = rbind(c(1, 2, 3), NA),
                                        volumetric_transform = "nonlinear", verbose = FALSE)
  expect_equal(mapped$MNI152_x, c(6, NA), tolerance = 1e-5)

  mapped <- transform_point_to_template(toy$subject, positions = c(1, 2, 3),
                                        volumetric_transform = "nonlinear", verbose = FALSE)
  expect_equal(mapped$MNI152_x, 6, tolerance = 1e-5)
})

test_that("transform_point_to_template only uses the affine when there is no warp", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  local_yael_rpyants_guard()
  toy <- yael_toy_warped_subject("YAELReaderAffine", "mni_icbm152_nlin_asym_09b")
  unlink(file.path(toy$work_path, "normalization"), recursive = TRUE)
  positions <- rbind(c(1, 2, 3), c(-4, 5, -6))

  mapped <- transform_point_to_template(toy$subject, positions = positions,
                                        volumetric_transform = "auto", verbose = FALSE)
  expect_equal(mapped$MNI152_x, c(1, -4), tolerance = 1e-3)
  expect_error(
    transform_point_to_template(toy$subject, positions = positions,
                                volumetric_transform = "nonlinear", verbose = FALSE),
    "Missing non-linear deformation files")
})

test_that("a failed lookup is reported before the affine is used", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  local_yael_rpyants_guard()
  toy <- yael_toy_warped_subject("YAELReaderFailure", "mni_icbm152_nlin_asym_09b")
  testthat::local_mocked_bindings(
    as_yael_process = function(...) { stop("cannot read the transforms") }
  )

  expect_warning(
    mapped <- transform_point_to_template(toy$subject, positions = c(1, 2, 3),
                                          volumetric_transform = "auto", verbose = FALSE),
    "cannot read the transforms")
  expect_equal(mapped$MNI152_x, 1, tolerance = 1e-3)
})

test_that("a normalization to `fsaverage` gives `MNI305` coordinates", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  local_yael_rpyants_guard()
  toy <- yael_toy_warped_subject("YAELReaderFsaverage", "fsaverage")

  # the warp takes x from 5 to 10: `fsaverage` is in `MNI305` space
  mapped <- transform_point_to_template(toy$subject, positions = c(5, 10, 15),
                                        volumetric_transform = "nonlinear", verbose = FALSE)
  expect_equal(c(mapped$MNI305_x, mapped$MNI305_y, mapped$MNI305_z), c(10, 10, 15),
               tolerance = 1e-4)
  # `MNI305_to_MNI152` applied to (10, 10, 15, 1) by hand
  expect_equal(c(mapped$MNI152_x, mapped$MNI152_y, mapped$MNI152_z),
               c(10.1231, 11.6686, 15.9175), tolerance = 1e-4)
})

# ---- Available templates ---------------------------------------------------------

test_that("get_available_morph_to_template lists the templates in this process", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  local_yael_rpyants_guard()
  local_yael_no_jobs()
  toy <- yael_toy_warped_subject(
    "YAELReaderAvailable", c("mni_icbm152_nlin_sym_09b", "mni_icbm152_nlin_asym_09a"))
  expect_identical(get_available_morph_to_template(toy$subject),
                   c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_sym_09b"))

  # a transform without its other direction is no mapping
  unlink(list.files(file.path(toy$work_path, "normalization", "transformations"),
                    pattern = "^sub-[^_]+_from-MNI152NLin2009aAsym", full.names = TRUE))
  expect_identical(get_available_morph_to_template(toy$subject), "mni_icbm152_nlin_sym_09b")

  unlink(file.path(toy$work_path, "normalization"), recursive = TRUE)
  expect_identical(get_available_morph_to_template(toy$subject), character(0L))
})

# ---- Atlases ---------------------------------------------------------------------

test_that("generate_atlases_from_template warps an atlas without 'Python'", {
  skip_on_cran()
  skip_if_no_native_yael()
  skip_if_not_installed("RNifti")
  local_yael_sandbox()
  local_yael_rpyants_guard()
  local_yael_no_jobs()
  template_name <- "mni_icbm152_nlin_asym_09a"
  toy <- yael_toy_warped_subject("YAELReaderAtlas", template_name)
  root <- tempfile(pattern = "ravecore-yael-atlas-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  yael_toy_write_atlas(toy, file.path(root, "toy.nii.gz"))

  generate_atlases_from_template(toy$subject, atlas_folders = root,
                                 template_name = template_name, surfaces = FALSE)

  # the label moves from x = 10 to 12 mm (voxels 31 to 33) by -5 mm
  atlas_path <- file.path(toy$work_path, "atlases", "toy.nii.gz")
  expect_equal(label_columns(yael_native_read_volume(atlas_path)), 26:28)
  # 'ANTs' writes `float32` (16), whatever the values are
  expect_equal(as.integer(RNifti::niftiHeader(atlas_path)$datatype), 16L)
})

test_that("generate_atlases_from_template accepts every form of `extra_transform`", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  local_yael_rpyants_guard()
  local_yael_no_jobs()
  template_name <- "mni_icbm152_nlin_asym_09a"
  toy <- yael_toy_warped_subject("YAELReaderExtra", template_name)
  root <- tempfile(pattern = "ravecore-yael-extra-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  atlas_folder <- file.path(root, "atlas")
  dir.create(atlas_folder)
  yael_toy_write_atlas(toy, file.path(atlas_folder, "toy.nii.gz"))

  # the image moves by +10 mm, so its points move by -10 mm
  extra <- diag(4)
  extra[1, 4] <- 10
  ants_path <- file.path(root, "extra.mat")
  ravetools::write_ants_transform(extra, ants_path)
  text_path <- file.path(root, "extra.txt")
  utils::write.table(extra, text_path, row.names = FALSE, col.names = FALSE)
  extra_path <- file.path(toy$work_path, "atlases_extra", "toy.nii.gz")

  # the same transform as 'ANTs' stores it, from 'LPS' to 'LPS'
  extra_lps <- diag(4)
  extra_lps[1, 4] <- -10

  forms <- list(
    list(extra_transform = extra, extra_transform_type = "native", surfaces = FALSE),
    list(extra_transform = extra_lps, extra_transform_type = "ants", surfaces = FALSE),
    list(extra_transform = ants_path, extra_transform_type = "ants", surfaces = FALSE),
    list(extra_transform = text_path, extra_transform_type = "native", surfaces = TRUE)
  )
  for (form in forms) {
    unlink(dirname(extra_path), recursive = TRUE)
    do.call(generate_atlases_from_template, c(
      list(subject = toy$subject, atlas_folders = atlas_folder,
           template_name = template_name), form))
    expect_true(file.exists(extra_path))
    # the first-axis origin is -20 mm in the image the atlases are mapped to
    expect_equal(attr(yael_native_read_volume(extra_path), "vox2ras")[1, 4], -30,
                 tolerance = 1e-4)
  }
  expect_true(all(file.exists(file.path(dirname(extra_path), c("toy.gii", "toy.stl")))))
})

test_that("generate_atlas_YBA maps the atlas without 'Python'", {
  skip_on_cran()
  skip_if_no_native_yael()
  # the YBA colormap ships with 'threeBrain' newer than the CRAN release 1.2.0
  skip_if(!nzchar(system.file("palettes", "datacube2", "YBA696ColorLUT.json",
                              package = "threeBrain")),
          message = "threeBrain lacks the YBA696 colormap")
  local_yael_sandbox()
  local_yael_rpyants_guard()
  local_yael_no_jobs()
  template_name <- "mni_icbm152_nlin_asym_09a"
  toy <- yael_toy_warped_subject("YAELReaderYBA", template_name)
  # the function asks for an existing subject
  toy$subject$initialize_paths(include_freesurfer = FALSE)
  root <- tempfile(pattern = "ravecore-yael-yba-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  atlas_path <- file.path(root, "YBA696.nii.gz")
  yael_toy_write_atlas(toy, atlas_path)
  # the real atlas is a download
  testthat::local_mocked_bindings(
    ieegio_sample_data = function(file, ...) { atlas_path },
    .package = "ieegio"
  )

  results <- generate_atlas_YBA(toy$subject, name = "YBA696", template_names = template_name,
                                verbose = FALSE, create_report = FALSE)

  expect_equal(label_columns(yael_native_read_volume(results$atlas_path)), 26:28)
})

# ---- Reading headers -------------------------------------------------------------

test_that("reading a header gives no warning", {
  skip_on_cran()
  path <- tempfile(fileext = ".nii.gz")
  on.exit({ unlink(path) }, add = TRUE)
  yael_native_write_volume(array(1, c(4L, 4L, 4L)), path, vox2ras = diag(4))

  expect_silent(header <- yael_native_read_header(path))
  expect_equal(header$dim, c(4L, 4L, 4L))
})

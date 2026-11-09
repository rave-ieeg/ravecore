# The two engines give the same outputs. Every exported function that moved from
# the 'rpyANTs' engine to the native one runs on both, on one subject that
# 'rpyANTs' imported and normalized, as existing subjects were. These functions
# now always ask for the native engine, so the other side is forced through a
# mock of `as_yael_process`. The tests start 'Python': they skip on 'CRAN' and
# without 'ANTs'. Set `RAVECORE_YAEL_NATIVE_T1W` to a `T1w` image to repeat the
# comparison on a real brain. Every test runs in the sandbox

# ---- Helpers ---------------------------------------------------------------------

yael_engine_template <- "mni_icbm152_nlin_asym_09a"

# Skips unless both engines can run; sandboxes the calling frame, refuses child
# jobs, and allows 'Python' (the sandbox restores the variable)
local_yael_engines <- function(env = parent.frame()) {
  testthat::skip_on_cran()
  # 'fs' refuses paths longer than 260 bytes on Windows (r-lib/fs#420)
  testthat::skip_on_os("windows")
  skip_if_no_native_yael()
  skip_if_old_testthat()
  testthat::skip_if_not_installed("rpyANTs")
  testthat::skip_if_not_installed("RNifti")
  testthat::skip_if_not(isTRUE(rpyANTs::ants_available()), "'ANTs' is not available")
  local_yael_sandbox(env = env)
  local_yael_no_jobs(env = env)
  Sys.unsetenv("IEEGIO_NO_PYTHON")
  local_yael_python_tempdir(env = env)
  invisible()
}

# Points the temporary folder of 'Python' (`tempfile.tempdir`) to an empty
# folder for the rest of the calling frame and removes it on exit:
# `ants.atropos` writes its posteriors there (`antsr*prob*.nii.gz`) and leaves
# them, which `R CMD check` reports as detritus in the temporary directory
local_yael_python_tempdir <- function(env = parent.frame()) {
  py_tempfile <- rpymat::import("tempfile")
  # a 'Python' object: `None`, or the folder already in use
  old <- rpymat::py_get_attr(py_tempfile, "tempdir")
  root <- tempfile(pattern = "ravecore-yael-python-")
  dir.create(root)
  rpymat::py_set_attr(py_tempfile, "tempdir", root)
  restore <- function() {
    rpymat::py_set_attr(py_tempfile, "tempdir", old)
    unlink(root, recursive = TRUE)
  }
  do.call(base::on.exit, list(as.call(list(restore)), TRUE, FALSE), envir = env)
  invisible(root)
}

# Uses a copy of `files` as the template for the rest of the calling frame:
# 'rpyANTs' writes a cache file into the template folder
local_yael_engine_template <- function(files, root, env = parent.frame()) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  file.copy(files, root, overwrite = TRUE)
  info <- yael_toy_template_info(root, name = yael_engine_template)
  call_rpyants_real <- call_rpyants
  testthat::local_mocked_bindings(
    yael_native_template = function(template_name = rpyants_builtin_templates()) {
      info
    },
    call_rpyants = function(.name, ...) {
      if (identical(.name, "ensure_template")) { return(root) }
      call_rpyants_real(.name, ...)
    },
    .package = "ravecore", .env = env
  )
  invisible(info)
}

# The toy subject imported and normalized by 'rpyANTs', built once and copied
# into the sandbox of each test, with a toy `ants` folder for `rave_brain()`
local_yael_engine_subject <- function(env = parent.frame()) {
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  cache_root <- file.path(tempdir(), "ravecore-yael-engines")
  dir.create(cache_root, recursive = TRUE, showWarnings = FALSE)
  local_yael_engine_template(c(template$t1, template$mask, template$atropos),
                             file.path(cache_root, "template"), env = env)
  subject_id <- "YAEL/YAELEngines"
  work_path <- as.character(as_yael_process(subject_id, engine = "native")$work_path)
  cached <- file.path(cache_root, basename(work_path))
  if (dir.exists(cached)) {
    dir_create2(dirname(work_path))
    file.copy(cached, dirname(work_path), recursive = TRUE)
  } else {
    ants_process <- as_yael_process(subject_id)
    ants_process$set_input_image(native$t1, type = "T1w")
    ants_process$map_to_template(yael_engine_template, use_images = "T1w",
                                 use_antspynet = FALSE, verbose = FALSE)
    process <- as_yael_process(subject_id, engine = "native")
    process$construct_ants_folder_from_template(template_name = NULL)
    process$write_talairach_xfm(yael_engine_template)
    file.copy(work_path, cache_root, recursive = TRUE)
  }
  process <- as_yael_process(subject_id, engine = "native")
  list(process = process, subject = process$get_subject(), work_path = work_path,
       template = template, native = native)
}

# Evaluates `code` with every `as_yael_process()` on the engine `target`
with_yael_engine <- function(target, code) {
  as_yael_process_real <- as_yael_process
  testthat::with_mocked_bindings(
    code,
    as_yael_process = function(subject, ..., engine) {
      as_yael_process_real(subject, ..., engine = target)
    },
    .package = "ravecore"
  )
}

# Mean distance between the vertices of two surfaces, in both directions
surface_gap <- function(path1, path2) {
  vertices <- function(path) {
    surface <- ieegio::read_surface(path)
    t(surface$geometry$transforms[[1]] %*% surface$geometry$vertices)[, 1:3]
  }
  vertices1 <- vertices(path1)
  vertices2 <- vertices(path2)
  mean(c(ravetools::vcg_kdtree_nearest(target = vertices2, query = vertices1, k = 1)$distance,
         ravetools::vcg_kdtree_nearest(target = vertices1, query = vertices2, k = 1)$distance))
}

# Compares two output folders: the same files, and for every volume the same
# grid, the same `NIfTI` type, and the same voxels (labels may differ in a share
# of `label_share` voxels; other values up to `value_gap`); surfaces are within
# `gap` mm of each other
expect_same_atlases <- function(ants_root, native_root, label_share = 0,
                                value_gap = 1e-4, gap = 1e-3) {
  files <- sort(list.files(ants_root, recursive = TRUE))
  expect_gt(length(files), 0)
  expect_identical(sort(list.files(native_root, recursive = TRUE)), files)
  for (file in files) {
    ants_path <- file.path(ants_root, file)
    native_path <- file.path(native_root, file)
    if (grepl("\\.(nii|nii\\.gz)$", file)) {
      ants_volume <- yael_native_read_volume(ants_path)
      native_volume <- yael_native_read_volume(native_path)
      expect_identical(dim(native_volume), dim(ants_volume), label = file)
      expect_lt(max(abs(attr(native_volume, "vox2ras") - attr(ants_volume, "vox2ras"))), 1e-3,
                label = file)
      expect_identical(as.integer(RNifti::niftiHeader(native_path)$datatype),
                       as.integer(RNifti::niftiHeader(ants_path)$datatype), label = file)
      ants_voxels <- as.vector(ants_volume)
      native_voxels <- as.vector(native_volume)
      if (all(ants_voxels == round(ants_voxels))) {
        expect_lte(mean(ants_voxels != native_voxels), label_share, label = file)
      } else {
        expect_lt(max(abs(ants_voxels - native_voxels)), value_gap, label = file)
      }
    } else if (grepl("\\.gii$", file)) {
      expect_lt(surface_gap(ants_path, native_path), gap, label = file)
    }
  }
}

# Compares two tables of `transform_point_to_template`: the same missing values,
# and every column within `gap` mm
expect_same_table <- function(ants_table, native_table, gap = 1e-3) {
  expect_identical(names(native_table), names(ants_table))
  ants_values <- as.matrix(ants_table)
  native_values <- as.matrix(native_table)
  expect_identical(is.na(native_values), is.na(ants_values))
  expect_lt(max(abs(ants_values - native_values), na.rm = TRUE), gap)
}

# Atlases that engines tend to read differently, on the toy template
yael_engine_write_atlases <- function(template, root) {
  dir.create(file.path(root, "sub"), recursive = TRUE)
  dim <- template$dim
  vox2ras <- template$vox2ras
  # large label IDs, on the grid of the template
  labels <- yael_toy_template_labels(dim, vox2ras) * 1001
  yael_native_write_volume(labels, file.path(root, "labels.nii.gz"), vox2ras = vox2ras)
  # a probability map on a finer grid
  file.copy(template$atropos[[3]], file.path(root, "prob_fine.nii.gz"))
  # a mask, not compressed, in a sub-folder
  mask <- yael_native_read_volume(template$mask)
  yael_native_write_volume((mask > 0) * 1, file.path(root, "sub", "mask.nii"), vox2ras = vox2ras)
  # the same labels stored from the other side
  vox2ras_las <- vox2ras
  vox2ras_las[1:3, 4] <- vox2ras[1:3, 4] + vox2ras[1:3, 1] * (dim[[1]] - 1)
  vox2ras_las[1:3, 1] <- -vox2ras[1:3, 1]
  yael_native_write_volume(labels[dim[[1]]:1, , ], file.path(root, "labels_las.nii.gz"),
                           vox2ras = vox2ras_las)
  # a rotated grid
  angle <- 10 * pi / 180
  rotation <- diag(4)
  rotation[1:2, 1:2] <- c(cos(angle), sin(angle), -sin(angle), cos(angle))
  prob <- yael_native_read_volume(template$atropos[[4]])
  yael_native_write_volume(array(as.vector(prob), dim(prob)),
                           file.path(root, "prob_rotated.nii.gz"),
                           vox2ras = rotation %*% attr(prob, "vox2ras"))
  invisible(root)
}

# ---- get_available_morph_to_template ---------------------------------------------

test_that("get_available_morph_to_template gives the same templates on both engines", {
  local_yael_engines()
  dim <- c(4L, 4L, 4L)
  t1_path <- tempfile(fileext = ".nii.gz")
  on.exit({ unlink(t1_path) }, add = TRUE)
  yael_native_write_volume(array(1, dim), t1_path, vox2ras = diag(4))
  code <- "YAELEnginesMorph"
  process <- as_yael_process(sprintf("YAEL/%s", code), engine = "native")
  process$set_input_image(t1_path, type = "T1w")
  subject <- process$get_subject()
  transform_dir <- file.path(as.character(process$work_path), "normalization", "transformations")
  transforms <- function(sub, camel, directions = c("forward", "inverse")) {
    c(
      if ("forward" %in% directions) {
        sprintf("sub-%s_from-T1w_to-%s_desc-affine+SyN_ants%s", sub, camel, c("0.mat", "1.nii.gz"))
      },
      if ("inverse" %in% directions) {
        sprintf("sub-%s_from-%s_to-T1w_desc-affine+SyN_ants%s", sub, camel, c("0.nii.gz", "1.mat"))
      }
    )
  }
  expect_templates <- function(expected) {
    ants <- with_yael_engine("rpyANTs", get_available_morph_to_template(subject))
    native <- suppressWarnings(with_yael_engine("native", get_available_morph_to_template(subject)))
    expect_identical(ants, expected)
    expect_identical(native, expected)
  }

  # no normalization
  expect_templates(character(0L))

  # complete mappings
  dir.create(transform_dir, recursive = TRUE)
  file.create(file.path(transform_dir, transforms(code, "MNI152NLin2009aAsym")))
  file.create(file.path(transform_dir, transforms(code, "fsaverage")))
  expect_templates(c("mni_icbm152_nlin_asym_09a", "fsaverage"))

  # one direction is no mapping
  file.create(file.path(transform_dir, transforms(code, "MNI152NLin2009bAsym", "forward")))
  expect_templates(c("mni_icbm152_nlin_asym_09a", "fsaverage"))

  # the subject in another letter case
  file.create(file.path(transform_dir, transforms(tolower(code), "MNI152NLin2009cAsym")))
  expect_templates(c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09c", "fsaverage"))

  # a renamed folder: the transforms and the image come from one other subject
  unlink(list.files(transform_dir, full.names = TRUE))
  file.create(file.path(transform_dir, transforms("Previous", "MNI152NLin2009aAsym")))
  input_dir <- file.path(as.character(process$work_path), "inputs", "anat")
  file.rename(process$get_input_image("T1w"),
              file.path(input_dir, "sub-Previous_ses-preop_desc-preproc_T1w.nii.gz"))
  expect_templates("mni_icbm152_nlin_asym_09a")
})

# ---- generate_atlases_from_template ----------------------------------------------

test_that("generate_atlases_from_template gives the same files on both engines", {
  local_yael_engines()
  toy <- local_yael_engine_subject()
  root <- tempfile(pattern = "ravecore-yael-engines-atlas-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  atlas_folder <- yael_engine_write_atlases(toy$template, file.path(root, "atlas"))

  # the 'ANTs' transform of this subject, with a rotation and a center
  mapping <- toy$process$get_template_mapping(yael_engine_template)
  affine_path <- ants_mapping_affine_paths(mapping)[[1]]
  lps_to_ras <- diag(c(-1, -1, 1, 1))
  expect_equal(ravetools::read_ants_transform(affine_path),
               lps_to_ras %*% rpyANTs::as_ANTsTransform(affine_path)[] %*% lps_to_ras,
               tolerance = 1e-6, ignore_attr = TRUE)

  for (engine in c("rpyANTs", "native")) {
    with_yael_engine(engine, generate_atlases_from_template(
      toy$subject, atlas_folders = atlas_folder, template_name = yael_engine_template,
      any_mni152 = FALSE, surfaces = TRUE, extra_transform = affine_path))
    for (folder in c("atlases", "atlases_extra")) {
      file.rename(file.path(toy$work_path, folder),
                  file.path(root, sprintf("%s-%s", folder, engine)))
    }
  }

  expect_same_atlases(file.path(root, "atlases-rpyANTs"), file.path(root, "atlases-native"))
  expect_same_atlases(file.path(root, "atlases_extra-rpyANTs"),
                      file.path(root, "atlases_extra-native"))
})

# ---- generate_atlas_YBA ----------------------------------------------------------

test_that("generate_atlas_YBA gives the same atlas and labels on both engines", {
  local_yael_engines()
  toy <- local_yael_engine_subject()
  toy$subject$initialize_paths(include_freesurfer = FALSE)
  root <- tempfile(pattern = "ravecore-yael-engines-yba-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  # the real atlas is a download; labels 1 to 6 are named in its color table
  atlas_path <- file.path(root, "YBA696.nii.gz")
  yael_native_write_volume(
    yael_toy_template_labels(toy$template$dim, toy$template$vox2ras) * 1,
    atlas_path, vox2ras = toy$template$vox2ras)
  testthat::local_mocked_bindings(
    ieegio_sample_data = function(file, ...) { atlas_path },
    .package = "ieegio"
  )

  # contacts inside the toy brain
  brain <- rave_brain(toy$subject, include_electrodes = FALSE)
  voxels <- which(toy$native$truth > 0, arr.ind = TRUE)
  voxels <- voxels[seq(1, nrow(voxels), length.out = 12), , drop = FALSE] - 1
  scanner <- toy$native$vox2ras %*% rbind(t(voxels), 1)
  tkr <- t(brain$Torig %*% solve(brain$Norig) %*% scanner)[, 1:3]
  utils::write.csv(
    data.frame(Electrode = seq_len(nrow(tkr)), Coord_x = tkr[, 1], Coord_y = tkr[, 2],
               Coord_z = tkr[, 3], Label = sprintf("A%d", seq_len(nrow(tkr))),
               LabelPrefix = "A", SignalType = "LFP"),
    file.path(toy$subject$meta_path, "electrodes.csv"), row.names = FALSE)

  labels <- list()
  for (engine in c("rpyANTs", "native")) {
    results <- with_yael_engine(engine, generate_atlas_YBA(
      toy$subject, name = "YBA696", template_names = yael_engine_template,
      verbose = FALSE, create_report = FALSE))
    dir.create(file.path(root, engine))
    file.copy(results$atlas_path, file.path(root, engine))
    labels[[engine]] <- utils::read.csv(results$label_path)
  }

  expect_same_atlases(file.path(root, "rpyANTs"), file.path(root, "native"))
  expect_equal(labels$native, labels$rpyANTs)
  # the comparison means nothing if every contact is unknown
  expect_true(any(unlist(lapply(labels$native[-1], function(x) {
    is.character(x) & !grepl("^(Unknown)?$", x)
  }))))
})

# ---- transform_point_to_template -------------------------------------------------

test_that("transform_point_to_template gives the same table on both engines", {
  local_yael_engines()
  toy <- local_yael_engine_subject()
  native <- toy$native
  lower <- as.vector(native$vox2ras %*% c(0, 0, 0, 1))[1:3]
  upper <- as.vector(native$vox2ras %*% c(native$dim - 1, 1))[1:3]
  center <- (lower + upper) / 2
  # inside the brain, at the edges of the image, outside the warp field
  positions <- rbind(center, center + c(10, -8, 6), center + c(-14, 12, -9),
                     lower, upper, lower - 40, upper + 40, c(upper[[1]] + 200, center[2:3]))
  dimnames(positions) <- NULL
  map_points <- function(engine, positions, ...) {
    with_yael_engine(engine, transform_point_to_template(
      toy$subject, positions = positions, verbose = FALSE, ...))
  }

  for (method in c("auto", "nonlinear", "affine")) {
    for (space in c("scannerRAS", "tkrRAS")) {
      for (flip in c(FALSE, TRUE)) {
        expect_same_table(
          map_points("rpyANTs", positions, volumetric_transform = method, space = space,
                     flip_hemisphere = flip),
          map_points("native", positions, volumetric_transform = method, space = space,
                     flip_hemisphere = flip))
      }
    }
  }

  # the warp is used: otherwise both engines could agree on the affine
  nonlinear <- map_points("native", positions[1:3, ], volumetric_transform = "auto")
  affine <- map_points("native", positions[1:3, ], volumetric_transform = "affine")
  expect_gt(max(abs(nonlinear$MNI152_x - affine$MNI152_x)), 0.1)

  # a missing point and a single point
  expect_same_table(
    map_points("rpyANTs", rbind(center, NA, center + 3), volumetric_transform = "nonlinear"),
    map_points("native", rbind(center, NA, center + 3), volumetric_transform = "nonlinear"))
  expect_same_table(
    map_points("rpyANTs", center, volumetric_transform = "nonlinear"),
    map_points("native", center, volumetric_transform = "nonlinear"))

  # the last template of the fallback, whose results need another conversion
  transform_dir <- file.path(toy$work_path, "normalization", "transformations")
  transforms <- list.files(transform_dir, pattern = "MNI152NLin2009aAsym")
  expect_length(transforms, 4)
  file.rename(file.path(transform_dir, transforms),
              file.path(transform_dir, gsub("MNI152NLin2009aAsym", "fsaverage", transforms)))
  expect_same_table(
    map_points("rpyANTs", positions, volumetric_transform = "nonlinear"),
    map_points("native", positions, volumetric_transform = "nonlinear"))
})

# ---- transform_thinfilm_to_mni152 ------------------------------------------------

# The function itself did not change, and it only depends on the engine through
# this call; a full run needs a 'FreeSurfer' template and a thin-film geometry
test_that("the volumetric step of transform_thinfilm_to_mni152 is the same on both engines", {
  local_yael_engines()
  toy <- local_yael_engine_subject()
  corners <- as.matrix(expand.grid(x = c(-16, -8, 0), y = c(-12, 0, 12), z = 10))
  dimnames(corners) <- NULL

  for (method in c("auto", "nonlinear")) {
    tables <- lapply(c("rpyANTs", "native"), function(engine) {
      with_yael_engine(engine, as.data.frame(transform_point_to_template(
        subject = toy$subject, positions = corners, space = "tkrRAS",
        mapping_method = "volumetric", flip_hemisphere = FALSE,
        volumetric_transform = method)))
    })
    expect_same_table(tables[[1]], tables[[2]])
  }
})

# ---- A real brain ----------------------------------------------------------------

test_that("both engines agree on a real image that 'rpyANTs' imported", {
  t1w_path <- Sys.getenv("RAVECORE_YAEL_NATIVE_T1W", unset = "")
  skip_if(!nzchar(t1w_path), "set RAVECORE_YAEL_NATIVE_T1W to a T1w image to run this test")
  local_yael_engines()
  t1w_path <- normalizePath(t1w_path, winslash = "/", mustWork = TRUE)
  root <- tempfile(pattern = "ravecore-yael-engines-real-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  # the template is read here, and only its copy is used
  template_files <- file.path(rpyANTs::ensure_template(yael_engine_template),
                              c("T1.nii.gz", "T1_brainmask.nii.gz",
                                sprintf("atropos_%d.nii.gz", 0:7)))
  template_files <- template_files[file.exists(template_files)]
  info <- local_yael_engine_template(template_files, file.path(root, "template"))

  subject_id <- "YAEL/YAELEnginesReal"
  ants_process <- as_yael_process(subject_id)
  ants_process$set_input_image(t1w_path, type = "T1w")
  ants_process$map_to_template(yael_engine_template, use_images = "T1w",
                               use_antspynet = FALSE, verbose = FALSE)
  process <- as_yael_process(subject_id, engine = "native")
  work_path <- as.character(process$work_path)

  atlas_folder <- file.path(root, "atlas")
  dir.create(atlas_folder)
  file.copy(file.path(info$root, "atropos_0.nii.gz"), file.path(atlas_folder, "labels.nii.gz"))
  file.copy(file.path(info$root, "atropos_3.nii.gz"), file.path(atlas_folder, "prob.nii.gz"))
  file.copy(info$mask, file.path(atlas_folder, "brainmask.nii.gz"))
  for (engine in c("rpyANTs", "native")) {
    with_yael_engine(engine, generate_atlases_from_template(
      process$get_subject(), atlas_folders = atlas_folder,
      template_name = yael_engine_template, any_mni152 = FALSE, surfaces = TRUE))
    file.rename(file.path(work_path, "atlases"), file.path(root, engine))
  }
  expect_same_atlases(file.path(root, "rpyANTs"), file.path(root, "native"),
                      label_share = 1e-4, gap = 0.01)

  # points inside the brain
  mask <- process$transform_image_from_template(info$mask, yael_engine_template,
                                                interpolator = "nearestNeighbor",
                                                verbose = FALSE)
  set.seed(42)
  voxels <- which(array(as.vector(mask[]) > 0, dim(mask)[1:3]), arr.ind = TRUE) - 1
  voxels <- voxels[sample(nrow(voxels), 5000), , drop = FALSE]
  points <- t(mask$transforms$vox2ras %*% rbind(t(voxels), 1))[, 1:3]
  expect_lt(max(abs(
    ants_process$transform_points_to_template(points, yael_engine_template, verbose = FALSE) -
      process$transform_points_to_template(points, yael_engine_template, verbose = FALSE)
  )), 1e-3)
  expect_lt(max(abs(
    ants_process$transform_points_from_template(points, yael_engine_template, verbose = FALSE) -
      process$transform_points_from_template(points, yael_engine_template, verbose = FALSE)
  )), 1e-3)
})

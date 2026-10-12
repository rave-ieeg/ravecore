# `YAELNativeProcess` (`R/class011-YAEL_native_process.R`) and the `engine`
# argument of `as_yael_process()`: the interface of `YAELProcess`, the queries
# of rpyANTs `YAELPreprocess` on fabricated files (ports of the rpyANTs tests
# `test-yael.R` and `test-yael-template-mapping.R`), and the whole process on
# toy images (see `helper-yael-native.R`). Every test that builds a process
# runs in the sandbox; nothing reads real images, and 'Python' is only started
# by the 'ANTs' oracle at the end, which needs `RAVECORE_YAEL_ANTS_ORACLE`

# ---- Helpers ---------------------------------------------------------------------

read_tsv_matrix <- function(path) {
  unname(as.matrix(utils::read.table(path, header = FALSE)))
}

# The 3 x 4 matrix of a 'FreeSurfer' `talairach.xfm`
read_xfm_matrix <- function(path) {
  lines <- readLines(path)
  start <- grep("^Linear_Transform", lines)
  values <- gsub(";", "", lines[start + 1:3], fixed = TRUE)
  do.call(rbind, lapply(strsplit(trimws(values), "\\s+"), as.numeric))
}

# Points of the toy subject's brain, at least 2 voxels away from the borders
# of both grids (the outermost layer of a warp field has no displacement)
yael_toy_inner_points <- function(native, template, process,
                                  template_name = "mni_icbm152_nlin_asym_09a") {
  ijk <- which(native$truth > 0, arr.ind = TRUE) - 1
  inner <- apply(ijk, 1, function(v) all(v >= 2 & v <= native$dim - 3))
  points <- yael_toy_grid(native$dim, native$vox2ras)[which(native$truth > 0)[inner], ]
  to_template <- process$transform_points_to_template(points, template_name, verbose = FALSE)
  template_ijk <- t(solve(template$vox2ras) %*% rbind(t(to_template), 1))[, 1:3]
  inner <- apply(template_ijk, 1, function(v) all(v >= 2 & v <= template$dim - 3))
  list(native = points[inner, , drop = FALSE],
       template = to_template[inner, , drop = FALSE])
}

# ---- Interface ---------------------------------------------------------------------

test_that("YAELNativeProcess has the methods, arguments and fields of YAELProcess", {
  defaults <- function(f) {
    fmls <- formals(f)
    structure(
      vapply(seq_along(fmls), function(i) paste(deparse(fmls[[i]]), collapse = " "), ""),
      names = names(fmls))
  }
  native <- YAELNativeProcess$public_methods
  ants <- YAELProcess$public_methods
  expect_setequal(names(native), names(ants))
  for (name in names(ants)) {
    expect_identical(defaults(native[[name]]), defaults(ants[[name]]), label = name)
  }
  expect_setequal(names(YAELNativeProcess$active), names(YAELProcess$active))
  for (name in names(YAELProcess$active)) {
    expect_identical(defaults(YAELNativeProcess$active[[name]]),
                     defaults(YAELProcess$active[[name]]), label = name)
  }
  expect_identical(YAELNativeProcess$get_inherit(), YAELProcess$get_inherit())
  # no 'Python' implementation behind the native class
  expect_false(".impl" %in% names(YAELNativeProcess$private_methods))
  expect_identical(defaults(as_yael_process),
                   c(subject = "", ... = "", engine = "c(\"rpyANTs\", \"native\")"))
})

test_that("as_yael_process(engine = 'native') builds a serializable YAELNativeProcess", {
  local_yael_sandbox()
  local_yael_rpyants_guard()

  process <- as_yael_process("YAEL/YAELNativeSerialize", engine = "native")
  expect_s3_class(process, "YAELNativeProcess")
  expect_false(inherits(process, "YAELProcess"))
  expect_equal(process$subject_code, "YAELNativeSerialize")
  expect_equal(process$image_types, YAEL_IMAGE_TYPES)
  expect_equal(process$get_subject()$subject_id, "YAEL/YAELNativeSerialize")
  marshal <- process$`@marshal`()
  expect_equal(marshal$r6_generator, "YAELNativeProcess")

  raw <- serialize(process, NULL, refhook = ravepipeline::rave_serialize_refhook)
  process2 <- unserialize(raw, refhook = ravepipeline::rave_unserialize_refhook)
  expect_s3_class(process2, "YAELNativeProcess")
  expect_equal(process2$subject_code, process$subject_code)
  expect_equal(process2$image_types, process$image_types)
  expect_equal(process2$work_path, process$work_path)

  # subject code without project, legacy argument, default engine
  expect_s3_class(as_yael_process("YAELNativeSerialize", engine = "native"),
                  "YAELNativeProcess")
  expect_equal(
    as_yael_process("YAELNativeSerialize", engine = "native")$work_path,
    process$work_path)
  # (the warning about `subject_code` is logged)
  utils::capture.output(
    legacy <- YAELNativeProcess$new(subject_code = "YAELNativeSerialize"),
    type = "message")
  expect_equal(legacy$subject_code, "YAELNativeSerialize")
  expect_s3_class(as_yael_process("YAEL/YAELNativeSerialize"), "YAELProcess")
  expect_error(as_yael_process("YAEL/YAELNativeSerialize", engine = "python"))

  # image types: the setter adds types; existing images add their types
  process$image_types <- "MEG"
  expect_equal(process$image_types, c(YAEL_IMAGE_TYPES, "MEG"))
  input_dir <- file.path(process$work_path, "inputs", "anat")
  dir.create(input_dir, recursive = TRUE)
  file.create(file.path(input_dir, "sub-YAELNativeSerialize_ses-preop_desc-preproc_DWI.nii.gz"))
  expect_true("DWI" %in% as_yael_process("YAEL/YAELNativeSerialize", engine = "native")$image_types)
})

# ---- Queries on fabricated files (ports of the rpyANTs tests) -----------------------

test_that("template mappings handle stray files and renamed subjects like rpyANTs", {
  local_yael_sandbox()
  local_yael_rpyants_guard()

  work_path <- tempfile(pattern = "ravecore-yael-mapping-")
  on.exit({ unlink(work_path, recursive = TRUE) }, add = TRUE)
  transform_dir <- file.path(work_path, "normalization", "transformations")
  input_dir <- file.path(work_path, "inputs", "anat")
  dir.create(transform_dir, recursive = TRUE)
  dir.create(input_dir, recursive = TRUE)

  # only the file names are parsed, the files can be empty
  transform_files <- function(sub, template = "MNI152NLin2009bAsym") {
    c(
      sprintf("sub-%s_from-T1w_to-%s_desc-affine+SyN_ants0.mat", sub, template),
      sprintf("sub-%s_from-T1w_to-%s_desc-affine+SyN_ants1.nii.gz", sub, template),
      sprintf("sub-%s_from-%s_to-T1w_desc-affine+SyN_ants0.nii.gz", sub, template),
      sprintf("sub-%s_from-%s_to-T1w_desc-affine+SyN_ants1.mat", sub, template)
    )
  }
  set_native_image <- function(sub) {
    unlink(list.files(input_dir, full.names = TRUE))
    file.create(file.path(input_dir, sprintf("sub-%s_ses-preop_desc-preproc_T1w.nii.gz", sub)))
  }
  get_mapping <- function(subject_code, relative = TRUE,
                          template = "mni_icbm152_nlin_asym_09b") {
    process <- YAELNativeProcess$new(subject_code, imaging_path = work_path)
    process$get_template_mapping(template_name = template, relative = relative)
  }
  transform_names <- function(mapping, which) {
    basename(unlist(mapping[[which]]$transformlist))
  }

  # files without `from`/`to` entities, and names that cannot be parsed, are
  # ignored
  file.create(file.path(transform_dir, c(transform_files("Demo"),
                                         "sub-Demo_desc-foo_ants0.mat",
                                         ".DS_Store")))
  set_native_image("Demo")
  mapping <- get_mapping("Demo")
  expect_equal(transform_names(mapping, "native_to_template"),
               transform_files("Demo")[c(2, 1)])
  expect_equal(transform_names(mapping, "template_to_native"),
               transform_files("Demo")[c(4, 3)])
  expect_equal(mapping[c("type", "template", "using")],
               list(type = "T1w", template = "MNI152NLin2009bAsym",
                    using = "ants.apply_transforms"))
  expect_equal(mapping$native_to_template$transformlist[[1]],
               paste0("normalization/transformations/", transform_files("Demo")[[2]]))
  expect_equal(mapping$template_to_native$whichtoinvert, c(FALSE, FALSE))
  expect_equal(get_mapping("Demo", relative = FALSE)$native_to_template$transformlist,
               file.path(work_path, "normalization", "transformations",
                         transform_files("Demo")[c(2, 1)]))

  # the subject code is matched case-insensitively (without the native image,
  # so the renamed-folder rule below cannot be the reason)
  set_native_image(NULL)
  expect_equal(transform_names(get_mapping("demo"), "native_to_template"),
               transform_files("Demo")[c(2, 1)])
  set_native_image("Demo")

  # renamed subject folder: the transforms and the native image share
  # the old subject code
  expect_warning(mapping <- get_mapping("Renamed"), "Using transforms of subject `Demo`")
  expect_equal(transform_names(mapping, "template_to_native"),
               transform_files("Demo")[c(4, 3)])

  # ... but not when the native image was imported under the new code, or
  # when it is missing
  set_native_image("Renamed")
  expect_null(get_mapping("Renamed"))
  set_native_image(NULL)
  expect_null(get_mapping("Renamed"))

  # transforms from several other subjects are ambiguous
  set_native_image("Demo")
  file.create(file.path(transform_dir, transform_files("Other")))
  expect_null(get_mapping("Renamed"))

  # the subject's own transforms are preferred
  expect_equal(transform_names(get_mapping("Other"), "native_to_template"),
               transform_files("Other")[c(2, 1)])

  # on case-sensitive file systems, two spellings of the subject code are
  # ambiguous rather than merged
  probe <- file.path(work_path, "case-probe")
  file.create(probe)
  if (!file.exists(toupper(probe))) {
    file.create(file.path(transform_dir, transform_files("DEMO")))
    expect_null(get_mapping("demo"))
    expect_equal(transform_names(get_mapping("Demo"), "native_to_template"),
                 transform_files("Demo")[c(2, 1)])
  }

  # the layout of rpyANTs <= 0.0.6 (three transforms) is ordered by `K`; types
  # that are not `ants<K>`, and folders, are ignored
  template_a <- "MNI152NLin2009aAsym"
  legacy <- c(
    sprintf("sub-Demo_from-T1w_to-%s_desc-affine+SyN_ants%s", template_a,
            c("0.mat", "1.mat", "2.nii.gz")),
    sprintf("sub-Demo_from-%s_to-T1w_desc-affine+SyN_ants%s", template_a,
            c("0.nii.gz", "1.mat", "2.mat"))
  )
  file.create(file.path(transform_dir, c(
    legacy,
    sprintf("sub-Demo_from-T1w_to-%s_desc-affine+SyN_ants.mat", template_a),
    sprintf("sub-Demo_from-T1w_to-%s_desc-affine+SyN_antsX.mat", template_a)
  )))
  dir.create(file.path(transform_dir, sprintf(
    "sub-Demo_from-T1w_to-%s_desc-affine+SyN_ants7.mat", template_a)))
  mapping <- get_mapping("Demo", template = "mni_icbm152_nlin_asym_09a")
  expect_equal(transform_names(mapping, "native_to_template"), legacy[c(3, 2, 1)])
  expect_equal(transform_names(mapping, "template_to_native"), legacy[c(6, 5, 4)])
  expect_equal(mapping$native_to_template$whichtoinvert, c(FALSE, FALSE, FALSE))

  # the queries do not create folders
  expect_null(get_mapping("Demo", template = "fsaverage"))
  expect_false(dir.exists(file.path(work_path, "coregistration")))
})

test_that("session labels follow the image types like rpyANTs", {
  preop_types <- c("T1w", "T2w", "FLAIR", "preopCT")
  postop_types <- c("CT", "postopCT", "postopT1w", "postopT2w", "postopFLAIR")
  input_name <- function(type, ...) {
    yael_format_name("Demo", type, "nii.gz", ...,
                     image_types = c(preop_types, postop_types))
  }
  for (type in preop_types) {
    expect_equal(input_name(type),
                 sprintf("sub-Demo_ses-preop_desc-preproc_%s.nii.gz", type))
  }
  for (type in postop_types) {
    expect_equal(input_name(type),
                 sprintf("sub-Demo_ses-postop_desc-preproc_%s.nii.gz", type))
  }
  # the session is only inferred when it is not given
  expect_equal(input_name("postopT1w", ses = "implant"),
               "sub-Demo_ses-implant_desc-preproc_postopT1w.nii.gz")
})

test_that("aligned images are found under earlier session labels and as named on disk, like rpyANTs", {
  local_yael_sandbox()
  local_yael_rpyants_guard()

  root <- tempfile(pattern = "ravecore-yael-aligned-")
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  # rpyANTs <= 0.0.6 labeled `postop*` images as `ses-preop`; the files are
  # empty since only their names are parsed
  work_path <- file.path(root, "session")
  input_dir <- file.path(work_path, "inputs", "anat")
  aligned_dir <- file.path(work_path, "coregistration", "anat")
  dir.create(input_dir, recursive = TRUE)
  dir.create(aligned_dir, recursive = TRUE)
  old_input <- "sub-Demo_ses-preop_desc-preproc_postopT1w.nii.gz"
  old_aligned <- "sub-Demo_ses-preop_space-scanner_desc-preproc_postopT1w.nii.gz"
  new_aligned <- "sub-Demo_ses-postop_space-scanner_desc-preproc_postopT1w.nii.gz"
  file.create(file.path(input_dir, old_input))
  file.create(file.path(aligned_dir, c(old_aligned, ".DS_Store")))

  process <- YAELNativeProcess$new("Demo", image_types = c("T1w", "postopT1w"),
                                   imaging_path = work_path)
  aligned_name <- function() {
    mapping <- process$get_native_mapping("postopT1w", relative = TRUE)
    basename(mapping$mappings$postopT1w_in_T1w)
  }
  expect_equal(basename(process$get_input_image("postopT1w")), old_input)
  expect_equal(aligned_name(), old_aligned)

  # registering the image again writes the aligned image with the new label,
  # next to the old one: the mapping uses the new one, although the old one is
  # listed last
  file.create(file.path(aligned_dir, new_aligned))
  expect_equal(utils::tail(list.files(aligned_dir), 1), old_aligned)
  expect_equal(aligned_name(), new_aligned)

  # the files spell the subject code in lower case; on file systems that
  # ignore the case of file names, a file named with the given subject code
  # "exists", but the name on disk is returned
  work_path <- file.path(root, "case")
  input_dir <- file.path(work_path, "inputs", "anat")
  aligned_dir <- file.path(work_path, "coregistration", "anat")
  dir.create(input_dir, recursive = TRUE)
  dir.create(aligned_dir, recursive = TRUE)
  aligned <- "sub-demo_ses-postop_space-scanner_desc-preproc_postopT1w.nii.gz"
  file.create(file.path(input_dir, "sub-demo_ses-postop_desc-preproc_postopT1w.nii.gz"))
  file.create(file.path(aligned_dir, aligned))
  process <- YAELNativeProcess$new("Demo", image_types = c("T1w", "postopT1w"),
                                   imaging_path = work_path)
  mapping <- process$get_native_mapping("postopT1w", relative = TRUE)
  expect_equal(basename(mapping$mappings$postopT1w_in_T1w), aligned)
})

test_that("queries before any registration give empty results and the errors of rpyANTs", {
  local_yael_sandbox()
  local_yael_rpyants_guard()

  work_path <- tempfile(pattern = "ravecore-yael-queries-")
  on.exit({ unlink(work_path, recursive = TRUE) }, add = TRUE)
  input_dir <- file.path(work_path, "inputs", "anat")
  dir.create(input_dir, recursive = TRUE)
  t1_name <- "sub-X_ses-preop_desc-preproc_T1w.nii.gz"
  t2_name <- "sub-X_ses-preop_desc-preproc_T2w.nii.gz"
  ieegio::write_volume(ieegio::as_ieegio_volume(array(1, c(4, 4, 4)), vox2ras = diag(4)),
                       file.path(input_dir, t1_name))
  file.create(file.path(input_dir, c(t2_name, ".sub-X_ses-preop_desc-preproc_CT.nii.gz")))
  process <- YAELNativeProcess$new("X", imaging_path = work_path)

  # nothing is registered yet: the mapping lists the images only
  mapping <- process$get_native_mapping("T2w", relative = TRUE)
  expect_named(mapping, c("subject", "work_path", "T2w_path", "T1w_path", "mappings"))
  expect_equal(mapping$subject, "X")
  expect_equal(mapping$work_path, work_path)
  expect_equal(mapping$T2w_path, paste0("inputs/anat/", t2_name))
  expect_equal(mapping$T1w_path, paste0("inputs/anat/", t1_name))
  expect_length(mapping$mappings, 0)
  expect_equal(process$get_native_mapping("T2w")$T2w_path,
               file.path(work_path, "inputs", "anat", t2_name))
  expect_named(process$get_native_mapping("T1w"),
               c("subject", "work_path", "T1w_path", "mappings"))
  # hidden files are not images
  mapping <- process$get_native_mapping("CT")
  expect_true("CT_path" %in% names(mapping))
  expect_null(mapping$CT_path)
  expect_null(process$get_input_image("CT"))
  expect_error(process$get_native_mapping("MEG"), "Invalid image type: MEG")
  expect_null(process$get_template_mapping("mni_icbm152_nlin_asym_09a"))
  expect_error(process$get_template_mapping("mni_icbm152_nlin_asym_09a", native_type = "MEG"),
               "Invalid image type: MEG")
  # the queries do not create folders
  expect_setequal(list.files(work_path), "inputs")

  # transforms of the image to T1w: `from` and `to` are case-sensitive, the
  # subject is not checked, names that cannot be parsed are ignored
  transform_dir <- file.path(work_path, "coregistration", "transformations")
  dir.create(transform_dir, recursive = TRUE)
  file.create(file.path(transform_dir, c(
    "sub-Other_from-T2w_to-T1w_desc-rigid_ants.mat",
    "sub-X_from-t2w_to-T1w_desc-rigid_ras2ras.tsv",
    "sub-X_from-T2w_to-T1w_desc-rigid_vox2ras.tsv",
    "sub-X_from-T1w_to-T2w_desc-rigid_ras2ras.tsv",
    "sub-X_from-T2w_to-T1w_extra_desc-rigid_x.tsv",
    ".DS_Store"
  )))
  mapping <- process$get_native_mapping("T2w", relative = TRUE)
  expect_equal(mapping$mappings, list(
    ants = "coregistration/transformations/sub-Other_from-T2w_to-T1w_desc-rigid_ants.mat",
    vox2ras = "coregistration/transformations/sub-X_from-T2w_to-T1w_desc-rigid_vox2ras.tsv"
  ))

  # errors (the texts of rpyANTs, with the method names of this class)
  roi <- file.path(work_path, "roi.nii.gz")
  ieegio::write_volume(ieegio::as_ieegio_volume(array(1, c(4, 4, 4)), vox2ras = diag(4)), roi)
  expect_error(process$register_to_T1w("T1w"), "Cannot register T1w to itself")
  expect_error(process$register_to_T1w("CT"), "Missing image for type `CT`")
  expect_error(process$transform_points_to_template(c(0, 0, 0), "mni_icbm152_nlin_asym_09a"),
               "Missing mapping from T1w to MNI152NLin2009aAsym")
  expect_error(process$transform_points_from_template(c(0, 0, 0), "mni_icbm152_nlin_asym_09a"),
               "Missing mapping from T1w to MNI152NLin2009aAsym")
  expect_error(process$transform_image_from_template(roi, "mni_icbm152_nlin_asym_09a"),
               "Missing mapping from T1w")
  expect_error(process$transform_image_to_template(roi, "mni_icbm152_nlin_asym_09a"),
               "Missing mapping from T1w")
  expect_error(process$write_talairach_xfm("mni_icbm152_nlin_asym_09a"), "No affine")
  # the template is only located once a mapping exists (`ensure_template` is
  # guarded in this test)
  atlas_dir <- file.path(work_path, "atlas")
  dir.create(atlas_dir)
  file.copy(roi, atlas_dir)
  expect_error(
    process$generate_atlas_from_template("mni_icbm152_nlin_asym_09a",
                                         atlas_folder = atlas_dir, surfaces = FALSE),
    "Missing mapping from T1w")
  expect_false(dir.exists(file.path(work_path, "atlases")))
  expect_error(
    process$generate_atlas_from_template("mni_icbm152_nlin_asym_09a",
                                         atlas_folder = file.path(work_path, "none")),
    "No atlases for template")
  expect_error(process$construct_ants_folder_from_template(NULL),
               NA)
  expect_true(file.exists(file.path(work_path, "ants", "mri", "T1.mgz")))
})

# ---- Requirements -------------------------------------------------------------------

test_that("with an older ravetools the native engine stops before it computes or removes anything", {
  skip_if_old_testthat()
  local_yael_sandbox()
  # no template may be located (or downloaded) either
  local_yael_rpyants_guard()
  testthat::local_mocked_bindings(yael_native_ravetools_ok = function() { FALSE })
  expect_error(yael_native_require_ravetools(),
               "requires `ravetools` 0.3.3 or later \\(installed: [0-9.]+\\)")
  root <- tempfile(pattern = "ravecore-yael-requirements-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  t1_path <- yael_native_write_volume(array(1, c(4, 4, 4)), file.path(root, "t1.nii.gz"),
                                      vox2ras = diag(4))

  # the workflow stops before it imports the images; the script can be built
  expect_error(
    yael_preprocess_native(subject = "YAEL/YAELNativeOld", t1w_path = t1_path,
                           normalize_template = "mni_icbm152_nlin_asym_09a",
                           verbose = FALSE),
    "requires `ravetools`")
  process <- as_yael_process("YAEL/YAELNativeOld", engine = "native")
  expect_false(dir.exists(process$work_path))
  expect_no_error(cmd_run_yael_preprocess_native(
    subject = "YAEL/YAELNativeOld", t1w_path = t1_path,
    normalize_template = "mni_icbm152_nlin_asym_09a", dry_run = TRUE, verbose = FALSE))

  # images are imported, but the normalization stops before it removes the
  # stage folder (the cache of an earlier run)
  expect_true(process$set_input_image(t1_path, type = "T1w"))
  work_path <- as.character(process$work_path)
  camel <- "MNI152NLin2009aAsym"
  stage_file <- file.path(work_path, "tmp", sprintf("coregister_T1w_with_%s", camel),
                          "T1.nii.gz")
  dir.create(dirname(stage_file), recursive = TRUE)
  file.create(stage_file)
  expect_error(process$map_to_template("mni_icbm152_nlin_asym_09a", verbose = FALSE),
               "requires `ravetools`")
  expect_true(file.exists(stage_file))

  # with a (fabricated) normalization, the transforms stop as well
  transform_dir <- file.path(work_path, "normalization", "transformations")
  dir.create(transform_dir, recursive = TRUE)
  code <- process$subject_code
  file.create(file.path(transform_dir, c(
    sprintf("sub-%s_from-T1w_to-%s_desc-affine+SyN_ants%s", code, camel, c("0.mat", "1.nii.gz")),
    sprintf("sub-%s_from-%s_to-T1w_desc-affine+SyN_ants%s", code, camel, c("0.nii.gz", "1.mat")))))
  expect_length(process$get_template_mapping("mni_icbm152_nlin_asym_09a")$native_to_template$transformlist, 2)
  template_name <- "mni_icbm152_nlin_asym_09a"
  expect_error(process$transform_points_to_template(c(0, 0, 0), template_name), "requires `ravetools`")
  expect_error(process$transform_points_from_template(c(0, 0, 0), template_name), "requires `ravetools`")
  expect_error(process$transform_image_from_template(t1_path, template_name), "requires `ravetools`")
  expect_error(process$transform_image_to_template(t1_path, template_name), "requires `ravetools`")
  expect_error(process$construct_ants_folder_from_template(template_name), "requires `ravetools`")
  expect_false(dir.exists(file.path(work_path, "ants")))
  atlas_dir <- file.path(root, "atlas")
  dir.create(atlas_dir)
  file.copy(t1_path, atlas_dir)
  expect_error(process$generate_atlas_from_template(template_name, atlas_folder = atlas_dir,
                                                    surfaces = FALSE),
               "requires `ravetools`")
  expect_false(dir.exists(file.path(work_path, "atlases")))
  tract_dir <- file.path(root, "tracts")
  dir.create(tract_dir)
  ieegio::write_streamlines(ieegio::as_ieegio_streamlines(list(cbind(1:3, 0, 0))),
                            file.path(tract_dir, "tract.tck"))
  expect_error(process$generate_streamlines_from_template(template_name, tract_dir),
               "requires `ravetools`")
  expect_false(dir.exists(file.path(work_path, "fs")))
})

# ---- Input images -------------------------------------------------------------------

test_that("set_input_image imports images like YAELPreprocess.set_image", {
  local_yael_sandbox()
  local_yael_rpyants_guard()

  root <- tempfile(pattern = "ravecore-yael-input-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  vox2ras <- yael_toy_rotation(3, 10) %*% diag(c(1.5, 1, 2, 1))
  vox2ras[1:3, 4] <- c(-3, 4, -5)
  x <- array(seq_len(60), c(5, 4, 3))
  mgz <- yael_native_write_volume(x, file.path(root, "sub-01_ses-01_acq-ax_T1w.mgz"),
                                  vox2ras = vox2ras)

  process <- as_yael_process("YAEL/YAELNativeInput", engine = "native")
  expect_true(process$set_input_image(mgz, type = "T1w"))
  path <- process$get_input_image("T1w")
  expect_equal(path, file.path(process$work_path, "inputs", "anat",
                               "sub-YAELNativeInput_ses-01_acq-ax_desc-preproc_T1w.nii.gz"))
  y <- yael_native_read_volume(path)
  expect_equal(as.vector(y), as.vector(x))
  expect_equal(attr(y, "vox2ras"), vox2ras, tolerance = 1e-5)
  # the T1w image is also the input of 'FreeSurfer'
  mri_raw <- file.path(process$work_path, "inputs", "MRI", "MRI_RAW.nii.gz")
  expect_equal(unname(tools::md5sum(mri_raw)), unname(tools::md5sum(path)))

  # an existing image is kept unless `overwrite` is true
  expect_warning(expect_false(process$set_input_image(mgz, type = "T1w")),
                 "already exists")
  expect_error(process$set_input_image(mgz, type = "T1w", on_error = "error"),
               "already exists")
  expect_false(process$set_input_image(mgz, type = "T1w", on_error = "ignore"))
  # a missing file is always an error
  expect_error(process$set_input_image(file.path(root, "missing.nii.gz"), type = "T1w",
                                       on_error = "ignore"))

  # the image can replace itself: it is read before the old file is removed
  expect_true(process$set_input_image(path, type = "T1w", overwrite = TRUE))
  expect_equal(as.vector(yael_native_read_volume(path)), as.vector(x))

  # a new name replaces the old image; a name that cannot be parsed gives
  # the subject entity only
  scan <- yael_native_write_volume(x * 2, file.path(root, "sub-01_acq-_x_T1w.nii"),
                                   vox2ras = vox2ras)
  expect_true(process$set_input_image(scan, type = "T1w", overwrite = TRUE))
  expect_false(file.exists(path))
  path <- process$get_input_image("T1w")
  expect_equal(basename(path), "sub-YAELNativeInput_ses-preop_desc-preproc_T1w.nii.gz")
  expect_equal(as.vector(yael_native_read_volume(path)), as.vector(x * 2))
  expect_equal(unname(tools::md5sum(mri_raw)), unname(tools::md5sum(path)))

  # a file that cannot be read is reported and the image is kept (the
  # reader warns as well)
  bad <- file.path(root, "bad.nii.gz")
  writeLines("not an image", bad)
  expect_error(suppressWarnings(
    process$set_input_image(bad, type = "T1w", overwrite = TRUE, on_error = "error")))
  expect_false(suppressWarnings(
    process$set_input_image(bad, type = "T1w", overwrite = TRUE, on_error = "ignore")))
  expect_true(file.exists(path))

  # new types are allowed, 4D images are stored as they are, and both are
  # found by new instances
  frames <- yael_native_write_volume(array(seq_len(120), c(5, 4, 3, 2)),
                                     file.path(root, "dwi.nii.gz"), vox2ras = vox2ras)
  expect_true(process$set_input_image(frames, type = "DWI"))
  expect_true("DWI" %in% process$image_types)
  process2 <- as_yael_process("YAEL/YAELNativeInput", engine = "native")
  expect_true("DWI" %in% process2$image_types)
  dwi <- yael_native_read_volume(process2$get_input_image("DWI"), allow_4d = TRUE)
  expect_equal(dim(dwi), c(5L, 4L, 3L, 2L))
  expect_equal(basename(process2$get_input_image("DWI")),
               "sub-YAELNativeInput_ses-preop_desc-preproc_DWI.nii.gz")
  # `CT` is a postoperative image
  expect_true(process2$set_input_image(mgz, type = "CT"))
  expect_equal(basename(process2$get_input_image("CT")),
               "sub-YAELNativeInput_ses-01_acq-ax_desc-preproc_CT.nii.gz")
  expect_true(process2$set_input_image(scan, type = "postopT1w"))
  expect_equal(basename(process2$get_input_image("postopT1w")),
               "sub-YAELNativeInput_ses-postop_desc-preproc_postopT1w.nii.gz")
})

# ---- Rigid co-registration ----------------------------------------------------------

test_that("register_to_T1w recovers a known CT/MR transform in both directions", {
  skip_on_cran()
  local_yael_sandbox()
  local_yael_rpyants_guard()

  root <- tempfile(pattern = "ravecore-yael-coreg-")
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  ph <- yael_toy_ct_mr(root)
  # brain points in MR (T1w) and CT space
  mr_points <- ph$brain
  ct_points <- ph$A %*% ph$brain

  for (reverse in c(FALSE, TRUE)) {
    process <- as_yael_process(sprintf("YAEL/YAELNativeCoreg%d", reverse), engine = "native")
    work_path <- as.character(process$work_path)
    code <- process$subject_code
    expect_true(process$set_input_image(ph$mr_path, type = "T1w"))
    expect_true(process$set_input_image(ph$ct_path, type = "CT"))
    extras <- file.path(work_path, "coregistration",
                        c("CT_RAW.nii.gz", "MRI_reference.nii.gz", "CT_IJK_to_MR_RAS.txt"))

    # the private part (used before normalizing) leaves the files of the
    # electrode localization alone
    private <- process$.__enclos_env__$private
    private$register_to_T1w_impl("CT", reverse = reverse, verbose = FALSE)
    expect_false(any(file.exists(extras)), label = sprintf("reverse = %s", reverse))
    process$register_to_T1w("CT", reverse = reverse, verbose = FALSE)
    expect_true(all(file.exists(extras)), label = sprintf("reverse = %s", reverse))

    transform_dir <- file.path(work_path, "coregistration", "transformations")
    prefix_to_t1 <- sprintf("sub-%s_from-CT_to-T1w_desc-rigid_", code)
    prefix_from_t1 <- sprintf("sub-%s_from-T1w_to-CT_desc-rigid_", code)
    expect_setequal(list.files(transform_dir), c(
      paste0(prefix_to_t1, c("ants.mat", "ras2ras.tsv", "vox2ras.tsv")),
      paste0(prefix_from_t1, c("ants.mat", "ras2ras.tsv"))
    ))
    label <- sprintf("reverse = %s", reverse)

    # CT RAS -> MR RAS is solve(A); as point maps, the 'ANTs' transform
    # `from-CT_to-T1w` goes from MR to CT (A)
    ct_to_mr <- read_tsv_matrix(file.path(transform_dir, paste0(prefix_to_t1, "ras2ras.tsv")))
    error <- yael_toy_rigid_error(ct_to_mr, solve(ph$A), ct_points)
    expect_lt(error[["angle"]], 1, label = label)
    expect_lt(error[["rms"]], 1, label = label)
    ants_to_t1 <- ravetools::read_ants_transform(file.path(transform_dir, paste0(prefix_to_t1, "ants.mat")))
    error <- yael_toy_rigid_error(ants_to_t1, ph$A, mr_points)
    expect_lt(error[["angle"]], 1, label = label)
    expect_lt(error[["rms"]], 1, label = label)
    expect_equal(ants_to_t1, solve(ct_to_mr), tolerance = 1e-5)
    expect_equal(read_tsv_matrix(file.path(transform_dir, paste0(prefix_from_t1, "ras2ras.tsv"))),
                 solve(ct_to_mr), tolerance = 1e-5)
    expect_equal(ravetools::read_ants_transform(file.path(transform_dir, paste0(prefix_from_t1, "ants.mat"))),
                 ct_to_mr, tolerance = 1e-5)

    # the realigned CT: the CT voxels with the `sform` of `vox2ras.tsv`
    ct_vox2ras <- attr(yael_native_read_volume(process$get_input_image("CT")), "vox2ras")
    vox2ras <- read_tsv_matrix(file.path(transform_dir, paste0(prefix_to_t1, "vox2ras.tsv")))
    expect_equal(vox2ras, ct_to_mr %*% ct_vox2ras, tolerance = 1e-5)
    aligned <- file.path(work_path, "coregistration", "anat",
                         sprintf("sub-%s_ses-postop_space-scanner_desc-preproc_CT.nii.gz", code))
    aligned_image <- yael_native_read_volume(aligned)
    expect_equal(attr(aligned_image, "vox2ras"), vox2ras, tolerance = 1e-4)
    expect_equal(as.vector(aligned_image),
                 as.vector(yael_native_read_volume(process$get_input_image("CT"))))

    # the files of the electrode localization
    expect_equal(unname(tools::md5sum(extras[[1]])),
                 unname(tools::md5sum(process$get_input_image("CT"))))
    expect_equal(unname(tools::md5sum(extras[[2]])),
                 unname(tools::md5sum(process$get_input_image("T1w"))))
    expect_equal(readLines(extras[[3]]),
                 readLines(file.path(transform_dir, paste0(prefix_to_t1, "vox2ras.tsv"))))

    # mapping, its log, and the registration next to the stages
    mapping <- process$get_native_mapping("CT", relative = TRUE)
    expect_setequal(names(mapping$mappings), c("ants", "ras2ras", "vox2ras", "CT_in_T1w"))
    log_path <- file.path(work_path, "coregistration", "log",
                          sprintf("sub-%s_space-scanner_desc-preproc_orig-CT_mapping.json", code))
    expect_equal(jsonlite::fromJSON(log_path), mapping)
    expect_true(all(file.exists(file.path(work_path, "tmp", "coregister_CT_with_T1w",
                                          c("out_.dcf", "out_0GenericAffine.mat",
                                            "out_Warped.nii.gz")))))
  }
})

# ---- Normalization ------------------------------------------------------------------

test_that("map_to_template normalizes a toy head and writes the files of rpyANTs", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  info <- local_yael_toy_template(template$root)
  root <- tempfile(pattern = "ravecore-yael-map-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  process <- as_yael_process("YAEL/YAELNativeToy", engine = "native")
  work_path <- as.character(process$work_path)
  code <- process$subject_code
  camel <- info$camel
  expect_true(process$set_input_image(native$t1, type = "T1w"))

  # a normalization in the layout of rpyANTs <= 0.0.6 (three transforms per
  # direction) with the subject code in lower case, and transforms of another
  # subject and of another template
  transform_dir <- file.path(work_path, "normalization", "transformations")
  dir.create(transform_dir, recursive = TRUE)
  legacy <- c(
    sprintf("sub-%s_from-T1w_to-%s_desc-affine+SyN_ants%s", tolower(code), camel,
            c("0.mat", "1.mat", "2.nii.gz")),
    sprintf("sub-%s_from-%s_to-T1w_desc-affine+SyN_ants%s", tolower(code), camel,
            c("0.nii.gz", "1.mat", "2.mat"))
  )
  kept <- c(
    sprintf("sub-Other_from-T1w_to-%s_desc-affine+SyN_ants0.mat", camel),
    sprintf("sub-%s_from-T1w_to-MNI152NLin2009bAsym_desc-affine+SyN_ants0.mat", code)
  )
  file.create(file.path(transform_dir, c(legacy, kept)))
  expect_length(process$get_template_mapping("mni_icbm152_nlin_asym_09a")$native_to_template$transformlist, 3)

  spy <- local_yael_registration_spy()
  mapping <- process$map_to_template("mni_icbm152_nlin_asym_09a", use_images = "T1w",
                                     use_antspynet = FALSE, verbose = FALSE)

  # the files of rpyANTs: two transforms per direction replace the three of
  # the earlier layout
  expected <- c(
    sprintf("sub-%s_from-T1w_to-%s_desc-affine+SyN_ants0.mat", code, camel),
    sprintf("sub-%s_from-T1w_to-%s_desc-affine+SyN_ants1.nii.gz", code, camel),
    sprintf("sub-%s_from-%s_to-T1w_desc-affine+SyN_ants0.nii.gz", code, camel),
    sprintf("sub-%s_from-%s_to-T1w_desc-affine+SyN_ants1.mat", code, camel)
  )
  expect_setequal(list.files(transform_dir), c(expected, kept))
  expect_setequal(list.files(file.path(work_path, "normalization", "anat")), c(
    sprintf("sub-%s_ses-preop_space-%s_desc-preproc_transform-deformable_T1w.nii.gz", code, camel),
    sprintf("sub-%s_ses-preop_space-T1w_desc-preproc_transform-deformable_%s.nii.gz", code, camel)
  ))
  log_name <- sprintf("sub-%s_desc-preproc_template-%s_native-T1w_mappings.json", code, camel)
  expect_equal(list.files(file.path(work_path, "normalization", "log")), log_name)
  stage_path <- file.path(work_path, "tmp", sprintf("coregister_T1w_with_%s", camel))
  expect_false(dir.exists(file.path(stage_path, "out")))
  expect_true(file.exists(file.path(stage_path, "SyN_w_atropos.dcf")))

  # the returned mapping, and its log with relative paths
  expect_equal(mapping, process$get_template_mapping("mni_icbm152_nlin_asym_09a"))
  expect_equal(basename(mapping$native_to_template$transformlist), expected[c(2, 1)])
  expect_equal(basename(mapping$template_to_native$transformlist), expected[c(4, 3)])
  log_path <- file.path(work_path, "normalization", "log", log_name)
  expect_equal(jsonlite::fromJSON(log_path),
               process$get_template_mapping("mni_icbm152_nlin_asym_09a", relative = TRUE))
  expect_match(readLines(log_path), "\"type\":\"T1w\"", fixed = TRUE)

  # the registrations: the deformable stage starts from the masked affine,
  # whose matrix (template RAS to native RAS) is `ants0.mat`, and `ants1.mat`
  # is its inverse
  calls <- spy()
  expect_equal(vapply(calls, `[[`, "", "type"), c("affine", "syn_only", "affine", "syn_only"))
  expect_equal(calls[[4]]$init_transform, calls[[3]]$transform)
  affine <- ravetools::read_ants_transform(file.path(transform_dir, expected[[1]]))
  expect_equal(affine, calls[[3]]$transform, tolerance = 1e-8)
  expect_equal(ravetools::read_ants_transform(file.path(transform_dir, expected[[4]])),
               solve(affine), tolerance = 1e-8)
  # the warps are on the template grid
  warp <- ravetools::read_ants_warp(file.path(transform_dir, expected[[2]]))
  expect_equal(dim(warp), c(template$dim, 3L))
  expect_equal(attr(warp, "vox2ras"), template$vox2ras, tolerance = 1e-6)
  inverse_warp <- ravetools::read_ants_warp(file.path(transform_dir, expected[[3]]))
  expect_equal(dim(inverse_warp), c(template$dim, 3L))

  # the deformed images: the native image on the template grid and the other way
  deformed <- yael_native_read_volume(file.path(
    work_path, "normalization", "anat",
    sprintf("sub-%s_ses-preop_space-%s_desc-preproc_transform-deformable_T1w.nii.gz", code, camel)))
  expect_equal(dim(deformed), template$dim)
  expect_equal(attr(deformed, "vox2ras"), template$vox2ras, tolerance = 1e-5)
  deformed <- yael_native_read_volume(file.path(
    work_path, "normalization", "anat",
    sprintf("sub-%s_ses-preop_space-T1w_desc-preproc_transform-deformable_%s.nii.gz", code, camel)))
  expect_equal(dim(deformed), native$dim)
  expect_equal(attr(deformed, "vox2ras"), native$vox2ras, tolerance = 1e-5)

  # template labels on the native image overlap the true labels
  labels <- process$transform_image_from_template(
    template$atropos[[1]], "mni_icbm152_nlin_asym_09a",
    interpolator = "nearestNeighbor", verbose = FALSE)
  expect_s3_class(labels, "ieegio_volume")
  expect_equal(dim(labels[]), native$dim)
  expect_equal(matrix(as.numeric(labels$transforms$vox2ras), 4L), native$vox2ras,
               tolerance = 1e-5)
  dice <- yael_toy_dice(labels[], native$truth)
  expect_true(all(dice >= 0.85), label = sprintf("Dice %s", paste(round(dice, 3), collapse = ", ")))

  # ... and so do the native labels on the template (sampled on a 1 mm grid of
  # the native space, so that the overlap measures the registration rather
  # than two coarse samplings)
  fine_dim <- c(104L, 132L, 100L)
  fine_vox2ras <- diag(c(-1, 1, 1, 1))
  fine_vox2ras[1:3, 4] <- c(47.5, -60.5, -52.5)
  fine_labels <- yael_toy_codes(yael_toy_native_to_template(yael_toy_grid(fine_dim, fine_vox2ras)))
  fine_labels[fine_labels > 6] <- 0L
  truth_path <- yael_toy_write(array(fine_labels, fine_dim), fine_vox2ras,
                               file.path(root, "native_labels.nii.gz"))
  labels <- process$transform_image_to_template(truth_path, "mni_icbm152_nlin_asym_09a",
                                                verbose = FALSE)
  expect_equal(dim(labels[]), template$dim)
  expect_equal(matrix(as.numeric(labels$transforms$vox2ras), 4L), template$vox2ras,
               tolerance = 1e-5)
  dice <- yael_toy_dice(labels[], yael_toy_template_labels(template$dim, template$vox2ras))
  expect_true(all(dice >= 0.85), label = sprintf("Dice %s", paste(round(dice, 3), collapse = ", ")))

  # points go to the template and back, and land near their true position
  points <- yael_toy_inner_points(native, template, process)
  back <- process$transform_points_from_template(points$template, "mni_icbm152_nlin_asym_09a",
                                                 verbose = FALSE)
  expect_lte(stats::median(sqrt(rowSums((back - points$native)^2))), 0.05)
  error <- sqrt(rowSums((points$template - yael_toy_native_to_template(points$native))^2))
  expect_lte(stats::median(error), 1.5)
  # the warps are used: where the bumps move the points by more than 1.5 mm,
  # the affine file alone (`ants1.mat`) is much further off, although the
  # medians over the whole brain are about the same
  affine_points <- ravetools::apply_transform3d_points(
    points$native, list(file.path(transform_dir, expected[[4]])))
  errors <- yael_toy_bump_errors(points$native, points$template, affine_points)
  expect_gt(errors[["n"]], 500)
  expect_lt(errors[["full"]], 0.6 * errors[["affine"]],
            label = sprintf("SyN %.3f mm vs affine %.3f mm", errors[["full"]], errors[["affine"]]))
  # data frames, vectors and missing rows (kept as missing rows)
  some <- points$native[1:3, ]
  with_na <- rbind(some[1, ], NA, some[2:3, ])
  mapped <- process$transform_points_to_template(
    as.data.frame(with_na), "mni_icbm152_nlin_asym_09a", verbose = FALSE)
  expect_equal(dim(mapped), c(4L, 3L))
  expect_true(all(is.na(mapped[2, ])))
  expect_equal(mapped[-2, ], points$template[1:3, ], tolerance = 1e-8)
  expect_equal(process$transform_points_to_template(as.vector(t(some)), "mni_icbm152_nlin_asym_09a"),
               points$template[1:3, ], tolerance = 1e-8)

  # interpolation: `"auto"` is linear for probability maps and nearest
  # neighbor otherwise; interpolators without a native version are linear
  gm <- template$atropos[[4]]
  linear <- process$transform_image_from_template(gm, "mni_icbm152_nlin_asym_09a",
                                                  interpolator = "linear", verbose = FALSE)
  nearest <- process$transform_image_from_template(gm, "mni_icbm152_nlin_asym_09a",
                                                   interpolator = "genericLabel", verbose = FALSE)
  expect_false(isTRUE(all.equal(linear[], nearest[])))
  expect_equal(process$transform_image_from_template(gm, "mni_icbm152_nlin_asym_09a",
                                                     verbose = FALSE)[], linear[])
  utils::capture.output({
    bspline <- process$transform_image_from_template(gm, "mni_icbm152_nlin_asym_09a",
                                                     interpolator = "bSpline", verbose = FALSE)
  })
  expect_equal(bspline[], linear[])
  expect_equal(yael_native_interpolation("auto", array(1, c(2, 2, 2))), "nearest")
  expect_equal(yael_native_interpolation("auto", array(c(0, 0.5), c(2, 2, 2))), "trilinear")
  expect_equal(yael_native_interpolation("auto", array(c(0, 2.5), c(2, 2, 2))), "nearest")

  # `talairach.xfm`: native RAS to MNI305, from the affine part
  xfm_path <- process$write_talairach_xfm("mni_icbm152_nlin_asym_09a")
  expect_equal(xfm_path, file.path(work_path, "ants", "mri", "transforms", "talairach.xfm"))
  expect_equal(read_xfm_matrix(xfm_path),
               (solve(MNI305_to_MNI152) %*% solve(affine))[1:3, ], tolerance = 1e-5)

  # normalizing again replaces the files without leaving others behind
  process$map_to_template("mni_icbm152_nlin_asym_09a", use_images = "T1w", verbose = FALSE)
  expect_setequal(list.files(transform_dir), c(expected, kept))
})

test_that("map_to_template registers the other images first and adds them as channels", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  local_yael_toy_template(template$root)
  root <- tempfile(pattern = "ravecore-yael-channels-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  # an image that cannot be registered (four dimensions)
  frames <- yael_native_write_volume(array(1, c(8, 8, 8, 2)), file.path(root, "frames.nii.gz"),
                                     vox2ras = native$vox2ras)

  process <- as_yael_process("YAEL/YAELNativeChannels", engine = "native")
  work_path <- as.character(process$work_path)
  code <- process$subject_code
  expect_true(process$set_input_image(native$t1, type = "T1w"))
  # a T2 scan in a scanner space moved by `native$t2_motion`
  expect_true(process$set_input_image(native$t2_moved, type = "T2w"))
  expect_true(process$set_input_image(frames, type = "FLAIR"))

  spy <- local_yael_registration_spy(keep_sources = TRUE)
  expect_warning(
    process$map_to_template("mni_icbm152_nlin_asym_09a", use_images = c("T1w", "T2w", "FLAIR"),
                            verbose = FALSE),
    "Unable to register the `FLAIR` image")

  # T2w is registered (two rigid passes), then the deformable stage has four
  # channels: the brain, T2w (weight 0.9), and the CSF and deep-structure maps
  calls <- spy()
  expect_equal(vapply(calls, `[[`, "", "type"),
               c("rigid", "rigid", "affine", "syn_only", "affine", "syn_only"))
  expect_equal(calls[[6]]$channels, 4L)
  expect_equal(calls[[6]]$weights, c(1, 0.9, 0.1, 0.5))

  # the registration of T2w, without the files of the electrode localization
  mapping <- process$get_native_mapping("T2w")
  expect_setequal(names(mapping$mappings), c("ants", "ras2ras", "vox2ras", "T2w_in_T1w"))
  expect_true(file.exists(file.path(work_path, "coregistration", "log",
                                    sprintf("sub-%s_space-scanner_desc-preproc_orig-T2w_mapping.json", code))))
  expect_false(any(file.exists(file.path(work_path, "coregistration",
                                         c("CT_RAW.nii.gz", "MRI_reference.nii.gz")))))
  # the motion of the T2 scan is undone: T2 RAS to native RAS is
  # solve(t2_motion). The bias field and the noise of the toy T1 make mutual
  # information flat for this pair, so the result depends on the voxel
  # sampler: over sampler seeds 1 to 8 (and three noise draws of the T2) the
  # registration ended at most 8.1 degrees (3.2 mm) from the truth, while
  # not registering is 8.3 mm off and the inverted transform about 17 mm.
  # This only checks that it did not go astray; the accuracy itself is tested
  # with the CT/MR phantom
  t2_to_t1 <- read_tsv_matrix(mapping$mappings$ras2ras)
  brain <- rbind(t(yael_toy_grid(native$dim, native$vox2ras)[which(native$truth > 0), ]), 1)
  t2_brain <- native$t2_motion %*% brain
  error <- yael_toy_rigid_error(t2_to_t1, solve(native$t2_motion), t2_brain)
  inverted <- yael_toy_rigid_error(t2_to_t1, native$t2_motion, t2_brain)
  expect_lt(error[["rms"]], 0.5 * inverted[["rms"]])
  expect_lt(error[["angle"]], 12)
  expect_lt(error[["rms"]], 5)
  expect_length(process$get_native_mapping("FLAIR")$mappings, 0)

  # the T2w channel is the registered image (`T2w_in_T1w`) on the native
  # grid, where it matches the anatomy of the toy subject; the imported scan,
  # in its own scanner space, does not
  t2_channel <- calls[[6]]$sources[[2]]
  expect_equal(dim(t2_channel), native$dim)
  # (the native grid as stored in the image files, in single precision)
  channel_vox2ras <- attr(t2_channel, "vox2ras")
  expect_equal(channel_vox2ras, native$vox2ras, tolerance = 1e-5)
  aligned <- yael_native_resample(yael_native_read_volume(mapping$mappings$T2w_in_T1w),
                                  native$dim, channel_vox2ras, na_fill = NA)
  covered <- is.finite(aligned)
  expect_equal(t2_channel[covered], aligned[covered], tolerance = 1e-8)
  inside <- native$truth > 0
  t2_truth <- yael_toy_native_t2(native)
  expect_gt(stats::cor(t2_channel[inside], t2_truth[inside]), 0.8)
  imported <- yael_native_resample(yael_native_read_volume(process$get_input_image("T2w")),
                                   native$dim, native$vox2ras, na_fill = NA)
  covered <- inside & is.finite(imported)
  expect_lt(stats::cor(imported[covered], t2_truth[covered]), 0.5)

  # "all" uses every allowed image; the registration of T2w is reused
  expect_warning(
    process$map_to_template("mni_icbm152_nlin_asym_09a", use_images = "all", verbose = FALSE),
    "Unable to register the `FLAIR` image")
  calls <- spy()[-(1:6)]
  expect_equal(vapply(calls, `[[`, "", "type"), c("affine", "syn_only", "affine", "syn_only"))
  expect_equal(calls[[4]]$weights, c(1, 0.9, 0.1, 0.5))

  # other native images do not use the registrations to T1w
  expect_error(
    process$map_to_template("mni_icbm152_nlin_asym_09a", native_type = "MEG", verbose = FALSE),
    "Invalid image type: MEG")
  expect_error(
    process$map_to_template("mni_icbm152_nlin_asym_09a", native_type = "CT", verbose = FALSE),
    "Missing image for type `CT`")
})

test_that("map_to_template passes its tuning arguments and keeps old results when it fails", {
  skip_if_old_testthat()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  info <- local_yael_toy_template(template$root)
  recorded <- NULL
  testthat::local_mocked_bindings(
    yael_native_normalize = function(...) {
      recorded <<- list(...)
      stop("the engine failed")
    },
    # the engine is mocked: this test also runs with an older 'ravetools'
    yael_native_require_ravetools = function() { invisible(TRUE) }
  )

  process <- as_yael_process("YAEL/YAELNativeArguments", engine = "native")
  work_path <- as.character(process$work_path)
  code <- process$subject_code
  expect_true(process$set_input_image(native$t1, type = "T1w"))
  # results of an earlier normalization
  transform_dir <- file.path(work_path, "normalization", "transformations")
  dir.create(transform_dir, recursive = TRUE)
  previous <- c(
    sprintf("sub-%s_from-T1w_to-%s_desc-affine+SyN_ants%s", code, info$camel, c("0.mat", "1.nii.gz")),
    sprintf("sub-%s_from-%s_to-T1w_desc-affine+SyN_ants%s", code, info$camel, c("0.nii.gz", "1.mat"))
  )
  file.create(file.path(transform_dir, previous))

  expect_error(
    process$map_to_template("mni_icbm152_nlin_asym_09a", use_images = "T1w", verbose = FALSE,
                            affine_transform = "/path/to/affine.mat", with_skull = TRUE,
                            cost_function = "MI", not_an_argument = 1),
    "the engine failed")
  expect_equal(recorded$fix_path, info$t1)
  expect_equal(unname(recorded$mov_paths), process$get_input_image("T1w"))
  expect_equal(recorded$weights, 1.25)
  expect_equal(recorded$working_path, file.path(
    work_path, "tmp", sprintf("coregister_T1w_with_%s", info$camel)))
  expect_equal(recorded$affine_transform, "/path/to/affine.mat")
  expect_true(recorded$with_skull)
  expect_equal(recorded$cost_function, "MI")
  expect_false(recorded$verbose)
  expect_false("not_an_argument" %in% names(recorded))
  # the defaults of rpyANTs
  expect_error(
    process$map_to_template("mni_icbm152_nlin_asym_09a", use_images = "T1w", verbose = FALSE),
    "the engine failed")
  expect_null(recorded$affine_transform)
  expect_false(recorded$with_skull)
  expect_equal(recorded$cost_function, "CC")

  # the earlier results are untouched
  expect_setequal(list.files(transform_dir), previous)
  expect_length(process$get_template_mapping("mni_icbm152_nlin_asym_09a")$native_to_template$transformlist, 2)
})

test_that("atlases, streamlines and the ants folder come from the toy template", {
  skip_on_cran()
  skip_if_no_native_yael()
  skip_if_old_testthat()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  root <- tempfile(pattern = "ravecore-yael-outputs-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  # a copy of the toy template with a brain mask that holds intensities (like
  # the mask of `fsaverage`), and a 'FreeSurfer' folder: `fs/mri/T1.mgz` and a
  # left pial surface (tkr RAS) on an ellipsoid inside the brain
  template_root <- file.path(root, "template")
  dir.create(file.path(template_root, "fs", "mri"), recursive = TRUE)
  dir.create(file.path(template_root, "fs", "surf"))
  file.copy(c(template$t1, template$atropos), template_root)
  yael_native_write_volume(
    yael_native_read_volume(template$t1) * yael_native_read_volume(template$mask),
    file.path(template_root, "T1_brainmask.nii.gz"), vox2ras = template$vox2ras)
  expect_gt(max(yael_native_read_volume(file.path(template_root, "T1_brainmask.nii.gz"))), 1)
  yael_native_write_volume(yael_native_read_volume(template$t1),
                           file.path(template_root, "fs", "mri", "T1.mgz"))
  header <- yael_native_read_header(file.path(template_root, "fs", "mri", "T1.mgz"))
  tkr_to_scanner <- header$vox2ras %*% solve(header$vox2ras_tkr)
  angles <- expand.grid(theta = seq(0.3, pi - 0.3, length.out = 6),
                        phi = seq(0, 2 * pi, length.out = 9)[-9])
  pial_scanner <- cbind(26 * sin(angles$theta) * cos(angles$phi),
                        2 + 34 * sin(angles$theta) * sin(angles$phi),
                        4 + 22 * cos(angles$theta))
  pial_tkr <- t(solve(tkr_to_scanner) %*% rbind(t(pial_scanner), 1))[, 1:3]
  faces <- cbind(1L, seq(2L, nrow(pial_scanner) - 1L), seq(3L, nrow(pial_scanner)))
  threeBrain::write.fs.surface(file.path(template_root, "fs", "surf", "lh.pial"),
                               vertex_coords = pial_tkr, faces = faces)
  template_files <- list.files(template_root, recursive = TRUE, all.files = TRUE,
                               full.names = TRUE)
  template_state <- file.info(template_files)[, c("size", "mtime")]
  info <- local_yael_toy_template(template_root)

  process <- as_yael_process("YAEL/YAELNativeOutputs", engine = "native")
  work_path <- as.character(process$work_path)
  expect_true(process$set_input_image(native$t1, type = "T1w"))
  process$map_to_template("mni_icbm152_nlin_asym_09a", use_images = "T1w", verbose = FALSE)

  # ---- atlases: a nested folder ----
  atlas_root <- file.path(root, "atlas")
  dir.create(file.path(atlas_root, "sub", "deeper"), recursive = TRUE)
  labels_path <- yael_toy_write(yael_toy_template_labels(template$dim, template$vox2ras),
                                template$vox2ras, file.path(atlas_root, "labels.nii.gz"))
  file.copy(template$atropos[[4]], file.path(atlas_root, "sub", "gm_prob.nii.gz"))
  yael_native_write_volume(array(1, template$dim), file.path(atlas_root, "sub", "deeper", "ones.nii"),
                           vox2ras = template$vox2ras)
  file.copy(labels_path, file.path(atlas_root, ".hidden.nii.gz"))
  writeLines("not an atlas", file.path(atlas_root, "notes.txt"))

  volumes <- process$generate_atlas_from_template(
    "mni_icbm152_nlin_asym_09a", atlas_folder = atlas_root, surfaces = FALSE, verbose = FALSE)
  atlas_dir <- file.path(work_path, "atlases")
  expect_setequal(substring(volumes, nchar(atlas_dir) + 2),
                  c("labels.nii.gz", "sub/gm_prob.nii.gz", "sub/deeper/ones.nii"))
  expect_false(file.exists(file.path(atlas_dir, ".hidden.nii.gz")))
  mapped <- yael_native_read_volume(file.path(atlas_dir, "labels.nii.gz"))
  expect_equal(dim(mapped), native$dim)
  expect_equal(attr(mapped, "vox2ras"), native$vox2ras, tolerance = 1e-5)
  expect_true(all(mapped %in% 0:6))
  expect_equal(as.vector(mapped), as.vector(process$transform_image_from_template(
    labels_path, "mni_icbm152_nlin_asym_09a", interpolator = "nearestNeighbor", verbose = FALSE)[]))
  mapped <- yael_native_read_volume(file.path(atlas_dir, "sub", "gm_prob.nii.gz"))
  expect_equal(as.vector(mapped), as.vector(process$transform_image_from_template(
    file.path(atlas_root, "sub", "gm_prob.nii.gz"), "mni_icbm152_nlin_asym_09a",
    interpolator = "linear", verbose = FALSE)[]), tolerance = 1e-5)
  # an image of ones is not a probability map
  mapped <- yael_native_read_volume(file.path(atlas_dir, "sub", "deeper", "ones.nii"))
  expect_setequal(unique(as.vector(mapped)), c(0, 1))

  # ---- streamlines: all points in one transform, missing points dropped ----
  tract_root <- file.path(root, "tracts")
  dir.create(file.path(tract_root, "left"), recursive = TRUE)
  line1 <- cbind(seq(-20, 20, length.out = 9), 4, 6)
  line2 <- cbind(-6, seq(-20, 25, length.out = 7), 10)
  ieegio::write_streamlines(ieegio::as_ieegio_streamlines(list(line1, line2)),
                            file.path(tract_root, "left", "tract.tck"))
  # 'TCK' files cannot hold a missing point: the reader of this one gives one;
  # 'ieegio' cannot write 'TT' files: the reader of `tract.tt` gives the lines
  file.copy(file.path(tract_root, "left", "tract.tck"), file.path(tract_root, "with_na.tck"))
  file.create(file.path(tract_root, "tract.tt"))
  # 'VTK' files are only read through 'Python': skipped without being opened
  file.create(file.path(tract_root, "left", "tract.vtk"))
  line3 <- line1
  line3[3, ] <- NA
  reader <- ieegio::as_ieegio_streamlines
  writer <- ieegio::write_streamlines
  # the value of `IEEGIO_NO_PYTHON` whenever a file is read or written
  python_flags <- character(0L)
  testthat::local_mocked_bindings(
    as_ieegio_streamlines = function(x, ...) {
      if (is.character(x) && length(x) == 1) {
        python_flags <<- c(python_flags, Sys.getenv("IEEGIO_NO_PYTHON"))
        if (identical(basename(x), "with_na.tck")) { return(reader(list(line3, line2))) }
        if (identical(basename(x), "tract.tt")) { return(reader(list(line1, line2))) }
      }
      reader(x, ...)
    },
    write_streamlines = function(...) {
      python_flags <<- c(python_flags, Sys.getenv("IEEGIO_NO_PYTHON"))
      writer(...)
    },
    ensure_py_package = function(...) { stop("'Python' was requested") },
    .package = "ieegio"
  )
  # the method disables the 'Python' support of 'ieegio' by itself (switched
  # off here, since the sandbox disables it as well) and restores it after
  Sys.unsetenv("IEEGIO_NO_PYTHON")
  expect_warning(
    files <- process$generate_streamlines_from_template(
      "mni_icbm152_nlin_asym_09a", streamlines_folder = tract_root, verbose = FALSE),
    "skipped: left/tract.vtk")
  expect_identical(Sys.getenv("IEEGIO_NO_PYTHON", unset = NA), NA_character_)
  Sys.setenv(IEEGIO_NO_PYTHON = "1")
  # three files read, three written
  expect_equal(python_flags, rep("1", 6))
  expect_setequal(files, c("left/tract.tck", "with_na.tck", "tract.tt"))
  to_native <- function(x) {
    process$transform_points_from_template(x, "mni_icbm152_nlin_asym_09a", verbose = FALSE)
  }
  result <- reader(file.path(work_path, "fs", "streamline", "left", "tract.tck"))[]
  expect_length(result, 2)
  expect_equal(result[[1]]$coords, to_native(line1), tolerance = 1e-5)
  expect_equal(result[[2]]$coords, to_native(line2), tolerance = 1e-5)
  result <- reader(file.path(work_path, "fs", "streamline", "with_na.tck"))[]
  expect_length(result, 2)
  expect_equal(result[[1]]$coords, to_native(line1[-3, ]), tolerance = 1e-5)
  # the 'TT' file is saved as 'TCK'
  result <- reader(file.path(work_path, "fs", "streamline", "tract.tck"))[]
  expect_equal(result[[2]]$coords, to_native(line2), tolerance = 1e-5)
  expect_false(file.exists(file.path(work_path, "fs", "streamline", "tract.tt")))
  # another format
  suppressWarnings(process$generate_streamlines_from_template(
    "mni_icbm152_nlin_asym_09a", streamlines_folder = tract_root, verbose = FALSE,
    format = "trk"))
  result <- suppressMessages(
    reader(file.path(work_path, "fs", "streamline", "left", "tract.trk")))[]
  expect_equal(result[[1]]$coords, to_native(line1), tolerance = 1e-4)
  expect_true(file.exists(file.path(work_path, "fs", "streamline", "tract.trk")))
  # 'VTK' would be written through 'Python', and 'TT' cannot be written
  expect_error(
    process$generate_streamlines_from_template(
      "mni_icbm152_nlin_asym_09a", streamlines_folder = tract_root, format = "vtk"),
    "writes `VTK` files through `Python`")
  expect_error(
    process$generate_streamlines_from_template(
      "mni_icbm152_nlin_asym_09a", streamlines_folder = tract_root, format = "tt"),
    "has no `TT` writer")
  expect_false(any(grepl("\\.(vtk|tt)$", list.files(file.path(work_path, "fs", "streamline"),
                                                     recursive = TRUE))))

  # ---- the ants folder, with the pial surface of the template ----
  process$construct_ants_folder_from_template("mni_icbm152_nlin_asym_09a", add_surfaces = TRUE)
  ants_dir <- file.path(work_path, "ants")
  expect_true(all(file.exists(file.path(ants_dir, c(
    "mri/T1.mgz", "mri/brainmask.nii.gz", "mri/nu.nii.gz", "mri/brain.finalsurfs.nii.gz",
    "mri/transforms/talairach.xfm", "README.txt", "surf/lh.pial.T1")))))
  expect_false(file.exists(file.path(ants_dir, "surf", "rh.pial.T1")))
  # the template mask (intensities) on the native image, binarized
  mask <- yael_native_read_volume(file.path(ants_dir, "mri", "brainmask.nii.gz"))
  expect_setequal(unique(as.vector(mask)), c(0, 1))
  expect_gt(2 * sum(mask > 0 & native$truth > 0) / (sum(mask > 0) + sum(native$truth > 0)), 0.9)
  brain <- yael_native_read_volume(file.path(ants_dir, "mri", "brain.finalsurfs.nii.gz"))
  expect_equal(max(brain), 255)
  expect_true(all(brain >= 0 & brain == round(brain)))
  expect_true(all(brain[mask == 0] == 0))
  affine <- ravetools::read_ants_transform(
    process$get_template_mapping("mni_icbm152_nlin_asym_09a")$native_to_template$transformlist[[2]])
  expect_equal(read_xfm_matrix(file.path(ants_dir, "mri", "transforms", "talairach.xfm")),
               (solve(MNI305_to_MNI152) %*% solve(affine))[1:3, ], tolerance = 1e-5)
  # the morphed surface: template scanner RAS -> native scanner RAS -> native
  # tkr RAS (of the header of `brain.finalsurfs.nii.gz`)
  native_header <- yael_native_read_header(file.path(ants_dir, "mri", "brain.finalsurfs.nii.gz"))
  scanner_to_tkr <- native_header$vox2ras_tkr %*% solve(native_header$vox2ras)
  expected <- t(scanner_to_tkr %*% rbind(t(to_native(pial_scanner)), 1))[, 1:3]
  surface <- threeBrain::read.fs.surface(file.path(ants_dir, "surf", "lh.pial.T1"))
  expect_equal(unname(surface$vertices), unname(expected), tolerance = 1e-4)
  expect_equal(unname(surface$faces), unname(faces))

  # without surfaces, the morphed ones are removed; without template, only
  # `T1.mgz` is written
  process$construct_ants_folder_from_template("mni_icbm152_nlin_asym_09a", add_surfaces = FALSE)
  expect_false(file.exists(file.path(ants_dir, "surf", "lh.pial.T1")))
  unlink(ants_dir, recursive = TRUE)
  process$construct_ants_folder_from_template(NULL)
  expect_equal(list.files(ants_dir, recursive = TRUE), "mri/T1.mgz")

  # nothing was written into the template folder
  expect_setequal(list.files(template_root, recursive = TRUE, all.files = TRUE, full.names = TRUE),
                  template_files)
  expect_equal(file.info(template_files)[, c("size", "mtime")], template_state)
})

# ---- 'ANTs' oracle (needs 'Python'; set RAVECORE_YAEL_ANTS_ORACLE=true) ------------

test_that("YAELProcess ('ANTs') and YAELNativeProcess read each other's files", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("RAVECORE_YAEL_ANTS_ORACLE"), "true"),
              "set RAVECORE_YAEL_ANTS_ORACLE=true to compare with 'ANTs'")
  skip_if_not_installed("rpyANTs")
  skip_if_no_native_yael()
  skip_if_old_testthat()
  skip_if_not(isTRUE(rpyANTs::ants_available()), "'ANTs' is not available")
  local_yael_sandbox()
  # the one test allowed to start 'Python' (the sandbox restores the variable)
  Sys.unsetenv("IEEGIO_NO_PYTHON")
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  root <- tempfile(pattern = "ravecore-yael-oracle-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  # rpyANTs writes a cache file into the template folder: use a copy
  template_root <- file.path(root, "template")
  dir.create(template_root)
  file.copy(c(template$t1, template$mask, template$atropos), template_root)
  info <- yael_toy_template_info(template_root)
  call_rpyants_real <- call_rpyants
  testthat::local_mocked_bindings(
    yael_native_template = function(template_name = rpyants_builtin_templates()) { info },
    call_rpyants = function(.name, ...) {
      if (identical(.name, "ensure_template")) { return(template_root) }
      call_rpyants_real(.name, ...)
    }
  )
  max_difference <- function(x, y) { max(abs(x - y)) }

  # 'ANTs' reads the files of the native process
  native_process <- as_yael_process("YAEL/YAELNativeOracle", engine = "native")
  native_process$set_input_image(native$t1, type = "T1w")
  native_process$map_to_template("mni_icbm152_nlin_asym_09a", use_images = "T1w", verbose = FALSE)
  ants_process <- as_yael_process("YAEL/YAELNativeOracle")
  expect_s3_class(ants_process, "YAELProcess")
  points <- yael_toy_inner_points(native, template, native_process)
  expect_lt(max_difference(
    ants_process$transform_points_to_template(points$native, "mni_icbm152_nlin_asym_09a", verbose = FALSE),
    points$template), 1e-3)
  expect_lt(max_difference(
    ants_process$transform_points_from_template(points$template, "mni_icbm152_nlin_asym_09a", verbose = FALSE),
    native_process$transform_points_from_template(points$template, "mni_icbm152_nlin_asym_09a", verbose = FALSE)), 1e-3)

  # the native process reads the files of 'ANTs'
  ants_process <- as_yael_process("YAEL/YAELNativeOracleANTs")
  native_process <- as_yael_process("YAEL/YAELNativeOracleANTs", engine = "native")
  native_process$set_input_image(native$t1, type = "T1w")
  ants_process$map_to_template("mni_icbm152_nlin_asym_09a", use_images = "T1w",
                               use_antspynet = FALSE, verbose = FALSE)
  points <- yael_toy_inner_points(native, template, native_process)
  expect_lt(max_difference(
    ants_process$transform_points_to_template(points$native, "mni_icbm152_nlin_asym_09a", verbose = FALSE),
    points$template), 1e-3)
  expect_lt(max_difference(
    ants_process$transform_points_from_template(points$template, "mni_icbm152_nlin_asym_09a", verbose = FALSE),
    native_process$transform_points_from_template(points$template, "mni_icbm152_nlin_asym_09a", verbose = FALSE)), 1e-3)
})

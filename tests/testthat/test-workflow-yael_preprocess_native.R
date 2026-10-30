# Engine of the native 'YAEL' pipeline (`R/workflow-yael_preprocess_native.R`):
# file names, image input and output, template resolver, stage cache, rigid
# co-registration and normalization. Toy images only (see
# `helper-yael-native.R`); nothing reads real images or starts 'Python'.

# ---- Test helpers -------------------------------------------------------------------

test_that("the sandbox redirects the RAVE directories and restores them", {
  folder_keys <- c("raw_data_dir", "data_dir", "bids_data_dir",
                   "mni_template_root", "tensor_temp_path")
  keys <- c(folder_keys, "max_worker")
  settings <- get(".settings", envir = asNamespace("ravepipeline"))
  before <- lapply(keys, ravepipeline::raveio_getopt, default = NULL)
  had_key <- vapply(keys, .subset2(settings, "has"), FALSE)
  no_python <- Sys.getenv("IEEGIO_NO_PYTHON", unset = NA)
  root <- NULL
  local({
    root <<- local_yael_sandbox()
    for (key in folder_keys) {
      path <- ravepipeline::raveio_getopt(key, default = NULL)
      expect_true(dir.exists(path), label = key)
      expect_true(startsWith(normalizePath(path, winslash = "/"), root), label = key)
    }
    # jobs run in this process
    expect_equal(ravepipeline::raveio_getopt("max_worker"), 1L)
    # 'ieegio' cannot start (or install) 'Python'
    expect_equal(Sys.getenv("IEEGIO_NO_PYTHON"), "1")
  })
  expect_false(dir.exists(root))
  expect_equal(lapply(keys, ravepipeline::raveio_getopt, default = NULL), before)
  expect_equal(vapply(keys, .subset2(settings, "has"), FALSE), had_key)
  expect_identical(Sys.getenv("IEEGIO_NO_PYTHON", unset = NA), no_python)

  # in a 'RAVE' job session the settings come from `options(rave.use_settings)`
  # and cannot be redirected: the sandbox refuses to run instead of leaving
  # the tests on the real folders, and cleans up after itself
  job_session <- Sys.getenv("RAVE_JOB_SESSION", unset = NA)
  job_settings <- options(rave.use_settings = list(
    raw_data_dir = file.path(tempdir(), "ravecore-yael-not-sandboxed"),
    max_worker = 4L, ..temp = list()))
  Sys.setenv(RAVE_JOB_SESSION = "1")
  restore_session <- function() {
    if (is.na(job_session)) {
      Sys.unsetenv("RAVE_JOB_SESSION")
    } else {
      Sys.setenv(RAVE_JOB_SESSION = job_session)
    }
    options(job_settings)
  }
  on.exit({ restore_session() }, add = TRUE)
  sandboxes <- list.files(tempdir(), pattern = "^ravecore-yael-sandbox-")
  expect_error(
    suppressWarnings(local({ local_yael_sandbox() })),
    "cannot redirect RAVE option `raw_data_dir`")
  restore_session()
  expect_setequal(list.files(tempdir(), pattern = "^ravecore-yael-sandbox-"), sandboxes)
  expect_equal(lapply(keys, ravepipeline::raveio_getopt, default = NULL), before)
  expect_identical(Sys.getenv("IEEGIO_NO_PYTHON", unset = NA), no_python)

  # the guard stops everything of rpyANTs that is not pure 'R' metadata
  guard <- yael_rpyants_guard()
  expect_error(guard("ensure_template", "mni_icbm152_nlin_asym_09a"), "not allowed")
  expect_error(guard("load_rpyants"), "not allowed")
  expect_error(guard("to_r", 1), "not allowed")
  skip_if_not_installed("rpyANTs")
  expect_true("mni_icbm152_nlin_asym_09a" %in% guard("BUILTIN_TEMPLATES"))
  expect_equal(guard("template_urls")$fsaverage$coord_sys, "MNI305")
})

# ---- File names ------------------------------------------------------------------

# The expected names were produced by the 'Python' code of rpyANTs 0.0.6.2:
# `rpyants/utils/paths.py` loaded by file path with `python3 -I`, and the
# methods `_fix_image_path`, `set_image`, `format_path` and
# `expected_image_path` of `rpyants/registration/yael.py` compiled with `ast`
# (`ants` stubbed out), with subject `X` and the image types `CT T1w T2w FLAIR
# preopCT T1wContrast fGATIR postopT1w`; the imported names marked below
# differ since 'bidsr' parses their source names differently

test_that("BIDS file names are parsed by bidsr", {
  # the type is the suffix; only the base name is parsed
  parsed <- yael_parse_bids_filename(
    "/some/anat/sub-X_from-CT_to-T1w_desc-rigid_ants0.mat")
  expect_equal(parsed@suffix, "ants0")
  expect_equal(parsed@extension, "mat")
  expect_equal(bidsr::get_bids_entity(parsed, "sub"), "X")
  expect_equal(bidsr::get_bids_entity(parsed, "from"), "CT")
  expect_equal(bidsr::get_bids_entity(parsed, "to"), "T1w")
  expect_null(bidsr::get_bids_entity(parsed, "ses"))
  expect_equal(yael_bids_entities(parsed),
               list(sub = "X", desc = "rigid", from = "CT", to = "T1w"))

  parsed <- yael_parse_bids_filename("sub-01_ses-01_echo-1_T1w.nii")
  expect_equal(parsed@suffix, "T1w")
  expect_equal(parsed@extension, "nii")
  expect_equal(yael_bids_entities(parsed), list(sub = "01", ses = "01", echo = "1"))
  # trailing tokens without dash belong to the suffix
  expect_equal(yael_parse_bids_filename("MRI_acpc.nii.gz")@suffix, "MRI_acpc")
  expect_length(yael_parse_bids_filename("T1w.nii.gz")@entities, 0)

  # names that bidsr rejects are `NULL`
  expect_null(yael_parse_bids_filename(".DS_Store"))
  expect_null(yael_parse_bids_filename("sub-01_acq-_x_T1w.nii.gz"))
  expect_null(yael_parse_bids_filename("sub-X_ses-01_T1w_desc-preproc_T1w.nii.gz"))
  expect_null(yael_parse_bids_filename(NA_character_))
})

test_that("subject codes with underscores stay in the `sub` entity", {
  parsed <- yael_parse_bids_filename("sub-LOC_01_LT_ses-preop_desc-preproc_T1w.nii.gz")
  expect_equal(parsed@suffix, "T1w")
  expect_equal(parsed@extension, "nii.gz")
  expect_equal(yael_bids_entities(parsed),
               list(sub = "LOC_01_LT", ses = "preop", desc = "preproc"))

  parsed <- yael_parse_bids_filename(
    "/some/anat/sub-LOC_01_LT_from-T1w_to-MNI152NLin2009aAsym_desc-affine+SyN_ants1.nii.gz")
  expect_equal(parsed@suffix, "ants1")
  expect_equal(bidsr::get_bids_entity(parsed, "sub"), "LOC_01_LT")
  expect_equal(bidsr::get_bids_entity(parsed, "to"), "MNI152NLin2009aAsym")

  # no other entity: the last token is the type
  parsed <- yael_parse_bids_filename("sub-LOC_01_T1w.nii.gz")
  expect_equal(parsed@suffix, "T1w")
  expect_equal(yael_bids_entities(parsed), list(sub = "LOC_01"))
})

test_that("the native engine reads images and ANTs transforms of subject codes with underscores", {
  skip_if_old_testthat()
  local_yael_sandbox()
  local_yael_rpyants_guard()
  root <- tempfile(pattern = "ravecore-yael-underscore-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  t1_path <- yael_native_write_volume(array(0, c(4, 4, 4)), file.path(root, "t1.nii.gz"),
                                      vox2ras = diag(4))
  code <- "LOC_01_LT"
  process <- as_yael_process(paste0("YAEL/", code), imaging_path = file.path(root, "imaging"),
                             engine = "native")

  expect_true(process$set_input_image(path = t1_path, type = "T1w", overwrite = TRUE))
  expect_equal(basename(process$get_input_image("T1w")),
               sprintf("sub-%s_ses-preop_desc-preproc_T1w.nii.gz", code))

  # the names that the 'rpyANTs' engine writes
  transform_dir <- file.path(root, "imaging", "normalization", "transformations")
  dir.create(transform_dir, recursive = TRUE)
  forward <- sprintf("sub-%s_from-T1w_to-MNI152NLin2009aAsym_desc-affine+SyN_ants%s",
                     code, c("1.nii.gz", "0.mat"))
  inverse <- sprintf("sub-%s_from-MNI152NLin2009aAsym_to-T1w_desc-affine+SyN_ants%s",
                     code, c("1.mat", "0.nii.gz"))
  file.create(file.path(transform_dir, c(forward, inverse)))
  mapping <- process$get_template_mapping("mni_icbm152_nlin_asym_09a", relative = TRUE)
  expect_equal(basename(unlist(mapping$native_to_template$transformlist)), forward)
  expect_equal(basename(unlist(mapping$template_to_native$transformlist)), inverse)
})

test_that("BIDS prefixes keep the reserved entity order of rpyANTs", {
  expect_equal(
    yael_bids_prefix(list(sub = "X", from = "T1w", to = "CT", desc = "rigid")),
    "sub-X_from-T1w_to-CT_desc-rigid_")
  expect_equal(
    yael_bids_prefix(list(sub = "X", from = "CT", to = "T1w", desc = "rigid")),
    "sub-X_from-CT_to-T1w_desc-rigid_")
  expect_equal(
    yael_bids_prefix(list(sub = "X", from = "T1w", to = "MNI152NLin2009bAsym",
                          desc = "affine+SyN")),
    "sub-X_from-T1w_to-MNI152NLin2009bAsym_desc-affine+SyN_")
  expect_equal(
    yael_bids_prefix(list(sub = "X", from = "MNI152NLin2009bAsym", to = "T1w",
                          desc = "affine+SyN")),
    "sub-X_from-MNI152NLin2009bAsym_to-T1w_desc-affine+SyN_")
  # extra entities override the components and come before their other keys
  expect_equal(
    yael_bids_prefix(list(foo = "1", sub = "A", desc = "d", bar = "2"),
                     space = "S", baz = "3", foo = "9"),
    "sub-A_space-S_desc-d_baz-3_foo-9_bar-2_")
  expect_equal(yael_bids_prefix(list()), "_")
})

test_that("imported images are named like `YAELPreprocess.set_image`", {
  types <- c("CT", "T1w", "T2w", "FLAIR", "preopCT", "T1wContrast", "fGATIR",
             "postopT1w")
  input_name <- function(path, type) {
    yael_input_image_name(path, type = type, subject_code = "X",
                          image_types = types)$fullname
  }
  expected <- c(
    "MRI_acpc.nii.gz|T1w" = "sub-X_ses-preop_desc-preproc_T1w.nii.gz",
    "sub-01_ses-01_echo-1_T1w.nii|T1w" = "sub-X_ses-01_desc-preproc_echo-1_T1w.nii.gz",
    "my-scan_T1.nii.gz|T1w" = "sub-X_ses-preop_desc-preproc_my-scan_T1w.nii.gz",
    # rpyANTs cannot parse a value with a dash and keeps the subject only
    "my-scan-v2_T1w.nii.gz|T1w" = "sub-X_ses-preop_desc-preproc_my-scan-v2_T1w.nii.gz",
    "ct_scan.nii.gz|CT" = "sub-X_ses-postop_desc-preproc_CT.nii.gz",
    "sub-01_ses-implant_CT.nii.gz|CT" = "sub-X_ses-implant_desc-preproc_CT.nii.gz",
    "x.nii|postopT1w" = "sub-X_ses-postop_desc-preproc_postopT1w.nii.gz",
    "x.nii|preopCT" = "sub-X_ses-preop_desc-preproc_preopCT.nii.gz",
    "x.nii|MEG" = "sub-X_ses-preop_desc-preproc_MEG.nii.gz",
    # `extra` joins the suffix in bidsr (rpyANTs: `acq-ax_extra`)
    "sub-01_desc-raw_acq-ax_extra_T2w.nii.gz|T2w" = "sub-X_ses-preop_acq-ax_desc-preproc_T2w.nii.gz",
    "sub-01_foo-bar_space-orig_dir-AP_task-rest_run-02_rec-norm_acq-ax_T2w.nii.gz|T2w" = "sub-X_ses-preop_acq-ax_rec-norm_run-02_task-rest_dir-AP_space-orig_desc-preproc_foo-bar_T2w.nii.gz",
    # an entity without value: bidsr cannot parse the name (rpyANTs: `acq-x`)
    "sub-01_acq-_x_T1w.nii.gz|T1w" = "sub-X_ses-preop_desc-preproc_T1w.nii.gz"
  )
  for (case in names(expected)) {
    item <- strsplit(case, "|", fixed = TRUE)[[1]]
    expect_equal(input_name(file.path("/data", item[[1]]), item[[2]]),
                 expected[[case]], label = case)
  }
  parsed <- yael_input_image_name("sub-01_ses-01_echo-1_T1w.nii", type = "T1w",
                                  subject_code = "X", image_types = types)
  expect_equal(parsed$folder, "inputs/anat")
  expect_equal(parsed$ext, "nii.gz")
  expect_equal(parsed$entities, list(sub = "X", ses = "01", echo = "1",
                                     desc = "preproc"))
})

test_that("co-registration and normalization files are named like rpyANTs", {
  types <- c("CT", "T1w", "T2w", "FLAIR", "preopCT", "T1wContrast", "fGATIR",
             "postopT1w")
  template <- "MNI152NLin2009bAsym"
  expected <- list(
    CT = c(
      coreg_anat = "sub-X_ses-postop_space-scanner_desc-preproc_CT.nii.gz",
      coreg_log = "sub-X_space-scanner_desc-preproc_orig-CT_mapping.json",
      norm_anat_in_template = "sub-X_ses-postop_space-MNI152NLin2009bAsym_desc-preproc_transform-deformable_CT.nii.gz",
      norm_anat_in_native = "sub-X_ses-postop_space-CT_desc-preproc_transform-deformable_MNI152NLin2009bAsym.nii.gz",
      norm_log = "sub-X_desc-preproc_template-MNI152NLin2009bAsym_native-CT_mappings.json"),
    T2w = c(
      coreg_anat = "sub-X_ses-preop_space-scanner_desc-preproc_T2w.nii.gz",
      coreg_log = "sub-X_space-scanner_desc-preproc_orig-T2w_mapping.json",
      norm_anat_in_template = "sub-X_ses-preop_space-MNI152NLin2009bAsym_desc-preproc_transform-deformable_T2w.nii.gz",
      norm_anat_in_native = "sub-X_ses-preop_space-T2w_desc-preproc_transform-deformable_MNI152NLin2009bAsym.nii.gz",
      norm_log = "sub-X_desc-preproc_template-MNI152NLin2009bAsym_native-T2w_mappings.json"),
    preopCT = c(
      coreg_anat = "sub-X_ses-preop_space-scanner_desc-preproc_preopCT.nii.gz",
      coreg_log = "sub-X_space-scanner_desc-preproc_orig-preopCT_mapping.json",
      norm_anat_in_template = "sub-X_ses-preop_space-MNI152NLin2009bAsym_desc-preproc_transform-deformable_preopCT.nii.gz",
      norm_anat_in_native = "sub-X_ses-preop_space-preopCT_desc-preproc_transform-deformable_MNI152NLin2009bAsym.nii.gz",
      norm_log = "sub-X_desc-preproc_template-MNI152NLin2009bAsym_native-preopCT_mappings.json"),
    postopT1w = c(
      coreg_anat = "sub-X_ses-postop_space-scanner_desc-preproc_postopT1w.nii.gz",
      coreg_log = "sub-X_space-scanner_desc-preproc_orig-postopT1w_mapping.json",
      norm_anat_in_template = "sub-X_ses-postop_space-MNI152NLin2009bAsym_desc-preproc_transform-deformable_postopT1w.nii.gz",
      norm_anat_in_native = "sub-X_ses-postop_space-postopT1w_desc-preproc_transform-deformable_MNI152NLin2009bAsym.nii.gz",
      norm_log = "sub-X_desc-preproc_template-MNI152NLin2009bAsym_native-postopT1w_mappings.json"),
    T1w = c(
      coreg_anat = "sub-X_ses-preop_space-scanner_desc-preproc_T1w.nii.gz",
      coreg_log = "sub-X_space-scanner_desc-preproc_orig-T1w_mapping.json",
      norm_anat_in_template = "sub-X_ses-preop_space-MNI152NLin2009bAsym_desc-preproc_transform-deformable_T1w.nii.gz",
      norm_anat_in_native = "sub-X_ses-preop_space-T1w_desc-preproc_transform-deformable_MNI152NLin2009bAsym.nii.gz",
      norm_log = "sub-X_desc-preproc_template-MNI152NLin2009bAsym_native-T1w_mappings.json")
  )
  for (type in names(expected)) {
    names <- yael_native_file_names("X", type, template_name = template,
                                    image_types = types)
    expect_equal(unlist(names[names(expected[[type]])]), expected[[type]],
                 label = type)
  }

  # transforms of `register_to_T1w` and `map_to_template`
  names <- yael_native_file_names("X", "CT", template_name = template,
                                  image_types = types)
  expect_equal(names$coreg_to_t1_ants, "sub-X_from-CT_to-T1w_desc-rigid_ants.mat")
  expect_equal(names$coreg_to_t1_ras2ras, "sub-X_from-CT_to-T1w_desc-rigid_ras2ras.tsv")
  expect_equal(names$coreg_to_t1_vox2ras, "sub-X_from-CT_to-T1w_desc-rigid_vox2ras.tsv")
  expect_equal(names$coreg_from_t1_ants, "sub-X_from-T1w_to-CT_desc-rigid_ants.mat")
  expect_equal(names$coreg_from_t1_ras2ras, "sub-X_from-T1w_to-CT_desc-rigid_ras2ras.tsv")
  names <- yael_native_file_names("X", "T1w", template_name = template,
                                  image_types = types)
  expect_equal(names$norm_to_template_affine,
               "sub-X_from-T1w_to-MNI152NLin2009bAsym_desc-affine+SyN_ants0.mat")
  expect_equal(names$norm_to_template_warp,
               "sub-X_from-T1w_to-MNI152NLin2009bAsym_desc-affine+SyN_ants1.nii.gz")
  expect_equal(names$norm_from_template_warp,
               "sub-X_from-MNI152NLin2009bAsym_to-T1w_desc-affine+SyN_ants0.nii.gz")
  expect_equal(names$norm_from_template_affine,
               "sub-X_from-MNI152NLin2009bAsym_to-T1w_desc-affine+SyN_ants1.mat")
  expect_null(yael_native_file_names("X", "CT")$norm_log)

  # `format_path` of a type that is not allowed: no session; given session kept
  expect_equal(yael_format_name("X", "MEG", "nii.gz", image_types = types),
               "sub-X_desc-preproc_MEG.nii.gz")
  expect_equal(yael_format_name("X", "postopT1w", "nii.gz", ses = "implant",
                                image_types = types),
               "sub-X_ses-implant_desc-preproc_postopT1w.nii.gz")

  # normalization weights relative to the T1w image
  expect_equal(
    yael_normalization_weight(c("CT", "preopCT", "postopCT", "FLAIR",
                                "postopFLAIR", "preopFLAIR", "T2w", "fGATIR")),
    c(0.6, 0.8, 0.6, 0.25, 0.2, 0.3, 0.9, 0.9))
})

# ---- Image input and output --------------------------------------------------------

test_that("native volumes keep their own geometry when read and written", {
  root <- tempfile(pattern = "ravecore-yael-io-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  # oblique grid, fractional values
  vox2ras <- yael_toy_rotation(1, 10) %*% yael_toy_rotation(3, -20) %*%
    diag(c(1.5, 2, 2.5, 1))
  vox2ras[1:3, 4] <- c(-30.25, 12.5, -7)
  x <- array(seq_len(6 * 5 * 4) / 7, c(6, 5, 4))
  path <- file.path(root, "sub", "image.nii.gz")
  expect_identical(yael_native_write_volume(x, path, vox2ras = vox2ras), path)
  y <- yael_native_read_volume(path)
  expect_equal(dim(y), c(6L, 5L, 4L))
  expect_true(is.double(y))
  expect_equal(attr(y, "vox2ras"), vox2ras, tolerance = 1e-6)
  expect_equal(as.vector(y), as.vector(x), tolerance = 1e-6)
  expect_equal(RNifti::niftiHeader(path)$sizeof_hdr, 348)

  # the "vox2ras" attribute is the default; a new one is honored when written
  attr(y, "vox2ras")[1:3, 4] <- c(1, 2, 3)
  yael_native_write_volume(y, path)
  expect_equal(attr(yael_native_read_volume(path), "vox2ras")[1:3, 4], c(1, 2, 3),
               tolerance = 1e-6)
  expect_error(yael_native_write_volume(array(0, c(2, 2, 2)), path), "vox2ras")

  # logical and integer arrays are written as numbers
  yael_native_write_volume(array(c(TRUE, FALSE), c(4, 4, 4)), path, vox2ras = diag(4))
  expect_equal(sum(yael_native_read_volume(path)), 32)

  # trailing singleton dimensions are dropped; 4D needs `allow_4d`
  ieegio::write_volume(ieegio::as_ieegio_volume(array(1, c(4, 3, 2, 1)), vox2ras = diag(4)),
                       file.path(root, "single.nii.gz"))
  expect_equal(dim(yael_native_read_volume(file.path(root, "single.nii.gz"))), c(4L, 3L, 2L))
  ieegio::write_volume(ieegio::as_ieegio_volume(array(1, c(4, 3, 2, 2)), vox2ras = diag(4)),
                       file.path(root, "frames.nii.gz"))
  expect_error(yael_native_read_volume(file.path(root, "frames.nii.gz")), "not a 3D volume")
  expect_equal(dim(yael_native_read_volume(file.path(root, "frames.nii.gz"), allow_4d = TRUE)),
               c(4L, 3L, 2L, 2L))

  # MGZ (read as 4D by ieegio)
  mgz <- file.path(root, "image.mgz")
  yael_native_write_volume(x, mgz, vox2ras = vox2ras)
  z <- yael_native_read_volume(mgz)
  expect_equal(dim(z), c(6L, 5L, 4L))
  expect_equal(attr(z, "vox2ras"), vox2ras, tolerance = 1e-5)
  expect_equal(as.vector(z), as.vector(x), tolerance = 1e-5)

  # `numpy.savetxt(fmt = "%f", delimiter = "\t")`
  tsv <- file.path(root, "matrix.tsv")
  yael_native_write_tsv(rbind(c(1, -0.5, 1e-7, 123.456789), c(0, -1e-9, 2, 1)), tsv)
  expect_equal(readLines(tsv), c("1.000000\t-0.500000\t0.000000\t123.456789",
                                 "0.000000\t-0.000000\t2.000000\t1.000000"))
  expect_equal(unname(as.matrix(utils::read.table(tsv)))[1, 1:2], c(1, -0.5))
})

# ---- Templates --------------------------------------------------------------------

test_that("the template resolver returns the template files or explains what is missing", {
  skip_if_not_installed("rpyANTs")
  toy <- yael_toy_dataset("template")
  root <- tempfile(pattern = "ravecore-yael-template-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  file.copy(list.files(toy$root, full.names = TRUE), root)

  # `ensure_template` returns the toy folder; everything else of rpyANTs that
  # would download files or start 'Python' is guarded
  local_yael_rpyants_guard(ensure_template = function(name, ...) {
    structure(root, name = name)
  })
  expect_error(call_rpyants("load_rpyants"), "not allowed")

  info <- yael_native_template("mni_icbm152_nlin_asym_09a")
  expect_named(info, c("name", "camel", "coord_sys", "root", "t1", "mask",
                       "atropos", "fs_dir", "atlas_dir"))
  expect_equal(info$name, "mni_icbm152_nlin_asym_09a")
  expect_equal(info$camel, "MNI152NLin2009aAsym")
  expect_equal(info$coord_sys, "MNI152")
  expect_equal(normalizePath(info$root), normalizePath(root))
  expect_equal(basename(info$atropos), sprintf("atropos_%d.nii.gz", 0:7))
  expect_true(all(file.exists(c(info$t1, info$mask, info$atropos))))
  expect_equal(basename(c(info$fs_dir, info$atlas_dir)), c("fs", "atlases"))
  expect_equal(info, yael_toy_template_info(root))
  expect_error(yael_native_template("not_a_template"))

  # the unused segmentation and background maps may be missing
  unlink(file.path(root, c("atropos_0.nii.gz", "atropos_1.nii.gz")))
  expect_no_error(yael_native_template("mni_icbm152_nlin_asym_09a"))
  unlink(file.path(root, "atropos_5.nii.gz"))
  expect_error(yael_native_template("mni_icbm152_nlin_asym_09a"), "atropos_5\\.nii\\.gz")
  unlink(file.path(root, "T1_brainmask.nii.gz"))
  expect_error(yael_native_template("mni_icbm152_nlin_asym_09a"), "T1_brainmask\\.nii\\.gz")

  # the template mask is binarized when read (`fsaverage` stores intensities)
  mask <- array(c(0, 3, 118, 0.2), c(2, 2, 2))
  path <- yael_native_write_volume(mask, file.path(root, "mask.nii.gz"), vox2ras = diag(4))
  expect_equal(as.vector(yael_native_read_mask(path)), as.vector((mask > 0) * 1))
})

# ---- Stage cache -------------------------------------------------------------------

test_that("stages are stored and reloaded like rpyANTs `StageContext`", {
  # the registration manifest records the source grid since 'ravetools' 0.3.3
  skip_if_no_native_yael()
  root <- tempfile(pattern = "ravecore-yael-stage-")
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  vox2ras <- diag(c(2, 2, 2, 1))
  image <- array(seq_len(60) / 3, c(5, 4, 3))
  attr(image, "vox2ras") <- vox2ras
  runs <- 0
  compute <- function(value) {
    function() {
      runs <<- runs + 1
      value
    }
  }

  # single image
  re <- yael_native_stage("toy", "image", root, compute(image), verbose = FALSE)
  expect_identical(re, image)
  expect_true(file.exists(file.path(root, "toy.nii.gz")))
  re <- yael_native_stage("toy", "image", root, compute(image), verbose = FALSE)
  expect_equal(runs, 1)
  expect_equal(re, image, tolerance = 1e-6)

  # several images, `<name>_<i>.nii.gz` from 0
  images <- list(image, image * 2, image + 1)
  re <- yael_native_stage("multi", "images", root, compute(images), n = 3,
                          verbose = FALSE)
  expect_equal(runs, 2)
  expect_true(all(file.exists(file.path(root, sprintf("multi_%d.nii.gz", 0:2)))))
  re <- yael_native_stage("multi", "images", root, compute(images), n = 3,
                          verbose = FALSE)
  expect_equal(runs, 2)
  expect_equal(re, images, tolerance = 1e-6)
  # an incomplete cache is computed again
  unlink(file.path(root, "multi_1.nii.gz"))
  yael_native_stage("multi", "images", root, compute(images), n = 3, verbose = FALSE)
  expect_equal(runs, 3)

  # registration: 'ANTs' transform files, manifest, and both warped images
  field <- array(stats::runif(5 * 4 * 3 * 3, -1, 1), c(5, 4, 3, 3))
  attr(field, "vox2ras") <- vox2ras
  transform <- yael_toy_rotation(3, 5)
  transform[1:3, 4] <- c(1, -2, 3)
  registration <- structure(list(
    transform = transform, forward_field = field, inverse_field = -field,
    type = "syn_only", metric = "mattes",
    geometry = list(source_vox2ras = vox2ras, target_vox2ras = vox2ras,
                    target_dim = c(5L, 4L, 3L), source_dim = c(5L, 4L, 3L))
  ), class = "ravetools_register_volume3d")
  result <- list(registration = registration, warpedmovout = image,
                 warpedfixout = image * 3)
  yael_native_stage("reg", "registration", root, compute(result), verbose = FALSE)
  expect_equal(runs, 4)
  expect_setequal(
    list.files(root, pattern = "^reg"),
    c("reg.dcf", "reg0GenericAffine.mat", "reg1Warp.nii.gz",
      "reg1InverseWarp.nii.gz", "reg_warped_mov.nii.gz", "reg_warped_fix.nii.gz"))
  re <- yael_native_stage("reg", "registration", root, compute(result), verbose = FALSE)
  expect_equal(runs, 4)
  expect_s3_class(re$registration, "ravetools_register_volume3d")
  expect_equal(re$registration$transform, transform, tolerance = 1e-10)
  expect_equal(as.vector(re$registration$forward_field), as.vector(field), tolerance = 1e-6)
  expect_equal(as.vector(re$registration$inverse_field), -as.vector(field), tolerance = 1e-6)
  expect_equal(re$registration$geometry$source_dim, c(5L, 4L, 3L))
  expect_equal(re$warpedmovout, image, tolerance = 1e-6)
  expect_equal(re$warpedfixout, image * 3, tolerance = 1e-6)
  expect_equal(ravetools::read_ants_transform(file.path(root, "reg0GenericAffine.mat")),
               transform, tolerance = 1e-10)

  # stored again without `warpedfixout`: the previous image does not stay
  unlink(file.path(root, "reg_warped_mov.nii.gz"))
  result$warpedfixout <- NULL
  yael_native_stage("reg", "registration", root, compute(result), verbose = FALSE)
  expect_equal(runs, 5)
  expect_false(file.exists(file.path(root, "reg_warped_fix.nii.gz")))
  re <- yael_native_stage("reg", "registration", root, compute(result), verbose = FALSE)
  expect_equal(runs, 5)
  expect_null(re$warpedfixout)
})

# ---- Rigid co-registration ---------------------------------------------------------

test_that("rigid co-registration recovers a known CT/MR transform in both directions", {
  skip_on_cran()
  root <- tempfile(pattern = "ravecore-yael-rigid-")
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  ph <- yael_toy_ct_mr(root)

  # fixed MR, moving CT: the transform maps MR RAS to CT RAS (`A`)
  reg <- yael_native_coregister_rigid(ph$mr_path, ph$ct_path, verbose = FALSE)
  error <- yael_toy_rigid_error(reg$transform, ph$A, ph$brain)
  expect_lt(error[["angle"]], 1)
  expect_lt(error[["rms"]], 1)
  expect_equal(dim(reg$image), c(48L, 48L, 48L))

  # fixed CT, moving MR (`reverse = TRUE` of `register_to_T1w`)
  reg <- yael_native_coregister_rigid(ph$ct_path, ph$mr_path, verbose = FALSE)
  error <- yael_toy_rigid_error(reg$transform, solve(ph$A), ph$A %*% ph$brain)
  expect_lt(error[["angle"]], 1)
  expect_lt(error[["rms"]], 1)
  expect_equal(dim(reg$image), c(100L, 90L, 60L))

  # arrays with their own grid work as well
  reg2 <- yael_native_coregister_rigid(yael_native_read_volume(ph$ct_path),
                                       yael_native_read_volume(ph$mr_path),
                                       verbose = FALSE)
  expect_equal(reg2$transform, reg$transform, tolerance = 1e-8)
})

# ---- Normalization ------------------------------------------------------------------

test_that("normalization checks its inputs", {
  root <- tempfile(pattern = "ravecore-yael-normalize-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  file.create(file.path(root, c("T1.nii.gz", "T2.nii.gz", "native.nii.gz")))
  expect_error(
    yael_native_normalize(
      fix_path = file.path(root, c("T1.nii.gz", "T2.nii.gz")),
      mov_paths = file.path(root, "native.nii.gz"),
      working_path = file.path(root, "work"), verbose = FALSE),
    "must have the same length")
  expect_error(
    yael_native_normalize(
      fix_path = file.path(root, "T1.nii.gz"),
      mov_paths = file.path(root, "native.nii.gz"), weights = c(1, 2),
      working_path = file.path(root, "work"), verbose = FALSE),
    "weights must be")
  # template folder without brain mask and tissue priors
  expect_error(
    yael_native_normalize(
      fix_path = file.path(root, "T1.nii.gz"),
      mov_paths = file.path(root, "native.nii.gz"),
      working_path = file.path(root, "work"), verbose = FALSE),
    "T1_brainmask\\.nii\\.gz")
  expect_false(dir.exists(file.path(root, "work")))
})

test_that("folding deformations, the smoothing of large templates and their memory are checked", {
  # x + u(x) with u = (a x, 0, 0) in RAS: the Jacobian determinant is 1 + a
  # everywhere, also on an oblique grid
  dm <- c(12L, 10L, 8L)
  vox2ras <- yael_toy_rotation(1, 20) %*% yael_toy_rotation(3, -30) %*%
    diag(c(1.5, 2, 0.5, 1))
  vox2ras[1:3, 4] <- c(-10, 5, 3)
  shear <- function(a, vox2ras) {
    u <- array(0, c(dm, 3))
    u[, , , 1] <- a * yael_toy_grid(dm, vox2ras)[, 1]
    attr(u, "vox2ras") <- vox2ras
    u
  }
  mask <- array(1, dm)
  expect_equal(yael_native_folding(shear(0.5, vox2ras), mask), 0)
  expect_equal(yael_native_folding(shear(-1.5, vox2ras), mask), 1)
  # on an axis-aligned grid, folding in the three lower inner slices only;
  # the outermost voxel layer and the voxels outside the mask do not count
  vox2ras <- diag(c(1.5, 2, 0.5, 1))
  u <- shear(0.5, vox2ras)
  folded_slices <- 2:4
  u[, , folded_slices, 1] <- -1.5 * array(yael_toy_grid(dm, vox2ras)[, 1], dm)[, , folded_slices]
  expect_equal(yael_native_folding(u, mask), 0.5)
  expect_equal(yael_native_folding(u, mask, slab = 1L), 0.5)
  mask[, , 2:4] <- 0
  expect_equal(yael_native_folding(u, mask), 0)
  mask[] <- 0
  mask[1, , ] <- 1
  expect_true(is.na(yael_native_folding(u, mask)))
  expect_error(yael_native_folding(u, array(1, dm - 1L)), "grid of `field`")

  # update fields are smoothed relative to the brain size in voxels: the
  # 1 mm human templates (09a, 09c, fsaverage) keep the rpyANTs values, the
  # 0.5 mm ones (09b) double them, the 0.25 mm macaque template (NMT) takes
  # one and a half times as much; the toy template keeps them
  brain_voxels <- c(asym_09a = 2047633, asym_09c = 1886574, fsaverage = 2465242,
                    asym_09b = 15562743, sym_09b = 15092094, nmt = 5903261,
                    toy = 34000, none = 0)
  expect_equal(vapply(brain_voxels, yael_native_syn_scale, 0.0),
               c(asym_09a = 1, asym_09c = 1, fsaverage = 1, asym_09b = 2,
                 sym_09b = 2, nmt = 1.5, toy = 1, none = 1))
  expect_equal(yael_native_syn_scale(NA), 1)

  # templates with more than 30 million voxels are flagged below 32 GB of
  # memory (the header is mocked: such files are large)
  skip_if_old_testthat()
  dims <- list(small = c(197L, 233L, 189L), large = c(394L, 466L, 378L))
  testthat::local_mocked_bindings(
    yael_native_read_header = function(path) { list(dim = dims[[basename(path)]]) })
  logged <- character(0)
  testthat::local_mocked_bindings(
    logger = function(..., level = "info", use_glue = FALSE) {
      logged <<- c(logged, paste0(level, ": ", paste0(..., collapse = "")))
    },
    .package = "ravepipeline")
  expect_false(yael_native_check_memory(file.path("09b", "large"), max_mem = 16))
  expect_length(logged, 1)
  expect_match(logged, "^warning: The template image `09b` has 69 million voxels")
  expect_match(logged, "16 GB", fixed = TRUE)
  expect_true(yael_native_check_memory(file.path("09b", "large"), max_mem = 64))
  expect_true(yael_native_check_memory(file.path("09a", "small"), max_mem = 8))
  expect_true(yael_native_check_memory(file.path("09b", "large"), max_mem = "unknown"))
  expect_length(logged, 1)
})

test_that("the white matter of stage `nu2` is found despite extreme intensities", {
  skip_if_no_native_yael()
  # a toy brain: white matter (100), gray matter (70) and CSF (30)
  set.seed(1)
  dm <- c(30L, 30L, 30L)
  grid <- expand.grid(x = 1:30, y = 1:30, z = 1:30)
  r <- sqrt((grid$x - 15.5)^2 + (grid$y - 15.5)^2 + (grid$z - 15.5)^2)
  t1 <- array(ifelse(r < 6, 100, ifelse(r < 9, 70, ifelse(r < 12, 30, 0))) +
                stats::rnorm(nrow(grid), sd = 3), dm)
  mask <- array((r < 12) * 1, dm)
  kmeans3 <- function(x, mask) {
    ravetools::segment_volume_tissue_gmm(
      x, mask = mask, n_classes = 3L, mrf_beta = 0.1, mrf_radius = 1L,
      iterations = 5L, vox2ras = diag(4))
  }

  # the raw image is segmented as in rpyANTs (classes ordered by intensity)
  wm <- yael_native_kmeans_wm(t1, mask, diag(4), verbose = FALSE)
  expect_equal(wm$prob, kmeans3(t1, mask)$posteriors[[3]])
  expect_equal(wm$mean, 100, tolerance = 0.01)

  # one voxel six times brighter than the white matter leaves a k-means class
  # empty, an error of 'ravetools' ('Atropos' goes on): the intensities are
  # clipped for the segmentation, and the mean still comes from the raw image
  t1[15, 15, 15] <- 600
  expect_error(kmeans3(t1, mask), "received no voxels")
  wm <- yael_native_kmeans_wm(t1, mask, diag(4), verbose = FALSE)
  expect_equal(wm$mean, 100, tolerance = 0.02)
  expect_gt(wm$mean, 100.3)
  expect_equal(sum(wm$prob > 0.5), sum(r < 6), tolerance = 0.05)
  # other errors are not hidden
  expect_error(yael_native_kmeans_wm(t1, array(0, dm), diag(4), verbose = FALSE),
               "mask contains no voxel")
})

test_that("rays away from a center find the points outside a surface", {
  skip_if_no_native_yael()
  sphere <- function(radius) {
    mesh <- ravetools::vcg_sphere(3L)
    mesh$vb[1:3, ] <- mesh$vb[1:3, ] * radius
    mesh
  }

  # the ray away from the center hits a closed surface from inside, and
  # misses it from outside; the center itself is inside
  outer <- sphere(10)
  expect_equal(
    yael_native_check_maybe_outside(
      outer, rbind(c(0, 0, 0), c(3, 2, 1), c(-4, 5, -6), c(15, 0, 0), c(0, 0, 20)),
      center = c(0, 0, 0)),
    c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_length(yael_native_check_maybe_outside(outer, matrix(0, 0, 3), c(0, 0, 0)), 0)

  # two sheets, like the outer and the inner surface of the cortex: the
  # points in the inner cavity and between the sheets are inside
  nested <- outer
  inner <- sphere(5)
  nested$vb <- cbind(outer$vb, inner$vb)
  nested$it <- cbind(outer$it, inner$it + ncol(outer$vb))
  nested$normals <- NULL
  expect_equal(
    yael_native_check_maybe_outside(
      nested, rbind(c(1, 0, 0), c(7, 0, 0), c(12, 0, 0)), center = c(0, 0, 0)),
    c(FALSE, FALSE, TRUE))
})

test_that("white matter outside the gray matter is left out of the white-matter channel", {
  skip_if_no_native_yael()
  # a toy brain: white matter (r < 16) in a closed gray-matter shell, CSF, a
  # cap of marrow on top that is segmented as white matter, and a brain stem
  # that leaves the brain downward through the gray matter; the voxels are
  # flipped and shifted in 'RAS', like a scanner grid
  n <- 64L
  grid <- expand.grid(x = seq_len(n), y = seq_len(n), z = seq_len(n))
  center <- (n + 1) / 2
  r <- array(sqrt((grid$x - center)^2 + (grid$y - center)^2 + (grid$z - center)^2),
             rep(n, 3))
  cap <- r >= 21 & r < 23 & array(grid$z > center + 14, rep(n, 3))
  stem <- r < 23 & array((grid$x - center)^2 + (grid$y - center)^2 <= 9 &
                           grid$z < center, rep(n, 3))
  labels <- array(0, rep(n, 3))
  labels[r < 23] <- 1
  labels[r < 19] <- 2
  labels[r < 16] <- 3
  labels[cap | stem] <- 3
  vox2ras <- diag(c(-1, 1, 1, 1))
  vox2ras[1:3, 4] <- c(40, -30, -20)
  dgwbs <- (labels == 3) * 1
  to_ras <- function(index) {
    ijk <- arrayInd(index, dim(labels)) - 1
    t(vox2ras[1:3, 1:3] %*% t(ijk) + vox2ras[1:3, 4])
  }

  # the cap, dilated by two voxels, with the gray-matter surface as it is and
  # with its convex hull (the shell is convex); the brain stem is kept
  # although its lowest voxel lies outside the gray matter
  near_cap <- array(ravetools::grow_volume(cap * 1, 5) > 0, dim(cap))
  for (method in c("none", "convex_hull")) {
    artifacts <- yael_native_clean_wm_mask(labels, vox2ras, dgwbs = dgwbs,
                                           method = method)
    expect_equal(attr(artifacts, "vox2ras"), vox2ras)
    expect_equal(attr(artifacts, "artifact_voxels"), sum(cap), label = method)
    expect_equal(which(artifacts > 0), which(near_cap), label = method)
    expect_false(any(artifacts[r < 16 | stem] > 0), label = method)
  }
  lowest <- which(stem)[which.min(grid$z[which(stem)])]
  expect_true(yael_native_check_maybe_outside(
    yael_native_gm_envelope(labels, vox2ras), to_ras(lowest),
    center = colMeans(to_ras(which(dgwbs > 0.5)))))
  expect_equal(
    yael_native_brainstem_cone(rbind(c(0, 0, -5), c(0, 0, 5), c(3, 4, -4), c(3, 4, -6)),
                               c(0, 0, 0)),
    c(TRUE, FALSE, FALSE, TRUE))

  # without the cap, nothing is left out
  no_cap <- labels
  no_cap[cap] <- 1
  expect_equal(sum(yael_native_clean_wm_mask(no_cap, vox2ras, dgwbs = dgwbs)), 0)

  # the convex hull is a closed mesh around the gray matter
  hull <- yael_native_gm_envelope(labels, vox2ras, "convex_hull")
  expect_s3_class(hull, "mesh3d")
  expect_equal(ravetools::vcg_mesh_volume(hull), 4 / 3 * pi * 19^3, tolerance = 0.05)
})

test_that("tissue classes out of intensity order give a warning", {
  labels <- array(rep(1:3, each = 8), c(4, 3, 2))
  nu2 <- array(rep(c(30, 70, 110), each = 8), c(4, 3, 2))
  expect_silent(ordered <- yael_native_segmentation_class_order(nu2, labels))
  expect_true(ordered)
  swapped <- array(rep(c(70, 30, 110), each = 8), c(4, 3, 2))
  expect_warning(ordered <- yael_native_segmentation_class_order(swapped, labels),
                 "not ordered by intensity")
  expect_false(ordered)
})

test_that("normalization aligns a toy head to a toy template", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  work <- tempfile(pattern = "ravecore-yael-normalize-")
  on.exit({ unlink(work, recursive = TRUE) }, add = TRUE)

  spy <- local_yael_registration_spy()
  started <- Sys.time()
  res <- yael_native_normalize(template$t1, native$t1, work, weights = 1.25,
                               verbose = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  expect_lt(elapsed, 120)

  # registrations: SyNabp (affine, then deformable without masks), then the
  # two stages of 'SyNAggro': the affine with both masks, the deformable
  # stage with the moving mask only (with a target mask, 'ravetools' pushes
  # the edge of the brain outward). The deformable stage starts from the
  # masked affine and adds the CSF and deep-structure posteriors
  calls <- spy()
  expect_equal(vapply(calls, `[[`, "", "type"),
               c("affine", "syn_only", "affine", "syn_only"))
  expect_equal(vapply(calls, `[[`, FALSE, "source_masked"), c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(vapply(calls, `[[`, FALSE, "target_masked"), c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(calls[[1]]$shrink_factors, c(8, 4, 2, 1))
  expect_equal(calls[[2]]$init_transform, calls[[1]]$transform)
  expect_equal(calls[[2]]$syn_iterations, c(50, 10, 0))
  expect_equal(calls[[2]]$syn_grad_step, 0.1)
  # the update-field smoothing of rpyANTs, unscaled for a template with
  # fewer brain voxels than a 1 mm human template
  expect_equal(calls[[2]]$syn_sigma, 3)
  # the convergence of 'antsRegistration' for 'SyNabp', of the 'ANTsPy' 'SyN'
  # transforms for the final deformable stage
  expect_equal(c(calls[[2]]$syn_convergence, calls[[2]]$syn_convergence_window), c(1e-6, 10))
  expect_equal(c(calls[[4]]$syn_convergence, calls[[4]]$syn_convergence_window), c(1e-7, 8))
  expect_null(calls[[3]]$init_transform)
  expect_equal(calls[[3]]$shrink_factors, c(4, 2, 2, 1))
  expect_equal(calls[[3]]$iterations, c(2100, 1200, 1200, 100))
  expect_equal(calls[[4]]$init_transform, calls[[3]]$transform)
  expect_equal(calls[[4]]$channels, 3L)
  expect_equal(calls[[4]]$weights, c(1, 0.1, 0.5))
  expect_equal(calls[[4]]$syn_grad_step, 0.15)
  expect_equal(calls[[4]]$syn_sigma, 3.5)
  expect_equal(res$registration$transform, calls[[3]]$transform)

  # outputs: fields on the template grid, warped images on the other grid
  reg <- res$registration
  expect_s3_class(reg, "ravetools_register_volume3d")
  expect_null(reg$image)
  expect_equal(dim(reg$forward_field), c(template$dim, 3L))
  expect_equal(dim(reg$inverse_field), c(template$dim, 3L))
  expect_equal(dim(res$warpedmovout), template$dim)
  expect_equal(attr(res$warpedmovout, "vox2ras"), template$vox2ras, tolerance = 1e-6)
  expect_equal(dim(res$warpedfixout), native$dim)
  expect_equal(attr(res$warpedfixout, "vox2ras"), native$vox2ras, tolerance = 1e-6)
  expect_setequal(
    list.files(work),
    c("T1.nii.gz", "nu.nii.gz", "fixing.nii.gz", "nu2.nii.gz", "brainmask.nii.gz",
      "brain.nii.gz", "wm_artifacts.nii.gz", "brain_mask_cleaned.nii.gz",
      sprintf("atropos_prior_%d.nii.gz", 0:5), sprintf("atropos_%d.nii.gz", 0:4),
      "brain2.nii.gz", "brainmask2.nii.gz",
      "SyNabp.dcf", "SyNabp0GenericAffine.mat", "SyNabp1Warp.nii.gz",
      "SyNabp1InverseWarp.nii.gz", "SyNabp_warped_mov.nii.gz",
      "SyN_w_atropos.dcf", "SyN_w_atropos0GenericAffine.mat",
      "SyN_w_atropos1Warp.nii.gz", "SyN_w_atropos1InverseWarp.nii.gz",
      "SyN_w_atropos_warped_mov.nii.gz", "SyN_w_atropos_warped_fix.nii.gz"))
  # little white matter is left out as artifacts (none when this test was
  # written), and the cleaned brain mask is the template mask without the
  # artifacts labeled white matter
  wm_artifacts <- yael_native_read_volume(file.path(work, "wm_artifacts.nii.gz"))
  expect_equal(dim(wm_artifacts), native$dim)
  expect_lt(mean(wm_artifacts[native$truth == 3] > 0), 0.05)
  segmentation <- yael_native_read_volume(file.path(work, "atropos_0.nii.gz"))
  expect_equal(
    as.vector(yael_native_read_volume(file.path(work, "brain_mask_cleaned.nii.gz")) > 0),
    as.vector(yael_native_read_volume(file.path(work, "brainmask.nii.gz")) > 0 &
                !(wm_artifacts > 0 & segmentation == 3)))

  # the template labels on the native grid overlap the true labels
  labels <- yael_native_read_volume(template$atropos[[1]])
  warped <- ravetools::apply_transform3d_volume(
    labels, reg, vox2ras = attr(labels, "vox2ras"), direction = "inverse",
    reference_dim = native$dim, reference_vox2ras = native$vox2ras,
    interpolation = "nearest")
  dice <- yael_toy_dice(warped, native$truth)
  expect_true(all(dice >= 0.85), label = sprintf("Dice %s", paste(round(dice, 3), collapse = ", ")))

  # the deformation is one-to-one inside the template brain
  template_mask <- yael_native_read_mask(template$mask)
  expect_equal(dim(template_mask), template$dim)
  expect_lt(yael_native_folding(reg$forward_field, template_mask), 0.001)

  # points inside the brain, at least 2 voxels from the grid borders, go to
  # the template and back
  ijk <- which(native$truth > 0, arr.ind = TRUE) - 1
  inner <- apply(ijk, 1, function(v) all(v >= 2 & v <= native$dim - 3))
  points <- yael_toy_grid(native$dim, native$vox2ras)[which(native$truth > 0)[inner], ]
  to_template <- ravetools::apply_transform3d_points(points, reg, direction = "forward")
  template_ijk <- t(solve(template$vox2ras) %*% rbind(t(to_template), 1))[, 1:3]
  inner <- apply(template_ijk, 1, function(v) all(v >= 2 & v <= template$dim - 3))
  back <- ravetools::apply_transform3d_points(to_template[inner, ], reg, direction = "inverse")
  round_trip <- sqrt(rowSums((back - points[inner, ])^2))
  expect_lte(stats::median(round_trip), 0.05)
  # ... and land near their known template positions
  error <- sqrt(rowSums((to_template - yael_toy_native_to_template(points))^2))
  expect_lte(stats::median(error), 1.5)
  # the deformable stage matters: where the bumps move the points by more than
  # 1.5 mm, the affine part alone is much further off (0.66 versus 1.60 mm
  # when this test was written), although the medians over the whole brain
  # are about the same
  affine_points <- t(solve(reg$transform) %*% rbind(t(points), 1))[, 1:3]
  errors <- yael_toy_bump_errors(points, to_template, affine_points)
  expect_gt(errors[["n"]], 500)
  expect_lt(errors[["full"]], 0.6 * errors[["affine"]],
            label = sprintf("SyN %.3f mm vs affine %.3f mm", errors[["full"]], errors[["affine"]]))

  # the final brain mask and image are staged next to the others
  brain_mask2 <- yael_native_read_volume(file.path(work, "brainmask2.nii.gz"))
  expect_gt(2 * sum(brain_mask2 > 0 & native$truth > 0) /
              (sum(brain_mask2 > 0) + sum(native$truth > 0)), 0.9)

  # normalizing again uses the cached stages
  res2 <- yael_native_normalize(template$t1, native$t1, work, verbose = FALSE)
  expect_length(spy(), 4)
  expect_equal(res2$registration$transform, reg$transform, tolerance = 1e-8)
  expect_equal(as.vector(res2$registration$forward_field),
               as.vector(reg$forward_field), tolerance = 1e-5)
  expect_equal(as.vector(res2$warpedmovout), as.vector(res$warpedmovout), tolerance = 1e-4)
})

test_that("white matter outside the gray matter leaves the white-matter channel", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  work <- tempfile(pattern = "ravecore-yael-normalize-")
  dir.create(work)
  on.exit({ unlink(work, recursive = TRUE) }, add = TRUE)

  # a brain mask that took in the skull and the inner half of the scalp on top
  # of the head (the toy scalp is brighter than the white matter, like a thick
  # skull marrow; the edge of the mask cuts through it); the `brainmask`
  # stage reads it from its cache
  points <- yael_toy_grid(native$dim, native$vox2ras)
  head <- yael_toy_native_to_template(points)
  codes <- array(yael_toy_codes(head), native$dim)
  inner_scalp <- (head[, 1] / 44)^2 + ((head[, 2] - 2) / 54)^2 + (head[, 3] / 42)^2 <= 1
  leak <- codes >= 7 & array(inner_scalp & points[, 3] > 15, native$dim)
  mask <- (codes >= 1 & codes <= 6 | leak) * 1
  yael_native_write_volume(mask, file.path(work, "brainmask.nii.gz"),
                           vox2ras = native$vox2ras)

  spy <- local_yael_registration_spy(keep_sources = TRUE)
  res <- yael_native_normalize(template$t1, native$t1, work, verbose = FALSE)

  # the scalp segmented as white matter is left out of the white-matter
  # channel, of the brain mask and of the brain image; the white matter and
  # the brain stem are not (all 729 such scalp voxels and none of the brain
  # when this test was written). The gray matter and CSF that the dilated
  # artifacts cover stay in the brain mask
  wm_artifacts <- yael_native_read_volume(file.path(work, "wm_artifacts.nii.gz"))
  segmentation <- yael_native_read_volume(file.path(work, "atropos_0.nii.gz"))
  scalp <- leak & codes == 8
  expect_gt(sum(scalp & segmentation == 3), 100)
  expect_gte(mean(wm_artifacts[scalp & segmentation == 3] > 0), 0.9)
  expect_lt(mean(wm_artifacts[codes == 3 | codes == 5] > 0), 0.05)
  removed <- wm_artifacts > 0 & segmentation == 3
  cleaned <- yael_native_read_volume(file.path(work, "brain_mask_cleaned.nii.gz"))
  expect_equal(as.vector(cleaned > 0), as.vector(mask > 0 & !removed))
  expect_true(any(wm_artifacts > 0 & segmentation != 3 & cleaned > 0))
  calls <- spy()
  for (call in calls[3:4]) {
    expect_equal(as.vector(call$source_mask > 0), as.vector(cleaned > 0))
  }
  # only the affine stage has a target mask; the fixed tissue channels of the
  # deformable stage are zero outside the template brain, like the moving
  # channels outside the native brain
  expect_false(is.null(calls[[3]]$target_mask))
  expect_null(calls[[4]]$target_mask)
  template_brain <- yael_native_read_volume(template$mask) > 0
  for (channel in calls[[4]]$targets[2:3]) {
    expect_equal(dim(channel), dim(template_brain))
    expect_true(all(channel[!template_brain] == 0))
  }
  sources <- calls[[4]]$sources
  expect_length(sources, 3L)
  expect_true(all(sources[[1]][removed] == 0))
  # the channels are the posteriors (read back from the stage files). In the
  # CSF channel, the removed white matter is CSF, while the gray matter and
  # CSF that the dilated artifacts cover keep their own posterior
  csf <- yael_native_read_volume(file.path(work, "atropos_1.nii.gz"))
  dgwbs <- yael_native_read_volume(file.path(work, "atropos_3.nii.gz"))
  expect_true(all(sources[[2]][removed] == 1))
  kept <- wm_artifacts > 0 & segmentation != 3
  expect_gt(sum(kept), 0)
  expect_equal(sources[[2]][kept], csf[kept], tolerance = 1e-6)
  expect_true(all(sources[[3]][wm_artifacts > 0] == 0))
  expect_equal(sources[[3]][wm_artifacts == 0], dgwbs[wm_artifacts == 0], tolerance = 1e-6)

  # the template labels on the native grid still overlap the true labels. The
  # mask took in the skull and the scalp on top of the head; without a target
  # mask, the deformable stage pulls them into the template brain, which
  # thins the CSF (label 1; Dice 0.839 when this test was written)
  labels <- yael_native_read_volume(template$atropos[[1]])
  warped <- ravetools::apply_transform3d_volume(
    labels, res$registration, vox2ras = attr(labels, "vox2ras"),
    direction = "inverse", reference_dim = native$dim,
    reference_vox2ras = native$vox2ras, interpolation = "nearest")
  dice <- yael_toy_dice(warped, native$truth)
  expect_true(dice[[1]] >= 0.8 && all(dice[-1] >= 0.85),
              label = sprintf("Dice %s", paste(round(dice, 3), collapse = ", ")))
})

test_that("normalization uses additional images that cover the brain", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  work <- tempfile(pattern = "ravecore-yael-normalize-")
  # not a 3D volume
  frames <- tempfile(fileext = ".nii.gz")
  on.exit({ unlink(c(work, frames), recursive = TRUE) }, add = TRUE)
  ieegio::write_volume(ieegio::as_ieegio_volume(array(1, c(8, 8, 8, 2)),
                                                vox2ras = native$vox2ras), frames)

  spy <- local_yael_registration_spy(keep_sources = TRUE)
  warnings <- character(0)
  res <- withCallingHandlers(
    yael_native_normalize(template$t1, c(native$t1, native$t2, native$t2_partial, frames),
                          work, weights = c(1.25, 0.9, 0.8, 0.7), verbose = FALSE),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(warnings, 2)
  expect_match(warnings, "T2_partial\\.nii\\.gz.*less than 95%", all = FALSE)
  expect_match(warnings, "not a 3D volume", all = FALSE)

  # the T2 image is the second channel, with its weight
  calls <- spy()
  expect_length(calls, 4)
  expect_equal(calls[[4]]$channels, 4L)
  expect_equal(calls[[4]]$weights, c(1, 0.9, 0.1, 0.5))
  expect_equal(calls[[4]]$init_transform, calls[[3]]$transform)
  expect_equal(dim(res$warpedfixout), native$dim)
  # ... moved from its own oblique grid onto the native grid, where it
  # matches the anatomy of the toy subject; the voxels it does not cover are
  # filled
  t2_channel <- calls[[4]]$sources[[2]]
  expect_equal(dim(t2_channel), native$dim)
  expect_false(anyNA(t2_channel))
  inside <- native$truth > 0
  expect_gt(stats::cor(t2_channel[inside], yael_toy_native_t2(native)[inside]), 0.8)
  # its template image (the template with skull) is zero outside the
  # template brain, like the moving channels outside the native brain
  template_brain <- yael_native_read_volume(template$mask) > 0
  expect_true(all(calls[[4]]$targets[[2]][!template_brain] == 0))
  expect_true(any(calls[[4]]$targets[[2]][template_brain] != 0))

  # other cost functions are not available for the deformable stage
  expect_warning(
    yael_native_normalize(template$t1, native$t1, work, cost_function = "MI",
                          verbose = FALSE),
    "not available")
})

test_that("normalization handles priors on the template grid and an empty tissue prior", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  # tissue priors on the 'T1' grid, an empty cerebellum prior, and a brain
  # mask that holds intensities (like the mask of `fsaverage`)
  template <- yael_toy_dataset("template_shared")
  native <- yael_toy_dataset("native")
  work <- tempfile(pattern = "ravecore-yael-normalize-")
  on.exit({ unlink(work, recursive = TRUE) }, add = TRUE)
  expect_gt(max(yael_native_read_volume(template$mask)), 1)

  expect_warning(
    res <- yael_native_normalize(template$t1, native$t1, work, verbose = FALSE),
    "class 4 is zero inside the brain mask")

  # the template mask is binarized: the brain masks of the stages are binary
  for (name in c("brainmask.nii.gz", "brainmask2.nii.gz")) {
    brain_mask <- yael_native_read_volume(file.path(work, name))
    expect_equal(sort(unique(as.vector(brain_mask))), c(0, 1), label = name)
  }

  # the cerebellum posterior is empty, the other classes are segmented
  segmentation <- yael_native_read_volume(file.path(work, "atropos_0.nii.gz"))
  expect_setequal(unique(as.vector(segmentation)), 0:3)
  expect_equal(sum(yael_native_read_volume(file.path(work, "atropos_4.nii.gz"))), 0)
  expect_gt(sum(yael_native_read_volume(file.path(work, "atropos_1.nii.gz"))), 0)

  # coarser priors and no cerebellum class: a little less accurate than the
  # main toy (smallest Dice 0.855 to 0.874 over five noise seeds)
  labels <- yael_native_read_volume(template$atropos[[1]])
  warped <- ravetools::apply_transform3d_volume(
    labels, res$registration, vox2ras = attr(labels, "vox2ras"),
    direction = "inverse", reference_dim = native$dim,
    reference_vox2ras = native$vox2ras, interpolation = "nearest")
  dice <- yael_toy_dice(warped, native$truth)
  label <- sprintf("Dice %s", paste(round(dice, 3), collapse = ", "))
  expect_true(all(dice >= 0.8), label = label)
  expect_gte(mean(dice), 0.85, label = label)
})

test_that("normalization completes when a voxel inside the brain is extremely bright", {
  skip_on_cran()
  skip_if_no_native_yael()
  skip_if_old_testthat()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  root <- tempfile(pattern = "ravecore-yael-bright-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  # one voxel near the middle of the white matter, 20 times brighter than the
  # rest of the head (a vessel with contrast, or a reconstruction spike)
  t1 <- yael_native_read_volume(native$t1)
  wm <- which(native$truth == 3, arr.ind = TRUE)
  spot <- wm[which.min(rowSums(sweep(wm, 2, colMeans(wm))^2)), ]
  t1[spot[[1]], spot[[2]], spot[[3]]] <- 20 * max(t1)
  path <- yael_native_write_volume(t1, file.path(root, "bright.nii.gz"))

  # the k-means of the raw image fails and is run again with clipped
  # intensities; then the segmentation with the template priors runs
  outcomes <- character(0L)
  segment <- ravetools::segment_volume_tissue_gmm
  testthat::local_mocked_bindings(
    segment_volume_tissue_gmm = function(...) {
      result <- tryCatch(segment(...), error = function(e) { e })
      outcomes <<- c(outcomes, ifelse(inherits(result, "error"), "error", "ok"))
      if (inherits(result, "error")) { stop(result) }
      result
    },
    .package = "ravetools"
  )
  res <- yael_native_normalize(template$t1, path, file.path(root, "work"),
                               verbose = FALSE)
  expect_equal(outcomes, c("error", "ok", "ok"))
  expect_s3_class(res$registration, "ravetools_register_volume3d")
  expect_true(file.exists(file.path(root, "work", "SyN_w_atropos.dcf")))
  # the white matter was scaled to about 110, not the bright voxel
  nu2 <- yael_native_read_volume(file.path(root, "work", "nu2.nii.gz"))
  expect_gt(stats::median(nu2[native$truth == 3]), 50)
})

test_that("normalization starts from a given affine and can keep the skull", {
  skip_on_cran()
  skip_if_no_native_yael()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  work <- tempfile(pattern = "ravecore-yael-normalize-")
  on.exit({ unlink(work, recursive = TRUE) }, add = TRUE)

  # an affine close to the truth (template RAS to native RAS), as an 'ANTs' file
  points <- yael_toy_grid(native$dim, native$vox2ras)[which(native$truth > 0), ]
  fit <- stats::lm.fit(cbind(yael_toy_native_to_template(points), 1), points)
  affine <- unname(rbind(t(fit$coefficients), c(0, 0, 0, 1)))
  affine_path <- file.path(work, "affine.mat")
  dir.create(work)
  ravetools::write_ants_transform(affine, affine_path)

  spy <- local_yael_registration_spy()
  res <- yael_native_normalize(template$t1, native$t1, file.path(work, "with_affine"),
                               affine_transform = affine_path, verbose = FALSE)
  calls <- spy()
  # 'SyNOnly' from the affine, then the masked affine starts from it as well
  expect_equal(vapply(calls, `[[`, "", "type"), c("syn_only", "affine", "syn_only"))
  expect_equal(calls[[1]]$init_transform, affine, tolerance = 1e-10)
  expect_equal(calls[[1]]$syn_iterations, c(40, 20, 0))
  expect_equal(calls[[1]]$syn_grad_step, 0.2)
  expect_equal(c(calls[[1]]$syn_convergence, calls[[1]]$syn_convergence_window), c(1e-7, 8))
  expect_equal(calls[[2]]$init_transform, affine, tolerance = 1e-10)
  expect_equal(calls[[3]]$init_transform, calls[[2]]$transform)

  labels <- yael_native_read_volume(template$atropos[[1]])
  warped <- ravetools::apply_transform3d_volume(
    labels, res$registration, vox2ras = attr(labels, "vox2ras"),
    direction = "inverse", reference_dim = native$dim,
    reference_vox2ras = native$vox2ras, interpolation = "nearest")
  dice <- yael_toy_dice(warped, native$truth)
  expect_true(all(dice >= 0.85), label = sprintf("Dice %s", paste(round(dice, 3), collapse = ", ")))

  # with the skull: the same masks, the default step of 'SyNAggro'. The
  # template is made to look like a 0.5 mm template (twice the smoothing of
  # the update fields) whose final deformation folds in 2 percent of the brain
  testthat::local_mocked_bindings(
    yael_native_syn_scale = function(brain_voxels) { 2 },
    yael_native_folding = function(field, mask, ...) { 0.02 })
  expect_warning(
    res <- yael_native_normalize(template$t1, native$t1, file.path(work, "with_skull"),
                                 with_skull = TRUE, verbose = FALSE),
    "folds \\(is not one-to-one\\) in 2\\.00% of the template brain mask")
  calls <- spy()[-(1:3)]
  expect_equal(vapply(calls, `[[`, "", "type"),
               c("affine", "syn_only", "affine", "syn_only"))
  expect_equal(vapply(calls, `[[`, FALSE, "masked"), c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(calls[[4]]$syn_grad_step, 0.2)
  expect_equal(c(calls[[2]]$syn_sigma, calls[[4]]$syn_sigma), c(6, 7))
  expect_equal(dim(res$warpedmovout), template$dim)
})

# ---- Workflow ------------------------------------------------------------------------

test_that("yael_preprocess_native runs the YAEL workflow on toy images", {
  skip_on_cran()
  # 'fs' refuses paths longer than 260 bytes on Windows (r-lib/fs#420)
  skip_on_os("windows")
  skip_if_no_native_yael()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  info <- local_yael_toy_template(template$root)
  root <- tempfile(pattern = "ravecore-yael-workflow-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  # a CT of the toy subject in a scanner space moved by `A` (native RAS to CT
  # RAS), and an atlas of the template
  A <- yael_toy_rotation(3, 6) %*% yael_toy_rotation(1, -4)
  A[1:3, 4] <- c(3, -4, 5)
  ct_path <- yael_toy_ct(file.path(root, "ct.nii.gz"), A)
  atlas_dir <- file.path(root, "atlas")
  dir.create(atlas_dir)
  yael_toy_write(yael_toy_template_labels(template$dim, template$vox2ras),
                 template$vox2ras, file.path(atlas_dir, "labels.nii.gz"))

  run <- function() {
    yael_preprocess_native(
      subject = "YAEL/YAELNativeWorkflow", t1w_path = native$t1, ct_path = ct_path,
      normalize_template = "mni_icbm152_nlin_asym_09a",
      atlases = list(mni_icbm152_nlin_asym_09a = atlas_dir),
      add_surfaces = FALSE, verbose = FALSE)
  }
  spy <- local_yael_registration_spy()
  expect_null(run())

  # the files of rpyANTs (and of `YAELProcess`), the conformed T1 and the atlas
  process <- as_yael_process("YAEL/YAELNativeWorkflow", engine = "native")
  work_path <- as.character(process$work_path)
  code <- process$subject_code
  camel <- info$camel
  files <- list.files(work_path, recursive = TRUE)
  files <- files[!startsWith(files, "tmp/")]
  expect_setequal(files, c(
    sprintf("inputs/anat/sub-%s_ses-preop_desc-preproc_T1w.nii.gz", code),
    sprintf("inputs/anat/sub-%s_ses-postop_desc-preproc_CT.nii.gz", code),
    "inputs/MRI/MRI_RAW.nii.gz",
    sprintf("coregistration/transformations/sub-%s_from-CT_to-T1w_desc-rigid_%s", code,
            c("ants.mat", "ras2ras.tsv", "vox2ras.tsv")),
    sprintf("coregistration/transformations/sub-%s_from-T1w_to-CT_desc-rigid_%s", code,
            c("ants.mat", "ras2ras.tsv")),
    sprintf("coregistration/anat/sub-%s_ses-postop_space-scanner_desc-preproc_CT.nii.gz", code),
    sprintf("coregistration/log/sub-%s_space-scanner_desc-preproc_orig-CT_mapping.json", code),
    "coregistration/CT_RAW.nii.gz", "coregistration/MRI_reference.nii.gz",
    "coregistration/CT_IJK_to_MR_RAS.txt",
    sprintf("normalization/transformations/sub-%s_from-T1w_to-%s_desc-affine+SyN_ants%s", code,
            camel, c("0.mat", "1.nii.gz")),
    sprintf("normalization/transformations/sub-%s_from-%s_to-T1w_desc-affine+SyN_ants%s", code,
            camel, c("0.nii.gz", "1.mat")),
    sprintf("normalization/anat/sub-%s_ses-preop_space-%s_desc-preproc_transform-deformable_T1w.nii.gz",
            code, camel),
    sprintf("normalization/anat/sub-%s_ses-preop_space-T1w_desc-preproc_transform-deformable_%s.nii.gz",
            code, camel),
    sprintf("normalization/log/sub-%s_desc-preproc_template-%s_native-T1w_mappings.json", code, camel),
    "ants/mri/T1.mgz", "ants/mri/brainmask.nii.gz", "ants/mri/nu.nii.gz",
    "ants/mri/brain.finalsurfs.nii.gz", "ants/mri/transforms/talairach.xfm",
    "atlases/labels.nii.gz", "atlases/labels.gii"
  ))

  # the CT was registered, in the right direction (CT RAS to native RAS is
  # solve(A)). Mutual information is flat for this toy pair (the bias field
  # and noise of the toy T1), so the result depends on the voxel sampler: over
  # sampler seeds 1 to 8 it ended 1.2 to 9.9 degrees (0.5 to 3.8 mm) from
  # solve(A), and 14.7 to 15.4 mm from the inverted transform; not
  # registering at all is about 7.5 mm off. The accuracy itself is tested
  # with the CT/MR phantom
  ct_to_t1 <- unname(as.matrix(utils::read.table(file.path(
    work_path, "coregistration", "transformations",
    sprintf("sub-%s_from-CT_to-T1w_desc-rigid_ras2ras.tsv", code)))))
  brain <- rbind(t(yael_toy_grid(native$dim, native$vox2ras)[which(native$truth > 0), ]), 1)
  error <- yael_toy_rigid_error(ct_to_t1, solve(A), A %*% brain)
  inverted <- yael_toy_rigid_error(ct_to_t1, A, A %*% brain)
  expect_lt(error[["rms"]], 0.5 * inverted[["rms"]])
  expect_lt(error[["rms"]], 5)
  expect_lt(error[["angle"]], 15)
  expect_equal(vapply(spy(), `[[`, "", "type"),
               c("rigid", "rigid", "affine", "syn_only", "affine", "syn_only"))

  # running it again with the "auto" policies keeps the registrations
  transform_files <- file.path(work_path, files[grepl("/transformations/", files)])
  mtimes <- file.info(transform_files)$mtime
  Sys.sleep(1)
  run()
  expect_equal(file.info(transform_files)$mtime, mtimes)
  expect_length(spy(), 6)
  expect_true(file.exists(file.path(work_path, "ants", "mri", "T1.mgz")))
})

test_that("the native workflow functions take the arguments of the rpyANTs ones", {
  defaults <- function(f) {
    fmls <- formals(f)
    structure(
      vapply(seq_along(fmls), function(i) paste(deparse(fmls[[i]]), collapse = " "), ""),
      names = names(fmls))
  }
  expect_identical(defaults(yael_preprocess_native), defaults(yael_preprocess))
  expect_identical(defaults(cmd_run_yael_preprocess_native), defaults(cmd_run_yael_preprocess))
  # the script template differs from the original only by the engine and by
  # the memory below which MNI152b is replaced with MNI152a (32 GB instead of
  # 12 GB: the native normalization to MNI152b needs up to 22 GB)
  original <- readLines(system.file("shell-templates/yael-preprocess.R", package = "ravecore"))
  native <- readLines(system.file("shell-templates/yael-preprocess-native.R", package = "ravecore"))
  expect_false(any(grepl("activate_ants", native, fixed = TRUE)))
  expect_equal(sum(grepl("ravecore::yael_preprocess_native(", native, fixed = TRUE)), 2)
  expect_equal(sum(grepl("ravecore::yael_preprocess(", original, fixed = TRUE)), 2)
  expect_equal(
    setdiff(gsub("ravecore::yael_preprocess(", "ravecore::yael_preprocess_native(", original,
                 fixed = TRUE), native),
    c("ravecore$activate_ants()",
      "if (length(max_mem_size) == 1 && is.numeric(max_mem_size) && !is.na(max_mem_size) && isTRUE(max_mem_size < 12)) {",
      "  # Cannot register to MNI152b as this requires at least 12GB RAM",
      "      \"It seems your computer memory is \", max_mem_size, \"GB. You need at least 16GB to normalize to MNI152b template. \",",
      "## END OF RAVE Script: YAEL Preprocessing + (optional) FreeSurfer recon-all ##"))
  expect_true(any(grepl("isTRUE(max_mem_size < 32)", native, fixed = TRUE)))
})

test_that("the YAEL scripts start over only with `overwrite = TRUE`", {
  skip_if_old_testthat()
  expect_false(formals(cmd_run_yael_preprocess_native)$overwrite)
  local_yael_sandbox()
  local_yael_rpyants_guard()
  root <- tempfile(pattern = "ravecore-yael-overwrite-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  t1_path <- yael_native_write_volume(array(0, c(4, 4, 4)), file.path(root, "t1.nii.gz"),
                                      vox2ras = diag(4))
  subject_id <- "YAEL/YAELOverwrite"
  dir.create(as_rave_subject(subject_id, strict = FALSE)$imaging_path, recursive = TRUE)

  find_calls <- function(exprs, fun) {
    calls <- list()
    walk <- function(e) {
      if (!is.call(e)) { return() }
      if (identical(e[[1]], fun)) { calls[[length(calls) + 1L]] <<- e }
      for (ii in seq_along(e)) {
        if (!identical(e[[ii]], quote(expr = ))) { walk(e[[ii]]) }
      }
    }
    for (e in exprs) { walk(e) }
    calls
  }

  check_script <- function(cmd, workflow, overwrite) {
    # no template: the 'rpyANTs' command would look for it, even for a dry run
    re <- cmd(subject = subject_id, t1w_path = t1_path, normalize_template = NULL,
              run_recon_all = TRUE, overwrite = overwrite, dry_run = TRUE,
              verbose = FALSE)
    exprs <- as.list(parse(text = re$script, keep.source = FALSE))

    # run what the script does first, on a folder that holds old temporary files
    imaging_path <- tempfile(tmpdir = root)
    old_file <- file.path(imaging_path, "tmp", "coregister_T1w_with_template", "old.txt")
    dir.create(dirname(old_file), recursive = TRUE)
    file.create(old_file)
    kept_file <- file.path(imaging_path, "normalization", "kept.txt")
    dir.create(dirname(kept_file), recursive = TRUE)
    file.create(kept_file)
    env <- new.env(parent = baseenv())
    env$subject <- list(imaging_path = imaging_path)
    for (e in exprs) {
      is_setting <- is.call(e) && identical(e[[1]], as.name("<-")) &&
        as.character(e[[2]]) %in% c("overwrite", "normalize_policy")
      is_start_over <- is.call(e) && identical(e[[1]], as.name("if")) &&
        identical(e[[2]], as.name("overwrite"))
      if (is_setting || is_start_over) { eval(e, envir = env) }
    }
    expect_identical(env$overwrite, overwrite)
    expect_identical(env$normalize_policy, ifelse(overwrite, "all", "auto"))
    expect_identical(file.exists(old_file), !overwrite)
    expect_identical(dir.exists(file.path(imaging_path, "tmp")), !overwrite)
    expect_true(file.exists(kept_file))

    # both calls of the workflow follow it, and so does 'FreeSurfer'
    calls <- find_calls(exprs, workflow)
    expect_length(calls, 2)
    for (call in calls) {
      expect_identical(call$normalize_policy, quote(normalize_policy))
    }
    calls <- find_calls(exprs, quote(ravecore::cmd_run_freesurfer_recon_all))
    expect_length(calls, 1)
    expect_identical(calls[[1]]$overwrite, quote(overwrite))
  }

  for (overwrite in c(FALSE, TRUE)) {
    check_script(cmd_run_yael_preprocess, quote(ravecore::yael_preprocess), overwrite)
    check_script(cmd_run_yael_preprocess_native, quote(ravecore::yael_preprocess_native),
                 overwrite)
  }
})

test_that("cmd_run_yael_preprocess_native builds the script without running it", {
  skip_if_old_testthat()
  local_yael_sandbox()
  local_yael_rpyants_guard()
  # no template is located (or downloaded) for a dry run
  testthat::local_mocked_bindings(
    yael_native_template = function(...) { stop("the template was accessed") })
  root <- tempfile(pattern = "ravecore-yael-cmd-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)
  t1_path <- yael_native_write_volume(array(0, c(4, 4, 4)), file.path(root, "t1.nii.gz"),
                                      vox2ras = diag(4))
  ct_path <- yael_native_write_volume(array(0, c(4, 4, 4)), file.path(root, "ct.nii.gz"),
                                      vox2ras = diag(4))

  re <- cmd_run_yael_preprocess_native(
    subject = "YAEL/YAELNativeCmd", t1w_path = t1_path, ct_path = ct_path,
    normalize_template = "mni_icbm152_nlin_asym_09a", normalize_images = "T2w",
    run_recon_all = TRUE, dry_run = TRUE, verbose = FALSE)
  expect_named(re, c("script", "script_path", "dry_run", "log_file", "src_path",
                     "dest_path", "execute", "command"))
  expect_true(re$dry_run)
  expect_true(is.function(re$execute))
  expect_false(file.exists(re$script_path))
  expect_equal(basename(re$script_path), "cmd-yael-preprocess-native-full.R")
  expect_match(basename(re$log_file), "^log-yael-preprocess-native-[0-9]{6}-[0-9]{6}\\.log$")
  expect_equal(re$src_path, normalizePath(t1_path, winslash = "/"))
  expect_false(grepl("{{", re$script, fixed = TRUE))
  expect_false(grepl("}}", re$script, fixed = TRUE))
  expect_false(grepl("activate_ants", re$script, fixed = TRUE))

  # the script parses; both calls of `yael_preprocess_native` (with and
  # without 'FreeSurfer') name their arguments, which are all formals
  exprs <- parse(text = re$script, keep.source = FALSE)

  # the guard of the original script switches from MNI152b to MNI152a, here
  # below 32 GB of memory (the native normalization to MNI152b needs up to
  # 22 GB)
  guard <- Filter(function(e) {
    is.call(e) && identical(e[[1]], as.name("if")) &&
      any(grepl("max_mem_size", deparse(e[[2]]), fixed = TRUE))
  }, as.list(exprs))
  expect_length(guard, 1)
  run_guard <- function(max_mem_size) {
    env <- new.env(parent = baseenv())
    env$max_mem_size <- max_mem_size
    env$normalize_template <- c("mni_icbm152_nlin_asym_09b", "fsaverage")
    eval(guard[[1]], envir = env)
    env$normalize_template
  }
  expect_message(switched <- run_guard(16), "needs at least 32GB")
  expect_equal(switched, c("mni_icbm152_nlin_asym_09a", "fsaverage"))
  expect_equal(suppressMessages(run_guard(31.9)), c("mni_icbm152_nlin_asym_09a", "fsaverage"))
  expect_silent(kept <- run_guard(32))
  expect_equal(kept, c("mni_icbm152_nlin_asym_09b", "fsaverage"))
  calls <- list()
  walk <- function(e) {
    if (!is.call(e)) { return() }
    if (identical(e[[1]], quote(ravecore::yael_preprocess_native))) {
      calls[[length(calls) + 1L]] <<- e
    }
    for (ii in seq_along(e)) {
      if (!identical(e[[ii]], quote(expr = ))) { walk(e[[ii]]) }
    }
  }
  for (e in exprs) { walk(e) }
  expect_length(calls, 2)
  formal_names <- setdiff(names(formals(yael_preprocess_native)), "...")
  for (call in calls) {
    arg_names <- names(as.list(call))[-1]
    expect_true(all(nzchar(arg_names)))
    expect_true(all(arg_names %in% formal_names),
                label = paste(setdiff(arg_names, formal_names), collapse = ", "))
  }

  # the values written into the script
  values <- new.env(parent = baseenv())
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        (is.atomic(e[[3]]) || (is.call(e[[3]]) && identical(e[[3]][[1]], as.name("c"))))) {
      assign(as.character(e[[2]]), eval(e[[3]], baseenv()), envir = values)
    }
  }
  expect_equal(values$project_name, "YAEL")
  expect_equal(values$subject_code, "YAELNativeCmd")
  expect_equal(values$t1w_path, normalizePath(t1_path, winslash = "/"))
  expect_equal(values$ct_path, normalizePath(ct_path, winslash = "/"))
  expect_equal(values$t2w_path, "")
  expect_equal(values$normalize_template, "mni_icbm152_nlin_asym_09a")
  expect_equal(values$normalize_images, c("T1w", "T2w"))
  expect_equal(values$normalize_affine, "")
  expect_equal(values$run_recon_all, 1)
  expect_true(values$use_antspynet)
  expect_false(values$register_reversed)

  # without 'FreeSurfer'; MNI152b comes first
  re <- cmd_run_yael_preprocess_native(
    subject = "YAEL/YAELNativeCmd", t1w_path = t1_path,
    normalize_template = c("mni_icbm152_nlin_asym_09a", "mni_icbm152_nlin_asym_09b"),
    run_recon_all = FALSE, register_reversed = TRUE, dry_run = TRUE, verbose = FALSE)
  expect_equal(basename(re$script_path), "cmd-yael-preprocess-native-simple.R")
  expect_true(grepl(
    "normalize_template <- c(\"mni_icbm152_nlin_asym_09b\", \"mni_icbm152_nlin_asym_09a\")",
    re$script, fixed = TRUE))
  expect_true(grepl("register_reversed <- TRUE", re$script, fixed = TRUE))

  # template names are checked before the script is built, still without
  # locating the templates; one wrong name among valid ones is enough
  expect_error(
    cmd_run_yael_preprocess_native(subject = "YAEL/YAELNativeCmd", t1w_path = t1_path,
                                   normalize_template = "not_a_template", dry_run = TRUE,
                                   verbose = FALSE),
    "should be one of")
  expect_error(
    cmd_run_yael_preprocess_native(subject = "YAEL/YAELNativeCmd", t1w_path = t1_path,
                                   normalize_template = c("mni_icbm152_nlin_asym_09a", "MNI152b"),
                                   dry_run = TRUE, verbose = FALSE),
    "should be one of")

  # a T1w image is required
  expect_error(
    cmd_run_yael_preprocess_native(subject = "YAEL/YAELNativeCmdEmpty", dry_run = TRUE,
                                   verbose = FALSE),
    "No `T1w` MRI specified")
})

test_that("the script of cmd_run_yael_preprocess_native runs the native workflow", {
  skip_on_cran()
  # 'fs' refuses paths longer than 260 bytes on Windows (r-lib/fs#420)
  skip_on_os("windows")
  skip_if_no_native_yael()
  local_yael_sandbox()
  template <- yael_toy_dataset("template")
  native <- yael_toy_dataset("native")
  info <- local_yael_toy_template(template$root)

  re <- cmd_run_yael_preprocess_native(
    subject = "YAEL/YAELNativeScript", t1w_path = native$t1,
    normalize_template = "mni_icbm152_nlin_asym_09a", normalize_images = "T1w",
    run_recon_all = FALSE, dry_run = TRUE, verbose = FALSE)
  # evaluated in this process: a child process would load the real settings;
  # the script prints the progress of the registrations
  expect_message(
    utils::capture.output(
      eval(parse(text = re$script), envir = new.env(parent = globalenv()))),
    "Done")

  process <- as_yael_process("YAEL/YAELNativeScript", engine = "native")
  work_path <- as.character(process$work_path)
  mapping <- process$get_template_mapping("mni_icbm152_nlin_asym_09a")
  expect_length(mapping$native_to_template$transformlist, 2)
  expect_length(mapping$template_to_native$transformlist, 2)
  # the script asks for surfaces when 'FreeSurfer' is not run
  expect_true(all(file.exists(file.path(work_path, "ants", c(
    "mri/T1.mgz", "mri/brainmask.nii.gz", "mri/nu.nii.gz", "mri/brain.finalsurfs.nii.gz",
    "mri/transforms/talairach.xfm", "README.txt")))))
  # the script itself is only saved when it is executed
  expect_false(file.exists(re$script_path))
})

# ---- Real data (set RAVECORE_YAEL_NATIVE_T1W to a T1w image) ---------------------------

test_that("yael_preprocess_native normalizes a real T1w image to MNI152a", {
  t1w_path <- Sys.getenv("RAVECORE_YAEL_NATIVE_T1W", unset = "")
  skip_if(!nzchar(t1w_path), "set RAVECORE_YAEL_NATIVE_T1W to a T1w image to run this test")
  skip_on_cran()
  skip_if_no_native_yael()
  skip_if_not_installed("rpyANTs")
  t1w_path <- normalizePath(t1w_path, winslash = "/", mustWork = TRUE)
  local_yael_sandbox()
  template_name <- "mni_icbm152_nlin_asym_09a"
  root <- tempfile(pattern = "ravecore-yael-real-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  # the template is read, never written; its own segmentation is the atlas
  template <- yael_native_template(template_name)
  template_header <- yael_native_read_header(template$t1)
  template_labels <- yael_native_resample(
    yael_native_read_volume(template$atropos[[1]]), template_header$dim,
    template_header$vox2ras, interpolation = "nearest")
  atlas_dir <- file.path(root, "atlas")
  yael_native_write_volume(template_labels, file.path(atlas_dir, "atropos_labels.nii.gz"))

  started <- Sys.time()
  yael_preprocess_native(subject = "YAEL/YAELNativeTest02", t1w_path = t1w_path,
                         normalize_template = template_name,
                         atlases = structure(list(atlas_dir), names = template_name),
                         verbose = FALSE)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  process <- as_yael_process("YAEL/YAELNativeTest02", engine = "native")
  work_path <- as.character(process$work_path)
  mapping <- process$get_template_mapping(template_name)
  expect_length(mapping$native_to_template$transformlist, 2)
  stage_path <- file.path(work_path, "tmp", sprintf("coregister_T1w_with_%s", template$camel))
  template_mask <- yael_native_resample(yael_native_read_mask(template$mask),
                                        template_header$dim, template_header$vox2ras,
                                        interpolation = "nearest") > 0

  # white matter (with deep gray matter and brain stem) of the native
  # segmentation, mapped to the template, against the template segmentation
  native_wm <- yael_native_read_volume(file.path(stage_path, "atropos_0.nii.gz")) == 3
  native_wm_path <- yael_native_write_volume(
    native_wm * 1, file.path(root, "native_wm.nii.gz"),
    vox2ras = attr(yael_native_read_volume(file.path(stage_path, "atropos_0.nii.gz")), "vox2ras"))
  mapped_wm <- process$transform_image_to_template(native_wm_path, template_name,
                                                   interpolator = "nearestNeighbor",
                                                   verbose = FALSE)[] > 0
  template_wm <- template_labels %in% c(3, 4, 5)
  dice <- 2 * sum(mapped_wm & template_wm & template_mask) /
    (sum(mapped_wm & template_mask) + sum(template_wm & template_mask))

  # correlation with the template brain, deformable versus affine only
  template_t1 <- yael_native_read_volume(template$t1)
  deformed <- yael_native_read_volume(file.path(
    work_path, "normalization", "anat",
    sprintf("sub-%s_ses-preop_space-%s_desc-preproc_transform-deformable_T1w.nii.gz",
            process$subject_code, template$camel)))
  correlation <- stats::cor(deformed[template_mask], template_t1[template_mask])
  native_brain <- yael_native_read_volume(file.path(stage_path, "brain.nii.gz"))
  affine_only <- ravetools::apply_transform3d_volume(
    native_brain, as.list(ants_mapping_affine_paths(mapping)),
    vox2ras = attr(native_brain, "vox2ras"), reference_dim = template_header$dim,
    reference_vox2ras = template_header$vox2ras)
  correlation_affine <- stats::cor(affine_only[template_mask], template_t1[template_mask])

  # 2000 points of the native brain mask: round trip, and the distance
  # between the deformable and the affine-only positions
  brain_mask <- yael_native_read_volume(file.path(work_path, "ants", "mri", "brainmask.nii.gz"))
  set.seed(42)
  voxels <- which(brain_mask > 0, arr.ind = TRUE) - 1
  voxels <- voxels[sample(nrow(voxels), 2000), , drop = FALSE]
  points <- t(attr(brain_mask, "vox2ras") %*% rbind(t(voxels), 1))[, 1:3]
  to_template <- process$transform_points_to_template(points, template_name, verbose = FALSE)
  back <- process$transform_points_from_template(to_template, template_name, verbose = FALSE)
  round_trip <- stats::median(sqrt(rowSums((back - points)^2)))
  affine <- Reduce(function(m, path) { ravetools::read_ants_transform(path) %*% m },
                   ants_mapping_affine_paths(mapping), diag(4))
  to_template_affine <- t(solve(affine) %*% rbind(t(points), 1))[, 1:3]
  affine_distance <- stats::median(sqrt(rowSums((to_template - to_template_affine)^2)))

  # the stored forward warp is one-to-one inside the template brain (about
  # 0.1 percent of the mask folds with these settings; 1 to 2 percent when a
  # 0.5 mm template was registered with the update-field smoothing of 1 mm)
  warp <- ravetools::read_ants_warp(file.path(
    work_path, "normalization", "transformations",
    sprintf("sub-%s_from-T1w_to-%s_desc-affine+SyN_ants1.nii.gz",
            process$subject_code, template$camel)))
  folded <- yael_native_folding(warp, template_mask)
  rm(warp)

  message(sprintf(paste(
    "Real T1w to %s: %.1f s, WM Dice %.3f, correlation %.3f (affine only %.3f),",
    "round trip %.4f mm, deformable vs affine %.2f mm, folded %.3f%% of the brain"),
    template_name, elapsed, dice, correlation, correlation_affine, round_trip,
    affine_distance, folded * 100))
  expect_lt(elapsed, 300)
  expect_gte(dice, 0.85)
  expect_gte(correlation, 0.8)
  expect_gt(correlation, correlation_affine)
  expect_lte(round_trip, 0.05)
  expect_lt(affine_distance, 20)
  expect_lt(folded, 0.005)
})

test_that("register_to_T1w recovers a known motion of a CT-like image made from a real T1w image", {
  t1w_path <- Sys.getenv("RAVECORE_YAEL_NATIVE_T1W", unset = "")
  skip_if(!nzchar(t1w_path), "set RAVECORE_YAEL_NATIVE_T1W to a T1w image to run this test")
  skip_on_cran()
  skip_if_no_native_yael()
  t1w_path <- normalizePath(t1w_path, winslash = "/", mustWork = TRUE)
  local_yael_sandbox()
  root <- tempfile(pattern = "ravecore-yael-real-ct-")
  dir.create(root)
  on.exit({ unlink(root, recursive = TRUE) }, add = TRUE)

  # a CT-like image of the same head: Hounsfield-like values from the T1w
  # intensity and the depth below the head surface, so that the relation
  # between the two images is not monotonic and, as with real CT and MRI, not
  # even a function of the T1w intensity (dark is bone near the surface and
  # cerebrospinal fluid deeper; very bright is fat near the surface and white
  # matter deeper)
  t1 <- yael_native_read_volume(t1w_path)
  t1_vox2ras <- attr(t1, "vox2ras")
  top <- stats::quantile(t1[t1 > 0], 0.99, names = FALSE)
  head <- yael_toy_blur(t1, sigma = 6) > 0.1 * top
  erode <- function(m) {
    n <- dim(m)
    e <- m
    e[-1, , ] <- e[-1, , ] & m[-n[1], , ]
    e[-n[1], , ] <- e[-n[1], , ] & m[-1, , ]
    e[, -1, ] <- e[, -1, ] & m[, -n[2], ]
    e[, -n[2], ] <- e[, -n[2], ] & m[, -1, ]
    e[, , -1] <- e[, , -1] & m[, , -n[3]]
    e[, , -n[3]] <- e[, , -n[3]] & m[, , -1]
    e
  }
  # voxels deeper than 15 voxels below the head surface
  deep <- head
  for (ii in 1:15) { deep <- erode(deep) }
  relative <- t1 / top
  set.seed(7)
  ct <- array(-1000, dim(t1))
  ct[head] <- 40
  ct[head & relative < 0.2] <- ifelse(deep[head & relative < 0.2], 10, 1000)
  ct[head & relative >= 0.55] <- 25
  ct[head & !deep & relative >= 0.85] <- -100
  ct <- ct + stats::rnorm(length(ct), sd = 10)
  attr(ct, "vox2ras") <- t1_vox2ras

  # moved by a known rigid transform `A` (T1w RAS to CT RAS) onto a CT grid of
  # 0.6 x 0.6 x 1.2 mm around the head, stored as integers
  A <- yael_toy_rotation(3, 7) %*% yael_toy_rotation(1, -5) %*% yael_toy_rotation(2, 3)
  A[1:3, 4] <- c(6, -4, 9)
  head_center <- t1_vox2ras %*% c(colMeans(which(head, arr.ind = TRUE) - 1), 1)
  ct_dim <- c(320L, 380L, 180L)
  ct_vox2ras <- diag(c(0.6, 0.6, 1.2, 1))
  ct_vox2ras[1:3, 4] <- (A %*% head_center)[1:3] - c(0.6, 0.6, 1.2) * (ct_dim - 1) / 2
  ct <- ravetools::apply_transform3d_volume(
    ct, list(solve(A)), vox2ras = t1_vox2ras, reference_dim = ct_dim,
    reference_vox2ras = ct_vox2ras, interpolation = "trilinear", na_fill = -1000)
  ct <- round(ct)
  storage.mode(ct) <- "integer"
  ct_path <- yael_native_write_volume(ct, file.path(root, "ct.nii.gz"), vox2ras = ct_vox2ras)
  # 5000 points of the head, deeper than 15 voxels, in CT RAS
  deep_voxels <- which(deep)
  deep_voxels <- deep_voxels[round(seq(1, length(deep_voxels), length.out = 5000))]
  points <- A %*% t1_vox2ras %*% rbind(t(arrayInd(deep_voxels, dim(t1)) - 1), 1)
  rm(t1, head, deep, relative, ct)

  process <- as_yael_process("YAEL/YAELNativeRealCT", engine = "native")
  process$set_input_image(t1w_path, type = "T1w")
  process$set_input_image(ct_path, type = "CT")
  transform_path <- file.path(
    process$work_path, "coregistration", "transformations",
    sprintf("sub-%s_from-CT_to-T1w_desc-rigid_ras2ras.tsv", process$subject_code))
  errors <- list()
  for (reverse in c(FALSE, TRUE)) {
    started <- Sys.time()
    process$register_to_T1w("CT", reverse = reverse, verbose = FALSE)
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    ct_to_t1 <- unname(as.matrix(utils::read.table(transform_path)))
    error <- yael_toy_rigid_error(ct_to_t1, solve(A), points)
    message(sprintf(
      "Real T1w and a CT-like image (reverse = %s): %.1f s, %.3f degrees, %.3f mm (root mean square)",
      reverse, elapsed, error[["angle"]], error[["rms"]]))
    errors[[length(errors) + 1]] <- error
  }
  for (error in errors) {
    expect_lt(error[["angle"]], 1)
    expect_lt(error[["rms"]], 1)
  }
})

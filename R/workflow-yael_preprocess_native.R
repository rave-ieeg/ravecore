# Native ('Python'-free) 'YAEL' imaging engine built on 'ravetools'.
#
# The helpers below port the logic of 'rpyANTs' (`rpyants/registration/yael.py`,
# `registration/normalization.py`, `registration/halpern.py`, `utils/paths.py`
# and `utils/cache.py`) to 'R': the same 'BIDS' file names, the same stages and
# stage cache, and the same inputs and outputs, with the 'ANTs' calls replaced
# by `ravetools::register_volume3d` (registration), `bias_correction_n4`
# (`abp_n4`), `segment_volume_tissue_gmm` ('Atropos') and
# `apply_transform3d_volume` (`antsApplyTransforms`). File names are parsed by
# `bidsr::parse_path_bids_entity` and written in the entity order of rpyANTs.
#
# Conventions: every image is a 3D double array carrying its own "vox2ras"
# attribute (0-indexed voxel to scanner 'RAS'); template images do not share
# one grid (the 'atropos' maps of `mni_icbm152_nlin_asym_09a` are 0.5 mm while
# its 'T1' is 1 mm), so a grid is never borrowed from another image. In
# 'ravetools' terms the template is the target (fixed) and the native image the
# source (moving), hence `registration$transform` maps template 'RAS' to native
# 'RAS', the same point map as the 'ANTs' affine of rpyANTs.

# ---- Requirements ------------------------------------------------------------

# Whether the installed 'ravetools' has the functions and arguments the native
# engine calls: released 'ravetools' 0.3.2 has none of them, and development
# builds before 0.3.2.3 lack some (the transform appliers, and the arguments
# `syn_grad_step`, `syn_total_sigma` and `syn_cc_radius` of
# `register_volume3d`, which are checked through the version since a wrapper
# of that function may hide its arguments)
yael_native_ravetools_ok <- function() {
  version <- ravetools_version()
  isTRUE(utils::compareVersion(version, "0.3.2.3") >= 0)
}

# Stops with an explanation unless 'ravetools' is recent enough; called before
# anything is computed or removed
yael_native_require_ravetools <- function() {
  if (!yael_native_ravetools_ok()) {
    stop(sprintf(
      "The native YAEL engine requires `ravetools` 0.3.3 or later (installed: %s). Please update `ravetools`, or use `as_yael_process(engine = \"rpyANTs\")`.",
      ravetools_version(missing = "none")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

# ---- BIDS file names ---------------------------------------------------------

#' Parse a 'BIDS'-like file name with \pkg{bidsr}
#' @param path file path; only the base name is parsed: with a folder,
#' \pkg{bidsr} takes the data type from it (e.g. \code{'anat'}), applies the
#' rules of that type (such as a required \code{sub}), and caches the class
#' @returns A \code{bidsr} \code{BIDSEntityFile} (its \code{suffix} is the
#' image type), or \code{NULL} for names \pkg{bidsr} rejects (such as
#' \code{'.DS_Store'}, or an entity without value)
#' @noRd
yael_parse_bids_filename <- function(path) {
  if (length(path) != 1 || is.na(path)) { return(NULL) }
  tryCatch(
    bidsr::parse_path_bids_entity(basename(path)),
    error = function(e) { NULL }
  )
}

# Entity values of a file parsed by `yael_parse_bids_filename`, as a named list
yael_bids_entities <- function(parsed) {
  lapply(parsed@entities, function(entity) { entity@value })
}

#' File name prefix from 'BIDS' entities (port of \code{to_bids_prefix})
#' @param entities named list of entity values
#' @param ... more entities, overriding \code{entities}
#' @returns Reserved entities first (\code{sub ses acq rec run task dir space
#' from to desc}), then the others in insertion order (those in \code{...}
#' first), joined by \code{'_'} and followed by a trailing \code{'_'}. This is
#' the order rpyANTs writes and reads; \code{format} of a \pkg{bidsr} file
#' orders the entities differently (e.g. \code{desc} before \code{from}), so
#' file names are always built here
#' @noRd
yael_bids_prefix <- function(entities = list(), ...) {
  extras <- list(...)
  reserved <- c("sub", "ses", "acq", "rec", "run", "task", "dir", "space",
                "from", "to", "desc")
  entity <- function(key, value) { sprintf("%s-%s", key, paste(value)) }
  parts <- character(0L)
  for (key in reserved) {
    if (key %in% names(extras)) {
      parts <- c(parts, entity(key, extras[[key]]))
    } else if (key %in% names(entities)) {
      parts <- c(parts, entity(key, entities[[key]]))
    }
  }
  for (key in names(extras)) {
    if (!key %in% reserved) {
      parts <- c(parts, entity(key, extras[[key]]))
      reserved <- c(reserved, key)
    }
  }
  for (ii in seq_along(entities)) {
    key <- names(entities)[[ii]]
    if (!key %in% reserved) {
      parts <- c(parts, entity(key, entities[[ii]]))
    }
  }
  sprintf("%s_", paste(parts, collapse = "_"))
}

#' Normalize a file name (port of \code{YAELPreprocess._fix_image_path})
#' @param entities named list of entity values; an existing key keeps its
#' position, new keys are appended
#' @param type image type (or the name of a log); decides the session
#' @param ext file extension without the leading dot
#' @param subject_code subject code without \code{'sub-'}
#' @param image_types allowed image types; the session of a type in this list
#' is inferred when missing
#' @param name file name (without extension) to use instead of the type
#' @returns A list with \code{type}, \code{ext} (\code{'nii'} turned into
#' \code{'nii.gz'}), \code{entities} (with \code{sub} and \code{desc-preproc}
#' set, and the session inferred: \code{preop*}, \code{postop*}, \code{'CT'}
#' is \code{'postop'}, other allowed types \code{'preop'}), \code{name} and
#' \code{fullname}
#' @noRd
yael_fix_image_path <- function(entities, type, ext, subject_code,
                                image_types = YAEL_IMAGE_TYPES, name = NULL) {
  entities <- as.list(entities)
  entities[["sub"]] <- subject_code
  entities[["desc"]] <- "preproc"
  if (identical(ext, "nii")) {
    ext <- "nii.gz"
  }
  if (is.null(entities[["ses"]])) {
    ses <- NULL
    if (startsWith(type, "preop")) {
      ses <- "preop"
    } else if (startsWith(type, "postop")) {
      ses <- "postop"
    } else if (type %in% image_types) {
      ses <- ifelse(identical(type, "CT"), "postop", "preop")
    }
    if (length(ses)) {
      entities[["ses"]] <- ses
    }
  }
  if (length(name)) {
    name <- sprintf("%s.%s", name, ext)
  } else {
    name <- sprintf("%s.%s", type, ext)
  }
  list(
    type = type,
    ext = ext,
    entities = entities,
    name = name,
    fullname = sprintf("%s%s", yael_bids_prefix(entities), name)
  )
}

#' File name of a derived image or log (port of \code{format_path} and
#' \code{expected_image_path})
#' @param subject_code subject code without \code{'sub-'}
#' @param type image type (or the name of a log, such as \code{'mapping'})
#' @param ext file extension without the leading dot
#' @param ... 'BIDS' entities, in order
#' @param name file name (without extension) to use instead of the type
#' @param image_types allowed image types, see \code{yael_fix_image_path}
#' @returns The file name (no folder)
#' @noRd
yael_format_name <- function(subject_code, type, ext, ..., name = NULL,
                             image_types = YAEL_IMAGE_TYPES) {
  yael_fix_image_path(list(...), type = type, ext = ext,
                      subject_code = subject_code, image_types = image_types,
                      name = name)$fullname
}

#' Name of an imported image under \code{'inputs/anat'} (the naming part of
#' \code{YAELPreprocess.set_image})
#' @param path path of the source image; its file name provides the entities
#' @param type image type
#' @param subject_code subject code without \code{'sub-'}
#' @param image_types allowed image types (\code{type} is added, as
#' \code{set_image} does)
#' @returns The file name as \code{yael_fix_image_path} returns it, with
#' \code{folder}; \code{ext} is always \code{'nii.gz'}, and a source name that
#' \pkg{bidsr} cannot parse gives the subject entity only
#' @noRd
yael_input_image_name <- function(path, type, subject_code,
                                  image_types = YAEL_IMAGE_TYPES) {
  image_types <- unique(c(image_types, type))
  source <- yael_parse_bids_filename(path)
  if (is.null(source)) {
    entities <- list(sub = subject_code)
  } else {
    entities <- yael_bids_entities(source)
  }
  parsed <- yael_fix_image_path(entities, type = type, ext = "nii.gz",
                                subject_code = subject_code,
                                image_types = image_types)
  c(list(folder = "inputs/anat"), parsed)
}

#' File names written by the co-registration and normalization
#' @param subject_code subject code without \code{'sub-'}
#' @param type image type: the modality registered to \code{'T1w'} (names
#' starting with \code{coreg_}) or the native image normalized to the template
#' (names starting with \code{norm_})
#' @param template_name camel-case template name (such as
#' \code{'MNI152NLin2009bAsym'}); \code{NULL} omits the normalization names
#' @param image_types allowed image types
#' @param ext extension of the input image of \code{type}
#' @returns A named list of file names (no folders): transform prefixes and
#' files under \code{'coregistration/transformations'} and
#' \code{'normalization/transformations'}, the aligned image under
#' \code{'coregistration/anat'}, the deformed images under
#' \code{'normalization/anat'}, and the logs
#' @noRd
yael_native_file_names <- function(subject_code, type, template_name = NULL,
                                   image_types = YAEL_IMAGE_TYPES,
                                   ext = "nii.gz") {
  to_t1 <- yael_bids_prefix(list(sub = subject_code, from = type, to = "T1w",
                                 desc = "rigid"))
  from_t1 <- yael_bids_prefix(list(sub = subject_code, from = "T1w", to = type,
                                   desc = "rigid"))
  re <- list(
    coreg_prefix_to_t1 = to_t1,
    coreg_prefix_from_t1 = from_t1,
    coreg_to_t1_ants = sprintf("%sants.mat", to_t1),
    coreg_to_t1_ras2ras = sprintf("%sras2ras.tsv", to_t1),
    coreg_to_t1_vox2ras = sprintf("%svox2ras.tsv", to_t1),
    coreg_from_t1_ants = sprintf("%sants.mat", from_t1),
    coreg_from_t1_ras2ras = sprintf("%sras2ras.tsv", from_t1),
    coreg_anat = yael_format_name(subject_code, type = type, ext = ext,
                                  space = "scanner", image_types = image_types),
    coreg_log = yael_format_name(subject_code, type = "mapping", ext = "json",
                                 space = "scanner", orig = type,
                                 image_types = image_types)
  )
  if (length(template_name)) {
    to_template <- yael_bids_prefix(list(
      sub = subject_code, from = type, to = template_name, desc = "affine+SyN"))
    from_template <- yael_bids_prefix(list(
      sub = subject_code, from = template_name, to = type, desc = "affine+SyN"))
    re <- c(re, list(
      norm_prefix_to_template = to_template,
      norm_prefix_from_template = from_template,
      # native_to_template list: [ants1 (forward warp), ants0 (affine)]
      norm_to_template_affine = sprintf("%sants0.mat", to_template),
      norm_to_template_warp = sprintf("%sants1.nii.gz", to_template),
      # template_to_native list: [ants1 (inverse affine), ants0 (inverse warp)]
      norm_from_template_warp = sprintf("%sants0.nii.gz", from_template),
      norm_from_template_affine = sprintf("%sants1.mat", from_template),
      norm_anat_in_template = yael_format_name(
        subject_code, type = type, ext = ext, name = type,
        space = template_name, transform = "deformable",
        image_types = image_types),
      norm_anat_in_native = yael_format_name(
        subject_code, type = type, ext = ext, name = template_name,
        space = type, transform = "deformable", image_types = image_types),
      norm_log = yael_format_name(
        subject_code, type = "mappings", ext = "json",
        template = template_name, native = type, image_types = image_types)
    ))
  }
  re
}

# Weight of an additional image in the deformable normalization stage, relative
# to the 'T1w' image (`YAELPreprocess.normalization_weights`)
yael_normalization_weight <- function(type) {
  weights <- c(CT = 0.6, preopCT = 0.8, postopCT = 0.6, FLAIR = 0.25,
               postopFLAIR = 0.2, preopFLAIR = 0.3)
  unname(vapply(type, function(x) {
    if (x %in% names(weights)) { weights[[x]] } else { 0.9 }
  }, 0.0))
}

# ---- Image input and output ----------------------------------------------------

#' Read an image as a double array with its own \code{"vox2ras"} attribute
#' @param path \code{'NIfTI'} or \code{'MGH'}/\code{'MGZ'} file
#' @param allow_4d whether to accept images with more than three non-singleton
#' dimensions; default is false (error)
#' @returns A double array; trailing singleton dimensions are dropped (an
#' \code{'MGZ'} volume is read as 4D), the \code{"vox2ras"} attribute is the
#' 0-indexed voxel to scanner 'RAS' matrix
#' @noRd
yael_native_read_volume <- function(path, allow_4d = FALSE) {
  volume <- ieegio::read_volume(path)
  vox2ras <- matrix(as.numeric(volume$transforms$vox2ras)[seq_len(16)], nrow = 4L)
  data <- volume[]
  rm(volume)
  dm <- dim(data)
  if (length(dm) < 3) {
    dm <- c(dm, rep(1L, 3 - length(dm)))
  }
  while (length(dm) > 3 && dm[[length(dm)]] == 1) {
    dm <- dm[-length(dm)]
  }
  if (length(dm) > 3 && !isTRUE(allow_4d)) {
    stop(sprintf("Image `%s` is not a 3D volume (dimension: %s).",
                 basename(path), paste(dm, collapse = "x")))
  }
  if (!is.double(data)) {
    storage.mode(data) <- "double"
  }
  attributes(data) <- list(dim = dm, vox2ras = vox2ras)
  data
}

#' Write an array as a new \code{'NIfTI-1'} (or \code{'MGZ'}) image
#' @param x array (3D, or more for storage)
#' @param path file path; \code{'.mgz'}/\code{'.mgh'} are written as 'MGH',
#' anything else as \code{'NIfTI-1'}
#' @param vox2ras voxel to 'RAS' matrix; default is the attribute of \code{x}
#' @returns \code{path}, invisibly. The header is always built from
#' \code{vox2ras} (editing the transforms of an image that was read is ignored
#' when writing, hence a new volume)
#' @noRd
yael_native_write_volume <- function(x, path, vox2ras = attr(x, "vox2ras")) {
  if (is.null(vox2ras)) {
    stop("`yael_native_write_volume`: `vox2ras` is missing for ", basename(path))
  }
  vox2ras <- matrix(as.numeric(vox2ras)[seq_len(16)], nrow = 4L)
  dm <- dim(x)
  if (is.logical(x)) {
    storage.mode(x) <- "integer"
  }
  attributes(x) <- list(dim = dm)
  dir_create2(dirname(path))
  volume <- ieegio::as_ieegio_volume(x, vox2ras = vox2ras)
  rm(x)
  if (grepl("\\.(mgz|mgh)$", path, ignore.case = TRUE)) {
    ieegio::write_volume(volume, con = path, format = "mgh")
  } else {
    ieegio::write_volume(volume, con = path, format = "nifti", version = 1)
  }
  invisible(path)
}

#' Write a matrix as tab-separated text, the way \code{numpy.savetxt} writes
#' with \code{fmt = '\%f'}
#' @noRd
yael_native_write_tsv <- function(x, path) {
  x <- as.matrix(x)
  lines <- vapply(seq_len(nrow(x)), function(ii) {
    paste(sprintf("%f", x[ii, ]), collapse = "\t")
  }, "")
  dir_create2(dirname(path))
  writeLines(lines, con = path, sep = "\n")
  invisible(path)
}

# The "vox2ras" attribute of an image, which every image must carry
yael_native_vox2ras <- function(x, what = "image") {
  vox2ras <- attr(x, "vox2ras")
  if (is.null(vox2ras)) {
    stop(sprintf("The %s has no `vox2ras` attribute.", what))
  }
  vox2ras
}

# Whether two grids are the same
yael_native_same_grid <- function(dim1, vox2ras1, dim2, vox2ras2) {
  isTRUE(all(dim1[1:3] == dim2[1:3])) &&
    isTRUE(max(abs(vox2ras1 - vox2ras2)) < 1e-6)
}

# Resample an image (with its own grid) onto another grid of the same space
# (identity transform); an image already on that grid is returned unchanged
yael_native_resample <- function(x, reference_dim, reference_vox2ras,
                                 interpolation = c("trilinear", "nearest"),
                                 na_fill = 0) {
  interpolation <- match.arg(interpolation)
  vox2ras <- yael_native_vox2ras(x)
  if (yael_native_same_grid(dim(x), vox2ras, reference_dim, reference_vox2ras)) {
    return(x)
  }
  ravetools::apply_transform3d_volume(
    x, list(diag(4)), vox2ras = vox2ras,
    reference_dim = reference_dim, reference_vox2ras = reference_vox2ras,
    interpolation = interpolation, na_fill = na_fill
  )
}

# Resample a template-space image onto the native grid through a registration
# (template = target, native = source). The image always passes its own grid:
# without it the registration geometry would be used silently
yael_native_to_native <- function(x, registration, reference_dim,
                                  reference_vox2ras,
                                  interpolation = c("trilinear", "nearest")) {
  interpolation <- match.arg(interpolation)
  ravetools::apply_transform3d_volume(
    x, registration, vox2ras = yael_native_vox2ras(x, "template image"),
    reference_dim = reference_dim, reference_vox2ras = reference_vox2ras,
    direction = "inverse", interpolation = interpolation
  )
}

# ---- Templates ------------------------------------------------------------------

# Files of a template folder that the normalization reads: `T1.nii.gz`,
# `T1_brainmask.nii.gz` and `atropos_{0..7}.nii.gz` (segmentation, background,
# CSF, gray matter, white matter, deep gray matter, brain stem, cerebellum).
# `atropos_0` and `atropos_1` are not used and may be missing; `T1.nii.gz` is
# only required with `require_t1` (the normalization takes the template image
# as an argument)
yael_native_template_files <- function(root, require_t1 = TRUE) {
  root <- as.character(root)
  t1 <- file.path(root, "T1.nii.gz")
  mask <- file.path(root, "T1_brainmask.nii.gz")
  atropos <- file.path(root, sprintf("atropos_%d.nii.gz", 0:7))
  missing_files <- c(if (require_t1) t1, mask, atropos[3:8])
  missing_files <- missing_files[!file.exists(missing_files)]
  if (length(missing_files)) {
    stop(sprintf(
      "Template folder `%s` misses %s, required by the normalization. Please re-install the template.",
      root, paste(sQuote(basename(missing_files), q = FALSE), collapse = ", ")
    ), call. = FALSE)
  }
  list(t1 = t1, mask = mask, atropos = atropos)
}

#' Template files used by the native 'YAEL' pipeline; the only place where a
#' template is located by its name
#' @param template_name one of the 'rpyANTs' built-in templates
#' @returns A list with \code{name}, \code{camel} (the 'BIDS' space name),
#' \code{coord_sys}, \code{root} (template folder), \code{t1}, \code{mask},
#' \code{atropos} (paths of \code{atropos_0} to \code{atropos_7}),
#' \code{fs_dir} and \code{atlas_dir} (not necessarily existing). Missing
#' \code{T1_brainmask.nii.gz} or \code{atropos_2} to \code{atropos_7} are
#' errors. Masks must be read as \code{x > 0} (the \code{'fsaverage'} mask
#' holds intensities)
#' @noRd
yael_native_template <- function(template_name = rpyants_builtin_templates()) {
  template_name <- match.arg(template_name)
  template_info <- call_rpyants("template_urls")[[template_name]]
  root <- call_rpyants("ensure_template", name = template_name)
  root <- as.character(path_abs(as.character(root), must_work = TRUE))
  files <- yael_native_template_files(root)
  list(
    name = template_name,
    camel = template_info$name,
    coord_sys = template_info$coord_sys,
    root = root,
    t1 = files$t1,
    mask = files$mask,
    atropos = files$atropos,
    fs_dir = file.path(root, "fs"),
    atlas_dir = file.path(root, "atlases")
  )
}

# Warns (in the log) when the template is large for the memory recorded in
# the RAVE settings (`max_mem`, the computer memory unless set otherwise).
# With a 1 mm 'T1' image, the normalization to a 0.5 mm template (69 million
# voxels) peaked at about 15 GB of resident memory, 22 GB with the memory
# compressed by the operating system, and to a 1 mm template at about 8 GB
yael_native_check_memory <- function(template_t1, max_mem = NULL) {
  if (is.null(max_mem)) {
    max_mem <- ravepipeline::raveio_getopt("max_mem", default = NA)
  }
  max_mem <- suppressWarnings(as.numeric(max_mem))
  if (length(max_mem) != 1 || !isTRUE(max_mem < 32)) { return(invisible(TRUE)) }
  header <- tryCatch(yael_native_read_header(template_t1), error = function(e) { NULL })
  if (is.null(header) || prod(as.numeric(header$dim)) <= 3e7) {
    return(invisible(TRUE))
  }
  ravepipeline::logger(
    level = "warning", use_glue = FALSE,
    sprintf(paste(
      "The template image `%s` has %.0f million voxels: the native normalization",
      "to such a template needs 16 to 22 GB of memory, while the RAVE settings",
      "record %g GB (`max_mem`). Please consider the 1 mm template",
      "`mni_icbm152_nlin_asym_09a`, or set the memory via",
      "`ravepipeline::raveio_setopt('max_mem', <your actual RAM in GB>)` if the",
      "recorded value is wrong."),
      basename(dirname(template_t1)), prod(as.numeric(header$dim)) / 1e6, max_mem)
  )
  invisible(FALSE)
}

# Template brain mask, binarized
yael_native_read_mask <- function(path) {
  mask <- yael_native_read_volume(path)
  vox2ras <- attr(mask, "vox2ras")
  mask <- (mask > 0) * 1
  attr(mask, "vox2ras") <- vox2ras
  mask
}

# ---- Stage cache ------------------------------------------------------------------

yael_native_log <- function(verbose, ...) {
  if (isTRUE(verbose)) {
    ravepipeline::logger(..., level = "info", use_glue = FALSE)
  }
  invisible()
}

# File names of a cached stage under `root`
yael_native_stage_files <- function(name, kind, root, n = 1L) {
  switch(
    kind,
    "image" = list(image = file.path(root, sprintf("%s.nii.gz", name))),
    "images" = list(images = file.path(
      root, sprintf("%s_%d.nii.gz", name, seq_len(n) - 1L))),
    "registration" = list(
      manifest = file.path(root, sprintf("%s.dcf", name)),
      warped_mov = file.path(root, sprintf("%s_warped_mov.nii.gz", name)),
      warped_fix = file.path(root, sprintf("%s_warped_fix.nii.gz", name))
    )
  )
}

yael_native_stage_read <- function(name, kind, root, n = 1L) {
  files <- yael_native_stage_files(name, kind, root, n)
  switch(
    kind,
    "image" = {
      if (!file.exists(files$image)) { stop("missing ", basename(files$image)) }
      yael_native_read_volume(files$image)
    },
    "images" = {
      missing_files <- files$images[!file.exists(files$images)]
      if (length(missing_files)) { stop("missing ", basename(missing_files[[1]])) }
      lapply(files$images, yael_native_read_volume)
    },
    "registration" = {
      if (!file.exists(files$manifest)) { stop("missing ", basename(files$manifest)) }
      if (!file.exists(files$warped_mov)) { stop("missing ", basename(files$warped_mov)) }
      registration <- ravetools::load_registration(files$manifest)
      if (is.null(registration$transform)) { stop("no affine transform recorded") }
      list(
        registration = registration,
        warpedmovout = yael_native_read_volume(files$warped_mov),
        # absent from caches that only kept the moving image
        warpedfixout = if (file.exists(files$warped_fix)) {
          yael_native_read_volume(files$warped_fix)
        }
      )
    }
  )
}

yael_native_stage_write <- function(result, name, kind, root, n = 1L) {
  files <- yael_native_stage_files(name, kind, root, n)
  dir_create2(root)
  switch(
    kind,
    "image" = {
      yael_native_write_volume(result, files$image)
    },
    "images" = {
      for (ii in seq_len(n)) {
        yael_native_write_volume(result[[ii]], files$images[[ii]])
      }
    },
    "registration" = {
      ravetools::save_registration(result$registration, path = files$manifest)
      yael_native_write_volume(result$warpedmovout, files$warped_mov)
      if (is.null(result$warpedfixout)) {
        # do not keep the image of a previous registration
        if (file.exists(files$warped_fix)) { unlink(files$warped_fix) }
      } else {
        yael_native_write_volume(result$warpedfixout, files$warped_fix)
      }
    }
  )
  invisible(files)
}

#' Run a stage of the normalization unless its result is cached (port of
#' \code{StageContext})
#' @param name stage name, also the file prefix
#' @param kind \code{'image'} (\code{<name>.nii.gz}), \code{'images'}
#' (\code{<name>_0.nii.gz} to \code{<name>_<n-1>.nii.gz}), or
#' \code{'registration'} (\code{<name>.dcf} written by
#' \code{ravetools::save_registration} with its \code{'ANTs'} transform files,
#' plus \code{<name>_warped_mov.nii.gz} and optionally
#' \code{<name>_warped_fix.nii.gz})
#' @param root stage folder
#' @param compute function without arguments computing the result: an image, a
#' list of \code{n} images, or a list of \code{registration},
#' \code{warpedmovout} and \code{warpedfixout} (may be \code{NULL})
#' @param n number of images for \code{kind = 'images'}
#' @param description what the stage computes, logged when it runs
#' @param verbose whether to log one line with the stage status
#' @returns The cached or computed result; a cache that cannot be read is
#' computed again and replaced
#' @noRd
yael_native_stage <- function(name, kind = c("image", "images", "registration"),
                              root, compute, n = 1L, description = NULL,
                              verbose = TRUE) {
  kind <- match.arg(kind)
  n <- as.integer(n)
  cached <- tryCatch({
    yael_native_stage_read(name = name, kind = kind, root = root, n = n)
  }, error = function(e) { NULL })
  if (!is.null(cached)) {
    yael_native_log(verbose, sprintf(
      "Stage `%s`: cache found, using the existing results", name))
    return(cached)
  }
  if (!length(description)) {
    description <- "computing"
  }
  yael_native_log(verbose, sprintf("Stage `%s`: %s", name, description))
  result <- compute()
  if (!is.null(result)) {
    yael_native_stage_write(result, name = name, kind = kind, root = root, n = n)
  }
  result
}

# ---- Rigid co-registration -------------------------------------------------------

#' Rigid co-registration of two images (port of
#' \code{halpern_coregister_ct_mri})
#' @param fixed,moving image paths, or arrays with a \code{"vox2ras"}
#' attribute
#' @param verbose whether to print the registration progress
#' @returns The 'ravetools' registration of the second of two rigid passes
#' (Mattes mutual information, 32 bins, shrink factors 8, 4, 2, 1, smoothing
#' 3, 2, 1, 0 voxels, at most 1000, 500, 250, 100 iterations, 25 percent of
#' the voxels sampled); the first pass starts from the centers of mass and the
#' second from the first. \code{$transform} maps fixed 'RAS' to moving 'RAS'
#' (the 'ANTs' affine, as a point map) and \code{$image} is the moving image on
#' the fixed grid
#' @noRd
yael_native_coregister_rigid <- function(fixed, moving, verbose = TRUE) {
  if (is.character(fixed)) { fixed <- yael_native_read_volume(fixed) }
  if (is.character(moving)) { moving <- yael_native_read_volume(moving) }
  rigid <- function(init_transform) {
    ravetools::register_volume3d(
      source = moving, target = fixed,
      source_vox2ras = yael_native_vox2ras(moving, "moving image"),
      target_vox2ras = yael_native_vox2ras(fixed, "fixed image"),
      type = "rigid", metric = "mattes", number_of_bins = 32L,
      shrink_factors = c(8, 4, 2, 1), smoothing_sigmas = c(3, 2, 1, 0),
      iterations = c(1000, 500, 250, 100), sampling_rate = 0.25,
      init_transform = init_transform, verbose = isTRUE(verbose)
    )
  }
  pass1 <- rigid(NULL)
  rigid(pass1$transform)
}

# ---- Normalization ------------------------------------------------------------------

#' Smoothing factor of the deformable update fields for a template
#' @param brain_voxels number of voxels of the template brain mask on the
#' template 'T1' grid (the grid of the deformation fields)
#' @returns A number, at least 1, that multiplies \code{syn_sigma} (given in
#' voxels of the template grid): the cube root of the ratio between the brain
#' voxels of the template and two million (a human brain at 1 mm), rounded to
#' a half. The \pkg{rpyANTs} values (3 and 3.5) were validated on 1 mm
#' templates (factor 1); the 0.5 mm \code{09b} templates hold eight times as
#' many brain voxels (factor 2) and the 0.25 mm macaque template about three
#' times as many (factor 1.5), so their update fields are smoothed the same
#' way relative to the brain. With factor 1 on the levels of a 0.5 mm
#' template, the greedy update of \pkg{ravetools} folded the deformation in
#' about 2 percent of the brain (one subject; 1.5 percent on \code{09b}),
#' against 0.01 percent with factor 2 and 0.2 percent on 1 mm levels
#' @noRd
yael_native_syn_scale <- function(brain_voxels) {
  scale <- round(2 * (as.numeric(brain_voxels) / 2e6)^(1 / 3)) / 2
  if (length(scale) != 1 || !isTRUE(scale >= 1)) { scale <- 1 }
  scale
}

#' Fraction of a brain mask where a deformation folds
#' @param field displacement field \code{(nx, ny, nz, 3)} in 'RAS'
#' millimeters, such as the \code{forward_field} of a registration
#' @param mask array on the grid of \code{field}; its non-zero voxels are
#' counted
#' @param vox2ras voxel to 'RAS' matrix of the grid; default is the
#' \code{"vox2ras"} attribute of \code{field}
#' @param slab number of slices processed at a time, which bounds the memory
#' @returns The fraction (0 to 1) of the mask voxels where the Jacobian
#' determinant of the map \code{x + u(x)} is not positive (zero, negative, or
#' not finite), that is, where the map is not one-to-one (central
#' differences; the outermost voxel layer of the grid is left out); \code{NA}
#' when no mask voxel is left
#' @noRd
yael_native_folding <- function(field, mask, vox2ras = attr(field, "vox2ras"),
                                slab = 16L) {
  dm <- dim(field)
  if (length(dm) != 4 || dm[[4]] != 3) {
    stop("yael_native_folding: `field` must be a (nx, ny, nz, 3) array")
  }
  if (!identical(as.integer(dim(mask))[1:3], as.integer(dm[1:3]))) {
    stop("yael_native_folding: `mask` must be on the grid of `field`")
  }
  if (is.null(vox2ras)) {
    stop("yael_native_folding: `vox2ras` is missing")
  }
  if (any(dm[1:3] < 3)) { return(NA_real_) }
  # change of the voxel index per millimeter along each RAS axis
  index_per_mm <- solve(matrix(as.numeric(vox2ras)[seq_len(16)], nrow = 4L)[1:3, 1:3])
  xs <- seq(2, dm[[1]] - 1)
  ys <- seq(2, dm[[2]] - 1)
  n_voxels <- 0
  n_folded <- 0
  for (z0 in seq(2, dm[[3]] - 1, by = slab)) {
    zs <- seq(z0, min(z0 + slab - 1, dm[[3]] - 1))
    inside <- mask[xs, ys, zs] > 0
    if (!any(inside)) { next }
    # derivative of displacement component `k` along voxel axis `a`
    du <- function(k, a) {
      difference <- switch(
        a,
        field[xs + 1, ys, zs, k] - field[xs - 1, ys, zs, k],
        field[xs, ys + 1, zs, k] - field[xs, ys - 1, zs, k],
        field[xs, ys, zs + 1, k] - field[xs, ys, zs - 1, k]
      )
      difference[inside] / 2
    }
    # Jacobian of x + u(x): identity plus d(u)/d(index) %*% d(index)/d(RAS)
    jacobian <- lapply(1:3, function(k) {
      gradient <- lapply(1:3, function(a) { du(k, a) })
      lapply(1:3, function(r) {
        value <- gradient[[1]] * index_per_mm[1, r] +
          gradient[[2]] * index_per_mm[2, r] + gradient[[3]] * index_per_mm[3, r]
        if (k == r) { value + 1 } else { value }
      })
    })
    J <- function(k, r) { jacobian[[k]][[r]] }
    determinant <- J(1, 1) * (J(2, 2) * J(3, 3) - J(2, 3) * J(3, 2)) -
      J(1, 2) * (J(2, 1) * J(3, 3) - J(2, 3) * J(3, 1)) +
      J(1, 3) * (J(2, 1) * J(3, 2) - J(2, 2) * J(3, 1))
    n_voxels <- n_voxels + length(determinant)
    n_folded <- n_folded + sum(!(determinant > 0))
    rm(jacobian, determinant)
  }
  if (n_voxels == 0) { return(NA_real_) }
  n_folded / n_voxels
}

#' White matter of a quick 3-class segmentation (the \verb{k-means}
#' \verb{Atropos} call of stage \code{nu2} in rpyANTs)
#' @param t1 raw native image
#' @param mask brain mask on the grid of \code{t1}
#' @param vox2ras voxel to 'RAS' matrix of \code{t1}
#' @param verbose whether to log the fallback below
#' @returns A list with \code{mean} (the posterior-weighted mean of the raw
#' intensities of the brightest class, taken as the white matter) and
#' \code{prob} (that posterior). The raw intensities are segmented, as in
#' rpyANTs. A few extreme intensities can leave a \verb{k-means} class
#' without voxels, which \pkg{ravetools} reports as an error while
#' \verb{Atropos} keeps the empty class and goes on; the segmentation is
#' then run again on a copy clipped at the 99.9 percent (then the 99 percent)
#' quantile of the intensities inside the mask. The means are always computed
#' from the raw intensities
#' @noRd
yael_native_kmeans_wm <- function(t1, mask, vox2ras, verbose = TRUE) {
  segment <- function(x) {
    ravetools::segment_volume_tissue_gmm(
      x, mask = mask, n_classes = 3L, mrf_beta = 0.1, mrf_radius = 1L,
      iterations = 5L, vox2ras = vox2ras
    )
  }
  seg3 <- tryCatch(segment(t1), error = function(e) { e })
  if (inherits(seg3, "error")) {
    inside <- t1[mask > 0 & is.finite(t1)]
    for (probability in c(0.999, 0.99)) {
      if (!length(inside)) { break }
      upper <- stats::quantile(inside, probability, names = FALSE)
      # nothing left to clip: the failure has another cause
      if (max(inside) <= upper) { break }
      yael_native_log(verbose, sprintf(
        "Stage `nu2`: the 3-class k-means of the raw image failed (%s); segmenting again with the intensities clipped at %.6g (the %g%% quantile inside the brain mask)",
        conditionMessage(seg3), upper, probability * 100))
      seg3 <- tryCatch(segment(pmin(t1, upper)), error = function(e) { e })
      if (!inherits(seg3, "error")) { break }
    }
    if (inherits(seg3, "error")) { stop(seg3) }
  }
  # the brightest class is the white matter
  means <- vapply(seg3$posteriors, function(p) {
    sum(t1 * p) / (sum(p) + 1e-8)
  }, 0.0)
  wm_idx <- which.max(means)
  list(mean = means[[wm_idx]], prob = seg3$posteriors[[wm_idx]])
}

#' Normalize native images to a template with tissue priors (port of
#' \code{normalization_with_atropos}, branch without \code{'antspynet'})
#' @param fix_path path to the template \code{T1.nii.gz}; its folder must hold
#' \code{T1_brainmask.nii.gz} and \code{atropos_2.nii.gz} to
#' \code{atropos_7.nii.gz}. A vector of the same length as \code{mov_paths}
#' gives the template image of each native image
#' @param mov_paths native images aligned in the same scanner space: the
#' \code{'T1'} first, then additional images (realigned to the \code{'T1'},
#' on their own grids)
#' @param working_path stage folder; cached stages are reused
#' @param weights weight of each native image; the first one (the
#' \code{'T1'}) is ignored as in 'rpyANTs', the main channel weighs 1
#' @param with_skull whether to register the images with skull instead of the
#' skull-stripped brain
#' @param cost_function \code{'CC'}; the deformable stage of 'ravetools' has no
#' mutual information, other values give a warning
#' @param affine_transform optional initial affine (\code{'ANTs'} \code{.mat}
#' file, or a 4-by-4 template-to-native 'RAS' matrix); the initial registration
#' then only runs its deformable stage, with the \code{'SyNOnly'} settings
#' @param verbose whether to log the stages and print the registration progress
#' @details Stages (cached in \code{working_path}): \code{T1} (raw image),
#' \code{nu} (\code{N4}), \code{SyNabp} (affine and deformable registration
#' without masks), \code{nu2} (k-means white matter normalized to 110, then
#' \code{N4} weighted by the white matter; see \code{yael_native_kmeans_wm}
#' for the k-means of images with extreme intensities), \code{brainmask} (template mask on
#' the native grid; also writes \code{brain.nii.gz}), \code{atropos_prior}
#' (template \code{atropos_2} to \code{atropos_7} on the native grid),
#' \code{atropos} (segmentation and four tissue posteriors: \verb{CSF}, gray
#' matter, white matter with deep gray matter and brain stem, cerebellum),
#' \code{wm_artifacts} (white matter outside the gray-matter envelope, such as
#' a thick skull marrow, dilated by 2 voxels, except the cone below the center
#' of the white matter that holds the brain stem; see
#' \code{yael_native_clean_wm_mask}), \code{brain_mask_cleaned} (the brain
#' mask without these artifacts, also used for the brain image of the next
#' stage), and
#' \code{SyN_w_atropos} (masked affine, then a deformable stage with the brain,
#' the additional images, and the \verb{CSF} and deep-structure posteriors, the
#' latter zero on the artifacts; also writes \code{brain2.nii.gz}
#' and \code{brainmask2.nii.gz}). The classes keep the order of their priors;
#' when the median intensities of the \verb{CSF}, gray-matter and white-matter
#' labels do not increase, a warning is given and no artifact is left out (see
#' \code{yael_native_segmentation_class_order}). A tissue prior
#' that is zero everywhere inside the brain mask is left out of the
#' segmentation with a warning (its posterior is zero). An additional image
#' that is not 3D, or covers less than 95 percent of the brain mask, is left
#' out with a warning. The smoothing of the deformable update fields
#' (\code{syn_sigma}) is multiplied by \code{yael_native_syn_scale} of the
#' template brain voxels (2 for the 0.5 mm templates). The final deformation is
#' checked with \code{yael_native_folding}: the share of the template brain
#' mask where it folds is logged, and above 1 percent a warning is given.
#' @returns A list with \code{registration} (the final 'ravetools'
#' registration: \code{transform} maps template 'RAS' to native 'RAS',
#' \code{forward_field} and \code{inverse_field} are on the template grid),
#' \code{warpedmovout} (the registered native image on the template grid) and
#' \code{warpedfixout} (the registered template image on the native grid). The
#' final registration is also saved as \code{SyN_w_atropos.dcf} in
#' \code{working_path}, with \code{SyN_w_atropos0GenericAffine.mat},
#' \code{SyN_w_atropos1Warp.nii.gz} and \code{SyN_w_atropos1InverseWarp.nii.gz}
#' @noRd
yael_native_normalize <- function(fix_path, mov_paths, working_path,
                                  weights = 1, with_skull = FALSE,
                                  cost_function = "CC", affine_transform = NULL,
                                  verbose = TRUE) {
  
  # DIPSAUS DEBUG START
  # list2env(
  #   envir = globalenv(),
  #   list(fix_path = "/Users/dipterix/Library/Application Support/org.R-project.R/R/rpyANTs/templates/mni_icbm152_nlin_sym_09a/T1.nii.gz", 
  #     mov_paths = "/Users/dipterix/rave_data/raw_dir/K20/rave-imaging/inputs/anat/sub-K20_ses-preop_desc-preproc_T1w.nii.gz", 
  #     working_path = "/Users/dipterix/rave_data/raw_dir/K20/rave-imaging/tmp/coregister_T1w_with_MNI152NLin2009aSym", 
  #     weights = 1.25, with_skull = FALSE, cost_function = "CC", 
  #     affine_transform = NULL, verbose = TRUE)
  # )

  verbose <- isTRUE(verbose)
  with_skull <- isTRUE(with_skull)
  # large arrays are removed from this environment once no stage needs them
  normalize_env <- environment()

  # ---- inputs ----
  mov_paths <- as.character(unlist(mov_paths))
  n_images <- length(mov_paths)
  if (n_images == 0) {
    stop("yael_native_normalize: `mov_paths` must have at least one image")
  }
  mov_paths <- normalizePath(mov_paths, winslash = "/", mustWork = TRUE)
  fix_paths <- as.character(unlist(fix_path))
  if (length(fix_paths) == 1) {
    fix_paths <- rep(fix_paths, n_images)
  } else if (length(fix_paths) != n_images) {
    stop("yael_native_normalize: `fix_path` and `mov_paths` must have the same length or `fix_path` must be a single path")
  }
  fix_paths <- normalizePath(fix_paths, winslash = "/", mustWork = TRUE)
  weights <- as.double(unlist(weights))
  if (length(weights) == 1) {
    weights <- rep(weights, n_images)
  } else if (length(weights) != n_images) {
    stop("yael_native_normalize: weights must be a single number or a list of the same length as `mov_paths`")
  }
  if (!identical(toupper(paste(cost_function, collapse = "")), "CC")) {
    warning(sprintf(
      "yael_native_normalize: cost function `%s` is not available for the deformable stage; using local cross-correlation (CC) instead.",
      paste(cost_function, collapse = "")), call. = FALSE)
  }
  if (length(affine_transform)) {
    if (is.character(affine_transform)) {
      affine_transform <- ravetools::read_ants_transform(
        normalizePath(affine_transform, winslash = "/", mustWork = TRUE))
    }
    affine_transform <- matrix(as.numeric(affine_transform)[seq_len(16)], nrow = 4L)
  } else {
    affine_transform <- NULL
  }
  # the first template image (the T1) locates the template files
  template_files <- yael_native_template_files(dirname(fix_paths[[1]]),
                                               require_t1 = FALSE)
  yael_native_require_ravetools()
  working_path <- normalizePath(working_path, winslash = "/", mustWork = FALSE)
  dir_create2(working_path)

  stage <- function(name, kind, description, compute, n = 1L) {
    yael_native_stage(name = name, kind = kind, root = working_path,
                      compute = compute, n = n, description = description,
                      verbose = verbose)
  }
  free_memory <- function() { invisible(gc(verbose = FALSE)) }

  # ---- T1: the raw native image ----
  t1 <- stage("T1", "image", "reading the native image", function() {
    yael_native_read_volume(mov_paths[[1]])
  })
  native_dim <- dim(t1)
  native_vox2ras <- yael_native_vox2ras(t1, "native image")

  # ---- nu: N4 bias field correction (`abp_n4`) ----
  nu <- stage("nu", "image", "N4 bias field correction", function() {
    ravetools::bias_correction_n4(t1, vox2ras = native_vox2ras)
  })

  # template image and its brain mask (binarized), on the template T1 grid
  template <- stage("fixing", "image", "Template fixing image", function() {
    yael_native_read_volume(fix_paths[[1]])
  })
  template_dim <- dim(template)
  template_vox2ras <- attr(template, "vox2ras")
  template_mask <- yael_native_resample(
    yael_native_read_mask(template_files$mask),
    template_dim, template_vox2ras, interpolation = "nearest")
  # the update fields of the deformable stages are smoothed relative to the
  # template brain (the settings were validated on 1 mm templates)
  syn_scale <- yael_native_syn_scale(sum(template_mask > 0))
  if (syn_scale != 1) {
    yael_native_log(verbose, sprintf(
      "The template brain spans more voxels than at 1 mm: the update fields of the deformable stages are smoothed %g times as many voxels",
      syn_scale))
  }

  # ---- SyNabp: initial registration of the whole head, without masks ----
  res_abp <- stage("SyNabp", "registration", "initial registration of the head (affine and deformable)", function() {
    if (is.null(affine_transform)) {
      linear <- ravetools::register_volume3d(
        source = nu, target = template,
        source_vox2ras = native_vox2ras, target_vox2ras = template_vox2ras,
        type = "affine", metric = "mattes",
        shrink_factors = c(8, 4, 2, 1), smoothing_sigmas = c(4, 2, 1, 0),
        iterations = c(1000, 500, 250, 100), sampling_rate = 0.25,
        verbose = verbose
      )
      init_transform <- linear$transform
      rm(linear)
      # 'ANTsPy' leaves the convergence of 'SyNabp' to the defaults of
      # 'antsRegistration' (1e-6, window 10)
      syn_settings <- list(syn_iterations = c(50, 10, 0), syn_grad_step = 0.1,
                           syn_convergence = 1e-6, syn_convergence_window = 10L)
    } else {
      # 'SyNOnly' from the given affine, with the convergence of the 'ANTsPy'
      # 'SyN' transforms (1e-7, window 8)
      init_transform <- affine_transform
      syn_settings <- list(syn_iterations = c(40, 20, 0), syn_grad_step = 0.2,
                           syn_convergence = 1e-7, syn_convergence_window = 8L)
    }
    # `syn_sigma` takes the flow sigma of 'ANTsPy' (3), but 'ravetools' reads
    # it as a Gaussian standard deviation in voxels, whereas 'ANTs' reads it
    # as a variance (sigma about 1.7): the native update field is smoothed
    # more (a documented deviation; the values are those validated on real
    # data with a 1 mm template), and more again for templates with more
    # voxels per brain (`syn_scale`)
    registration <- ravetools::register_volume3d(
      source = nu, target = template,
      source_vox2ras = native_vox2ras, target_vox2ras = template_vox2ras,
      type = "syn_only", metric = "mattes", init_transform = init_transform,
      shrink_factors = c(4, 2, 1), smoothing_sigmas = c(2, 1, 0),
      syn_iterations = syn_settings$syn_iterations,
      syn_grad_step = syn_settings$syn_grad_step, syn_sigma = 3 * syn_scale,
      syn_total_sigma = 0, syn_cc_radius = 4L,
      syn_convergence = syn_settings$syn_convergence,
      syn_convergence_window = syn_settings$syn_convergence_window,
      verbose = verbose
    )
    warped <- registration$image
    attr(warped, "vox2ras") <- template_vox2ras
    registration$image <- NULL
    registration$images <- NULL
    list(registration = registration, warpedmovout = warped, warpedfixout = NULL)
  })$registration
  # template images are brought onto the native grid with the inverse field
  # only; the forward field is large for 0.5 mm templates
  res_abp$forward_field <- NULL
  rm(nu)
  free_memory()

  # template mask on the native grid through the initial registration
  native_mask0 <- NULL
  initial_native_mask <- function() {
    if (is.null(native_mask0)) {
      native_mask0 <<- yael_native_to_native(
        template_mask, res_abp, native_dim, native_vox2ras,
        interpolation = "nearest")
    }
    native_mask0
  }

  # ---- nu2: white matter normalized to 110, then N4 weighted by it ----
  nu2 <- stage("nu2", "image", "intensity normalization with the white matter", function() {
    # quick 3-class k-means segmentation of the raw image inside the brain
    wm <- yael_native_kmeans_wm(t1, mask = initial_native_mask(),
                                vox2ras = native_vox2ras, verbose = verbose)
    scale <- 110 / ifelse(wm$mean > 0, wm$mean, 1)
    wm_prob <- wm$prob
    rm(wm)
    ravetools::bias_correction_n4(
      t1 * scale,
      vox2ras = native_vox2ras,
      weight_mask = wm_prob > 0.5,
      intensity_truncation = NULL,
      shrink_factor = 1,
      iterations = c(50, 50, 30, 20),
      tolerance = 1e-7,
      rescale_intensities = TRUE
    )
  })
  rm(t1)
  free_memory()

  # ---- brainmask: template mask on the native grid ----
  brain_mask <- stage("brainmask", "image", "template brain mask on the native image", function() {
    mask <- initial_native_mask()
    yael_native_write_volume(nu2 * mask, file.path(working_path, "brain.nii.gz"),
                             vox2ras = native_vox2ras)
    mask
  })
  native_mask0 <- NULL
  brain <- nu2 * brain_mask
  attr(brain, "vox2ras") <- native_vox2ras

  # ---- atropos: prior-guided segmentation of the native brain ----
  atropos <- stage("atropos", "images", "tissue segmentation with the template priors", n = 5L, compute = function() {
    priors <- stage("atropos_prior", "images", "template tissue priors on the native image", n = 6L, compute = function() {
      # one map at a time, each with its own grid (0.5 mm for some templates)
      lapply(template_files$atropos[3:8], function(path) {
        prior <- yael_native_to_native(
          yael_native_read_volume(path), res_abp, native_dim, native_vox2ras,
          interpolation = "trilinear")
        free_memory()
        prior
      })
    })
    priors <- list(
      priors[[1]] * 0.2 + 0.4,                               # CSF: 0.4 to 0.6
      priors[[2]] * 0.2 + 0.4,                               # gray matter: 0.2 to 0.8
      (priors[[3]] + priors[[4]] + priors[[5]]) * 0.2 + 0.4, # white matter, deep gray matter, brain stem
      priors[[6]] * 0.4 + 0.2                                # cerebellum
    )
    # a class whose prior is zero inside the brain mask cannot be estimated
    inside <- brain_mask > 0
    keep <- vapply(priors, function(p) { any(p[inside] > 0) }, FALSE)
    rm(inside)
    if (!all(keep)) {
      warning(sprintf(
        "yael_native_normalize: the tissue prior of class %s is zero inside the brain mask; the class is left out of the segmentation (posterior set to zero).",
        paste(which(!keep), collapse = ", ")), call. = FALSE)
    }
    if (sum(keep) < 2) {
      stop("yael_native_normalize: fewer than two tissue priors overlap the brain mask; please check the template `atropos_*` images.")
    }
    seg <- ravetools::segment_volume_tissue_gmm(
      nu2, mask = brain_mask, priors = priors[keep], prior_weight = 0,
      mrf_beta = 0.3, mrf_radius = 1L, iterations = 50L,
      vox2ras = native_vox2ras, verbose = verbose
    )
    rm(priors)
    # labels and posteriors of the four classes, left-out classes are zero
    segmentation <- seg$segmentation
    labeled <- segmentation > 0
    segmentation[labeled] <- which(keep)[segmentation[labeled]]
    attr(segmentation, "vox2ras") <- native_vox2ras
    posteriors <- lapply(seq_along(keep), function(k) {
      if (keep[[k]]) {
        post <- seg$posteriors[[sum(keep[seq_len(k)])]]
      } else {
        post <- array(0, dim = native_dim)
      }
      attr(post, "vox2ras") <- native_vox2ras
      post
    })
    c(list(segmentation), posteriors)
  })
  # the initial registration is not needed anymore
  res_abp <- NULL
  free_memory()

  # ---- wm_artifacts: white matter outside the gray-matter envelope ----
  # the template mask is only placed to a few millimeters: bright non-brain
  # tissue next to the brain (a thick skull marrow) is segmented as white
  # matter and would pull the final registration, so it is left out of the
  # white-matter channel and of the brain mask. The classes keep the order of
  # their priors
  classes_ordered <- yael_native_segmentation_class_order(nu2, atropos[[1]])
  # how the gray-matter surface is wrapped (see `yael_native_gm_envelope`)
  wm_envelope_method <- "outer_smoothed"
  wm_artifacts <- stage("wm_artifacts", "image", "clean white matter outside of the gray-matter envelope", function() {
    if (!classes_ordered) {
      mask <- array(0, dim = native_dim)
      attr(mask, "vox2ras") <- native_vox2ras
      return(mask)
    }
    mask <- yael_native_clean_wm_mask(atropos[[1]], native_vox2ras,
                                      dgwbs = atropos[[4]],
                                      method = wm_envelope_method)
    voxel_ml <- abs(det(native_vox2ras[1:3, 1:3])) / 1000
    yael_native_log(verbose, sprintf(
      "Stage `wm_artifacts`: %.1f mL of white matter outside the gray-matter envelope (%.1f mL after dilating by 2 voxels) is left out of the white-matter channel and of the brain mask",
      attr(mask, "artifact_voxels") * voxel_ml, sum(mask > 0) * voxel_ml))
    mask
  })

  # ---- brain_mask_cleaned: brain mask without the white-matter artifacts ----
  # only the artifact voxels labeled white matter leave the mask: the dilated
  # artifacts also cover gray matter and CSF, which stay. The final
  # registration uses this mask for its masks and its brain image
  brain_mask <- stage("brain_mask_cleaned", "image", "brain mask without the white-matter artifacts", function() {
    mask <- brain_mask
    mask[wm_artifacts > 0 & atropos[[1]] == 3] <- 0
    mask
  })
  brain <- nu2 * brain_mask
  attr(brain, "vox2ras") <- native_vox2ras

  # ---- SyN_w_atropos: masked affine, then multi-channel deformable stage ----
  res_syn <- stage("SyN_w_atropos", "registration", "affine and deformable registration with the tissue posteriors", function() {
    # template CSF and deep-structure maps on the template T1 grid
    template_prior <- function(indices) {
      re <- NULL
      for (path in template_files$atropos[indices]) {
        prior <- yael_native_resample(yael_native_read_volume(path),
                                      template_dim, template_vox2ras)
        re <- if (is.null(re)) { prior } else { re + prior }
        rm(prior)
        free_memory()
      }
      attr(re, "vox2ras") <- template_vox2ras
      re
    }
    fixing_csf <- template_prior(3)
    fixing_dgwbs <- template_prior(5:7)
    moving_csf <- atropos[[2]]
    moving_dgwbs <- atropos[[4]]
    # white matter outside the gray matter (artifacts) is not white matter;
    # the gray matter and CSF that the dilated artifacts cover keep their
    # posteriors, as they stay in the brain mask
    moving_csf[wm_artifacts > 0 & atropos[[1]] == 3] <- 1
    moving_dgwbs[wm_artifacts > 0] <- 0

    # additional images (with skull) and their template images
    extra_moving <- list()
    extra_fixed <- list()
    extra_weights <- numeric(0)
    inside <- brain_mask > 0
    for (ii in seq_len(n_images)[-1]) {
      path <- mov_paths[[ii]]
      image <- yael_native_read_volume(path, allow_4d = TRUE)
      if (length(dim(image)) != 3) {
        warning(sprintf(
          "yael_native_normalize: additional image `%s` is not a 3D volume and is not used.",
          basename(path)), call. = FALSE)
        next
      }
      image <- yael_native_resample(image, native_dim, native_vox2ras,
                                    na_fill = NA)
      covered <- is.finite(image)
      coverage <- mean(covered[inside])
      if (!isTRUE(coverage >= 0.95)) {
        warning(sprintf(
          "yael_native_normalize: additional image `%s` covers %.1f%% of the brain mask (less than 95%%) and is not used.",
          basename(path), coverage * 100), call. = FALSE)
        next
      }
      image[!covered] <- stats::median(image[inside & covered])
      attr(image, "vox2ras") <- native_vox2ras
      if (identical(fix_paths[[ii]], fix_paths[[1]])) {
        fixed_image <- template
      } else {
        fixed_image <- yael_native_resample(
          yael_native_read_volume(fix_paths[[ii]]), template_dim, template_vox2ras)
      }
      extra_moving[[length(extra_moving) + 1]] <- image
      extra_fixed[[length(extra_fixed) + 1]] <- fixed_image
      extra_weights <- c(extra_weights, weights[[ii]])
      rm(image, fixed_image, covered)
    }
    rm(inside)

    if (with_skull) {
      moving_main <- nu2
      fixed_main <- template
      grad_step <- 0.2
    } else {
      moving_main <- brain
      fixed_main <- template * template_mask
      grad_step <- 0.15
      # the deformable stage has no target mask (see below): the other fixed
      # images are zero outside the template brain too, like the moving
      # channels outside the native brain
      mask_template <- function(image) {
        image <- image * template_mask
        attr(image, "vox2ras") <- template_vox2ras
        image
      }
      fixing_csf <- mask_template(fixing_csf)
      fixing_dgwbs <- mask_template(fixing_dgwbs)
      extra_fixed <- lapply(extra_fixed, mask_template)
    }
    attr(fixed_main, "vox2ras") <- template_vox2ras
    # the template with skull is only kept if a channel uses it
    rm("template", envir = normalize_env)
    free_memory()

    # stage 1: the affine stage of 'SyNAggro', with both masks
    affine <- ravetools::register_volume3d(
      source = moving_main, target = fixed_main,
      source_vox2ras = native_vox2ras, target_vox2ras = template_vox2ras,
      source_mask = brain_mask, target_mask = template_mask,
      type = "affine", metric = "mattes",
      shrink_factors = c(4, 2, 2, 1), smoothing_sigmas = c(3, 2, 1, 0),
      iterations = c(2100, 1200, 1200, 100), sampling_rate = 0.2,
      init_transform = affine_transform, verbose = verbose
    )
    affine_matrix <- affine$transform
    rm(affine)
    free_memory()

    # stage 2: the deformable stage of 'SyNAggro' from the affine, with the
    # additional images and the tissue posteriors as extra channels. As in
    # 'SyNabp', `syn_sigma` (3.5, the 'ANTsPy' flow sigma) is a standard
    # deviation in voxels for 'ravetools' but a variance for 'ANTs' (sigma
    # about 1.9), so the native update field is smoothed more. Without the
    # skull it has no target mask: 'ravetools' only drives the voxels inside
    # the target mask, so the moving tissue that the affine leaves outside the
    # template mask is never pulled in, and the edge of the brain is pushed
    # outward at every iteration (the brain inflates). The levels end with the
    # convergence of the 'ANTsPy' 'SyN' transforms (1e-7, window 8): a level
    # runs at least 8 iterations, and stops once its cost no longer decreases
    yael_native_log(verbose, sprintf(
      "Stage `SyN_w_atropos`: deformable stage with %d channels (weights: %s)",
      length(extra_moving) + 3L,
      paste(format(c(1, extra_weights, 0.1, 0.5)), collapse = ", ")))
    registration <- ravetools::register_volume3d(
      source = c(list(moving_main), extra_moving, list(moving_csf, moving_dgwbs)),
      target = c(list(fixed_main), extra_fixed, list(fixing_csf, fixing_dgwbs)),
      source_vox2ras = native_vox2ras, target_vox2ras = template_vox2ras,
      source_mask = brain_mask, target_mask = if (with_skull) template_mask,
      weights = c(1, extra_weights, 0.1, 0.5), metric = "mattes",
      type = "syn_only", 
      init_transform = affine_matrix,
      shrink_factors = c(8, 4, 2, 1), smoothing_sigmas = c(3, 2, 1, 0),
      syn_iterations = c(100, 70, 50, 0), syn_grad_step = grad_step,
      syn_sigma = 3.5 * syn_scale, syn_total_sigma = 0, syn_cc_radius = 4L,
      syn_convergence = 1e-7, syn_convergence_window = 8L,
      verbose = verbose
    )
    rm(extra_moving, extra_fixed, fixing_csf, fixing_dgwbs, moving_csf,
       moving_dgwbs, moving_main)
    warpedmovout <- registration$image
    # tmp <- ravetools::apply_transform3d_volume(brain_mask, registration)
    # fields::image.plot((tmp)[,150,], asp=1)
    # fields::image.plot((registration$image)[,150,], asp=1)
    attr(warpedmovout, "vox2ras") <- template_vox2ras
    registration$image <- NULL
    registration$images <- NULL
    free_memory()

    # the deformation must be one-to-one inside the brain: where it folds,
    # points and labels are mapped inconsistently (a check that fails does
    # not stop the normalization)
    folded <- tryCatch({
      yael_native_folding(registration$forward_field, template_mask,
                          vox2ras = template_vox2ras)
    }, error = function(e) {
      yael_native_log(verbose, sprintf(
        "Stage `SyN_w_atropos`: unable to check the deformation for folding (%s)",
        conditionMessage(e)))
      NA_real_
    })
    if (is.finite(folded)) {
      yael_native_log(verbose, sprintf(
        "Stage `SyN_w_atropos`: the deformation folds in %.3f%% of the template brain mask",
        folded * 100))
    }
    if (isTRUE(folded > 0.01)) {
      warning(sprintf(
        "yael_native_normalize: the deformation folds (is not one-to-one) in %.2f%% of the template brain mask; points and labels mapped through these voxels may be off by a few millimeters.",
        folded * 100), call. = FALSE)
    }

    # the registered template image on the native grid
    warpedfixout <- yael_native_to_native(fixed_main, registration, native_dim,
                                          native_vox2ras)
    rm(fixed_main)

    # final brain mask, and the raw image restored from its stage
    brain_mask2 <- yael_native_to_native(template_mask, registration, native_dim,
                                         native_vox2ras, interpolation = "nearest")
    raw <- tryCatch(yael_native_stage_read("T1", "image", working_path),
                    error = function(e) { NULL })
    if (!is.null(raw)) {
      yael_native_write_volume(raw * brain_mask2, file.path(working_path, "brain2.nii.gz"),
                               vox2ras = native_vox2ras)
      yael_native_write_volume(brain_mask2, file.path(working_path, "brainmask2.nii.gz"),
                               vox2ras = native_vox2ras)
    }
    rm(raw, brain_mask2)

    list(registration = registration, warpedmovout = warpedmovout,
         warpedfixout = warpedfixout)
  })
  free_memory()

  res_syn
}

# ---- Workflow ----------------------------------------------------------------------

#' @name cmd_run_yael_preprocess_native
#' @title Run the native \verb{YAEL} imaging processing workflow (without 'Python')
#' @description
#' The image processing pipeline of \code{\link{cmd_run_yael_preprocess}}
#' (\doi{10.1523/ENEURO.0328-23.2023}), with every step computed in 'R' by
#' \code{\link{YAELNativeProcess}} on top of \pkg{ravetools} (version 0.3.3 or
#' later) instead of \verb{ANTs} through 'Python': cross-modality rigid
#' registration, normalization of the 'T1'-weighted 'MRI' to template brains,
#' and subject-level atlases from the inverse normalization. The functions
#' take the same arguments, run the same steps and write the same files as
#' \code{yael_preprocess} and \code{cmd_run_yael_preprocess}, so the results
#' can be read by either engine.
#'
#' @inheritParams cmd_run_yael_preprocess
#' @param t1w_path path to the 'T1'-weighted preoperative 'MRI', used as
#' underlay and base image. To have an \verb{ACPC}-aligned scanner coordinate
#' system, please align the image before feeding it into this function. All
#' images must contain skulls (do not strip skulls)
#' @param normalize_template template to normalize to: default is
#' \code{'mni_icbm152_nlin_asym_09b'} (\verb{MNI152b}, 0.5 mm resolution).
#' The script of \code{cmd_run_yael_preprocess_native} switches to
#' \code{'mni_icbm152_nlin_asym_09a'} (\verb{MNI152a}, 1 mm resolution) when
#' the computer memory is below 32 gigabytes (see 'Details'), and
#' \code{yael_preprocess_native} gives a warning instead. Other choices are
#' \code{'mni_icbm152_nlin_asym_09c'} and \code{'fsaverage'} (also known as
#' \verb{MNI305})
#' @param normalize_affine optional path to an initial linear transform: the
#' normalization then starts from this transform, and its first registration
#' only runs the non-linear stage; for advanced users only
#' @param add_surfaces whether to add surfaces for the subject; default is
#' \code{FALSE}. The surfaces are created by reversing the normalization from
#' the template brain, hence the results will not be accurate. Enable this
#' option only if cortical surface estimation is not critical (and
#' \verb{FreeSurfer} reconstructions are inaccessible)
#' @param run_recon_all whether to run \verb{FreeSurfer}; default is true
#' @param use_antspynet ignored: the native engine has no \code{deep_atropos},
#' and the tissue segmentation always uses the template priors; kept so that
#' the arguments are the same as \code{\link{cmd_run_yael_preprocess}}
#' @details
#' Templates are located (and downloaded when missing) by \pkg{rpyANTs}, whose
#' template functions are pure 'R'; 'Python' is never started. The full
#' template resolution is used, which takes a lot of memory. With a 1 mm
#' 'T1' image (256 by 204 by 256 voxels), the normalization to
#' \code{'mni_icbm152_nlin_asym_09a'} (1 mm, with 0.5 mm tissue maps) took
#' about 2 minutes and peaked at about 8 gigabytes of resident memory; the
#' normalization to \code{'mni_icbm152_nlin_asym_09b'} (0.5 mm) took about 7
#' minutes and peaked at 15 gigabytes of resident memory, 22 gigabytes with
#' the memory that the operating system compressed. Hence the script built by
#' \code{cmd_run_yael_preprocess_native} switches from
#' \code{'mni_icbm152_nlin_asym_09b'} to \code{'mni_icbm152_nlin_asym_09a'}
#' when the memory recorded in the 'RAVE' settings (\code{max_mem}, the
#' computer memory unless set otherwise) is below 32 gigabytes, where the
#' script of \code{cmd_run_yael_preprocess} switches below 12 gigabytes.
#' \code{yael_preprocess_native} does not switch templates; it warns when
#' \code{max_mem} is below 32 gigabytes and a template has more than 30
#' million voxels.
#'
#' The algorithms differ from \verb{ANTs} in a few ways, described in
#' \code{\link{YAELNativeProcess}}: no \code{deep_atropos}, local
#' cross-correlation instead of mutual information in the non-linear stage,
#' greedy single-field \verb{SyN}, whose update-field smoothing takes the
#' \pkg{rpyANTs} values (3 and 3.5) as a \verb{Gaussian} standard deviation in
#' voxels where \verb{ANTs} reads them as a variance, and is increased for
#' templates with more voxels per brain (doubled for the 0.5 mm templates) so
#' that the deformation stays one-to-one; additional images resampled onto
#' the native 'T1' grid (and left out when they cover less than 95 percent of
#' the brain), and a binary template brain mask. Streamlines in \verb{VTK}
#' format are not read or written, since \pkg{ieegio} needs 'Python' for
#' them.
#'
#' @returns \code{yael_preprocess_native} returns nothing; the subject imaging
#' folder is created under the 'RAVE' raw folder. It will take a while to run
#' the workflow. \code{cmd_run_yael_preprocess_native} returns, invisibly, a
#' list with the script, its path, the log file, the image paths, and a
#' function \code{execute} that runs the script; the script is saved as
#' \code{'scripts/cmd-yael-preprocess-native-full.R'} (or \code{'-simple.R'}
#' without \verb{FreeSurfer}) under the imaging folder when it runs.
#' @seealso \code{\link{YAELNativeProcess}}, \code{\link{as_yael_process}},
#' \code{\link{cmd_run_yael_preprocess}}
#' @examples
#'
#' \dontrun{
#'
#' # For T1 normalization only; add ct_path to include coregistration
#' cmd_run_yael_preprocess_native(
#'   subject = "pt01",
#'   t1w_path = "/path/to/T1w.nii.gz",
#'
#'   additional_images = list(
#'     MEG = "/path/to/meg.nii.gz"
#'   ),
#'
#'   # normalize T1 to MNI152
#'   normalize_template = 'mni_icbm152_nlin_asym_09a'
#' )
#'
#' }
NULL

#' @rdname cmd_run_yael_preprocess_native
#' @export
yael_preprocess_native <- function(
    subject, t1w_path = NULL, ct_path = NULL,
    t2w_path = NULL, fgatir_path = NULL, preopct_path = NULL,
    flair_path = NULL, t1w_contrast_path = NULL,
    register_policy = c("auto", "all"), register_reversed = FALSE,
    normalize_template = "mni_icbm152_nlin_asym_09b",
    normalize_policy = c("auto", "all"),
    normalize_images = c("T1w", "T2w", "T1wContrast", "fGATIR", "preopCT"),
    normalize_back = ifelse(length(normalize_template) >= 1, normalize_template[[1]], NA),
    normalize_affine = NULL,
    atlases = list(),
    add_surfaces = FALSE,
    use_antspynet = TRUE,
    verbose = TRUE,
    additional_images = list(), ...
) {

  # DIPSAUS DEBUG START
  # list2env(
  #   envir = globalenv(),
  #   list(
  #     subject = "YAEL/K20",
  #     t1w_path = "/Users/dipterix/rave_data/raw_dir/K20/rave-imaging/inputs/MRI/MRI_RAW.nii.gz",
  #     ct_path = NULL,
  #     t2w_path = NULL,
  #     fgatir_path = NULL,
  #     preopct_path = NULL,
  #     flair_path = NULL,
  #     t1w_contrast_path = NULL,
  #     register_policy = "auto",
  #     register_reversed = FALSE,
  #     normalize_template = "mni_icbm152_nlin_sym_09a",
  #     normalize_policy = "auto",
  #     normalize_images = "T1w",
  #     normalize_back = "mni_icbm152_nlin_sym_09a",
  #     normalize_affine = NULL,
  #     atlases = list(),
  #     add_surfaces = FALSE,
  #     use_antspynet = TRUE,
  #     verbose = TRUE,
  #     additional_images = list()
  #   )
  # )

  if (missing(subject)) {
    subject <- list(...)$subject_code
    if (!length(subject)) {
      stop("yael_preprocess_native: `subject` is missing. Please provide a RAVE subject ID")
    }
    ravepipeline::logger(
      level = "warning",
      "yael_preprocess_native: initializing with argument `subject_code` is no longer ",
      "appropriate. Use argument `subject` instead"
    )
  }

  register_policy <- match.arg(register_policy)
  normalize_policy <- match.arg(normalize_policy)
  # fail before any image is imported or registered
  yael_native_require_ravetools()

  if (!is.null(normalize_affine)) {
    affine_transform <- normalizePath(normalize_affine, winslash = "/", mustWork = TRUE)
  } else {
    affine_transform <- NULL
  }

  normalize_template <- match.arg(normalize_template, rpyants_builtin_templates(), several.ok = TRUE)
  # generate atlases
  atlases <- as.list(atlases)

  mni_template_path <- function(...) {
    path <- ravepipeline::raveio_getopt(
      key = "mni_template_root",
      default = file.path(threeBrain::default_template_directory(), "templates")
    )
    path_abs(file_path(path, ...), must_work = FALSE)
  }

  for (template_name in normalize_template) {
    # makes sure the template files are there
    yael_native_template(template_name)
    if (!length(atlases[[template_name]])) {

      atlas_folder <- mni_template_path(template_name, "atlases")
      if (dir.exists(atlas_folder)) {
        atlases[[template_name]] <- path_abs(atlas_folder)
      }
    }
  }

  additional_images <- as.list(additional_images)
  image_types <- unique(c(
    "T1w", "T2w", "FLAIR", "preopCT", "T1wContrast", "fGATIR",
    "postopT1w", "postopT2w", "postopFLAIR", "CT", names(additional_images)))

  image_types <- image_types[nzchar(image_types)]
  additional_images <- additional_images[names(additional_images) %in% image_types]

  yael_process <- as_yael_process(subject = subject, image_types = image_types,
                                  engine = "native")

  if (length(t1w_path) && !is.na(t1w_path) && nzchar(t1w_path)) {
    ravepipeline::logger("Migrating T1w image: ", t1w_path, level = "trace")
    yael_process$set_input_image(path = t1w_path, type = "T1w", overwrite = TRUE)
  } else {
    t1w_path <- yael_process$get_input_image("T1w")
    stopifnot2(
      length(t1w_path) == 1 && file.exists(t1w_path),
      msg = "Please specify T1w image path"
    )
  }

  ct_path <- ct_path %||% additional_images$CT
  if (length(ct_path) && !is.na(ct_path) && nzchar(ct_path)) {
    ravepipeline::logger(level = "trace", "Migrating CT image: ", ct_path)
    yael_process$set_input_image(path = ct_path, type = "CT", overwrite = TRUE)
  }

  t2w_path <- t2w_path %||% additional_images$T2w
  if (length(t2w_path) && !is.na(t2w_path) && nzchar(t2w_path)) {
    ravepipeline::logger(level = "trace", "Migrating T2w image: ", t2w_path)
    yael_process$set_input_image(path = t2w_path, type = "T2w", overwrite = TRUE)
  }

  fgatir_path <- fgatir_path %||% additional_images$fGATIR
  if (length(fgatir_path) && !is.na(fgatir_path) && nzchar(fgatir_path)) {
    ravepipeline::logger(level = "trace", "Migrating fGATIR image: ", fgatir_path)
    yael_process$set_input_image(path = fgatir_path, type = "fGATIR", overwrite = TRUE)
  }

  preopct_path <- preopct_path %||% additional_images$preopCT
  if (length(preopct_path) && !is.na(preopct_path) && nzchar(preopct_path)) {
    ravepipeline::logger(level = "trace", "Migrating preop-CT image: ", preopct_path)
    yael_process$set_input_image(path = preopct_path, type = "preopCT", overwrite = TRUE)
  }

  flair_path <- flair_path %||% additional_images$FLAIR
  if (length(flair_path) && !is.na(flair_path) && nzchar(flair_path)) {
    ravepipeline::logger(level = "trace", "Migrating FLAIR image: ", flair_path)
    yael_process$set_input_image(path = flair_path, type = "FLAIR", overwrite = TRUE)
  }

  t1w_contrast_path <- t1w_contrast_path %||% additional_images$T1wContrast
  if (length(t1w_contrast_path) && !is.na(t1w_contrast_path) && nzchar(t1w_contrast_path)) {
    ravepipeline::logger(level = "trace", "Migrating T1w with contrast image: ", t1w_contrast_path)
    yael_process$set_input_image(path = t1w_contrast_path, type = "T1wContrast", overwrite = TRUE)
  }

  additional_images <- additional_images[!names(additional_images) %in% c(
    "T1w", "T2w", "FLAIR", "preopCT", "T1wContrast", "fGATIR", "CT"
  )]

  for (additional_type in names(additional_images)) {
    additional_path <- additional_images[[additional_type]]
    if (length(additional_path) && !is.na(additional_path) && nzchar(additional_path)) {
      ravepipeline::logger(level = "trace", sprintf("Migrating %s image: %s", additional_type, additional_path))
      yael_process$set_input_image(path = additional_path, type = additional_type, overwrite = TRUE)
    }
  }

  # Coregistration
  image_types <- unique(yael_process$image_types)
  image_types <- image_types[!tolower(image_types) %in% "t1w"]
  lapply(image_types, function(native_type) {
    impath <- yael_process$get_input_image(native_type)
    if (!length(impath)) { return() }
    ravepipeline::logger(level = "info", "Co-registering [", native_type, "] image with [T1w] image.")
    suppressWarnings({
      pexists <- tryCatch({
        conf <- yael_process$get_native_mapping(image_type = native_type)
        length(conf$mappings) > 0
      }, error = function(e) { FALSE })
    })
    if (!pexists || register_policy == "all") {
      yael_process$register_to_T1w(image_type = native_type,
                                   reverse = register_reversed,
                                   verbose = verbose)
    }
    return()
  })

  # Normalization
  if ( length(normalize_back) < 1 || is.na(normalize_back[[1]]) ) {
    normalize_back <- NULL
  } else {
    normalize_back <- normalize_back[[1]]
  }
  normalize_template <- unique(c(normalize_back, normalize_template))
  lapply(normalize_template, function(template_name) {
    ravepipeline::logger(level = "info", "Normalizing [T1w] image to template [", template_name, "].")
    suppressWarnings({
      pexists <- tryCatch({
        conf <- yael_process$get_template_mapping(template_name = template_name, native_type = "T1w")
        length(conf) > 0
      }, error = function(e) { FALSE })
    })
    if (!pexists || normalize_policy == "all") {
      yael_process$map_to_template(template_name = template_name,
                                   native_type = "T1w",
                                   use_images = normalize_images,
                                   use_antspynet = use_antspynet,
                                   affine_transform = affine_transform,
                                   verbose = verbose)
    }
    if ( template_name %in% normalize_back ) {
      # Generate ANTs folder
      yael_process$construct_ants_folder_from_template(
        template_name = normalize_back,
        add_surfaces = add_surfaces
      )
    }
  })

  # Make sure the Norig and Torig transforms are set to conformed image
  t1_mgz <- file.path(yael_process$work_path, "ants", "mri", "T1.mgz")
  if (!file.exists(t1_mgz)) {
    yael_process$construct_ants_folder_from_template(
      template_name = NULL,
      add_surfaces = FALSE
    )
  }

  for (template_name in names(atlases)) {
    if (template_name != "") {
      tryCatch({
        yael_process$generate_atlas_from_template(
          template_name = template_name,
          atlas_folder = atlases[[template_name]],
          verbose = verbose,
          surfaces = TRUE
        )
      }, error = function(e) {
        warning(e)
      })
    }
  }

  invisible()
}


#' @rdname cmd_run_yael_preprocess_native
#' @export
cmd_run_yael_preprocess_native <- function(
    subject,
    t1w_path = NULL,
    ct_path = NULL,
    t2w_path = NULL,
    fgatir_path = NULL,
    preopct_path = NULL,
    flair_path = NULL,
    t1w_contrast_path = NULL,
    register_reversed = FALSE,
    normalize_template = "mni_icbm152_nlin_asym_09b",
    normalize_images = c("T1w", "T2w", "T1wContrast", "fGATIR", "preopCT"),
    normalize_affine = NULL,
    run_recon_all = TRUE,
    dry_run = FALSE,
    use_antspynet = TRUE,
    verbose = TRUE, ...) {

  use_antspynet <- isTRUE(use_antspynet)

  if (missing(subject)) {
    subject <- list(...)$subject_code
    if (!length(subject)) {
      stop("cmd_run_yael_preprocess_native: `subject` is missing. Please provide a RAVE subject ID")
    }
    ravepipeline::logger(
      level = "warning",
      "cmd_run_yael_preprocess_native: initializing with argument `subject_code` is no longer ",
      "appropriate. Use argument `subject` instead"
    )
  }

  run_recon_all <- as.integer(isTRUE(as.logical(run_recon_all)))
  register_reversed <- isTRUE(as.logical(register_reversed))
  if (!dry_run) {
    # the script would fail in the child process
    yael_native_require_ravetools()
  }

  yael_process <- as_yael_process(subject = subject, engine = "native")
  subject <- yael_process$get_subject()
  project_name <- subject$project_name
  subject_code <- subject$subject_code

  if (length(t1w_path)) {
    t1w_path <- normalizePath(t1w_path, winslash = "/", mustWork = TRUE)
  } else {
    if (!length(yael_process$get_input_image("T1w"))) {
      stop("`cmd_run_yael_preprocess_native`: No `T1w` MRI specified. Please specify a valid file")
    }
    t1w_path <- ""
  }
  if (length(ct_path)) { ct_path <- normalizePath(ct_path, winslash = "/", mustWork = TRUE) } else { ct_path <- "" }
  if (length(t2w_path)) { t2w_path <- normalizePath(t2w_path, winslash = "/", mustWork = TRUE) } else { t2w_path <- "" }
  if (length(fgatir_path)) { fgatir_path <- normalizePath(fgatir_path, winslash = "/", mustWork = TRUE) } else { fgatir_path <- "" }
  if (length(preopct_path)) { preopct_path <- normalizePath(preopct_path, winslash = "/", mustWork = TRUE) } else { preopct_path <- "" }
  if (length(flair_path)) { flair_path <- normalizePath(flair_path, winslash = "/", mustWork = TRUE) } else { flair_path <- "" }
  if (length(t1w_contrast_path)) { t1w_contrast_path <- normalizePath(t1w_contrast_path, winslash = "/", mustWork = TRUE) } else { t1w_contrast_path <- "" }

  if (length(normalize_affine)) {
    normalize_affine <- normalizePath(normalize_affine, winslash = "/", mustWork = TRUE)
  } else {
    normalize_affine <- NULL
  }

  if (length(normalize_template)) {
    # check every name now, also for a dry run, as `cmd_run_yael_preprocess`
    # does through `rpyANTs::ensure_template` (nothing is downloaded here): a
    # wrong name would otherwise only fail in the script, with the error in
    # its log file
    builtin_templates <- rpyants_builtin_templates()
    normalize_template <- vapply(normalize_template, function(template_name) {
      match.arg(template_name, builtin_templates)
    }, "", USE.NAMES = FALSE)
    if ("mni_icbm152_nlin_asym_09b" %in% normalize_template) {
      normalize_template <- unique(c("mni_icbm152_nlin_asym_09b", normalize_template))
    }
    if (!dry_run) {
      # download the templates now, not in the script
      lapply(normalize_template, yael_native_template)
    }
    normalize_template_str <- deparse1(normalize_template)
  } else {
    normalize_template_str <- "NULL"
  }

  normalize_images <- unname(unique(c("T1w", unlist(normalize_images))))
  normalize_images_str <- deparse1(normalize_images)

  log_path <- normalizePath(
    file.path(subject$imaging_path, "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-yael-preprocess-native-%y%m%d-%H%M%S.log")

  # Always use a temporary working path since the target directory might contain spaces
  work_path_actual <- path_abs(subject$preprocess_settings$raw_path, must_work = FALSE)

  template <- c(readLines(system.file("shell-templates/yael-preprocess-native.R", package = "ravecore")), "")
  cmd <- ravepipeline::glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE)

  if ( run_recon_all ) {
    script_name <- "cmd-yael-preprocess-native-full.R"
  } else {
    script_name <- "cmd-yael-preprocess-native-simple.R"
  }

  script_path <- normalizePath(
    file.path(subject$imaging_path, "scripts", script_name),
    mustWork = FALSE, winslash = "/"
  )
  execute <- function(...) {
    initialize_imaging_paths(subject)
    dir_create2(log_path)
    log_abspath <- normalizePath(file.path(log_path, log_file),
                                 winslash = "/", mustWork = FALSE)
    cmd_execute(script = cmd, script_path = script_path,
                args = c("--no-save", "--no-restore"),
                command = rscript_path(),
                stdout = log_abspath, stderr = log_abspath, ...)
  }
  re <- list(
    script = cmd,
    script_path = script_path,
    dry_run = dry_run,
    log_file = file.path(log_path, log_file, fsep = "/"),
    src_path = t1w_path,
    # the imaging folder may not exist before the script runs
    dest_path = path_abs(subject$imaging_path, must_work = FALSE),
    execute = execute,
    command = rscript_path()
  )
  if ( verbose ) {
    message(cmd)
  }
  if (dry_run) {
    return(invisible(re))
  }

  execute()

  return(invisible(re))

}

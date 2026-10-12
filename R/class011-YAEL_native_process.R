# Native ('Python'-free) 'YAEL' imaging process: the public interface of
# `YAELProcess` (`R/class011-YAEL_process.R`) and the same files on disk, with
# the methods of 'rpyANTs' `YAELPreprocess` (`rpyants/registration/yael.py`)
# ported to 'R' on top of the engine in `R/workflow-yael_preprocess_native.R`.
# The class never starts 'Python': templates are located by
# `yael_native_template()`, and the 'rpyANTs' metadata it reads
# (`BUILTIN_TEMPLATES`, `template_urls`) are pure 'R'.

# ---- Helpers -------------------------------------------------------------------

# Files (not folders) of a folder in sorted order, hidden files included, like
# `os.listdir` filtered by `os.path.isfile`; a missing folder has none
yael_native_list_files <- function(path) {
  if (!dir.exists(path)) { return(character(0L)) }
  files <- list.files(path, all.files = TRUE, no.. = TRUE)
  files[!dir.exists(file.path(path, files))]
}

# Image types as 'Python' prints a list, for the error messages
yael_native_type_list <- function(types) {
  sprintf("[%s]", paste(sprintf("'%s'", types), collapse = ", "))
}

# Order `K` of a transform of type `ants<K>`, read the way
# `int(type.replace("ants", ""))` reads it in 'Python'; `NA` if not a number
yael_native_ants_order <- function(type) {
  x <- trimws(gsub("ants", "", type, fixed = TRUE))
  if (!grepl("^[+-]?[0-9]+(_[0-9]+)*$", x)) { return(NA_integer_) }
  suppressWarnings(as.integer(gsub("_", "", x, fixed = TRUE)))
}

# Transforms between the native image `native_type` and a template (camel-case
# name) under `transform_dir`: the candidates of `get_template_mapping`. Returns
# a data frame with the file name, its subject entity, the order `K` of
# `ants<K>`, and whether it maps the native image to the template (`forward`)
# or the other way. `from` and `to` are compared case-insensitively; files
# without `sub`, `from` or `to` are ignored
yael_native_template_transforms <- function(transform_dir, native_type,
                                            template_name) {
  native_lower <- tolower(native_type)
  template_lower <- tolower(template_name)
  re <- data.frame(filename = character(0L), sub = character(0L),
                   order = integer(0L), forward = logical(0L),
                   stringsAsFactors = FALSE)
  for (filename in yael_native_list_files(transform_dir)) {
    parsed <- yael_parse_bids_filename(filename)
    if (is.null(parsed)) { next }
    order <- yael_native_ants_order(parsed@suffix)
    if (is.na(order)) { next }
    sub <- bidsr::get_bids_entity(parsed, "sub")
    from_space <- bidsr::get_bids_entity(parsed, "from")
    to_space <- bidsr::get_bids_entity(parsed, "to")
    if (is.null(sub) || is.null(from_space) || is.null(to_space)) { next }
    from_space <- tolower(from_space)
    to_space <- tolower(to_space)
    if (from_space == native_lower && to_space == template_lower) {
      forward <- TRUE
    } else if (from_space == template_lower && to_space == native_lower) {
      forward <- FALSE
    } else {
      next
    }
    re[nrow(re) + 1L, ] <- list(filename, sub, order, forward)
  }
  re
}

# Grid of an image without its voxels: the dimension (first three), and the
# voxel to scanner 'RAS' and 'FreeSurfer' "tkr" 'RAS' matrices
yael_native_read_header <- function(path) {
  header <- withCallingHandlers(
    ieegio::read_volume(path, header_only = TRUE),
    warning = function(w) {
      # header-only reading does not use 'RNifti', which is expected
      if (grepl("header-only mode", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
  dm <- c(as.integer(header$shape), 1L, 1L, 1L)[1:3]
  list(
    dim = dm,
    vox2ras = matrix(as.numeric(header$transforms$vox2ras)[seq_len(16)], nrow = 4L),
    vox2ras_tkr = matrix(as.numeric(header$transforms$vox2ras_tkr)[seq_len(16)], nrow = 4L)
  )
}

# 'ravetools' interpolation of an 'ANTs' interpolator. `"auto"` uses linear
# interpolation for probability maps (values between 0 and 1, some of them
# fractional) and nearest neighbor otherwise, the rule of
# `YAELPreprocess.generate_atlas_from_template` (without its failure on an
# image of ones); interpolators that 'ravetools' does not have are linear
yael_native_interpolation <- function(interpolator, volume = NULL) {
  switch(
    interpolator,
    "auto" = {
      values <- volume[is.finite(volume)]
      if (length(values) && min(values) >= 0 && max(values) <= 1 &&
          any(values > 1e-5 & values < 1)) {
        "trilinear"
      } else {
        "nearest"
      }
    },
    "nearestNeighbor" = ,
    "genericLabel" = "nearest",
    "linear" = "trilinear",
    {
      ravepipeline::logger(
        level = "warn", use_glue = FALSE,
        sprintf("Interpolator `%s` is not available in the native engine; using linear (trilinear) interpolation instead.", interpolator)
      )
      "trilinear"
    }
  )
}

# Writes a mapping list as 'JSON' with the structure that `json.dump` gives
# in 'Python': length-one vectors are scalars, `NULL` is `null`, transform
# lists stay arrays, and an empty list of mappings is an object
yael_native_write_json <- function(x, path) {
  prepare <- function(x) {
    if (is.null(x)) { return(NULL) }
    if (!is.list(x)) { return(unname(as.vector(x))) }
    keys <- names(x)
    re <- lapply(x, prepare)
    names(re) <- keys
    for (key in intersect(c("transformlist", "whichtoinvert"), keys)) {
      re[[key]] <- I(re[[key]])
    }
    if (!length(re) && !is.null(keys)) {
      re <- structure(list(), names = character(0L))
    }
    re
  }
  dir_create2(dirname(path))
  writeLines(
    as.character(jsonlite::toJSON(prepare(x), auto_unbox = TRUE, null = "null")),
    con = path
  )
  invisible(path)
}

# 'FreeSurfer' "tkr" 'RAS' to scanner 'RAS' of a template `fs` folder, from
# the header of the volume that 'threeBrain' would use for the transforms
# (identity when there is none, as the default transforms of 'threeBrain')
yael_native_fs_tkr_to_scanner <- function(fs_dir) {
  prefixes <- c("brain.finalsurfs", "synthSR.norm", "synthSR", "brain",
                "brainmask", "brainmask.auto", "T1")
  files <- file.path(fs_dir, "mri", as.vector(rbind(
    sprintf("%s.nii.gz", prefixes),
    sprintf("%s.nii", prefixes),
    sprintf("%s.mgz", prefixes)
  )))
  files <- files[file.exists(files)]
  if (!length(files)) {
    files <- list.files(file.path(fs_dir, "mri"), pattern = "\\.mg(z|h)$",
                        full.names = TRUE, ignore.case = TRUE)
  }
  if (!length(files)) { return(diag(4)) }
  header <- yael_native_read_header(files[[1]])
  header$vox2ras %*% solve(header$vox2ras_tkr)
}


#' @title Class definition of the native \verb{YAEL} image pipeline
#' @description
#' Rigid registration across multiple types of images, non-linear
#' normalization from the native brain to common templates, and mapping of
#' template atlases or regions of interest back to the native brain, without
#' 'Python'. The class has the same methods, arguments and active fields as
#' \code{\link{YAELProcess}} and writes the same files (names, folders,
#' \verb{'ANTs'}-compatible \code{.mat} and warp files, \verb{'JSON'} logs), so
#' the results of one class can be read by the other. The registrations,
#' bias-field correction and tissue segmentation come from \pkg{ravetools}
#' (version 0.3.3 or later). See examples at \code{\link{as_yael_process}}
#' (with \code{engine = "native"}) and \code{\link{yael_preprocess_native}}.
#' @details
#' Differences from \code{\link{YAELProcess}} and the \pkg{rpyANTs} pipeline
#' behind it:
#' \itemize{
#' \item There is no \code{deep_atropos}: argument \code{use_antspynet} of
#' \code{map_to_template} is ignored, and the tissue segmentation with the
#' template priors (the \verb{Atropos} branch) always runs.
#' \item The non-linear stage uses the local cross-correlation for every image
#' (\pkg{ravetools} has no non-linear mutual information), also when an
#' initial \verb{affine} transform is given.
#' \item Algorithms: \pkg{ravetools} runs a greedy \verb{SyN} with a single
#' field and a fixed-point inverse; its linear stages use an automatic step
#' size, random (\verb{Bernoulli}) sampling and no histogram matching, and its
#' \verb{affine} registration starts with a rigid pass. The smoothing of the
#' \verb{SyN} update field takes the \pkg{rpyANTs} values (3 for the initial
#' registration, 3.5 for the final one) as a \verb{Gaussian} standard
#' deviation in voxels, whereas \verb{ANTs} reads them as a variance, so the
#' native update field is smoothed more. These values hold for 1 mm
#' templates; for templates with more voxels per brain they are multiplied by
#' the cube root of the ratio of brain voxels, rounded to a half (2 for the
#' 0.5 mm \code{'mni_icbm152_nlin_asym_09b'} and
#' \code{'mni_icbm152_nlin_sym_09b'}, 1.5 for the 0.25 mm macaque template):
#' with the 1 mm values, the greedy update folded the deformation in about
#' 1.5 percent of the brain on \code{'mni_icbm152_nlin_asym_09b'}. The share
#' of the template brain mask where the final deformation folds (is not
#' one-to-one) is logged, with a warning above 1 percent.
#' \item The quick three-class segmentation that finds the white matter for
#' the intensity normalization runs on the raw image, as in \pkg{rpyANTs};
#' when a few extreme intensities inside the brain leave one of its classes
#' empty (\verb{Atropos} keeps going, \pkg{ravetools} stops), it is run again
#' with the intensities clipped at their 99.9 percent quantile inside the
#' brain (then at the 99 percent quantile).
#' \item Grids: additional images are resampled onto the grid of the native
#' \code{'T1w'} image, and left out (with a warning) when they cover less than
#' 95 percent of the brain mask; template tissue priors are resampled onto the
#' grid of the template \code{'T1'} image; the outermost voxel layer of the
#' stored warp fields has no displacement.
#' \item The template brain mask is made binary (this matters for
#' \code{'fsaverage'}, whose mask holds intensities); a tissue class whose
#' prior is empty inside the brain is left out of the segmentation (with a
#' warning) instead of failing.
#' \item Outputs: \code{transform_image_from_template} and
#' \code{transform_image_to_template} return \code{ieegio_volume} objects;
#' only nearest-neighbor and linear (\verb{trilinear}) interpolation are
#' available, and \code{interpolator = "auto"} follows its documented rule
#' (\code{YAELProcess} uses nearest-neighbor); the queries never create
#' folders; \code{set_input_image} reads the new image before it removes the
#' old one; the \verb{'NIfTI'} data types are chosen by \pkg{ieegio};
#' normalization results are replaced together, after the new ones are
#' complete, and stale \code{ants<K>} transforms of earlier layouts are
#' removed; nothing is written into the template folders.
#' \item Streamlines: \pkg{ieegio} reads and writes \verb{VTK} files through
#' 'Python', so \code{generate_streamlines_from_template} skips \verb{VTK}
#' inputs (with a warning) and refuses \code{format = "vtk"}; it also refuses
#' \code{format = "tt"} and saves \verb{TT} inputs as \verb{TCK} files, since
#' \pkg{ieegio} cannot write \verb{TT} files.
#' }
#' @export
YAELNativeProcess <- R6::R6Class(
  classname = "YAELNativeProcess",
  portable = TRUE,
  inherit = RAVESerializable,
  private = list(
    .work_path = NULL,
    .project_name = character(),
    .subject_code = character(),
    .image_types = character(),

    # Images under `inputs/anat` by type (`YAELPreprocess.reload_images`): files
    # whose parsed type is allowed; hidden files and folders are skipped, and
    # the last file of a type (in sorted order) is used
    input_images = function() {
      input_path <- file.path(self$work_path, "inputs", "anat")
      images <- list()
      for (filename in yael_native_list_files(input_path)) {
        if (startsWith(filename, ".")) { next }
        parsed <- yael_parse_bids_filename(filename)
        if (!is.null(parsed) && parsed@suffix %in% private$.image_types) {
          # the file name on disk, not `format(parsed)`: 'bidsr' orders the
          # entities differently from rpyANTs
          images[[parsed@suffix]] <- list(
            folder = "inputs/anat",
            fullname = filename,
            type = parsed@suffix,
            ext = tolower(paste(parsed@extension, collapse = "")),
            entities = yael_bids_entities(parsed)
          )
        }
      }
      images
    },

    # Path of an input image (`YAELPreprocess.input_image_path`), or `NULL`
    input_image_path = function(type, relative = FALSE,
                                images = private$input_images()) {
      if (length(type) != 1 || is.na(type)) { return(NULL) }
      item <- images[[type]]
      if (is.null(item)) { return(NULL) }
      if (isTRUE(relative)) {
        return(file.path(item$folder, item$fullname))
      }
      file.path(as.character(self$work_path), item$folder, item$fullname)
    },

    check_image_type = function(type) {
      if (length(type) != 1 || is.na(type) || !type %in% private$.image_types) {
        stop(sprintf("Invalid image type: %s. Must be one of %s",
                     paste(type, collapse = ", "),
                     yael_native_type_list(private$.image_types)))
      }
      invisible(type)
    },

    # Rigid registration of an image with the `T1w` image (port of
    # `YAELPreprocess.register_to_T1w`); the public method adds the files of
    # the electrode localization for `CT`, this one is also used to register
    # missing images before the normalization
    register_to_T1w_impl = function(image_type, reverse = FALSE, verbose = TRUE) {
      reverse <- isTRUE(reverse)
      verbose <- isTRUE(verbose)
      if (identical(image_type, "T1w")) {
        stop("Cannot register T1w to itself.")
      }
      images <- private$input_images()
      t1_path <- private$input_image_path("T1w", images = images)
      if (is.null(t1_path)) {
        stop("Missing image for type `T1w`. Please set the image first via `set_input_image`.")
      }
      image_path <- private$input_image_path(image_type, images = images)
      if (is.null(image_path)) {
        stop(sprintf("Missing image for type `%s`. Please set the image first via `set_input_image`.", image_type))
      }
      work_path <- as.character(self$work_path)
      tmp_path <- file.path(work_path, "tmp", sprintf("coregister_%s_with_T1w", image_type))
      if (file.exists(tmp_path)) { unlink(tmp_path, recursive = TRUE) }
      dir_create2(tmp_path)

      if (reverse) {
        fixed_path <- image_path
        moving_path <- t1_path
      } else {
        fixed_path <- t1_path
        moving_path <- image_path
      }
      yael_native_log(verbose, sprintf(
        "Registering [%s] with [T1w] (fixed: %s)", image_type,
        ifelse(reverse, image_type, "T1w")))
      registration <- yael_native_coregister_rigid(
        fixed = fixed_path, moving = moving_path, verbose = verbose)
      # the registration and the moving image on the fixed grid, as 'ANTs'
      # leaves them (`out_0GenericAffine.mat`, `out_Warped.nii.gz`)
      ravetools::save_registration(registration, path = file.path(tmp_path, "out_.dcf"))
      if (!is.null(registration$image)) {
        yael_native_write_volume(registration$image, file.path(tmp_path, "out_Warped.nii.gz"),
                                 vox2ras = registration$geometry$target_vox2ras)
      }

      # `transform` maps fixed 'RAS' to moving 'RAS'
      transform <- registration$transform
      rm(registration)
      if (reverse) {
        # image (fixed) to T1w (moving)
        image_to_t1 <- transform
      } else {
        # T1w (fixed) to image (moving)
        image_to_t1 <- solve(transform)
      }
      file_names <- yael_native_file_names(
        private$.subject_code, image_type, image_types = private$.image_types,
        ext = images[[image_type]]$ext)
      transform_dir <- dir_create2(file.path(work_path, "coregistration", "transformations"))
      # as point maps, the 'ANTs' transform `from-<type>_to-T1w` goes from T1w
      # to the image (it resamples the image onto the T1w grid)
      ravetools::write_ants_transform(
        solve(image_to_t1), file.path(transform_dir, file_names$coreg_to_t1_ants))
      ravetools::write_ants_transform(
        image_to_t1, file.path(transform_dir, file_names$coreg_from_t1_ants))
      yael_native_write_tsv(
        image_to_t1, file.path(transform_dir, file_names$coreg_to_t1_ras2ras))
      yael_native_write_tsv(
        solve(image_to_t1), file.path(transform_dir, file_names$coreg_from_t1_ras2ras))

      # the image voxels with the realigned `sform`
      image <- yael_native_read_volume(image_path, allow_4d = TRUE)
      vox2ras <- image_to_t1 %*% attr(image, "vox2ras")
      yael_native_write_tsv(
        vox2ras, file.path(transform_dir, file_names$coreg_to_t1_vox2ras))
      yael_native_write_volume(
        image, file.path(work_path, "coregistration", "anat", file_names$coreg_anat),
        vox2ras = vox2ras)
      rm(image)

      mapping <- self$get_native_mapping(image_type = image_type, relative = TRUE)
      yael_native_write_json(
        mapping, file.path(work_path, "coregistration", "log", file_names$coreg_log))
      invisible(mapping)
    },

    # Template image mapped onto the native grid: an array with "vox2ras"
    transform_from_template = function(path, template_name, native_type,
                                       interpolator, verbose = TRUE) {
      mapping <- self$get_template_mapping(template_name = template_name,
                                           native_type = native_type,
                                           relative = FALSE)
      if (is.null(mapping)) {
        stop(sprintf(
          "Missing mapping from %s to %s. Please register the image to the template first.",
          native_type, camel_template_name(template_name)))
      }
      native_path <- private$input_image_path(native_type)
      if (is.null(native_path)) {
        stop(sprintf("Missing image for type `%s`. Please set the image first via `set_input_image`.", native_type))
      }
      yael_native_require_ravetools()
      reference <- yael_native_read_header(native_path)
      volume <- yael_native_read_volume(path, allow_4d = TRUE)
      interpolation <- yael_native_interpolation(interpolator, volume)
      yael_native_log(verbose, sprintf(
        "Transforming %s from template [%s] to [%s] (%s)", basename(path),
        template_name, native_type, interpolation))
      ravetools::apply_transform3d_volume(
        volume, as.list(mapping$template_to_native$transformlist),
        vox2ras = attr(volume, "vox2ras"), reference_dim = reference$dim,
        reference_vox2ras = reference$vox2ras, interpolation = interpolation
      )
    },

    # Points through the `template_to_native` (`to_template = TRUE`) or
    # `native_to_template` transforms, with the point semantics of 'ANTs'
    transform_points = function(points, template_name, native_type, to_template) {
      if (is.vector(points)) {
        points <- matrix(points, ncol = 3, byrow = TRUE)
      } else if (!is.matrix(points)) {
        points <- as.matrix(points)
      }
      if (ncol(points) > 3) {
        points <- points[, 1:3, drop = FALSE]
      }
      dimnames(points) <- NULL
      storage.mode(points) <- "double"
      mapping <- self$get_template_mapping(template_name = template_name,
                                           native_type = native_type,
                                           relative = FALSE)
      if (is.null(mapping)) {
        stop(sprintf(
          "Missing mapping from %s to %s. Please register the image to the template first.",
          native_type, camel_template_name(template_name)))
      }
      yael_native_require_ravetools()
      if (to_template) {
        transforms <- mapping$template_to_native$transformlist
      } else {
        transforms <- mapping$native_to_template$transformlist
      }
      # rows with a missing coordinate stay missing
      ravetools::apply_transform3d_points(points, as.list(transforms))
    },

    # Template pial surfaces morphed onto the native brain, if the template
    # has a `fs/surf` folder. The template surfaces are read directly, so no
    # 'threeBrain' brain is built on the template folder
    morph_template_pial = function(template, template_name, ants_dir, surf_dir) {
      template_surf_dir <- file.path(template$fs_dir, "surf")
      if (!dir.exists(template_surf_dir)) { return(invisible()) }
      hemispheres <- c("lh", "rh")
      sources <- lapply(hemispheres, function(hemi) {
        files <- file.path(template_surf_dir, sprintf(c("%s.pial.T1", "%s.pial"), hemi))
        files <- files[file.exists(files)]
        if (length(files)) { files[[1]] } else { NULL }
      })
      if (all(vapply(sources, is.null, FALSE))) { return(invisible()) }

      # template tkr RAS -> template scanner RAS
      tkr_to_scanner <- yael_native_fs_tkr_to_scanner(template$fs_dir)
      # native scanner RAS -> native tkr RAS
      native_brain <- threeBrain::threeBrain(path = ants_dir, subject_code = self$subject_code)
      scanner_to_tkr <- native_brain$Torig %*% solve(native_brain$Norig)

      for (ii in seq_along(hemispheres)) {
        if (is.null(sources[[ii]])) { next }
        surface <- threeBrain::read.fs.surface(sources[[ii]])
        scan_ras <- tkr_to_scanner %*% t(cbind(surface$vertices, 1))
        # template scanner RAS -> native scanner RAS
        native_ras <- self$transform_points_from_template(
          template_ras = t(scan_ras[seq_len(3), , drop = FALSE]),
          template_name = template_name,
          native_type = "T1w",
          verbose = FALSE
        )
        native_tkr <- scanner_to_tkr %*% t(cbind(native_ras, 1))
        dir_create2(surf_dir)
        threeBrain::write.fs.surface(
          filepath = file.path(surf_dir, sprintf("%s.pial.T1", hemispheres[[ii]])),
          vertex_coords = t(native_tkr[seq_len(3), , drop = FALSE]),
          faces = surface$faces
        )
      }
      invisible()
    }
  ),
  public = list(

    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      list(
        namespace = "ravecore",
        r6_generator = "YAELNativeProcess",
        data = list(
          project_name = private$.project_name,
          subject_code = private$.subject_code,
          work_path = private$.work_path,
          image_types = private$.image_types
        )
      )
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      # called from the class generator, without an instance
      stopifnot(object$namespace == "ravecore")
      stopifnot(object$r6_generator == "YAELNativeProcess")
      subject <- RAVESubject$new(
        project_name = object$data$project_name,
        subject_code = object$data$subject_code,
        strict = FALSE
      )
      YAELNativeProcess$new(
        subject = subject,
        image_types = object$data$image_types,
        imaging_path = object$data$work_path
      )
    },

    #' @description
    #' Constructor to instantiate the class
    #' @param subject 'RAVE' subject or subject ID; for native standard,
    #' this can be character code without project names, but for 'BIDS'
    #' subjects, this must be a full subject ID with project information
    #' @param image_types vector of image types, such as \code{'T1w'},
    #' \code{'CT'}, \code{'fGATIR'}. All images except \code{'CT'} will be
    #' considered \code{'preop'} (before electrode implantation). Please
    #' use \code{'postop'} to indicate if an image is taken after the
    #' implantation (for example, \code{'postopT1w'})
    #' @param imaging_path imaging path (path to \code{'rave-imaging'}
    #' if not default); internally used to set the work path during
    #' serialization. Please do not set it manually unless you know what you
    #' are doing
    #' @param ... reserved for legacy code
    initialize = function(subject, image_types, imaging_path = NULL, ...) {
      if (missing(subject)) {
        subject <- list(...)$subject_code
        if (!length(subject)) {
          stop("YAELNativeProcess: `subject` is missing. Please provide a RAVE subject ID")
        }
        ravepipeline::logger(
          level = "warning",
          "YAELNativeProcess: initializing with argument `subject_code` is no longer ",
          "appropriate. Use argument `subject` instead"
        )
      }

      if (!is.character(subject) || grepl("/", subject, fixed = TRUE)) {
        # user passed a subject ID
        subject <- as_rave_subject(subject, strict = FALSE)
      } else {
        stopifnot2(length(subject) == 1 && is.character(subject) &&
                     !is.na(subject) && nzchar(subject),
                   msg = "Please enter a valid subject code (with combinations of letter and digits)")
        subject <- gsub("^sub-", "", subject, ignore.case = TRUE)
        subject <- RAVESubject$new(project_name = "YAEL",
                                   subject_code = subject,
                                   strict = FALSE)
      }

      private$.subject_code <- subject$subject_code
      private$.project_name <- subject$project_name
      if (length(imaging_path) && !is.na(imaging_path)) {
        private$.work_path <- imaging_path
      } else {
        private$.work_path <- path_abs(subject$imaging_path, must_work = FALSE)
      }

      if (missing(image_types)) {
        image_types <- YAEL_IMAGE_TYPES
      }

      existing_modalities <- NULL

      input_path <- file.path(self$work_path, "inputs", "anat")
      if (dir.exists(input_path)) {
        input_images <- list.files(
          input_path,
          all.files = FALSE,
          full.names = FALSE,
          recursive = FALSE,
          include.dirs = FALSE,
          ignore.case = TRUE,
          pattern = "\\.(nii|nii\\.gz)$"
        )
        existing_modalities <- unlist(lapply(input_images, function(fname) {
          fname <- strsplit(fname, "_", fixed = TRUE)[[1]]
          fname <- fname[[length(fname)]]
          gsub(pattern = "\\.(nii|nii\\.gz)$", "", x = fname, ignore.case = TRUE)
        }))
      }

      image_types <- unique(c(image_types, existing_modalities))
      image_types <- unname(image_types[!is.na(image_types)])
      private$.image_types <- as.character(image_types)
    },

    #' @description
    #' Set the raw input for different image types
    #' @param path path to the image files in \code{'NIfTI'} or \code{'MGZ'}
    #' format
    #' @param type type of the image
    #' @param overwrite whether to overwrite existing images if the same type
    #' has been imported before; default is false
    #' @param on_error when the file exists and \code{overwrite} is false,
    #' how should this error be reported; choices are \code{'warning'} (default),
    #' \code{'error'} (throw error and abort), or \code{'ignore'}.
    #' @returns whether the image has been set (or replaced)
    set_input_image = function(path,
                               type = YAEL_IMAGE_TYPES,
                               overwrite = FALSE,
                               on_error = c("warning", "error", "ignore") ) {
      path <- as.character(path_abs(path, must_work = TRUE))
      if (length(type) != 1) {
        type <- match.arg(type)
      }
      on_error <- match.arg(on_error)
      dir_create2(self$work_path)
      if (!type %in% private$.image_types) {
        private$.image_types <- c(private$.image_types, type)
      }

      tryCatch({
        existing_path <- private$input_image_path(type)
        if (!isTRUE(overwrite) && !is.null(existing_path)) {
          stop(sprintf("Image type %s already exists. If you want to overwrite it, set `overwrite = TRUE`.", type))
        }
        parsed <- yael_input_image_name(path, type = type,
                                        subject_code = private$.subject_code,
                                        image_types = private$.image_types)
        target_path <- file.path(as.character(self$work_path), parsed$folder,
                                 parsed$fullname)
        # read first: `path` may be the image that is replaced
        image <- yael_native_read_volume(path, allow_4d = TRUE)
        if (!is.null(existing_path) && file.exists(existing_path)) {
          unlink(existing_path)
        }
        yael_native_write_volume(image, target_path)
        rm(image)
        if ( type == "T1w" ) {
          path <- self$get_input_image("T1w")
          if (length(path)) {
            mri_dir <- file.path(self$work_path, "inputs", "MRI")
            dir_create2(mri_dir)
            file.copy(path, to = file.path(mri_dir, "MRI_RAW.nii.gz"), overwrite = TRUE)
          }
        }
        TRUE
      }, error = function(e) {
        switch(
          on_error,
          "warning" = { warning(e$message) },
          "error" = { stop(e$message) },
          {}
        )
        FALSE
      })
    },

    #' @description Get image path
    #' @param type type of the image
    #' @returns Absolute path if the image, or \code{NULL} if missing
    get_input_image = function(type = YAEL_IMAGE_TYPES) {
      if (length(type) != 1) {
        type <- match.arg(type)
      }
      private$input_image_path(type)
    },

    #' @description Get 'RAVE' subject instance
    #' @param ... ignored
    #' @param strict passed to \code{\link{as_rave_subject}}
    #' @returns 'RAVE' subject instance
    get_subject = function(..., strict = FALSE) {
      RAVESubject$new(
        project_name = private$.project_name,
        subject_code = private$.subject_code,
        strict = strict
      )
    },

    #' @description Register other images to \code{'T1'} weighted \code{'MRI'}
    #' with two rigid passes (mutual information)
    #' @param image_type type of the image to register, must be set via
    #' \code{process$set_input_image} first.
    #' @param reverse whether to reverse the registration; default is false,
    #' meaning the fixed (reference) image is the \code{'T1'}. When setting to
    #' true, then the \code{'T1'} 'MRI' will become the moving image
    #' @param verbose whether to print out the process; default is true
    #' @returns Nothing
    register_to_T1w = function(image_type = "CT", reverse = FALSE, verbose = TRUE) {
      reverse <- isTRUE(reverse)
      verbose <- isTRUE(verbose)
      private$register_to_T1w_impl(image_type = image_type, reverse = reverse,
                                   verbose = verbose)
      if ( image_type == "CT" ) {
        # write down extras for YAEL localization module
        mapping <- self$get_native_mapping(
          image_type = "CT", relative = FALSE)
        file.copy(
          from = mapping$CT_path,
          to = file.path(mapping$work_path, "coregistration", "CT_RAW.nii.gz"),
          overwrite = TRUE
        )
        file.copy(
          from = mapping$T1w_path,
          to = file.path(mapping$work_path, "coregistration", "MRI_reference.nii.gz"),
          overwrite = TRUE
        )
        file.copy(
          from = mapping$mappings$vox2ras,
          to = file.path(mapping$work_path, "coregistration", "CT_IJK_to_MR_RAS.txt"),
          overwrite = TRUE
        )
      }
      invisible()
    },

    #' @description Get the mapping configurations used by \code{register_to_T1w}
    #' @param image_type type of the image registered to 'T1' weighted 'MRI'
    #' @param relative whether to use relative path (to the \code{work_path} field)
    #' @returns A list of moving and fixing images, with rigid transformations
    #' from different formats; \code{mappings} is empty before the image is
    #' registered. No folder is created.
    get_native_mapping = function(image_type = YAEL_IMAGE_TYPES, relative = FALSE) {
      if (length(image_type) != 1) {
        image_type <- match.arg(image_type)
      }
      private$check_image_type(image_type)
      relative <- isTRUE(relative)
      work_path <- as.character(self$work_path)
      images <- private$input_images()
      transform_dir <- file.path(work_path, "coregistration", "transformations")
      aligned_dir <- file.path(work_path, "coregistration", "anat")
      if (relative) {
        transform_prefix <- "coregistration/transformations"
        aligned_prefix <- "coregistration/anat"
      } else {
        transform_prefix <- transform_dir
        aligned_prefix <- aligned_dir
      }

      mappings <- structure(list(), names = character(0L))
      # transforms from the image to T1w, by type (`ants`, `ras2ras`, `vox2ras`)
      for (filename in yael_native_list_files(transform_dir)) {
        parsed <- yael_parse_bids_filename(filename)
        if (is.null(parsed)) { next }
        if (identical(bidsr::get_bids_entity(parsed, "from"), image_type) &&
            identical(bidsr::get_bids_entity(parsed, "to"), "T1w")) {
          mappings[[parsed@suffix]] <- file.path(transform_prefix, filename)
        }
      }
      # the image aligned to T1w
      aligned_key <- sprintf("%s_in_T1w", image_type)
      aligned_files <- yael_native_list_files(aligned_dir)
      for (filename in aligned_files) {
        parsed <- yael_parse_bids_filename(filename)
        if (!is.null(parsed) && identical(parsed@suffix, image_type)) {
          mappings[[aligned_key]] <- file.path(aligned_prefix, filename)
        }
      }
      # several aligned images may share the type (e.g. a `postop*` image
      # saved as `ses-preop` by rpyANTs <= 0.0.6): use the one that
      # `register_to_T1w` writes now, if it is there under this exact name
      item <- images[[image_type]]
      if (!is.null(item)) {
        expected <- yael_native_file_names(
          private$.subject_code, image_type, image_types = private$.image_types,
          ext = item$ext)$coreg_anat
        if (expected %in% aligned_files) {
          mappings[[aligned_key]] <- file.path(aligned_prefix, expected)
        }
      }

      result <- list(
        private$.subject_code,
        work_path,
        private$input_image_path(image_type, relative = relative, images = images),
        private$input_image_path("T1w", relative = relative, images = images),
        mappings
      )
      names(result) <- c("subject", "work_path", sprintf("%s_path", image_type),
                         "T1w_path", "mappings")
      if (identical(image_type, "T1w")) {
        result <- result[-4L]
      }
      result
    },

    #' @description Normalize native brain to \code{'MNI152'} template
    #' @param template_name which template to use, choices are
    #' \code{'mni_icbm152_nlin_asym_09a'}, \code{'mni_icbm152_nlin_asym_09b'},
    #' \code{'mni_icbm152_nlin_asym_09c'}, and \code{'fsaverage'}.
    #' @param native_type which type of image should be used to map to template;
    #' default is \code{'T1w'}
    #' @param use_images a vector of image types to use for normalization;
    #' default types are \code{'T1w'}, \code{'T2w'}, \code{'T1wContrast'},
    #' \code{'fGATIR'}, and \code{'preopCT'}. To use all available images
    #' for normalization, use wildcard \code{"all"}. Images that are not
    #' registered to \code{'T1w'} yet are registered first (a failure is
    #' reported as a warning and the image is left out)
    #' @param use_antspynet ignored: the native engine has no
    #' \code{deep_atropos}, and the tissue segmentation always uses the
    #' template priors
    #' @param verbose whether to print out the process; default is true
    #' @param ... additional tuning parameters: \code{affine_transform}
    #' (optional initial \verb{affine} transform, an \verb{'ANTs'} \code{.mat} file
    #' from the template to the native image as point map; the initial
    #' registration then only runs its non-linear stage), \code{with_skull}
    #' (whether to register the images with skull; default is false), and
    #' \code{cost_function} (only \code{'CC'}, the local cross-correlation, is
    #' available; other values give a warning); other arguments are ignored
    #' @returns See method \code{get_template_mapping}
    map_to_template = function(template_name = rpyants_builtin_templates(),
                               use_images = c("T1w", "T2w", "T1wContrast",
                                              "fGATIR", "preopCT"),
                               native_type = "T1w",
                               use_antspynet = TRUE,
                               verbose = TRUE, ...) {
      template_name <- match.arg(template_name)
      verbose <- isTRUE(verbose)
      # before the template is located (or downloaded), other images are
      # registered, or the stage folder is removed
      yael_native_require_ravetools()
      template <- yael_native_template(template_name)
      # the full template resolution takes a lot of memory
      yael_native_check_memory(template$t1)
      # camel version for BIDS
      template_name2 <- template$camel
      use_images <- unique(unlist(use_images))
      if (any(use_images %in% "all")) {
        use_images <- private$.image_types
      }
      private$check_image_type(native_type)
      images <- private$input_images()
      native_path <- private$input_image_path(native_type, images = images)
      if (is.null(native_path)) {
        stop(sprintf("Missing image for type `%s`. Please set the image first via `set_input_image`.", native_type))
      }
      if (isTRUE(use_antspynet) && verbose) {
        ravepipeline::logger(
          level = "info", use_glue = FALSE,
          "The native engine has no `deep_atropos` (antspynet); the tissue segmentation uses the template priors."
        )
      }

      # images of other types aligned with the native image; missing
      # registrations to T1w are computed first
      other_images <- list()
      other_types <- use_images[use_images %in% private$.image_types]
      for (modality in other_types) {
        if (identical(modality, native_type)) { next }
        aligned_key <- sprintf("%s_in_%s", modality, native_type)
        mapping <- self$get_native_mapping(image_type = modality, relative = FALSE)
        mapped_path <- mapping$mappings[[aligned_key]]
        if (identical(native_type, "T1w") && is.null(mapped_path) &&
            !is.null(mapping[[sprintf("%s_path", modality)]])) {
          registered <- tryCatch({
            private$register_to_T1w_impl(image_type = modality, reverse = FALSE,
                                         verbose = verbose)
            TRUE
          }, error = function(e) {
            warning(sprintf(
              "Unable to register the `%s` image to `T1w` (%s); the image is not used for the normalization.",
              modality, conditionMessage(e)), call. = FALSE)
            FALSE
          })
          if (registered) {
            mapping <- self$get_native_mapping(image_type = modality, relative = FALSE)
            mapped_path <- mapping$mappings[[aligned_key]]
          }
        }
        if (!is.null(mapped_path)) {
          other_images[[modality]] <- mapped_path
        }
      }

      work_path <- as.character(self$work_path)
      working_path <- file.path(work_path, "tmp", sprintf(
        "coregister_%s_with_%s", native_type, template_name2))
      if (file.exists(working_path)) { unlink(working_path, recursive = TRUE) }
      dir_create2(working_path)
      dir_create2(file.path(work_path, "normalization"))

      # 'Python' passes these through `**kwargs`; others are ignored
      args <- list(...)
      cost_function <- args[["cost_function"]]
      if (!length(cost_function)) { cost_function <- "CC" }
      yael_native_log(verbose, sprintf(
        "Normalizing [%s] to template [%s] with %d additional image(s)%s",
        native_type, template_name, length(other_images),
        ifelse(length(other_images), sprintf(" (%s)", paste(names(other_images), collapse = ", ")), "")))
      result <- yael_native_normalize(
        fix_path = template$t1,
        mov_paths = c(native_path, unlist(other_images)),
        working_path = working_path,
        weights = c(1.25, yael_normalization_weight(names(other_images))),
        with_skull = isTRUE(args[["with_skull"]]),
        cost_function = cost_function,
        affine_transform = args[["affine_transform"]],
        verbose = verbose
      )

      # new results are written next to the stages, then replace the old ones
      file_names <- yael_native_file_names(
        private$.subject_code, native_type, template_name = template_name2,
        image_types = private$.image_types, ext = images[[native_type]]$ext)
      out_path <- dir_create2(file.path(working_path, "out"))
      stage_prefix <- file.path(working_path, "SyN_w_atropos")
      registration <- result$registration
      field_grid <- function(field) {
        vox2ras <- attr(field, "vox2ras")
        if (is.null(vox2ras)) { vox2ras <- registration$geometry$target_vox2ras }
        vox2ras
      }
      # the stage files are the 'ANTs' files of the final registration (the
      # affine maps template RAS to native RAS, the fields are on the template
      # grid); they are written again only if they cannot be copied
      copy_or_write <- function(stage_file, name, write) {
        dst <- file.path(out_path, name)
        if (!file.exists(stage_file) || !isTRUE(file.copy(stage_file, dst, overwrite = TRUE))) {
          write(dst)
        }
        dst
      }
      copy_or_write(
        sprintf("%s0GenericAffine.mat", stage_prefix),
        file_names$norm_to_template_affine,
        function(dst) { ravetools::write_ants_transform(registration$transform, dst) })
      copy_or_write(
        sprintf("%s1Warp.nii.gz", stage_prefix),
        file_names$norm_to_template_warp,
        function(dst) {
          ravetools::write_ants_warp(registration$forward_field, dst,
                                     vox2ras = field_grid(registration$forward_field))
        })
      copy_or_write(
        sprintf("%s1InverseWarp.nii.gz", stage_prefix),
        file_names$norm_from_template_warp,
        function(dst) {
          ravetools::write_ants_warp(registration$inverse_field, dst,
                                     vox2ras = field_grid(registration$inverse_field))
        })
      ravetools::write_ants_transform(
        solve(registration$transform),
        file.path(out_path, file_names$norm_from_template_affine))
      rm(registration)
      yael_native_write_volume(result$warpedmovout,
                               file.path(out_path, file_names$norm_anat_in_template))
      yael_native_write_volume(result$warpedfixout,
                               file.path(out_path, file_names$norm_anat_in_native))
      rm(result)

      # remove the previous normalization between the image and the template,
      # including the transforms of earlier layouts (such as `ants2`)
      transform_dir <- dir_create2(file.path(work_path, "normalization", "transformations"))
      anat_dir <- dir_create2(file.path(work_path, "normalization", "anat"))
      log_dir <- dir_create2(file.path(work_path, "normalization", "log"))
      previous <- yael_native_template_transforms(transform_dir, native_type, template_name2)
      previous <- previous$filename[tolower(previous$sub) == tolower(private$.subject_code)]
      unlink(file.path(transform_dir, previous))
      unlink(file.path(anat_dir, c(file_names$norm_anat_in_template,
                                   file_names$norm_anat_in_native)))
      unlink(file.path(log_dir, file_names$norm_log))

      # move the new files in, the log last
      for (name in c(file_names$norm_to_template_affine, file_names$norm_to_template_warp,
                     file_names$norm_from_template_warp, file_names$norm_from_template_affine)) {
        file_move(file.path(out_path, name), file.path(transform_dir, name))
      }
      for (name in c(file_names$norm_anat_in_template, file_names$norm_anat_in_native)) {
        file_move(file.path(out_path, name), file.path(anat_dir, name))
      }
      mapping <- self$get_template_mapping(template_name = template_name,
                                           native_type = native_type,
                                           relative = TRUE)
      if (!is.null(mapping)) {
        yael_native_write_json(mapping, file.path(out_path, file_names$norm_log))
        file_move(file.path(out_path, file_names$norm_log),
                  file.path(log_dir, file_names$norm_log))
      }
      unlink(out_path, recursive = TRUE)

      return(invisible(
        self$get_template_mapping(template_name = template_name,
                                  native_type = native_type)
      ))
    },

    #' @description Get configurations used for normalization
    #' @param template_name which template is used
    #' @param native_type which native image is mapped to template
    #' @param relative whether the paths should be relative or absolute; default
    #' is false (absolute paths)
    #' @returns A list of input, output images, with forward and inverse
    #' transform files (usually two \code{'Affine'} with one displacement
    #' field), or \code{NULL} if the image is not normalized to the template
    #' yet. The transforms of the subject are used; transforms whose subject
    #' entity is spelled in another letter case are used if there is only one
    #' such spelling, and the transforms of another subject only if they are
    #' the only ones and the native image was imported under that subject
    #' (with a warning)
    get_template_mapping = function(template_name = rpyants_builtin_templates(),
                                    native_type = "T1w", relative = FALSE) {
      template_name <- match.arg(template_name)
      template_name2 <- camel_template_name(template_name)
      private$check_image_type(native_type)
      transform_dir <- file.path(as.character(self$work_path), "normalization",
                                 "transformations")
      if (!dir.exists(transform_dir)) { return(NULL) }
      candidates <- yael_native_template_transforms(transform_dir, native_type,
                                                    template_name2)
      select_transforms <- function(subjects) {
        selected <- candidates[candidates$sub %in% subjects, , drop = FALSE]
        list(forward = selected[selected$forward, , drop = FALSE],
             inverse = selected[!selected$forward, , drop = FALSE])
      }
      is_complete <- function(selected) {
        nrow(selected$forward) > 0 && nrow(selected$inverse) > 0
      }
      subject_code <- private$.subject_code
      all_subjects <- unique(candidates$sub)

      # the subject's own transforms
      selected <- select_transforms(subject_code)
      if (!is_complete(selected)) {
        # `sub-` entity in another letter case, only if it is spelled one way
        same_subject <- all_subjects[tolower(all_subjects) == tolower(subject_code)]
        if (length(same_subject) == 1) {
          selected <- select_transforms(same_subject)
        }
      }
      if (!is_complete(selected)) {
        # the `sub-` entity differs from the subject code when the folder was
        # renamed or copied: use such transforms only if they all come from
        # one subject, under which the native image was imported as well
        native_image <- private$input_images()[[native_type]]
        native_subject <- NULL
        if (!is.null(native_image)) {
          native_subject <- native_image$entities[["sub"]]
        }
        if (length(all_subjects) == 1 && length(native_subject) == 1 &&
            native_subject %in% all_subjects) {
          selected <- select_transforms(all_subjects)
          if (is_complete(selected)) {
            warning(sprintf(
              "Using transforms of subject `%s` from %s for subject `%s` (template `%s`)",
              native_subject, transform_dir, subject_code, template_name2),
              call. = FALSE)
          }
        }
      }
      if (!is_complete(selected)) { return(NULL) }

      if (isTRUE(relative)) {
        path_prefix <- "normalization/transformations"
      } else {
        path_prefix <- transform_dir
      }
      # ordered by `K` of `ants<K>`, the largest first (stable)
      transform_list <- function(x) {
        x <- x[order(-x$order, seq_len(nrow(x))), , drop = FALSE]
        list(
          transformlist = file.path(path_prefix, x$filename),
          whichtoinvert = rep(FALSE, nrow(x))
        )
      }
      list(
        type = native_type,
        template = template_name2,
        using = "ants.apply_transforms",
        native_to_template = transform_list(selected$forward),
        template_to_native = transform_list(selected$inverse)
      )
    },

    #' @description Apply transform from images (usually an atlas or a region
    #' of interest)
    #' on template to native space
    #' @param template_roi_path path to the template image file which will be
    #' transformed into individuals' image
    #' @param template_name templates to use
    #' @param native_type which type of native image to use for calculating
    #' the coordinates (default \code{'T1w'})
    #' @param interpolator how to interpolate the \code{'voxels'}; default is
    #' \code{"auto"}: linear for probability maps (all values between 0 and 1,
    #' some of them fractional) and \code{'nearestNeighbor'} otherwise.
    #' \code{'nearestNeighbor'} and \code{'genericLabel'} use the nearest
    #' neighbor, all others the linear (\verb{trilinear}) interpolation
    #' @param verbose whether the print out the progress
    #' @returns transformed image on the grid of the native image, as an
    #' \code{ieegio_volume} object
    transform_image_from_template = function(template_roi_path,
                                             template_name = rpyants_builtin_templates(),
                                             native_type = "T1w",
                                             interpolator = c(
                                               "auto", "nearestNeighbor",
                                               "linear", "gaussian", "bSpline",
                                               "cosineWindowedSinc",
                                               "welchWindowedSinc",
                                               "hammingWindowedSinc",
                                               "lanczosWindowedSinc",
                                               "genericLabel"),
                                             verbose = TRUE
    ) {
      stopifnot(file.exists(template_roi_path))
      template_roi_path <- as.character(path_abs(template_roi_path, must_work = TRUE))
      interpolator <- match.arg(interpolator)
      template_name <- match.arg(template_name)
      volume <- private$transform_from_template(
        path = template_roi_path, template_name = template_name,
        native_type = native_type, interpolator = interpolator,
        verbose = isTRUE(verbose))
      vox2ras <- attr(volume, "vox2ras")
      attributes(volume) <- list(dim = dim(volume))
      ieegio::as_ieegio_volume(volume, vox2ras = vox2ras)
    },

    #' @description Apply transform to images (usually an atlas or a region
    #' of interest)
    #' from native space to template
    #' @param native_roi_path path to the native image file that will be
    #' transformed into template
    #' @param template_name templates to use
    #' @param native_type which type of native image to use for calculating
    #' the coordinates (default \code{'T1w'})
    #' @param interpolator how to interpolate the \code{'voxels'}; default is
    #' \code{"auto"}: linear for probability maps (all values between 0 and 1,
    #' some of them fractional) and \code{'nearestNeighbor'} otherwise.
    #' \code{'nearestNeighbor'} and \code{'genericLabel'} use the nearest
    #' neighbor, all others the linear (\verb{trilinear}) interpolation
    #' @param verbose whether the print out the progress
    #' @returns transformed image on the grid of the template \code{'T1'}
    #' image, as an \code{ieegio_volume} object
    transform_image_to_template = function(native_roi_path,
                                           template_name = rpyants_builtin_templates(),
                                           native_type = "T1w",
                                           interpolator = c(
                                             "auto", "nearestNeighbor",
                                             "linear", "gaussian", "bSpline",
                                             "cosineWindowedSinc",
                                             "welchWindowedSinc",
                                             "hammingWindowedSinc",
                                             "lanczosWindowedSinc",
                                             "genericLabel"),
                                           verbose = TRUE
    ) {
      stopifnot(file.exists(native_roi_path))
      native_roi_path <- as.character(path_abs(native_roi_path, must_work = TRUE))
      interpolator <- match.arg(interpolator)
      template_name <- match.arg(template_name)
      mapping <- self$get_template_mapping(template_name = template_name,
                                           native_type = native_type,
                                           relative = FALSE)
      if (is.null(mapping)) {
        stop(sprintf(
          "Missing mapping from %s to %s. Please register the image to the template first.",
          native_type, camel_template_name(template_name)))
      }
      yael_native_require_ravetools()
      reference <- yael_native_read_header(yael_native_template(template_name)$t1)
      volume <- yael_native_read_volume(native_roi_path, allow_4d = TRUE)
      interpolation <- yael_native_interpolation(interpolator, volume)
      yael_native_log(isTRUE(verbose), sprintf(
        "Transforming %s from [%s] to template [%s] (%s)",
        basename(native_roi_path), native_type, template_name, interpolation))
      volume <- ravetools::apply_transform3d_volume(
        volume, as.list(mapping$native_to_template$transformlist),
        vox2ras = attr(volume, "vox2ras"), reference_dim = reference$dim,
        reference_vox2ras = reference$vox2ras, interpolation = interpolation
      )
      attributes(volume) <- list(dim = dim(volume))
      ieegio::as_ieegio_volume(volume, vox2ras = reference$vox2ras)
    },

    #' @description Generate atlas maps from template and morph to native brain
    #' @param template_name which template to use
    #' @param atlas_folder path to the atlas folder (that contains the atlas
    #' files); every \code{'NIfTI'} file in the folder and its sub-folders is
    #' mapped to \code{'atlases/<sub-folder>/<file name>'} under the working
    #' directory (file names starting with a dot are skipped). Probability
    #' maps (values between 0 and 1, some fractional) are interpolated
    #' linearly, other images with the nearest neighbor
    #' @param surfaces whether to generate surfaces (triangle mesh); default is
    #' \code{NA} (generate if not existed). Other choices are \code{TRUE}
    #' for always generating and overwriting surface files, or \code{FALSE}
    #' to disable this function. The generated surfaces will stay in native
    #' \code{'T1'} space.
    #' @param verbose whether the print out the progress
    #' @param lambda,degree,threshold_lb,threshold_ub passed to
    #' \code{\link[threeBrain]{volume_to_surf}}
    #' @param smooth_method how to smooth the surfaces: \code{"implicit"}
    #' (default, \code{\link[ravetools]{vcg_smooth_implicit}} with
    #' \code{lambda} and \code{degree}) or \code{"explicit"}
    #' (\code{\link[ravetools]{mris_smooth}}); passed to
    #' \code{\link[threeBrain]{volume_to_surf}}
    #' @param max_vertices surfaces with more vertices are reduced to about
    #' this many after smoothing; passed to
    #' \code{\link[threeBrain]{volume_to_surf}}; default is \code{500000}
    #' @returns Paths to the atlas (volume) files under the \code{'atlases'}
    #' folder of the working directory
    generate_atlas_from_template = function(
        template_name = rpyants_builtin_templates(),
        atlas_folder = NULL, surfaces = NA, verbose = TRUE,
        lambda = 0.2, degree = 2, threshold_lb = 0.5, threshold_ub = NA,
        smooth_method = c("implicit", "explicit"), max_vertices = 500000
    ) {
      template_name <- match.arg(template_name)
      smooth_method <- match.arg(smooth_method)
      verbose <- isTRUE(verbose)

      if ( length(atlas_folder) != 1 || is.na(atlas_folder) || !nzchar(atlas_folder) ) {
        atlas_folder <- yael_native_template(template_name)$atlas_dir
      }
      if (!dir.exists(atlas_folder)) {
        stop("No atlases for template [", template_name, "]. Please place your atlas files under \n\t", atlas_folder)
      }
      atlas_folder <- normalizePath(atlas_folder, winslash = "/", mustWork = TRUE)
      atlas_root <- file.path(as.character(self$work_path), "atlases")

      # every folder (`os.walk`), and the volumes in it
      folders <- list.dirs(atlas_folder, full.names = FALSE, recursive = TRUE)
      atlas_files <- unlist(lapply(folders, function(folder) {
        files <- yael_native_list_files(file.path(atlas_folder, folder))
        files <- files[!startsWith(files, ".") &
                         grepl("\\.(nii|nii\\.gz)$", files, ignore.case = TRUE)]
        if (!length(files)) { return(NULL) }
        if (nzchar(folder)) { file.path(folder, files) } else { files }
      }))
      if (length(atlas_files)) {
        mapping <- self$get_template_mapping(template_name = template_name,
                                             native_type = "T1w")
        if (is.null(mapping)) {
          stop(sprintf(
            "Missing mapping from %s to %s. Please register the image to the template first.",
            "T1w", camel_template_name(template_name)))
        }
        yael_native_require_ravetools()
      }
      for (folder in folders) {
        dir_create2(if (nzchar(folder)) file.path(atlas_root, folder) else atlas_root)
      }
      for (atlas_file in atlas_files) {
        volume <- private$transform_from_template(
          path = file.path(atlas_folder, atlas_file),
          template_name = template_name, native_type = "T1w",
          interpolator = "auto", verbose = verbose)
        yael_native_write_volume(volume, file.path(atlas_root, atlas_file))
        rm(volume)
      }

      volume_files <- list.files(
        atlas_root,
        pattern = "\\.(nii|nii\\.gz)$",
        recursive = TRUE,
        include.dirs = FALSE,
        all.files = FALSE,
        full.names = TRUE
      )
      if (isFALSE(surfaces)) { return(invisible(volume_files)) }
      paths <- ravepipeline::lapply_jobs(volume_files, function(path) {
        asNamespace("ravecore")$generate_atlas_surface(
          path,
          overwrite = surfaces,
          lambda = lambda,
          degree = degree,
          threshold_lb = threshold_lb,
          threshold_ub = threshold_ub,
          smooth_method = smooth_method,
          max_vertices = max_vertices
        )
      }, .globals = list(
        surfaces = surfaces,
        lambda = lambda,
        degree = degree,
        threshold_lb = threshold_lb,
        threshold_ub = threshold_ub,
        smooth_method = smooth_method,
        max_vertices = max_vertices
      ), callback = function(path) {
        fname <- gsub("\\.(nii|nii\\.gz)$", "", basename(path), ignore.case = TRUE)
        sprintf("Generating surfaces | %s", fname)
      })
      invisible(volume_files)
    },

    #' @description Transform streamline tract files from template space to
    #' native brain space
    #' @param template_name which template to use; the template must have been
    #' used in \code{map_to_template} prior to calling this method
    #' @param streamlines_folder path to folder containing streamline tract
    #' files (\code{'.tck'}, \code{'.trk'}, \code{'.tt'}, or \code{'.vtk'});
    #' all matching files are processed recursively, except \verb{VTK} files
    #' (\code{'.vtk'}, \code{'.vtp'}), which \pkg{ieegio} can only read
    #' through 'Python': they are skipped with a warning
    #' @param verbose whether to print progress; default is \code{TRUE}
    #' @param format output format for the saved streamlines; \code{"auto"}
    #' (default) preserves the original file extension, except that \verb{TT}
    #' files are saved as \verb{TCK} (\pkg{ieegio} has no \verb{TT} writer);
    #' \code{"tck"} and \code{"trk"} convert every file. \code{"tt"} (no
    #' writer) and \code{"vtk"} (written through 'Python') are errors
    #' @param native_type which native image type defines the target space;
    #' default is \code{"T1w"}
    #' @returns Invisibly returns a character vector of the relative paths to
    #' the input streamline files that were processed (\verb{VTK} files are
    #' left out); transformed files are written under \code{fs/streamline/}
    #' from the working directory. The points of all files are transformed
    #' together; points with missing coordinates are dropped. 'Python' is
    #' never started: the files are read and written with the 'Python'
    #' support of \pkg{ieegio} disabled.
    generate_streamlines_from_template = function(
        template_name = rpyants_builtin_templates(),
        streamlines_folder = NULL, verbose = TRUE,
        format = c("auto", "tck", "trk", "tt", "vtk"),
        native_type = "T1w"
    ) {
      template_name <- match.arg(template_name)
      format <- match.arg(format)
      if (format %in% c("tt", "vtk")) {
        stop(sprintf(
          "The native engine cannot save streamlines as `%s` (%s). Please use format \"auto\", \"tck\" or \"trk\".",
          format,
          ifelse(identical(format, "tt"), "`ieegio` has no `TT` writer",
                 "`ieegio` writes `VTK` files through `Python`")
        ), call. = FALSE)
      }
      streamlines_folder <- path_abs(streamlines_folder, must_work = TRUE)

      streamline_files <- list.files(
        streamlines_folder,
        pattern = "\\.(tck|trk|trk\\.gz|tt|tt\\.gz|vtk|vtp)$",
        all.files = FALSE,
        full.names = FALSE,
        recursive = TRUE,
        ignore.case = TRUE,
        include.dirs = FALSE
      )
      # 'VTK' files can only be read through 'Python'
      vtk_files <- grepl("\\.(vtk|vtp)$", streamline_files, ignore.case = TRUE)
      if (any(vtk_files)) {
        warning(sprintf(
          "The native engine does not read `VTK` streamlines, which need `Python`; skipped: %s. Please convert them to `TCK` or `TRK`, or use `as_yael_process(engine = \"rpyANTs\")`.",
          paste(streamline_files[vtk_files], collapse = ", ")), call. = FALSE)
        streamline_files <- streamline_files[!vtk_files]
      }

      if (!length(streamline_files)) {
        return(invisible(streamline_files))
      }
      yael_native_require_ravetools()

      # 'ieegio' must not start (or install) 'Python' from here on
      no_python <- Sys.getenv("IEEGIO_NO_PYTHON", unset = NA)
      Sys.setenv(IEEGIO_NO_PYTHON = "1")
      on.exit({
        if (is.na(no_python)) {
          Sys.unsetenv("IEEGIO_NO_PYTHON")
        } else {
          Sys.setenv(IEEGIO_NO_PYTHON = no_python)
        }
      }, add = TRUE)

      save_root <- file_path(self$work_path, "fs", "streamline")

      # lines of every file
      items <- lapply(streamline_files, function(streamline_file) {
        streamline <- tryCatch({
          ieegio::as_ieegio_streamlines(file_path(streamlines_folder, streamline_file))
        }, error = function(e) {
          NULL
        })
        if (!inherits(streamline, "ieegio_streamlines") || !length(streamline)) {
          return(NULL)
        }
        lines <- lapply(streamline[], function(item) {
          coords <- as.matrix(item$coords)
          storage.mode(coords) <- "double"
          coords
        })
        list(file = streamline_file, lines = lines)
      })
      items <- items[!vapply(items, is.null, FALSE)]
      if (!length(items)) {
        return(invisible(streamline_files))
      }

      # one transform for all points: the warp files are large
      line_sizes <- unlist(lapply(items, function(item) {
        vapply(item$lines, nrow, 0L)
      }))
      coords <- do.call("rbind", lapply(items, function(item) {
        do.call("rbind", item$lines)
      }))
      yael_native_log(isTRUE(verbose), sprintf(
        "Transforming %d streamline points of %d file(s) from template [%s]",
        nrow(coords), length(items), template_name))
      coords <- self$transform_points_from_template(
        template_ras = coords, template_name = template_name,
        native_type = native_type, verbose = FALSE)
      line_ends <- cumsum(line_sizes)
      line_starts <- line_ends - line_sizes + 1

      line_index <- 0L
      for (item in items) {
        streamline_data <- lapply(seq_along(item$lines), function(ii) {
          idx <- line_index + ii
          if (line_sizes[[idx]] < 1) { return(NULL) }
          line <- coords[seq(line_starts[[idx]], line_ends[[idx]]), , drop = FALSE]
          line <- line[rowSums(is.na(line)) == 0, , drop = FALSE]
          if (!nrow(line)) { return(NULL) }
          line
        })
        line_index <- line_index + length(item$lines)
        streamline_data <- streamline_data[!vapply(streamline_data, is.null, FALSE)]
        if (!length(streamline_data)) { next }

        streamline <- ieegio::as_ieegio_streamlines(streamline_data)
        streamline_file <- item$file
        if (format == "auto") {
          # no 'TT' writer: such files are saved as 'TCK'
          streamline_file <- gsub("\\.tt(\\.gz)?$", ".tck", streamline_file,
                                  ignore.case = TRUE)
        } else {
          streamline_file <- gsub("\\.(tck|trk|trk\\.gz|tt|tt\\.gz|vtk|vtp)$",
                                  sprintf(".%s", format), streamline_file,
                                  ignore.case = TRUE)
        }
        save_path <- file_path(save_root, streamline_file)
        dir_create2(dirname(save_path))
        ieegio::write_streamlines(streamline, save_path, format = format)
      }

      invisible(streamline_files)
    },

    #' @description Transform points from native images to template
    #' @param native_ras matrix or data frame with 3 columns indicating points
    #' sitting on native images in right-anterior-superior (\code{'RAS'})
    #' coordinate system.
    #' @param template_name template to use for mapping
    #' @param native_type native image type where the points sit on
    #' @param verbose whether the print out the progress
    #' @returns A matrix of 3 columns, each row is a transformed points (
    #' invalid rows will be filled with \code{NA})
    transform_points_to_template = function(native_ras,
                                            template_name = rpyants_builtin_templates(),
                                            native_type = "T1w", verbose = TRUE) {
      template_name <- match.arg(template_name)
      private$transform_points(native_ras, template_name = template_name,
                               native_type = native_type, to_template = TRUE)
    },

    #' @description Transform points from template images to native
    #' @param template_ras matrix or data frame with 3 columns indicating points
    #' sitting on template images in right-anterior-superior (\code{'RAS'})
    #' coordinate system.
    #' @param template_name template to use for mapping
    #' @param native_type native image type where the points sit on
    #' @param verbose whether the print out the progress
    #' @returns A matrix of 3 columns, each row is a transformed points (
    #' invalid rows will be filled with \code{NA})
    transform_points_from_template = function(template_ras,
                                              template_name = rpyants_builtin_templates(),
                                              native_type = "T1w", verbose = TRUE
    ) {
      template_name <- match.arg(template_name)
      if (length(native_type) != 1) {
        native_type <- match.arg(native_type)
      }
      private$transform_points(template_ras, template_name = template_name,
                               native_type = native_type, to_template = FALSE)
    },

    #' @description
    #' Write the linear transform \code{'ants/mri/transforms/talairach.xfm'}
    #' from native \code{'T1w'} scanner \code{'RAS'} coordinates to
    #' \code{'MNI305'} space, using the \verb{affine} part of the normalization
    #' created by \code{map_to_template}. For templates that are not in
    #' \code{'MNI152'} space, the transform maps to the template space
    #' instead (\code{'fsaverage'} is already in \code{'MNI305'} space).
    #' This method is called by \code{construct_ants_folder_from_template}.
    #' @param template_name template that has been used in
    #' \code{map_to_template}
    #' @returns The path to the transform file, invisibly
    write_talairach_xfm = function(template_name = rpyants_builtin_templates()) {
      template_name <- match.arg(template_name)

      mapping <- self$get_template_mapping(template_name = template_name,
                                           native_type = "T1w",
                                           relative = FALSE)
      affine_paths <- ants_mapping_affine_paths(mapping)
      if (!length(affine_paths)) {
        stop("No affine (.mat) normalization from `T1w` to template `",
             template_name, "`. Please run `map_to_template()` first.")
      }

      # `native_to_template` transforms resample native images into the
      # template space, hence as point maps, they go from template to native;
      # `read_ants_transform` gives 'RAS' matrices, the 'ANTs' files hold 'LPS'
      lps_to_ras <- diag(c(-1, -1, 1, 1))
      affines <- lapply(affine_paths, function(path) {
        lps_to_ras %*% ravetools::read_ants_transform(path) %*% lps_to_ras
      })
      coord_sys <- call_rpyants("template_urls")[[template_name]]$coord_sys
      xfm <- ants_affine_to_talairach_xfm(affines, coord_sys = coord_sys)

      transform_dir <- dir_create2(file.path(self$work_path, "ants", "mri", "transforms"))
      xfm_path <- file.path(transform_dir, "talairach.xfm")
      writeLines(
        con = xfm_path,
        sep = "\n",
        c(
          "MNI Transform File",
          paste0("% Generated by ravecore YAELNativeProcess$write_talairach_xfm (template: ",
                 template_name, ")"),
          "",
          "Transform_Type = Linear;",
          "Linear_Transform = ",
          paste(sprintf("%.6f", xfm[1, ]), collapse = " "),
          paste(sprintf("%.6f", xfm[2, ]), collapse = " "),
          sprintf("%s;", paste(sprintf("%.6f", xfm[3, ]), collapse = " "))
        )
      )
      invisible(xfm_path)
    },

    #' @description
    #' Create a reconstruction folder (as an alternative option) that
    #' is generated from template brain to facilitate the three-dimensional
    #' viewer. Please make sure method \code{map_to_template} is called before
    #' using this method (or the program will fail). The folder holds the
    #' conformed \code{'mri/T1.mgz'}, the template brain mask mapped to the
    #' native image (\code{'mri/brainmask.nii.gz'}, binary), the bias-corrected
    #' image (\code{'mri/nu.nii.gz'}), its skull-stripped version scaled to
    #' 0 to 255 (\code{'mri/brain.finalsurfs.nii.gz'}), and the linear
    #' transform \code{'mri/transforms/talairach.xfm'} written by
    #' \code{write_talairach_xfm}; if that fails, the file is removed and a
    #' warning is logged.
    #' @param template_name template to use for mapping; \code{NULL} only
    #' writes \code{'mri/T1.mgz'}
    #' @param add_surfaces whether to create surfaces that is morphed from
    #' template to local; default is \code{TRUE}. The surfaces are only
    #' created for templates with \code{'fs/surf'} \verb{pial} surfaces. Please enable
    #' this option only if the cortical surfaces are not critical (for example,
    #' you are studying the deep brain structures). Always use
    #' \code{'FreeSurfer'} if cortical information is used.
    construct_ants_folder_from_template = function(template_name = rpyants_builtin_templates(),
                                                   add_surfaces = TRUE) {
      ants_dir <- file.path(self$work_path, "ants")
      mri_dir <- file.path(ants_dir, "mri")
      surf_dir <- file.path(ants_dir, "surf")

      mr_path <- self$get_input_image("T1w")
      if (length(mr_path) != 1 || is.na(mr_path) || !file.exists(mr_path)) {
        stop("T1w image is not set. Please set the input image via\n\tself$set_input_image('/path/to/T1-weighted/MRI.nii.gz', type = 'T1w')")
      }
      if (length(template_name)) {
        # the brain mask and the bias correction below need it
        yael_native_require_ravetools()
      }

      dir_create2(mri_dir)

      # T1.mgz (freesurfer style so users don't need to have to re-localize)
      threeBrain::conform_volume(mr_path, save_to = file.path(mri_dir, "T1.mgz"))

      if (!length(template_name)) {
        return(invisible())
      }
      template_name <- match.arg(template_name)
      template <- yael_native_template(template_name)

      # brainmask: template mask on the native image, binarized
      mask <- private$transform_from_template(
        path = template$mask, template_name = template_name,
        native_type = "T1w", interpolator = "nearestNeighbor", verbose = FALSE)
      vox2ras <- attr(mask, "vox2ras")
      mask <- (mask > 0) * 1
      yael_native_write_volume(mask, file.path(mri_dir, "brainmask.nii.gz"),
                               vox2ras = vox2ras)

      # intensity normalization (`abp_n4` with the mask)
      t1 <- yael_native_read_volume(mr_path)
      nu <- ravetools::bias_correction_n4(
        t1, vox2ras = attr(t1, "vox2ras"), mask = mask,
        intensity_truncation = c(0.025, 0.975, 256))
      rm(t1)
      yael_native_write_volume(nu, file.path(mri_dir, "nu.nii.gz"), vox2ras = vox2ras)

      # skull-strip, normalized to 0-255
      skullstrip <- nu * mask
      rm(nu)
      skullstrip[skullstrip < 0] <- 0
      max_intensity <- max(skullstrip, na.rm = TRUE)
      if (is.finite(max_intensity) && max_intensity > 0) {
        skullstrip <- floor(skullstrip / max_intensity * 255)
      }
      skullstrip[!is.finite(skullstrip)] <- 0
      yael_native_write_volume(skullstrip, file.path(mri_dir, "brain.finalsurfs.nii.gz"),
                               vox2ras = vox2ras)
      rm(skullstrip, mask)

      # write down the linear transform (native scanner RAS -> MNI305)
      tryCatch({
        self$write_talairach_xfm(template_name = template_name)
      }, error = function(e) {
        # never leave a stale transform behind
        xfm_path <- file.path(mri_dir, "transforms", "talairach.xfm")
        if (file.exists(xfm_path)) { unlink(xfm_path) }
        ravepipeline::logger(
          level = "warning", use_glue = FALSE,
          "Unable to write `talairach.xfm` for template ", template_name, ": ",
          conditionMessage(e)
        )
      })

      # surfaces
      lh_pial_dst <- file.path(surf_dir, "lh.pial.T1")
      rh_pial_dst <- file.path(surf_dir, "rh.pial.T1")
      if ( add_surfaces ) {
        private$morph_template_pial(template = template, template_name = template_name,
                                    ants_dir = ants_dir, surf_dir = surf_dir)

        writeLines(con = file.path(ants_dir, "README.txt"), text = c(
          sprintf("Generated by %s at %s.", paste(Sys.info()[["user"]], collapse = ""), strftime(Sys.time())),
          sprintf("This folder is created by morphing template `%s` with native T1w MRI via the native (ravetools) SyN algorithm.", template_name),
          "The surfaces are just coarsed estimation for visualization purposes, and **may not be accurate**.",
          "Please use `FreeSurfer` for better cortical surface reconstruction."
        ))
      } else {
        if (file.exists(lh_pial_dst)) { unlink(lh_pial_dst) }
        if (file.exists(rh_pial_dst)) { unlink(rh_pial_dst) }
      }
      return(invisible())
    },

    #' @description Get three-dimensional brain model
    #' @param electrodes whether to add electrodes to the viewers; can be
    #' logical, data frame, or a character (path to electrode table). When
    #' the value is \code{TRUE}, the electrode file under \code{project_name}
    #' will be loaded; when \code{electrodes} is a \code{\link{data.frame}},
    #' or path to a \code{'csv'} file, then please specify \code{coord_sys}
    #' on what is the coordinate system used for columns \code{"x"}, \code{"y"},
    #' and \code{"z"}.
    #' @param coord_sys coordinate system if \code{electrodes} is a data frame
    #' with columns \code{"x"}, \code{"y"}, and \code{"z"}, available choices
    #' are \code{'scannerRAS'} (defined by 'T1' weighted native 'MRI' image),
    #' \code{'tkrRAS'} (\code{'FreeSurfer'} defined native 'TK-registered'),
    #' \code{'MNI152'} (template \verb{MNI} coordinate system averaged over 152
    #' subjects; this is the common \verb{MNI} coordinate space we often refer to),
    #' and \code{'MNI305'} (template \verb{MNI} coordinate system averaged over 305
    #' subjects; this coordinate system used by templates such as
    #' \code{'fsaverage'})
    #' @param ... passed to \code{\link[threeBrain]{threeBrain}}
    get_brain = function(electrodes = TRUE,
                         coord_sys = c("scannerRAS", "tkrRAS", "MNI152", "MNI305"), ...) {
      coord_sys <- match.arg(coord_sys)
      subject <- self$get_subject(strict = FALSE)
      if (is.na(subject$freesurfer_path)) {
        return(NULL)
      }
      brain <- threeBrain::threeBrain(path = subject$freesurfer_path, subject_code = self$subject_code, ...)
      if (isTRUE(electrodes)) {
        tryCatch({
          brain$set_electrodes(electrodes = subject$get_electrode_table())
        }, error = function(e) {})
      } else if (is.data.frame(electrodes) || is.character(electrodes)) {
        brain$set_electrodes(electrodes = electrodes, coord_sys = coord_sys)
      }
      brain
    }

  ),
  active = list(
    #' @field subject_code 'RAVE' subject code
    subject_code = function() {
      private$.subject_code
    },

    #' @field image_types allowed image types
    image_types = function(v) {
      if (!missing(v) && length(v)) {
        image_types <- c(private$.image_types, as.character(unlist(v)))
        image_types <- unname(unlist(image_types))
        private$.image_types <- image_types
      }
      private$.image_types
    },

    #' @field work_path Working directory ('RAVE' imaging path)
    work_path = function() {
      private$.work_path
    }
  )
)

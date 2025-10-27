#' @title Get names of available non-linear transforms to the templates
#' @description
#' This function obtains existing transforms rather than creating new
#' transforms. Please check function \code{\link{cmd_run_yael_preprocess}}
#' about mapping native images to the templates. This function requires
#' additional 'Python' configuration.
#' @param subject 'RAVE' subject instance or character; see
#' \code{\link{as_rave_subject}}
#' @returns Template names with valid non-linear transforms.
#' @examples
#'
#' # Please check out https://rave.wiki to configure Python for RAVE
#' # or run ravemanager::configure_python()
#' \dontrun{
#'
#' get_available_morph_to_template("project_name/subject_code")
#'
#' }
#'
#'
#' @export
get_available_morph_to_template <- function(subject) {
  subject <- restore_subject_instance(subject)
  job_id <- ravepipeline::start_job(
    fun = function(subject) {
      ravecore <- asNamespace("ravecore")
      yael_process <- ravecore$as_yael_process(subject)
      template_names <- ravecore$rpyants_builtin_templates()
      missing_template <- vapply(template_names, function(template_name) {
        is.null(yael_process$get_template_mapping(template_name = template_name))
      }, FALSE)
      template_names[!missing_template]
    },
    fun_args = list(subject = subject),
    ensure_init = TRUE
  )
  ravepipeline::resolve_job(job_id, timeout = 90)
}

#' @title Create brain atlases from template
#' @description
#' Reverse-transform from template atlases via volume mapping
#' @param subject 'RAVE' subject instance or character; see
#' \code{\link{as_rave_subject}}
#' @param atlas_folders paths to the atlas folders
#' @param template_name template name where the atlases are created;
#' see \code{\link{get_available_morph_to_template}} to get available templates
#' and \code{\link{cmd_run_yael_preprocess}} to generate transforms to the
#' templates
#' @param any_mni152 if the template is 'MNI152', then whether to check other
#' template folders; default is true
#' @param surfaces whether to generate surfaces; if true, then a 'GIfTI' file
#' and a 'STL' file will be created for each atlas, if applicable. The
#' 'GIfTI' file will be in right-anterior-superior 'RAS' coordinate system to
#' comply with the 'NIfTI' standard, and 'STL' will be in
#' left-posterior-superior 'LPS' system that can be imported to 'ITK'-based
#' software such as 'BrainLab' or 'ANTs'.
#' @param as_job whether to run as a background job for cleaner environment;
#' default is false.
#' @param extra_transform additional 'affine' transform (a four-by-four matrix)
#' that maps the subject native image (not points) to the target image. This
#' argument is for pipelines that take in transformed/processed images as input;
#' default is \code{NULL}, see 'Details'.
#' @param extra_transform_type type of transform if \code{extra_transform} is
#' provided.
#'
#' @returns The function returns nothing, but will create a folder named
#' \code{'atlases'} under raw subject \code{'rave-imaging'} folder.
#'
#' @details
#' The workflow generates atlases from a template. To run this function, a
#' separate normalization is required (see
#' \code{\link[ravecore]{cmd_run_yael_preprocess}}). That function normalizes
#' native subject's brain (\code{'T1w'} image) to
#' a specified template (typically 'MNI152b' non-linear asymmetric version) via
#' a deformation field. Once obtaining the normalization transform,
#' \code{generate_atlases_from_template} inverse the process, creating
#' subject-level atlases based on that template.
#'
#' When \code{surfaces} is \code{TRUE}, each atlas or mask file will be binned
#' with a threshold of \code{0.5}. The resulting binary mask will be used
#' to generate a mask surface using \code{\link[threeBrain]{volume_to_surf}}
#' function via implicit Laplacian smoothing.
#'
#' The resulting surfaces and atlases typically sit at native space aligned
#' with input \code{'T1w'} image. For each volume, a 'GIfTI' surface will be
#' created under right-anterior-superior ('RAS') coordinate system for
#' further inferences. In addition, an 'STL' surface will be created under
#' left-posterior-superior ('LPS') coordinate system. This file is mainly to
#' be used as object file for visualizations in software such as 'BrainLab'.
#'
#' Some users might have extra processing before
#' 'YAEL' pipeline (such as 'ACPC' realignment). This function provides a set of
#' extra arguments (\code{extra_transform} and \code{extra_transform_type})
#' allowing generating atlases to align with the raw 'DICOM' images.
#' \code{extra_transform} needs to be an \code{'affine'} matrix that maps
#' the image from the input of the 'YAEL' pipeline to the raw 'DICOM' image.
#' Be aware that this transform is not the point transformation matrix.
#' When \code{extra_transform} is non-empty, an extra folder
#' \code{atlases_extra} will be created under the subject imaging folder,
#' with transformed files aligned to 'DICOM' stored.
#'
#' @examples
#'
#' # Please check out https://rave.wiki to configure Python for RAVE
#' # or run ravemanager::configure_python()
#' \dontrun{
#'
#' generate_atlases_from_template(
#'   "YAEL/OCD07", "/path/to/OCD_atlases")
#'
#' # If the image was ACPC realigned before being fed into the YAEL
#' # pipeline
#' generate_atlases_from_template(
#'   "YAEL/OCD07", "/path/to/OCD_atlases",
#'   extra_transform =
#'     "/path/to/sub-OCD7_from-T1wACPC_to-T1wNative_mode-image_xfm.mat"
#' )
#'
#' }
#'
#'
#' @export
generate_atlases_from_template <- function(
    subject, atlas_folders,
    template_name = 'mni_icbm152_nlin_asym_09b', any_mni152 = TRUE,
    surfaces = TRUE, as_job = FALSE,
    # extra transform from the input to whatever
    extra_transform = NULL, extra_transform_type = c("ants", "native")
) {

  subject <- restore_subject_instance(subject)
  extra_transform_type <- match.arg(extra_transform_type)

  available_mappings <- get_available_morph_to_template(subject)
  if(!length(available_mappings)) {
    stop("No non-linear morph available. Please run `ravecore::cmd_run_yael_preprocess` first.")
  }
  template_name <- template_name[[1]]
  template_name0 <- template_name
  template_name <- template_name[template_name %in% available_mappings]
  if(
    any_mni152 &&
    identical(template_name0, 'mni_icbm152_nlin_asym_09b') &&
    !length(template_name)
  ) {
    template_name <- c('mni_icbm152_nlin_asym_09a', 'mni_icbm152_nlin_asym_09c')
    template_name <- template_name[template_name %in% available_mappings]
  }
  if(!length(template_name)) {
    stop("The requested template ", sQuote(template_name0),
         " does not exist for subject ", subject$subject_id,
         ". Available templates are: ",
         paste(sQuote(available_mappings), collapse = ", "))
  }

  # order is a first, then c, then b to not worry about overwrite
  mni152_templates <- c('mni_icbm152_nlin_asym_09a', 'mni_icbm152_nlin_asym_09c',
                        'mni_icbm152_nlin_asym_09b')

  if(missing(atlas_folders) || !length(atlas_folders)) {
    atlas_root <- ravepipeline::raveio_getopt(
      key = "mni_template_root",
      default = file.path(threeBrain::default_template_directory(), "templates")
    )
    if( any_mni152 && template_name %in% mni152_templates ) {
      atlas_folders <- path_abs(file_path(atlas_root, mni152_templates, "atlases"),
                                must_work = FALSE)
    } else {
      atlas_folders <- path_abs(file_path(atlas_root, template_name, "atlases"),
                                must_work = FALSE)
    }
  } else {
    atlas_folders <- path_abs(atlas_folders, must_work = FALSE)
  }
  if(!any(dir_exists(atlas_folders))) {
    stop("The atlas paths are missing. Please make create a directory along one of the following paths and copy-paste template atlases in it. The missing directories are:\n",
         paste0("\n  ", atlas_folders, "\n", collapse = ""))
  }

  atlas_folders <- atlas_folders[dir_exists(atlas_folders)]

  if(length(extra_transform)) {
    if(is.character(extra_transform)) {
      extra_transform <- normalizePath(extra_transform, mustWork = TRUE)
    } else {
      extra_transform <- as.matrix(extra_transform[drop = FALSE])
      if(!identical(as.integer(dim(extra_transform)), c(4L, 4L))) {
        stop("`extra_transform` must be a file path to the affine transform or a 4x4 matrix")
      }
    }
  } else {
    extra_transform <- NULL
  }

  job_fun <- function(subject, template_name, atlas_folders, surfaces, extra_transform, extra_transform_type) {
    ravecore <- asNamespace("ravecore")
    yael_process <- ravecore$as_yael_process(subject)

    if(length(extra_transform) && is.character(extra_transform)) {
      switch (
        extra_transform_type,
        "ants" = {
          # LPS to LPS
          extra_transform <- rpyANTs::as_ANTsTransform(extra_transform)[]
          # RAS to RAS
          extra_transform <- diag(c(-1, -1, 1, 1)) %*% extra_transform %*% diag(c(-1, -1, 1, 1))
        },
        {
          # native, assuming extra_transform is a 4x4 RAS to RAS matrix
        }
      )
      image_transform <- extra_transform
      point_transform <- solve(image_transform)
    } else {
      image_transform_ants <- NULL
      point_transform <- NULL
    }

    atlas_root <- ravecore$path_abs(file.path(subject$imaging_path, "atlases"), must_work = FALSE)
    atlas_extra <- ravecore$path_abs(file.path(subject$imaging_path, "atlases_extra"), must_work = FALSE)

    lapply(atlas_folders, function(atlas_folder) {

      atlas_paths <- yael_process$generate_atlas_from_template(
        template_name = template_name,
        atlas_folder = atlas_folder,
        verbose = TRUE,
        surfaces = surfaces
      )

      if(surfaces && length(atlas_paths) > 0) {
        lapply(atlas_paths, function(atlas_path) {
          gii_path <- gsub("\\.(nii|nii\\.gz)$", ".gii", x = atlas_path, ignore.case = TRUE)
          stl_path <- gsub("\\.(nii|nii\\.gz)$", ".stl", x = atlas_path, ignore.case = TRUE)
          if(file.exists(gii_path)) {
            tryCatch({
              surf <- ieegio::read_surface(gii_path)
              lps <- diag(c(-1, -1, 1, 1)) %*% surf$geometry$transforms[[1]] %*% surf$geometry$vertices
              surf <- ieegio::as_ieegio_surface(
                t(lps),
                faces = t(surf$geometry$faces),
                face_start = surf$geometry$face_start
              )
              ieegio::write_surface(surf, con = stl_path, type = "geometry", format = 'freesurfer')
            })
          }

          if(is.matrix(point_transform)) {
            relpath <- ravecore$path_rel(atlas_path, start = atlas_root)
            extrapath_nii <- file.path(atlas_extra, relpath)
            ravecore$dir_create2(dirname(extrapath_nii))

            # transform volume
            volume <- ieegio::read_volume(atlas_path)
            volume <- ieegio::as_ieegio_volume(volume[], vox2ras = point_transform %*% volume$transforms[[1]])
            ieegio::write_volume(volume, con = extrapath_nii)

            # transform surface
            if(file.exists(gii_path)) {
              extrapath_gii <- file.path(atlas_extra, ravecore$path_rel(gii_path, start = atlas_root))
              surf <- ieegio::read_surface(gii_path)
              ras_pos <- point_transform %*% surf$geometry$transforms[[1]] %*% surf$geometry$vertices

              surf_gii <- ieegio::as_ieegio_surface(
                t(ras_pos),
                faces = t(surf$geometry$faces),
                face_start = surf$geometry$face_start
              )
              ieegio::write_surface(surf_gii, con = extrapath_gii, type = "geometry", format = 'gifti')

              # STL
              extrapath_stl <- gsub("\\.gii$", ".stl", x = extrapath_gii, ignore.case = TRUE)
              surf_stl <- ieegio::as_ieegio_surface(
                t(diag(c(-1, -1, 1, 1)) %*% ras_pos),
                faces = t(surf$geometry$faces),
                face_start = surf$geometry$face_start
              )
              ieegio::write_surface(surf_stl, con = extrapath_stl, type = "geometry", format = 'freesurfer')
            }

          }


          invisible()
        })

      }

      return()

    })
  }


  if( as_job ) {
    job_id <- ravepipeline::start_job(
      fun = job_fun,
      method = "rs_job",
      fun_args = list(
        subject = subject,
        template_name = template_name,
        atlas_folders = atlas_folders,
        surfaces = surfaces,
        extra_transform = extra_transform,
        extra_transform_type = extra_transform_type
      ),
      packages = "ravecore",
      ensure_init = TRUE,
      name = sprintf("Atlas: %s -> %s", template_name, subject$subject_id)
    )
    ravepipeline::resolve_job(job_id)
  } else {
    job_fun(
      subject = subject,
      template_name = template_name,
      atlas_folders = atlas_folders,
      surfaces = surfaces,
      extra_transform = extra_transform,
      extra_transform_type = extra_transform_type
    )
  }

  return(invisible())
}


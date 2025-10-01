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
#' @returns The function returns nothing, but will create a folder named
#' \code{'atlases'} under raw subject \code{'rave-imaging'} folder.
#'
#' @examples
#'
#' # Please check out https://rave.wiki to configure Python for RAVE
#' # or run ravemanager::configure_python()
#' \dontrun{
#'
#' generate_atlases_from_template("YAEL/yael_demo_001")
#'
#' }
#'
#'
#' @export
generate_atlases_from_template <- function(
    subject, atlas_folders,
    template_name = 'mni_icbm152_nlin_asym_09b', any_mni152 = TRUE) {

  subject <- restore_subject_instance(subject)

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

  job_id <- ravepipeline::start_job(
    fun = function(subject, template_name, atlas_folders) {
      ravecore <- asNamespace("ravecore")
      yael_process <- ravecore$as_yael_process(subject)

      lapply(atlas_folders, function(atlas_folder) {

        yael_process$generate_atlas_from_template(
          template_name = template_name,
          atlas_folder = atlas_folder,
          verbose = TRUE,
          surfaces = TRUE
        )

        return()

      })
    },
    method = "rs_job",
    fun_args = list(
      subject = subject,
      template_name = template_name,
      atlas_folders = atlas_folders
    ),
    packages = "ravecore",
    ensure_init = TRUE,
    name = sprintf("Atlas: %s -> %s", template_name, subject$subject_id)
  )

  ravepipeline::resolve_job(job_id)
  return(invisible())
}



#' @name cmd_run_yael_preprocess
#' @title Run a built-in 'YAEL' imaging processing workflow
#' @description
#' Image processing pipeline \doi{10.1523/ENEURO.0328-23.2023},
#' allowing cross-modality image registration, T1-weighted MRI normalization
#' to template brain, creating subject-level brain atlas from inverse
#' normalization.
#'
#' @param subject subject ID
#' @param t1w_path path to 'T1'-weighted preoperative 'MRI', used as underlay
#' and base image. If you want to have 'ACPC' aligned scanner coordinate
#' system. Please align the image before feeding into this function. All images
#' must contain skulls (do not strip skulls)
#' @param ct_path,t2w_path,fgatir_path,preopct_path,flair_path,t1w_contrast_path
#' additional optional images to be aligned to the underlay; the registration
#' will be symmetric and the rigid-body transforms will be stored.
#' @param register_policy whether to skip already registered images;
#' default is true (\code{'auto'}); set to \code{'all'} to ignore existing
#' registrations and force calculation
#' @param register_reversed whether to swap the moving images and the
#' fixing image; default is false
#' @param normalize_template template to normalize to: default is
#' \code{'mni_icbm152_nlin_asym_09b'} ('MNI152b', 0.5 mm resolution); when
#' the computer memory is below 12 gigabytes, the template will automatically
#' switch to \code{'mni_icbm152_nlin_asym_09a'} (known as 'MNI152a', 1 mm
#' voxel resolution). Other choices are \code{'mni_icbm152_nlin_asym_09c'}
#' and \code{'fsaverage'} (or known as 'MNI305')
#' @param normalize_policy whether to skip existing normalization, if
#' calculated; default is \code{'auto'} (yes); set to \code{'all'} to ignore
#' @param normalize_images images used for normalization; default is to
#' include common images before the implantation (if available)
#' @param normalize_back length of one (select from \code{normalize_template}),
#' which template is to be used to generate native brain mask and transform
#' matrices
#' @param atlases a named list: the names must be template names from
#' \code{normalize_template} and the values must be directories of atlases of
#' the corresponding templates (see 'Examples').
#' @param add_surfaces whether to add surfaces for the subject; default is
#' \code{FALSE}. The surfaces are created by reversing the normalization from
#' template brain, hence the results will not be accurate. Enable this option
#' only if cortical surface estimation is not critical (and 'FreeSurfer'
#' reconstructions are inaccessible)
#' @param use_antspynet whether to try \code{'antspynet'} if available;
#' default is true, which uses \code{deep_atropos} instead of the
#' conventional \code{atropos} to speed up and possibly with more accurate
#' results.
#' @param verbose whether to print out the information; default is \code{TRUE}
#' @param run_recon_all whether to run 'FreeSurfer'; default is true
#' @param dry_run whether to dry-run
#' @param ... reserved for legacy code and deprecated arguments
#' @returns Nothing, a subject imaging folder will be created under 'RAVE'
#' raw folder. It will take a while to run the workflow.
#' @examples
#'
#' \dontrun{
#'
#' # For T1 normalization only; add ct_path to include coregistration
#' cmd_run_yael_preprocess(
#'   subject = "pt01",
#'   t1w_path = "/path/to/T1w.nii.gz",
#'
#'   # normalize T1 to MNI152
#'   normalize_template = 'mni_icbm152_nlin_asym_09b'
#' )
#'
#'
#' }
NULL


#' @rdname cmd_run_yael_preprocess
#' @export
yael_preprocess <- function(
    subject, t1w_path = NULL, ct_path = NULL,
    t2w_path = NULL, fgatir_path = NULL, preopct_path = NULL,
    flair_path = NULL, t1w_contrast_path = NULL,
    register_policy = c("auto", "all"), register_reversed = FALSE,
    normalize_template = "mni_icbm152_nlin_asym_09b",
    normalize_policy = c("auto", "all"),
    normalize_images = c("T1w", "T2w", "T1wContrast", "fGATIR", "preopCT"),
    normalize_back = ifelse(length(normalize_template) >= 1, normalize_template[[1]], NA),
    atlases = list(),
    add_surfaces = FALSE,
    use_antspynet = TRUE,
    verbose = TRUE, ...
) {
  if(missing(subject)) {
    subject <- list(...)$subject_code
    if(!length(subject)) {
      stop("yael_preprocess: `subject` is missing. Please provide a RAVE subject ID")
    }
    ravepipeline::logger(
      level = "warning",
      "yael_preprocess: initializing with argument `subject_code` is no longer ",
      "appropriate. Use argument `subject` instead"
    )
  }

  register_policy <- match.arg(register_policy)
  normalize_policy <- match.arg(normalize_policy)

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

  for(template_name in normalize_template) {
    rpyANTs::ensure_template(template_name)
    if(!length(atlases[[template_name]])) {

      atlas_folder <- mni_template_path(template_name, "atlases")
      if(dir.exists(atlas_folder)) {
        atlases[[template_name]] <- path_abs(atlas_folder)
      }
    }
  }

  # DIPSAUS DEBUG START
  # subject_code = "testtest2"
  # t1w_path = "/Users/dipterix/rave_data/raw_dir/DBS_93/rave-imaging/coregistration/MRI_reference.nii.gz"
  # ct_path = "/Users/dipterix/rave_data/raw_dir/DBS_93/rave-imaging/coregistration/CT_RAW.nii.gz"
  # preopct_path = NULL
  # verbose <- TRUE
  # register_policy = "auto"
  # normalize_policy = "auto"
  # register_reversed = FALSE
  # yael_process <- YAELProcess$new(subject_code = subject_code)
  # normalize_template = c("mni_icbm152_nlin_asym_09a")
  # normalize_back <- normalize_template[[1]]
  # add_surfaces <- TRUE

  yael_process <- as_yael_process(subject = subject)
  subject <- yael_process$get_subject()
  project_name <- subject$project_name
  subject_code <- subject$subject_code

  if(length(t1w_path) && !is.na(t1w_path) && nzchar(t1w_path)) {
    ravepipeline::logger("Migrating T1w image: ", t1w_path, level = "trace")
    yael_process$set_input_image(path = t1w_path, type = "T1w", overwrite = TRUE)
  } else {
    t1w_path <- yael_process$get_input_image("T1w")
    stopifnot2(
      length(t1w_path) == 1 && file.exists(t1w_path),
      msg = "Please specify T1w image path"
    )
  }

  if(length(ct_path) && !is.na(ct_path) && nzchar(ct_path)) {
    ravepipeline::logger(level = "trace", "Migrating CT image: ", ct_path)
    yael_process$set_input_image(path = ct_path, type = "CT", overwrite = TRUE)
  }

  if(length(t2w_path) && !is.na(t2w_path) && nzchar(t2w_path)) {
    ravepipeline::logger(level = "trace", "Migrating T2w image: ", t2w_path)
    yael_process$set_input_image(path = t2w_path, type = "T2w", overwrite = TRUE)
  }

  if(length(fgatir_path) && !is.na(fgatir_path) && nzchar(fgatir_path)) {
    ravepipeline::logger(level = "trace", "Migrating fGATIR image: ", fgatir_path)
    yael_process$set_input_image(path = fgatir_path, type = "fGATIR", overwrite = TRUE)
  }

  if(length(preopct_path) && !is.na(preopct_path) && nzchar(preopct_path)) {
    ravepipeline::logger(level = "trace", "Migrating preop-CT image: ", preopct_path)
    yael_process$set_input_image(path = preopct_path, type = "preopCT", overwrite = TRUE)
  }

  if(length(flair_path) && !is.na(flair_path) && nzchar(flair_path)) {
    ravepipeline::logger(level = "trace", "Migrating FLAIR image: ", flair_path)
    yael_process$set_input_image(path = flair_path, type = "FLAIR", overwrite = TRUE)
  }

  if(length(t1w_contrast_path) && !is.na(t1w_contrast_path) && nzchar(t1w_contrast_path)) {
    ravepipeline::logger(level = "trace", "Migrating T1w with contrast image: ", t1w_contrast_path)
    yael_process$set_input_image(path = t1w_contrast_path, type = "T1wContrast", overwrite = TRUE)
  }

  # Coregistration
  image_types <- unique(yael_process$image_types)
  image_types <- image_types[!tolower(image_types) %in% "t1w"]
  lapply(image_types, function(native_type) {
    impath <- yael_process$get_input_image(native_type)
    if(!length(impath)) { return() }
    ravepipeline::logger(level = "info", "Co-registering [", native_type, "] image with [T1w] image.")
    suppressWarnings({
      pexists <- tryCatch({
        conf <- yael_process$get_native_mapping(image_type = native_type)
        length(conf$mappings) > 0
      }, error = function(e){ FALSE })
    })
    if(!pexists || register_policy == "all") {
      yael_process$register_to_T1w(image_type = native_type,
                                   reverse = register_reversed,
                                   verbose = verbose)
    }
    return()
  })

  # Normalization
  if( length(normalize_back) < 1 || is.na(normalize_back[[1]]) ) {
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
      }, error = function(e){ FALSE })
    })
    if(!pexists || normalize_policy == "all") {
      yael_process$map_to_template(template_name = template_name,
                                   native_type = "T1w",
                                   use_images = normalize_images,
                                   use_antspynet = use_antspynet,
                                   verbose = verbose)
    }
    if( template_name %in% normalize_back ) {
      # Generate ANTs folder
      yael_process$construct_ants_folder_from_template(
        template_name = normalize_back,
        add_surfaces = add_surfaces
      )
    }
  })

  # Make sure the Norig and Torig transforms are set to conformed image
  t1_mgz <- file.path(yael_process$work_path, "ants", "T1.mgz")
  if(!file.exists(t1_mgz)) {
    yael_process$construct_ants_folder_from_template(
      template_name = NULL,
      add_surfaces = FALSE
    )
  }

  for(template_name in names(atlases)) {
    if(template_name != "") {
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

}


#' @rdname cmd_run_yael_preprocess
#' @export
cmd_run_yael_preprocess <- function(
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
    run_recon_all = TRUE,
    dry_run = FALSE,
    use_antspynet = TRUE,
    verbose = TRUE, ...) {
  # DIPSAUS DEBUG START
  # subject_code = "testtest3"
  # t1w_path = "/Users/dipterix/rave_data/raw_dir/DBS_93/rave-imaging/coregistration/MRI_reference.nii.gz"
  # ct_path = "/Users/dipterix/rave_data/raw_dir/DBS_93/rave-imaging/coregistration/CT_RAW.nii.gz"
  # t2w_path = NULL
  # fgatir_path = NULL
  # preopct_path = NULL
  # verbose <- TRUE
  # register_reversed = FALSE
  # flair_path=NULL;t1w_contrast_path=NULL
  # run_recon_all <- FALSE
  # normalize_images = c("T1w", "T2w", "T1wContrast", "fGATIR", "preopCT")
  # normalize_template = "mni_icbm152_nlin_asym_09b"
  # use_antspynet <- TRUE

  use_antspynet <- isTRUE(use_antspynet)

  if(missing(subject)) {
    subject <- list(...)$subject_code
    if(!length(subject)) {
      stop("cmd_run_yael_preprocess: `subject` is missing. Please provide a RAVE subject ID")
    }
    ravepipeline::logger(
      level = "warning",
      "cmd_run_yael_preprocess: initializing with argument `subject_code` is no longer ",
      "appropriate. Use argument `subject` instead"
    )
  }

  run_recon_all <- as.integer(isTRUE(as.logical(run_recon_all)))
  register_reversed <- isTRUE(as.logical(register_reversed))

  yael_process <- as_yael_process(subject = subject)
  subject <- yael_process$get_subject()
  project_name <- subject$project_name
  subject_code <- subject$subject_code

  if(length(t1w_path)) {
    t1w_path <- normalizePath(t1w_path, winslash = "/", mustWork = TRUE)
  } else {
    if(!length(yael_process$get_input_image("T1w"))) {
      stop("`cmd_run_yael_preprocess`: No `T1w` MRI specified. Please specify a valid file")
    }
    t1w_path <- ""
  }
  if(length(ct_path)) { ct_path <- normalizePath(ct_path, winslash = "/", mustWork = TRUE) } else { ct_path <- "" }
  if(length(t2w_path)) { t2w_path <- normalizePath(t2w_path, winslash = "/", mustWork = TRUE) } else { t2w_path <- "" }
  if(length(fgatir_path)) { fgatir_path <- normalizePath(fgatir_path, winslash = "/", mustWork = TRUE) } else { fgatir_path <- "" }
  if(length(preopct_path)) { preopct_path <- normalizePath(preopct_path, winslash = "/", mustWork = TRUE) } else { preopct_path <- "" }
  if(length(flair_path)) { flair_path <- normalizePath(flair_path, winslash = "/", mustWork = TRUE) } else { flair_path <- "" }
  if(length(t1w_contrast_path)) { t1w_contrast_path <- normalizePath(t1w_contrast_path, winslash = "/", mustWork = TRUE) } else { t1w_contrast_path <- "" }

  if(length(normalize_template)) {
    if("mni_icbm152_nlin_asym_09b" %in% normalize_template) {
      normalize_template <- unique(c("mni_icbm152_nlin_asym_09b", normalize_template))
    }
    lapply(normalize_template, rpyANTs::ensure_template)
    normalize_template_str <- deparse1(normalize_template)
  } else {
    normalize_template_str <- "NULL"
  }

  normalize_images <- unname(unique(c("T1w", unlist(normalize_images))))
  normalize_images_str <- deparse1(normalize_images)

  # default_fs_path <- cmd_freesurfer_home(error_on_missing = FALSE)
  # freesurfer_home <- tryCatch({
  #   freesurfer <- normalize_commandline_path(
  #     path = command_path,
  #     unset = default_fs_path,
  #     type = "freesurfer"
  #   )
  #   if(length(freesurfer) != 1 || is.na(freesurfer) || !isTRUE(dir.exists(freesurfer))) {
  #     freesurfer <- NULL
  #   } else if(!identical(default_fs_path, freesurfer)) {
  #     raveio_setopt("freesurfer_path", freesurfer)
  #   }
  #   freesurfer
  # }, error = function(e){ NULL })
  #
  # has_freesurfer <- !is.null(freesurfer_home)
  # if(has_freesurfer) {
  #   freesurfer_home <- normalizePath(freesurfer_home, winslash = "/")
  # }
  # cmd_recon <- "recon-all"

  log_path <- normalizePath(
    file.path(subject$imaging_path, "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-yael-preprocess-%y%m%d-%H%M%S.log")

  # Always use a temporary working path since the target directory might contain spaces
  work_path_actual <- path_abs(subject$preprocess_settings$raw_path, must_work = FALSE)

  template <- c(readLines(system.file('shell-templates/yael-preprocess.R', package = "ravecore")), "")
  cmd <- ravepipeline::glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE)

  if( run_recon_all ) {
    script_name <- "cmd-yael-preprocess-full.R"
  } else {
    script_name <- "cmd-yael-preprocess-simple.R"
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
    dest_path = path_abs(subject$imaging_path),
    execute = execute,
    command = rscript_path()
  )
  if( verbose ) {
    message(cmd)
  }
  if(dry_run) {
    return(invisible(re))
  }

  execute()

  return(invisible(re))

}

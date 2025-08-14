#' @title Workflow: 'FreeSurfer' surface reconstruction
#' @description
#' Runs 'FreeSurfer' \code{recon-all} command underneath; must have 'FreeSurfer'
#' installed.
#' @param subject 'RAVE' subject or subject ID
#' @param mri_path path to 'T1'-weighted 'MRI', must be a 'NIfTI' file
#' @param args type of workflow; see 'FreeSurfer' \code{recon-all} command
#' documentation; default choice is \code{'-all'} to run all workflows
#' @param work_path working directory; 'FreeSurfer' errors out when
#' working directory contains white spaces. By default, 'RAVE' automatically
#' creates a symbolic link to a path that contains no white space. Do not
#' set this input manually unless you know what you are doing
#' @param overwrite whether to overwrite existing work by deleting the folder;
#' default is false. In case of errors, set this to true to restart the
#' workflow; make sure you back up the files first.
#' @param command_path 'FreeSurfer' home directory. In some cases, 'RAVE' might
#' not be able to find environment variable \code{'FREESURFER_HOME'}. Please
#' manually set the path if the workflow fails. Alternatively, you can manually
#' set FreeSurfer' home directory via 'RAVE' options
#' \code{\link[ravepipeline:raveio-option]{raveio_setopt("freesurfer_path", "/path/to/freesurfer/home")}}
#' prior to running the script
#' @param dry_run avoid running the code, but print the process instead
#' @param verbose print messages
#' @returns A list of shell command set.
#'
#' @examples
#'
#' # Requires `FreeSurfer` and only works on MacOS or Linux
#' # as `FreeSurfer` does not support Windows
#'
#' \dontrun{
#'
#' library(ravecore)
#'
#' # Install subject: `install_subject("yael_demo_001")`
#'
#' # Create subject instance; strict=FALSE means it's OK if the subject
#' # is missing
#' subject <- as_rave_subject("YAEL/s01", strict = FALSE)
#'
#' cmd_freesurfer_recon_all(subject = subject,
#'                          mri_path = "/path/to/T1.nii.gz")
#'
#' }
#' @export
cmd_freesurfer_recon_all <- function(
    subject, mri_path,
    args = c(
      "-all", "-autorecon1", "-autorecon2", "-autorecon3",
      "-autorecon2-cp", "-autorecon2-wm", "-autorecon2-pial"
    ),
    work_path = NULL,
    overwrite = FALSE, command_path = NULL,
    dry_run = FALSE, verbose = dry_run) {
  # DIPSAUS DEBUG START
  # subject <- as_rave_subject("devel/testtest2", strict = FALSE)
  # mri_path <- "/Users/dipterix/rave_data/raw_dir/DBS_93/rave-imaging/coregistration/MRI_reference.nii.gz"
  # command_path = NULL
  # overwrite <- FALSE
  # list2env(list(subject = "devel/testtest2",
  #               args = "-all", overwrite = FALSE, dry_run = TRUE, verbose = FALSE,
  #               command_path = NULL), globalenv())

  all_args <- c(
    "-all", "-autorecon1", "-autorecon2", "-autorecon3",
    "-autorecon2-cp", "-autorecon2-wm", "-autorecon2-pial"
  )
  args <- all_args[all_args %in% args]
  if('-all' %in% args) {
    args <- '-all'
  } else if(!length(args)) {
    stop("`cmd_freesurfer_recon_all`: recon-all flag is invalid")
  }

  if(missing(mri_path) || length(mri_path) != 1 || is.na(mri_path) || !file.exists(mri_path) ||
     dir.exists(mri_path)) {
    stop("`cmd_freesurfer_recon_all`: `mri_path` is not a valid path.")
  }
  mri_path <- normalizePath(mri_path, winslash = "/", mustWork = TRUE)

  if(!grepl("\\.nii($|\\.gz$)", mri_path, ignore.case = TRUE)) {
    stop("`cmd_freesurfer_recon_all`: `mri_path` is not a valid NifTi file.")
  }

  subject <- restore_subject_instance(subject, strict = FALSE)

  # Auto-correct freesurfer path
  default_fs_path <- cmd_freesurfer_home(error_on_missing = FALSE)
  freesurfer_home <- tryCatch({
    freesurfer <- normalize_commandline_path(
      path = command_path,
      unset = default_fs_path,
      type = "freesurfer"
    )
    if(length(freesurfer) != 1 || is.na(freesurfer) || !isTRUE(dir.exists(freesurfer))) {
      freesurfer <- NULL
    } else if(!identical(default_fs_path, freesurfer)) {
      ravepipeline::raveio_setopt("freesurfer_path", freesurfer)
    }
    freesurfer
  }, error = function(e){ NULL })

  has_freesurfer <- !is.null(freesurfer_home)
  if(has_freesurfer) {
    freesurfer_home <- normalizePath(freesurfer_home, winslash = "/")
  }
  cmd_recon <- "recon-all"

  log_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- strftime(Sys.time(), "log-recon-all-%y%m%d-%H%M%S.log")

  # Always use a temporary working path since the target directory might contain spaces
  if(length(work_path) != 1 || is.na(work_path) || !dir.exists(work_path)) {
    work_path_symlink <- file.path(
      tools::R_user_dir("ravecore", which = "cache"),
      "FreeSurfer",
      subject$subject_code,
      fsep = "/"
    )
  } else {
    work_path_symlink <- file.path(work_path, subject$subject_code, fsep = "/")
  }

  work_path_actual <- path_abs(subject$preprocess_settings$raw_path, must_work = FALSE)

  template <- c(readLines(system.file('shell-templates/recon-all-t1.sh', package = "ravecore")), "")
  cmd <- ravepipeline::glue(paste(template, collapse = "\n"), .sep = "\n", .open = "{{", .close = "}}", .trim = FALSE)

  script_path <- normalizePath(
    file.path(subject$preprocess_settings$raw_path, "rave-imaging", "scripts", "cmd-fs-recon.sh"),
    mustWork = FALSE, winslash = "/"
  )
  execute <- function(...) {
    initialize_imaging_paths(subject)
    cmd_execute(script = cmd, script_path = script_path, command = "bash", ...)
  }
  re <- list(
    script = cmd,
    script_path = script_path,
    dry_run = dry_run,
    freesurfer_home = freesurfer_home,
    log_file = file.path(log_path, log_file, fsep = "/"),
    src_path = mri_path,
    dest_path = file.path(work_path_actual, "rave-imaging", "fs", fsep = "/"),
    execute = execute,
    command = "bash"
  )
  if( verbose ) {
    message(cmd)
  }
  if(dry_run) {
    return(invisible(re))
  }

  execute()

  return(invisible(re))

  # cmd_freesurfer_recon_all(subject = "devel/YCQ", mri_path = "/Volumes/PennRAID/Dropbox (PENN Neurotrauma)/BeauchampServe/rave_data/raw/YCQ/rave-imaging/inputs/MRI/YCQ_MRI.nii", args = "-autorecon1", verbose = TRUE)
}

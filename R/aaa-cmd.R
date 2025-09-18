#' @name cmd-external
#' @title External shell commands for 'RAVE'
#' @description These shell commands are only tested on 'MacOS' and 'Linux'.
#' On 'Windows' machines, please use the 'WSL2' system.
#' @param dry_run whether to run in dry-run mode; under such mode, the shell
#' command will not execute. This is useful for debugging scripts; default is
#' false
#' @param verbose whether to print out the command script; default is true under
#' dry-run mode, and false otherwise
#' @param expr expression to run as command
#' @param quoted whether \code{expr} is quoted; default is false
#' @param args further arguments in the shell command, especially the
#' 'FreeSurfer' reconstruction command
#' @param script the shell script
#' @param script_path path to run the script
#' @param log_file where should log file be stored
#' @param command which command to invoke; default is \code{'bash'}
#' @param backup whether to back up the script file immediately; default is true
#' @param ... passed to \code{\link{system2}}, or additional arguments
#' @returns A list of data containing the script details:
#' \describe{
#' \item{\code{script}}{script details}
#' \item{\code{script_path}}{where the script should/will be saved}
#' \item{\code{dry_run}}{whether dry-run mode is turned on}
#' \item{\code{log_file}}{path to the log file}
#' \item{\code{execute}}{a function to execute the script}
#' }
NULL

#' @rdname cmd-external
#' @export
cmd_execute <- function(script, script_path, command = "bash", dry_run = FALSE, backup = TRUE, args = NULL, ...) {

  dir_create2(dirname(script_path))
  writeLines(script, con = script_path)

  # Back up the script
  if(backup) {
    backup_dir <- file.path(dirname(script_path), "backups")
    backup_path <- backup_file(script_path, remove = FALSE, quiet = TRUE)
    if(!isFALSE(backup_path) && isTRUE(file.exists(backup_path))) {
      backup_dir <- dir_create2(backup_dir)
      to_path <- file.path(backup_dir, basename(backup_path))
      if(file.exists(to_path)) {
        unlink(backup_path)
      } else {
        file_move(backup_path, to_path)
      }
    }
  }

  script_path <- normalizePath(script_path)

  if( dry_run ) {
    args <- paste(args, collapse = " ")
    if(nzchar(args)) {
      args <- sprintf("%s ", args)
    }
    if( .Platform$OS.type == "windows" ) {
      command <- gsub("/", "\\", command)
    }
    cmd <- sprintf("%s %s%s", shQuote(command), args, shQuote(script_path))
    return(cmd)
  } else {
    message("\nOutputs might be hidden and redirected to the log file.\n  ")
    additional_args <- list(...)
    message("Standard log output -> ", additional_args$stdout)
    message("Message or error output -> ", additional_args$stderr)
    system2(command = command, args = c(args, shQuote(script_path)), ...)
  }

}


validate_nii <- function(path) {
  if(missing(path) || length(path) != 1 || is.na(path) || !file.exists(path) ||
     dir.exists(path)) {
    stop("`validate_nii`: `path` is not a valid file path.")
  }
  path <- normalizePath(path, winslash = "/")
  if(!grepl("\\.nii($|\\.gz$)", path, ignore.case = TRUE)) {
    stop("`validate_nii`: `path` is not a valid NifTi file (.nii or .nii.gz)")
  }
  path
}



#' @rdname cmd-external
#' @export
cmd_run_r <- function(
    expr, quoted = FALSE,
    verbose = TRUE, dry_run = FALSE,
    log_file = tempfile(), script_path = tempfile(),
    ...) {

  force(dry_run)
  if(!quoted) {
    expr <- substitute(expr)
  }

  # work_path <- normalizePath(
  #   file.path(subject$preprocess_settings$raw_path2, "rave-imaging"),
  #   winslash = "/", mustWork = FALSE
  # )
  log_path <- normalizePath(dirname(log_file), mustWork = FALSE, winslash = "/")
  log_file <- basename(log_file)

  script_path <- normalizePath(
    script_path,
    mustWork = FALSE, winslash = "/"
  )

  cmd <- paste(collapse = "\n", c(
    "#!/usr/bin/env Rscript --no-save --no-restore",
    deparse(expr),
    "",
    "# END OF SCRIPT"
  ))

  execute <- function(...) {
    dir_create2(log_path)
    log_abspath <- normalizePath(file.path(log_path, log_file), winslash = "/", mustWork = FALSE)
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

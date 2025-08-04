
# Correct path if prefix has incorrect cases (works on case-sensitive os)
correct_filepath <- function(path) {
  stopifnot(length(path) == 1)
  if(file_exists(path)) { return(path) }
  # path_prefix = "bidsr.rproj"
  dir <- dirname(path)
  prefix <- basename(path)
  files <- list.files(
    path = dir,
    full.names = FALSE,
    all.files = TRUE,
    recursive = FALSE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  sel <- toupper(files) == toupper(prefix)
  if(!any(sel)) { return(NA) }

  return(files[sel][[1]])
}

path_to_nearest_file <- function(filename, start, root = NA, ignore_cases = FALSE) {

  # filename <- "rave"
  # start = "."

  if( ignore_cases ) {
    filename <- tolower(filename)
    start <- tolower(start)
  }

  if(file_exists(start)) {
    if(fs::is_file(start)) {
      if( basename(start) == filename ) {
        # start is a file and is the filename
        return(start)
      }
      start <- fs::path_dir(start)
    }
    # now start is a folder
    tmp <- fs::path(start, filename)
    if( file_exists(tmp) ) {
      return(tmp)
    }
    # folder does not have this file
  }
  if(!fs::is_absolute_path(start)) {
    start <- fs::path_abs(start)
  }
  start_ <- fs::path_dir(start)

  if(!is.na(root)) {
    if(!fs::is_absolute_path(root)) {
      root <- fs::path_abs(root)
    }
    if(!fs::path_has_parent(start_, root)) {
      return(NA_character_)
    }
  } else {
    if(start_ == start) {
      return(NA_character_)
    }
  }
  Recall(filename = filename, start = start_, root = root)
}



path_abs <- function(path, must_work = NA) {
  # normalizePath(path, winslash = "/", mustWork = FALSE)
  # path <- c(
  #   "~/../asdad",
  #   "../asd/./../../aa",
  #   "./././a/.././"
  # )
  if(!length(path)) { return(character(0L)) }
  path <- unlist(lapply(fs::path_split(path), function(p) {
    p[[1]] <- normalizePath(p[[1]], winslash = "/", mustWork = FALSE)
    do.call(fs::path, as.list(p))
  }))
  path <- normalizePath(path, mustWork = must_work)
  fs::path_norm(path)
}

path_expand <- function(path) {
  fs::path_norm(fs::path_expand(path))
}

path_norm <- function(path, must_work = NA) {
  fs::path_norm(path)
}

path_rel <- function(path, start = ".") {
  fs::path_rel(path, start = start)
}

file_path <- function(..., ext = "") {
  fs::path(..., ext = ext)
}

file_exists <- function(path) {
  fs::file_exists(as.character(path))
}

file_assert <- function(path, dir_ok = FALSE, follow = TRUE) {
  if(length(path) != 1 || is.na(path)) {
    stop("File path must be length of 1 and cannot be N/A")
  }
  if( dir_ok ) {
    if(!fs::file_exists(path)) {
      stop("File or directory `", path, "` is missing.")
    }
  } else {
    if( !fs::is_file(path, follow = follow) ) {
      stop("File `", path, "` is missing.")
    }
  }
  invisible(TRUE)
}

dir_exists <- function(path) {
  fs::dir_exists(path)
}

path_has_parent <- function(path, parent) {
  fs::path_has_parent(path = path, parent = parent)
}

path_split <- function(path) {
  fs::path_split(path)
}

path_join <- function(parts) {
  fs::path_join(parts)
}

is_absolute_path <- function(path) {
  fs::is_absolute_path(path)
}

file_delete <- function(path, use_base_r = FALSE, ...) {
  if( use_base_r ) {
    if( dir_exists(path) ) {
      unlink(x = path, recursive = TRUE, ...)
    } else if (file_exists(path)) {
      unlink(path, recursive = FALSE, ...)
    }
  } else {
    fs::file_delete(path)
  }
}

file_size <- function(path, fail = TRUE) {
  fs::file_size(path, fail = fail)
}

ARCHIVE_EXTENSIONS <- c("zip", "rar", "7z", "tar", "tar.gz", "tar.bz2", "gz", "bz2", "xz", "iso")

path_ext_remove <- function(path, archive_ext = ARCHIVE_EXTENSIONS) {
  ext <- path_ext(path, archive_ext = archive_ext)
  fs::path_ext_remove(path)

  dir <- fs::path_dir(path)
  bname <- basename(path)
  na <- is.na(path)
  no_dir <- dir == "." | dir == ""

  ext <- path_ext(bname)
  has_ext <- !na & ext != ""

  bname2 <- bname[has_ext]
  bname[has_ext] <- substr(bname2, 1L, nchar(bname2) - nchar(ext[has_ext]) - 1)


  path[!na & no_dir] <- fs::path_tidy(bname[!na & no_dir])
  path[!na & !no_dir] <- fs::path(dir[!na & !no_dir], bname[!na & !no_dir])
  path
}

path_ext <- function(path, archive_ext = ARCHIVE_EXTENSIONS) {
  # DIPSAUS DEBUG START
  # path <- c("asda.dasd/aadda.nii.gz.tar.bz2", ".asda.dasd/aadda.nii.gz.tar.bz2",
  #   ".nii.gz.tar.bz2", "asda", ".", "NA", NA, "nii.gz.aaa.nii.GZ", "././..")
  if (length(path) == 0) {
    return(character())
  }

  vapply(strsplit(basename(path), ".", fixed = TRUE), function(x) {
    if(isTRUE(is.na(x))) { return(NA_character_) }
    if(length(x) == 1) { return("") }
    if(x[[1]] == "") {
      x <- x[-c(1, 2)]
    } else {
      x <- x[-1]
    }
    if(!length(x)) { return("") }
    sel <- which(!tolower(x) %in% tolower(archive_ext))
    if(!length(sel)) {
      return(paste(x, collapse = "."))
    }
    sel <- max(sel)
    if(sel == length(x)) {
      return(x[[sel]])
    }
    paste(x[seq.int(sel, length(x))], collapse = ".")
  }, FUN.VALUE = "")
}

file_move <- function(path, new_path) {
  fs::file_move(path = path, new_path = new_path)
}


dir_create2 <- function(x, showWarnings = FALSE, recursive = TRUE, check = TRUE, ...) {
  if (!dir.exists(x)) {
    dir.create(x, showWarnings = showWarnings, recursive = recursive, ...)
  }
  if (check && !dir.exists(x)) {
    stop('Cannot create directory at ', shQuote(x))
  }
  invisible(path_abs(x))
}

fileexts <- function(file){
  call_pkg_fun("ieegio", "path_ext", file)
  # x <- filenames(file)
  # sapply(strsplit(x = file, split = '\\.'), function(x){
  #   l <- length(x)
  #   ifelse(l > 1, x[[l]], '')
  # })
}



#' @title Back up and rename the file or directory
#' @param path path to a file or a directory
#' @param remove whether to remove the original path; default is false
#' @param quiet whether not to verbose the messages; default is false
#' @returns \code{FALSE} if nothing to back up, or the back-up path
#' if \code{path} exists
#' @examples
#'
#' path <- tempfile()
#' file.create(path)
#'
#' path2 <- backup_file(path, remove = TRUE)
#'
#' file.exists(c(path, path2))
#' unlink(path2)
#'
#'
#' @export
backup_file <- function(path, remove = FALSE, quiet = FALSE) {

  if(length(path) != 1 || is.na(path)) {
    return(invisible(FALSE))
  }
  if(!file.exists(path)){ return(invisible(FALSE)) }

  path <- normalizePath(path, mustWork = TRUE, winslash = "/")

  is_dir <- dir.exists(path)

  # find the extension
  ext <- fileexts(path)

  bname <- basename(path)
  dname <- dirname(path)

  if(ext == '') {
    bname <- gsub("[/]+$", "", bname)
  } else {
    bname <- substr(bname, start = 1L, stop = nchar(bname) - nchar(ext) - 1)
  }

  # check if bname contains timestamp
  bname <- gsub("_\\[backup_[0-9]{8}_[0-9]{6}\\]$", "", x = bname)

  bname2 <- sprintf(
    "%s_[backup_%s]%s",
    bname,
    strftime(Sys.time(), "%Y%m%d_%H%M%S"),
    ifelse(ext == "", "", sprintf(".%s", ext))
  )
  if (!quiet) {
    ravepipeline::logger(
      use_glue = TRUE,
      level = 'info',
      "{ifelse(remove, 'Moving', 'Copying')} {ifelse(is_dir, 'directory', 'file')} {basename(path)}\n  => {bname2}"
    )
  }
  path2 <- file.path(dname, bname2)

  if( remove ) {
    file_move(path = path, new_path = path2)
  } else {
    if(is_dir) {
      dir_create2(path2)
      file.copy(
        from = list.files(
          path = path, all.files = TRUE, full.names = TRUE,
          recursive = FALSE, include.dirs = TRUE, no.. = TRUE
        ),
        to = path2, overwrite = TRUE, recursive = TRUE,
        copy.mode = TRUE, copy.date = TRUE
      )
    } else {
      file.copy(from = path, to = path2, overwrite = TRUE,
                copy.mode = TRUE, copy.date = TRUE, recursive = FALSE)
    }
  }

  return(invisible(path2))

}



#' @name cache_path
#' @title Manipulate cached data on the file systems
#' @param check whether to ensure the cache root path
#' @param quiet whether to suppress the message
#' @param subject_code subject code to remove; default is missing. If
#' \code{subject_code} is provided, then only this subject-related cache
#' files will be removed.
#' @returns \code{cache_root} returns the root path that stores the 'RAVE'
#' cache data; \code{clear_cached_files} returns nothing
#' @details 'RAVE' intensively uses cache files. If running on personal
#' computers, the disk space might be filled up very quickly. These cache
#' files are safe to remove if there is no 'RAVE' instance running.
#' Function \code{clear_cached_files} is designed to remove these cache files.
#' To run this function, please make sure that all 'RAVE' instances
#' are shutdown.
#'
#' @examples
#'
#' cache_root()
#'
#' @export
cache_root <- function(check = FALSE){
  re <- ravepipeline::raveio_getopt(key = 'tensor_temp_path', default = NULL)
  if(!length(re)){
    re <- '~/rave_data/cache_dir/'
    ravepipeline::raveio_setopt(key = 'tensor_temp_path', value = re)
  }
  if(check){
    re <- dir_create2(re)
  }
  re
}

#' @rdname cache_path
#' @export
clear_cached_files <- function(subject_code, quiet = FALSE){

  miss_subject <- missing(subject_code)
  if(miss_subject) {
    clear_dir <- function(dir) {
      if(!dir.exists(dir)) { return() }
      if(!quiet){
        ravepipeline::logger("Clearing ", dir, level = "trace")
      }
      unlink(dir, recursive = TRUE)
    }
  } else {

    stopifnot2(grepl("^[a-zA-Z0-9_-]{1,}$", subject_code), msg = "clear_cached_files: Invalid subject_code, only letter, digits, _, - are allowed. Subject code cannot be blank as well.")

    clear_dir <- function(dir) {
      if(!dir.exists(dir)) { return() }

      blpath <- file.path(dir, "_baselined_arrays_")
      if(dir.exists(blpath)) {
        if(!quiet){ ravepipeline::logger("Clearing ", blpath, level = "info") }
        unlink(blpath, recursive = TRUE)
      }

      rdirs <- list.files(
        path = dir,
        pattern = sprintf("(^|/)%s$", subject_code),
        recursive = TRUE,
        all.files = FALSE,
        include.dirs = TRUE,
        full.names = TRUE,
        no.. = TRUE
      )
      rfiles <- rdirs[dir.exists(rdirs)]
      rdirs <- rdirs[dir.exists(rdirs)]

      if(length(rfiles)) {
        if(!quiet){ ravepipeline::logger("Clearing {length(rfiles)} files with name {subject_code}",
                          level = "trace", use_glue = TRUE) }
        unlink(rfiles)
      }
      if(length(rdirs)) {
        if(!quiet){ ravepipeline::logger("Clearing {length(rdirs)} directories with name {subject_code}",
                          level = "trace", use_glue = TRUE) }
        unlink(rdirs, recursive = TRUE)
      }

    }


  }

  clear_dir('~/rave_data/cache_dir/')
  clear_dir(tools::R_user_dir('raveio', "cache"))
  clear_dir(cache_root())

  # ravetools_path <- file.path(
  #   getOption(
  #     x = "ravetools.tempdir",
  #     default = Sys.getenv(
  #       x ="RAVETOOLS_TEMPDIR",
  #       unset = tempdir(check = FALSE)
  #     )
  #   ),
  #   "ravetools"
  # )
  #
  # if(isTRUE(dir.exists(ravetools_path))) {
  #   if(!quiet){ catgl("Clearing ", ravetools_path, level = "DEFAULT") }
  #   unlink(ravetools_path, recursive = TRUE)
  # }

  if(!quiet){
    ravepipeline::logger("Done", level = "trace")
  }
}

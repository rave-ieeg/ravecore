

RAVESubjectRawImpl <- S7::new_class(
  name = "RAVESubjectRawImpl",
  properties = list(
    code = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        value <- trimws(paste(value, collapse = ""))
        value <- gsub("^sub-", "", x = value, ignore.case = TRUE)
        S7::prop(self, "code") <- value
        self
      },
      validator = function(value) {
        if (length(value) != 1) {
          return("RAVE subject code must be a string")
        }
        if (!nzchar(value) ||
            !grepl("^[a-zA-Z0-9]", value, ignore.case = TRUE)) {
          return("RAVE subject code must not be empty and must starts with a letter/digits")
        }
        return()
      }
    ),
    format_standard = S7::new_property(
      class = S7::class_character,
      setter = function(self, value) {
        value <- match.arg(value, choices = c("native", "bids"))
        S7::prop(self, "format_standard") <- value
        self
      }
    ),
    parent_path = S7::new_property(
      class = S7::new_union(S7::class_character, NULL, bidsr::BIDSProject),
      setter = function(self, value) {
        if(self@format_standard == "bids" && !S7::S7_inherits(value, bidsr::BIDSProject)) {
          value <- bidsr::bids_project(value)
        }
        S7::prop(self, "parent_path") <- value
        self
      },
      validator = function(value) {
        if(is.null(value)) { return() }
        if(S7::S7_inherits(value, bidsr::BIDSProject)) { return() }
        if(length(value) != 1 || is.na(value)) {
          return("RAVE subject (raw) parent folder must be either a `bidsr::BIDSProject`, a `string` or `NULL`")
        }
      }
    ),
    path = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        parent_path <- self@parent_path
        switch (
          self@format_standard,
          "bids" = {
            if(S7::S7_inherits(parent_path, bidsr::BIDSProject)) {
              parent_path <- file_path(format(parent_path, storage = "derivative"), "rave", "raw_dir")
            }
            path <- file_path(path_abs(path = parent_path, must_work = FALSE), sprintf("sub-%s", self@code))
          },
          {
            if(is.null(parent_path)) {
              parent_path <- ravepipeline::raveio_getopt(key = "raw_data_dir")
            }
            path <- file_path(path_abs(path = parent_path, must_work = FALSE), self@code)
          }
        )
        return(path)
      }
    )
  ),
  validator = function(self) {
    if(self@format_standard == "bids" && grepl("[^a-zA-Z0-9]", self@code)) {
      return("RAVE-BIDS subject code must only contain letters and/or digits (other characters, especially underscore (_) and dash (-) are disallowed)")
    }
  }
)

S7::method(format.generic, RAVESubjectRawImpl) <- function(x, ...) {
  if(x@format_standard == "bids") {
    sprintf("sub-%s", x@code)
  } else {
    x@code
  }

}

# examples <- bidsr::download_bids_examples()
# parent_path = file.path(examples, "ieeg_epilepsy_ecog")
# sub <- RAVESubjectRawImpl(code = "ecog01", parent_path = parent_path, format_standard = "bids")
# sub@path
# format(sub)
# sub <- RAVESubjectRawImpl(code = "ecog01_1", format_standard = "native", parent_path = NULL)
# sub@path
# format(sub)

S7::method(rave_path, RAVESubjectRawImpl) <- function(x, ..., storage = NULL) {

  # storage:
  # 'source' path: where

  storage <- match.arg(storage, choices = c("rave_raw", "rave_imaging", "bids_raw", "bids_source", "bids_derivative"))

  if(startsWith(storage, "bids")) {
    if( x@format_standard != "bids" ) { return(NA_character_) }

    bids_subject <- bidsr::bids_subject(x@parent_path, x@code, strict = FALSE)
    path <- switch (
      storage,
      "bids_raw" = bidsr::resolve_bids_path(bids_subject, storage = "raw"),
      "bids_source" = bidsr::resolve_bids_path(bids_subject, storage = "source"),
      "bids_derivative" = bidsr::resolve_bids_path(bids_subject, storage = "derivative"),
      {
        stop("Unknown storage type: `", storage, "`")
      }
    )
    return(path)
  }

  path <- switch (
    storage,

    # re$root_raw <- normalizePath(raveio_getopt('raw_data_dir'), mustWork = FALSE)
    # re$raw_path2 <- file.path(re$root_raw, subject_code)
    "rave_raw" = x@path,
    "rave_imaging" = file_path(x@path, "rave-imaging"),

    {
      stop("Unknown storage type: `", storage, "`")
    }
  )
  path

}



RAVESubjectDerivativeImpl <- S7::new_class(
  name = "RAVESubjectDerivativeImpl",
  properties = list(
    project = S7::new_property(class = RAVEProjectImpl),
    subject_raw = S7::new_property(class = RAVESubjectRawImpl),
    code = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        self@subject_raw@code
      }
    ),
    id = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        sprintf("%s/%s", format(self@project), self@code)
      }
    )
  ),
  validator = function(self) {
    if(self@project@format_standard != self@subject_raw@format_standard) {
      return("Incompatible format standard: the project is ", sQuote(self@project@format_standard),
             " but subject (raw) is ", sQuote(self@subject_raw@format_standard))
    }
    # TODO: check if parent_paths are the same in BIDS
  }
)

# examples <- bidsr::download_bids_examples()
# parent_path = file.path(examples, "ieeg_epilepsy_ecog")
# subj <- RAVESubjectRawImpl(code = "ecog01", parent_path = parent_path, format_standard = "bids")
# proj <- RAVEProjectImpl(name = "demo", parent_path = parent_path, format_standard = "bids")
#
# sub <- RAVESubjectDerivativeImpl(project = proj, subject_raw = subj)
# sub@id
#
# subj <- RAVESubjectRawImpl(code = "ecog01", parent_path = parent_path, format_standard = "native")
# proj <- RAVEProjectImpl(name = "demo", parent_path = parent_path, format_standard = "native")
#
# sub <- RAVESubjectDerivativeImpl(project = proj, subject_raw = subj)
# sub@id

S7::method(format.generic, RAVESubjectDerivativeImpl) <- function(x, ...) {
  x@id
}

S7::method(rave_path, RAVESubjectDerivativeImpl) <- function(x, storage = NULL, meta_name = NULL, ...) {

  # storage:
  # 'source' path: where

  # for project-based paths
  project_based_storages <- c(
    "project_subject", "project_parent", "project", "project_groupdata",
    "notes", "preprocess", "meta", "pipelines", "reports", "signals", "cache", "reference", "freesurfer")
  # For metadata
  metadata_storages <- c('electrodes', 'frequencies', 'time_points', 'time_excluded', 'epoch', 'references')
  # For subject-only paths
  subject_only_storages <- c("rave_raw", "rave_imaging", "bids_raw", "bids_source", "bids_derivative")

  storage <- match.arg(storage, choices = c(project_based_storages, metadata_storages, subject_only_storages))

  if(storage %in% subject_only_storages) {
    return(rave_path(x@subject_raw, storage = storage, ...))
  }

  if(storage %in% project_based_storages) {
    # re$root_data <- normalizePath(raveio_getopt('data_dir'), mustWork = FALSE)
    path <- switch (
      storage,
      "project_parent" = dirname(x@project@path),
      "project" = x@project@path,
      "project_groupdata" = file_path(x@project@path, "_project_data"),
      "project_subject" = file_path(x@project@path, format(x@subject_raw)),
      "notes" = file_path(x@project@path, format(x@subject_raw), "notes"),
      "preprocess" = file_path(x@project@path, format(x@subject_raw), "rave", "preprocess"),
      "meta" = file_path(x@project@path, format(x@subject_raw), "rave", "meta"),
      "pipelines" = file_path(x@project@path, format(x@subject_raw), "pipelines"),
      "reports" = file_path(x@project@path, format(x@subject_raw), "reports"),
      "signals" = file_path(x@project@path, format(x@subject_raw), "rave", "data"),
      "cache" = file_path(x@project@path, format(x@subject_raw), "cache"),
      "reference" = file_path(x@project@path, format(x@subject_raw), "rave", "data", "reference"),
      "freesurfer" = {
        # This is a messy historical issue, there could be many places where the freesurfer folder
        # is stored; sometimes not even a fs folder can be used for visualization.
        is_fs_dir <- function(re) {
          if(!is.na(re) && dir_exists(re) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)) {
            return(TRUE)
          }
          return(FALSE)
        }

        # To find freesurfer directory, here are the paths to search
        # 0. if options('rave.freesurfer_dir') is provided, then XXX/subject/
        # 1. raw_dir/subject/rave-imaging/fs
        # 2. raw_dir/subject/rave-imaging/ants
        # 3. raw_dir/subject/fs
        # 4. rave_data/project/subject/rave/fs
        # 5. rave_data/project/subject/fs
        # 6. (BIDS-only) derivatives/freesurfer*/sub-code(_|$)/
        # 6. get SUBJECT envvar

        # If options `rave.freesurfer_dir` is set, this over-rules all
        fs_paths <- as.character(c(
          file_path(getOption('rave.freesurfer_dir'), x@code),
          file_path(getOption('rave.freesurfer_dir'), sprintf("sub-%s", x@code))
        ))
        for (re in fs_paths) {
          if(is_fs_dir(re)) { return(re) }
        }
        # If the preprocessing path has it
        rave_image_path <- rave_path(x@subject_raw, storage = "rave_imaging")
        # update: check subject/imaging/fs provided by the new pipeline
        re <- as.character(file_path(rave_image_path, "fs"))
        if( is_fs_dir(re) ) { return(re) }
        re <- as.character(file_path(rave_image_path, "ants"))
        if( is_fs_dir(re) ) { return(re) }

        # Could be in the raw path
        re <- file_path(rave_path(x@subject_raw, storage = "rave_raw"), "fs")
        if( is_fs_dir(re) ) { return(re) }
        # under rave folder (project/subject/[rave]/fs)
        re <- file_path(x@project@path, format(x@subject_raw), "fs")
        if( is_fs_dir(re) ) { return(re) }
        re <- file_path(x@project@path, format(x@subject_raw), "rave", "fs")
        if( is_fs_dir(re) ) { return(re) }

        # For BIDS only
        derivative_path <- NULL
        fs_roots <- NULL
        if( x@project@format_standard == "bids" ) {
          derivative_path <- bidsr::resolve_bids_path(x@project@parent_path, storage = "derivative")
        }
        if(length(derivative_path) == 1 && dir_exists(derivative_path)) {
          fs_roots <- list.files(
            derivative_path,
            pattern = "freesurfer",
            include.dirs = TRUE,
            all.files = FALSE,
            full.names = FALSE,
            recursive = FALSE,
            ignore.case = TRUE,
            no.. = TRUE
          )
          fs_roots <- file_path(derivative_path, fs_roots)
          fs_roots <- fs_roots[dir_exists(fs_roots)]
        }
        for(fs_root in fs_roots) {
          fs_relpaths <- list.files(
            fs_root,
            pattern = sprintf("^sub-%s(_|$)", x@subject_raw@code),
            all.files = FALSE,
            full.names = FALSE,
            recursive = TRUE,
            ignore.case = TRUE,
            include.dirs = TRUE
          )
          fs_paths <- file_path(fs_root, fs_relpaths)
          for(fs_path in fs_paths) {
            if(is_fs_dir(fs_path)) { return(fs_path) }
          }
        }
        # re <- as.character(file.path(self$rave_path, 'fs'))
        # if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
        # # update: check subject/imaging/fs provided by the new pipeline
        # re <- as.character(file.path(self$path, 'imaging', "fs"))
        # if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
        # re <- as.character(file.path(self$path, 'fs'))
        # if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }
        # re <- as.character(file.path(self$path, self$subject_code))
        # if(isTRUE(dir.exists(re)) && threeBrain::check_freesurfer_path(re, autoinstall_template = FALSE)){ return(re) }

        # Finally, check environment variable `SUBJECTS_DIR`
        subdir <- Sys.getenv('SUBJECTS_DIR', unset = "")
        if(!is.na(subdir) && nzchar(subdir) && dir_exists(subdir)) {
          fs_paths <- file_path(subdir, sprintf(c("%s", "sub-%s"), x@code))
          for (re in fs_paths) {
            if(is_fs_dir(re)) { return(re) }
          }
        }
        return(NA)
      },
      {
        stop("Unknown storage type: `", storage, "`")
      }
    )
    return(path)
  }

  # metadata
  meta_folder <- file_path(x@project@path, format(x@subject_raw), "rave", "meta")

  path <- switch (
    storage,
    "electrodes" = {
      opt1 <- file_path(meta_folder, "electrodes.csv")
      if(!file_exists(opt1)) {
        opt2 <- file_path(rave_path(x@subject_raw, storage = "rave_imaging"), "derivative", "electrodes.csv")
        if(file_exists(opt2)) {
          opt1 <- opt2
        }
      }
      opt1
    },
    "frequencies" = file_path(meta_folder, "frequencies.csv"),
    "time_points" = file_path(meta_folder, "time_points.csv"),
    "time_excluded" = file_path(meta_folder, "time_excluded.csv"),
    "epoch" = {
      stopifnot(length(meta_name) == 1)
      file_path(meta_folder, sprintf("epoch_%s.csv", meta_name))
    },
    "references" = {
      stopifnot(length(meta_name) == 1)
      file_path(meta_folder, sprintf("reference_%s.csv", meta_name))
    },
    {
      stop("Unknown storage type: `", storage, "`")
    }
  )


  path
}


# Function to convert things to `RAVESubjectDerivativeImpl`

restore_subject_impl <- function(subject_id, strict = FALSE) {
  if(S7::S7_inherits(subject_id, RAVESubjectDerivativeImpl)) { return(subject_id) }

  if(inherits(subject_id, 'RAVESubject')){
    # ravecore
    if(S7::S7_inherits(subject_id$`@impl`, RAVESubjectDerivativeImpl)) { return(subject_id$`@impl`) }
    # legacy raveio no direct BIDS support
    project <- subject_id$project
    project_impl <- project$`@impl`
    if(!S7::S7_inherits(project_impl, RAVEProjectImpl)) {
      project_name <- project$name
      if(grepl("@bids", project_name)) {
        format_standard <- "bids"
      } else {
        format_standard <- "native"
      }
      project_impl <- RAVEProjectImpl(name = project_name,
                                      format_standard = format_standard,
                                      parent_path = NULL)
    }

    subject_raw <- RAVESubjectRawImpl(
      code = subject_id$subject_code,
      format_standard = format_standard,
      parent_path = project_impl@parent_path
    )
    return(RAVESubjectDerivativeImpl(project = project_impl, subject_raw = subject_raw))
  }
  # RAVE 1.0
  if(inherits(subject_id, "Subject")) {
    # RAVE 1.0 subject instance
    stopifnot2(is.character(subject_id$id),
               msg = "`as_rave_subject`: Cannot find subject ID from the given input")
    subject_id <- subject_id$id
  }

  # Assuming subject_id is a character string
  split_res <- strsplit(subject_id, "/", fixed = TRUE)[[1]]
  project_name <- split_res[[1]]
  subject_code <- split_res[[2]]
  if(grepl("@bids", project_name)) {
    format_standard <- "bids"
  } else {
    format_standard <- "native"
  }

  project <- as_rave_project(x = project_name, strict = FALSE, parent_path = NULL)
  project_impl <- project$`@impl`
  subject_raw <- RAVESubjectRawImpl(code = subject_code, format_standard = format_standard, parent_path = project_impl@parent_path)

  return(RAVESubjectDerivativeImpl(project = project_impl, subject_raw = subject_raw))

  # if(inherits(subject_id, 'RAVESubject')){
  #   return(subject_id)
  # } else {
  #   if(inherits(subject_id, "Subject")) {
  #     # RAVE 1.0 subject instance
  #     stopifnot2(is.character(subject_id$id),
  #                msg = "`as_rave_subject`: Cannot find subject ID from the given input")
  #     subject_id <- subject_id$id
  #   }
  #   # if(startsWith(subject_id, "@meta_analysis")) {
  #   #   subject_id <- gsub("^@meta_analysis/", "", subject_id)
  #   #   return( RAVEMetaSubject$new(subject_id) )
  #   # } else {
  #     return( RAVESubject$new(subject_id, strict = strict) )
  #   # }
  # }
}

#' @name meta-data
#' @title Load or save meta data to 'RAVE' subject
#' @param data data table
#' @param meta_type see load meta
#' @param project_name project name
#' @param subject_code subject code
#' @param subject_id subject identified, alternative way to specify the project
#' and subject in one string
#' @param meta_name for epoch and reference only, the name the of the table
#' @returns The corresponding metadata
#' @examples
#'
#'
#' if(has_rave_subject("demo/DemoSubject")) {
#'   subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)
#'
#'   electrode_table <- subject$get_electrode_table()
#'
#'   save_meta2(
#'     data = electrode_table,
#'     meta_type = "electrodes",
#'     project_name = subject$project_name,
#'     subject_code = subject$subject_code
#'   )
#'
#'   load_meta2(meta_type = "electrodes", subject_id = subject)
#' }
#'
#' @export
save_meta2 <- function(data, meta_type, project_name, subject_code){
  subject_code <- gsub('^sub-', "", x = subject_code, ignore.case = TRUE)

  impl <- restore_subject_impl(sprintf("%s/%s", project_name, subject_code))
  meta_dir <- rave_path(impl, "meta")

  if(!dir_exists(meta_dir)){
    dir_create2(meta_dir)
  }

  if(meta_type == 'electrodes'){
    names(data)[1] <- c('Electrode')
    if(!'Coord_x' %in% names(data)){
      # try not to overwrite original data
      data$Coord_x <- 0
      data$Coord_y <- 0
      data$Coord_z <- 0
      data$Label <- ''
    }
    if(!"LocationType" %in% names(data)){
      data$LocationType <- "iEEG"
    }
    data$SubjectCode <- subject_code

    safe_write_csv(data, file = file.path(meta_dir, 'electrodes.csv'), row.names = FALSE)
  }else if(meta_type == 'time_points'){
    names(data) <- c('Block', 'Time')
    safe_write_csv(data, file = file.path(meta_dir, 'time_points.csv'), row.names = FALSE)
  }else if(meta_type == 'frequencies'){
    names(data) <- c('Frequency')
    safe_write_csv(data, file = file.path(meta_dir, 'frequencies.csv'), row.names = FALSE)
  }else if(meta_type == 'time_excluded'){
    # deprecated
    if(!is.data.frame(data)){
      data <- as.data.frame(data, stringsAsFactors = FALSE)
    }
    if(nrow(data)){
      names(data) <- c('Block', 'Start', 'End')
      safe_write_csv(data, file = file.path(meta_dir, 'time_excluded.csv'), row.names = FALSE)
    }
  }


}

#' @rdname meta-data
#' @export
load_meta2 <- function(
    meta_type = c(
      'electrodes',
      'frequencies',
      'time_points',
      'epoch',
      'references',
      'time_excluded',
      'info'
    ),
    project_name,
    subject_code,
    subject_id,
    meta_name
) {
  meta_type <- match.arg(meta_type)
  if(missing(subject_id)){
    subject_id <- sprintf("%s/%s", project_name, subject_code)
  }
  subject <- as_rave_subject(subject_id)
  if(missing(meta_name)) {
    meta_name <- NULL
  }

  if(meta_type %in% c('electrodes', 'frequencies', 'time_points', 'epoch', 'references')) {
    return(subject$meta_data(meta_type = meta_type, meta_name = meta_name, strict = FALSE))
  }

  switch (
    meta_type,
    "time_excluded" = {
      # Read time_excluded.csv if exists
      time_excluded_path <- file.path(subject$meta_path, 'time_excluded.csv')
      if(file.exists(time_excluded_path)){
        return(safe_read_csv(time_excluded_path, colClasses = c(Block = 'character')))
      }else{
        return(data.frame(
          Block = NULL,
          Electrode = NULL,
          Start = NULL,
          End = NULL
        ))
      }
    },
    "info" = {
      info_file <- file.path(subject$meta_path, 'info.yaml')
      if(file.exists(info_file)){
        info <- load_yaml(info_file)
        return(as.list(info))
      }
    }
  )
  return(NULL)
}

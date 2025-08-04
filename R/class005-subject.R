
#' Defines 'RAVE' subject class
#' @description \code{R6} class definition
#' @export
RAVESubject <- R6::R6Class(
  classname = 'RAVESubject',
  class = TRUE,
  portable = TRUE,
  inherit = RAVESerializable,
  private = list(
    # .name = character(0),
    # .path = character(0),
    # .dirs = NULL,
    # .project = NULL,
    .preprocess = NULL,
    .cached_config = NULL,
    .reference_tables = list(),
    impl = NULL

  ),
  public = list(
    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      list(
        namespace = "ravecore",
        r6_generator = "RAVESubject",
        data = list(
          project = self$project$`@marshal`(),
          # subject = self$subject$subject_id,#`@marshal`(),
          code = self$subject_code
        )
      )
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(identical(object$r6_generator, "RAVESubject"))
      return(RAVESubject$new(
        project_name = RAVEProject$public_methods$`@unmarshal`(object$data$project),
        subject_code = object$data$code,
        strict = FALSE
      ))
    },

    #' @description override print method
    #' @param ... ignored
    print = function(...){
      cat('RAVE subject <', self$subject_id, '>\n', sep = '')
    },

    #' @description constructor
    #' @param project_name character project name
    #' @param subject_code character subject code
    #' @param strict whether to check if subject folders exist
    #' @param parent_path parent path if no default path is used, this is for
    #' the root directory if subject is in 'BIDS' format
    initialize = function(project_name, subject_code = NULL, strict = TRUE, parent_path = NULL){
      if(length(subject_code) != 1) {
        private$impl <- restore_subject_impl(subject_id = project_name)
      } else {
        project <- as_rave_project(project_name, strict = FALSE)
        project_impl <- project$`@impl`
        if( project_impl@format_standard == "bids" ) {
          # reload project as parent_path can be shared, in case the project needs it
          project <- as_rave_project(project_name, strict = FALSE, parent_path = parent_path)
          project_impl <- project$`@impl`
          subject_raw <- RAVESubjectRawImpl(code = subject_code, format_standard = project_impl@format_standard, parent_path = project_impl@parent_path)
        } else {
          subject_raw <- RAVESubjectRawImpl(code = subject_code, format_standard = project_impl@format_standard, parent_path = parent_path)
        }
        subject_impl <- RAVESubjectDerivativeImpl(project = project_impl, subject_raw = subject_raw)
        private$impl <- subject_impl
      }

      if(strict) {
        # check paths
        project_subject_path <- rave_path(private$impl, storage = "project_subject")
        if(!dir_exists(project_subject_path)) {
          stop(sprintf("RAVE subject folder is missing for [%s]", private$impl@id))
        }
      }

      private$.preprocess <- RAVEPreprocessSettings$new(subject = self, read_only = TRUE)
      private$.cached_config <- fastmap2()
    },

    #' @description get subject meta data located in \code{"meta/"} folder
    #' @param meta_type choices are 'electrodes', 'frequencies', 'time_points',
    #' 'epoch', 'references'
    #' @param meta_name if \code{meta_type='epoch'}, read in
    #' \code{'epoch_<meta_name>.csv'}; if \code{meta_type='references'},
    #' read in \code{'reference_<meta_name>.csv'}.
    #' @param strict whether to raise errors if the files are missing; default
    #' is true; alternative is to return \code{NULL} on missing
    #' @seealso \code{\link{load_meta2}}
    #' @returns data frame
    meta_data = function(
      meta_type = c('electrodes', 'frequencies', 'time_points', 'epoch', 'references'),
      meta_name = 'default', strict = TRUE
    ){
      meta_type <- match.arg(meta_type)
      meta_file <- rave_path(private$impl, storage = meta_type, meta_name = meta_name)
      if(length(meta_file) != 1 || is.na(meta_file) || !file_exists(meta_file)) {
        if(strict) {
          stop(sprintf("Invalid metadata path for type [%s] and name [%s]", meta_type, paste(meta_name, collapse = "")))
        } else {
          return(NULL)
        }
      }

      switch (
        meta_type,
        "electrodes" = {
          return(load_electrodes_csv(meta_file))
        },
        "time_points" = {
          return(safe_read_csv(meta_file, colClasses = c(Block = 'character')))
        },
        "frequencies" = {
          return(safe_read_csv(meta_file, colClasses = c(Frequency = 'numeric')))
        },
        "epoch" = {
          default_cols <- c('Block', 'Time', 'Trial', 'Condition', 'Duration', 'ExcludedElectrodes')
          epochs <- utils::read.csv(meta_file, header = TRUE, stringsAsFactors = FALSE, colClasses = 'character')

          # check blocks in case block leading 0s are removed by excel
          preprocess_yaml <- file_path(rave_path(private$impl, storage = "preprocess"), 'rave.yaml')
          if(file_exists(preprocess_yaml)) {
            preproc_info <- load_yaml(preprocess_yaml)
            if(length(preproc_info$blocks)){
              pass_test <- TRUE
              # let's check block!
              invalid_blocks <- !epochs$Block %in% preproc_info$blocks
              if(any(invalid_blocks)){
                t1 <- data.frame(idx = seq_along(epochs$Block), block = epochs$Block, stringsAsFactors = FALSE)
                numeric_blocks <- suppressWarnings({ as.numeric(preproc_info$blocks) })
                t2 <- data.frame(block = preproc_info$blocks, numblock = numeric_blocks, value = preproc_info$blocks, stringsAsFactors = FALSE)
                t1 <- merge(t1, t2, all.x = TRUE, by.x = 'block', by.y = 'block')
                t1 <- merge(t1[, c('block', 'idx', 'value')], t2, all.x = TRUE, by.x = 'block', by.y = 'numblock', suffixes = c('1', '2'))
                sel <- is.na(t1$value1)
                t1$value1[sel] <- t1$value2[sel]

                if(any(is.na(t1$value1))){
                  # block cannot find
                  # TODO
                }

                epochs$Block <- t1$value1[order(t1$idx)]
              }
            }
          }

          epochs$Time <- as.numeric(epochs$Time)
          epochs$Trial <- as.numeric(epochs$Trial)
          epochs$Duration %?<-% NA
          epochs$Duration <- as.numeric(epochs$Duration)

          epochs$Condition %?<-% 'NoCondition'
          epochs$Condition[is.na(epochs$Condition)] <- 'NoCondition'
          epochs$Condition <- as.character(epochs$Condition)

          epochs$ExcludedElectrodes %?<-% ''
          # sort column orders
          nms <- names(epochs)
          nms <- c(default_cols, nms[!nms %in% default_cols])
          epochs <- epochs[, nms]
          # get column names with leading "Event_xxx"
          events <- nms[grepl('^Event_.+', nms)]
          for(evt in events){
            epochs[[evt]] <- as.numeric(epochs[[evt]])
          }
          return(epochs)
        },
        "snapshot" = {
          # originally called "info"
          # info_file <- file.path(meta_dir, 'info.yaml')
          return(ieegio::io_read_yaml(meta_file))
        },
        "time_excluded" = {
          # file <- file.path(meta_dir, 'time_excluded.csv')
          return(safe_read_csv(meta_file, header = TRUE, stringsAsFactors = FALSE, colClasses = c('character', 'numeric', 'numeric')))
        },
        "references" = {
          ref_tbl <- safe_read_csv(meta_file, header = TRUE, stringsAsFactors = FALSE)
          nms <- names(ref_tbl)
          dft <- c("Electrode", "Group", "Reference", "Type")
          ref_tbl <- ref_tbl[, c(dft[dft %in% nms], nms[!nms %in% dft])]
          ref_tbl$Electrode <- as.integer(ref_tbl$Electrode)
          return(ref_tbl)
        },
        {
          NULL
        }
      )
      if(strict) {
        stop(sprintf("Invalid metadata path for type [%s] and name [%s]", meta_type, paste(meta_name, collapse = "")))
      } else {
        return(NULL)
      }
    },

    #' @description get valid electrode numbers
    #' @param reference_name character, reference name, see \code{meta_name}
    #' in \code{self$meta_data} or \code{\link{load_meta2}} when
    #' \code{meta_type} is 'reference'
    #' @param refresh whether to reload reference table before obtaining data,
    #' default is false
    #' @returns integer vector of valid electrodes
    valid_electrodes = function(reference_name = NULL, refresh = FALSE){
      if(missing(reference_name) || is.null(reference_name)) {
        reference_name <- "default" %OF% self$reference_names
      }
      if(refresh){
        private$.reference_tables[[reference_name]] <- self$meta_data(
          meta_type = 'references', meta_name = reference_name)
      } else {
        private$.reference_tables[[reference_name]] %?<-% self$meta_data(
          meta_type = 'references', meta_name = reference_name)
      }
      ref_table <- private$.reference_tables[[reference_name]]
      as.integer(ref_table$Electrode[ref_table$Reference != ''])
    },

    #' @description create subject's directories on hard disk
    #' @param include_freesurfer whether to create 'FreeSurfer' path
    initialize_paths = function(include_freesurfer = TRUE){
      dir_create2(self$rave_path)
      dir_create2(self$preprocess_path)
      dir_create2(self$data_path)
      dir_create2(self$reference_path)
      dir_create2(self$data_path)
      dir_create2(self$meta_path)
      dir_create2(self$pipeline_path)
      dir_create2(self$note_path)

      imaging_path <- self$imaging_path
      dir_create2(file_path(imaging_path, "coregistration"))
      dir_create2(file_path(imaging_path, "log"))
      dir_create2(file_path(imaging_path, "scripts"))
      dir_create2(file_path(imaging_path, "inputs"))

      # save preprocess
      self$preprocess_settings$save()

      if(include_freesurfer){
        if(is.na(self$freesurfer_path) || !dir_exists(self$freesurfer_path)){
          dir_create2(self$imaging_path)
        }
      }
    },

    #' @description set default key-value pair for the subject, used by 'RAVE'
    #' modules
    #' @param key character
    #' @param value value of the key
    #' @param namespace file name of the note (without post-fix)
    #' @returns The same as \code{value}
    set_default = function(key, value, namespace = "default"){

      stopifnot2(is.character(key) && length(key) == 1, msg = "`key` must be a character of length 1")
      stopifnot2(is.character(namespace) && length(namespace) == 1, msg = "`namespace` must be a character of length 1")

      stopifnot2(!grepl("[^A-Za-z0-9_-]", namespace), msg = "`namespace` can only contain letters, digits, dash (-), and/or underscore (_)")

      force(value)
      if(!dir_exists(self$note_path)){
        dir_create2(self$note_path)
      }
      default_path <- file.path(self$note_path, sprintf("%s.json", namespace))
      default_path_backup <- file.path(self$note_path, sprintf("%s.yaml", namespace))
      defaults <- fastmap2()
      exists <- FALSE
      if(file.exists(default_path)){
        try(silent = TRUE, {
          load_json(default_path, map = defaults)
          exists <- TRUE
        })
      }

      if (!exists && file.exists(default_path_backup)) {
        try(silent = TRUE, {
          load_yaml(default_path_backup, map = defaults)
          exists <- TRUE
        })
      }

      old_val <- defaults[[key]]
      if(is.null(old_val)) {
        defaults[[key]] <- structure(
          list(), entry_value = value, timestamp = Sys.time(),
          class = "raveio-subject-entry"
        )
      } else if( !identical(attr(old_val, "entry_value"), value) ){
        defaults[[key]] <- structure(
          list(), entry_value = value, timestamp = Sys.time(),
          previous_value = old_val, class = "raveio-subject-entry"
        )
      }

      # defaults[[key]] <- value
      tmpfile <- tempfile()
      on.exit({ unlink(tmpfile) })
      save_json(x = as.list(defaults), con = tmpfile, serialize = TRUE)
      file.copy(tmpfile, default_path, overwrite = TRUE, recursive = FALSE)

      # get backup yaml format
      entry_names <- sort(names(defaults))
      entries <- structure(
        lapply(entry_names, function(nm) {
          val <- defaults[[nm]]
          if(inherits(val, "raveio-subject-entry")) {
            return(attr(val, "entry_value"))
          }
          return(val)
        }),
        names = entry_names
      )
      save_yaml(x = entries, file = tmpfile)
      file.copy(tmpfile, default_path_backup, overwrite = TRUE, recursive = FALSE)
      invisible(value)
    },

    #' @description get default key-value pairs for the subject, used by 'RAVE'
    #' modules
    #' @param ... single key, or a vector of character keys
    #' @param default_if_missing default value is any key is missing
    #' @param simplify whether to simplify the results if there is only one key
    #' to fetch; default is \code{TRUE}
    #' @param namespace file name of the note (without post-fix)
    #' @returns A named list of key-value pairs, or if one key is specified and
    #' \code{simplify=TRUE}, then only the value will be returned.
    get_default = function(..., default_if_missing = NULL, simplify = TRUE,
                           namespace = "default"){
      stopifnot2(is.character(namespace) && length(namespace) == 1, msg = "`namespace` must be a character of length 1")
      stopifnot2(!grepl("[^A-Za-z0-9_-]", namespace), msg = "`namespace` can only contain letters, digits, dash (-), and/or underscore (_)")
      default_path <- file_path(self$note_path, sprintf("%s.json", namespace))
      default_path_backup <- file_path(self$note_path, sprintf("%s.yaml", namespace))

      defaults <- fastmap2(missing_default = default_if_missing)

      exists <- FALSE
      if(file.exists(default_path)){
        tryCatch({
          load_json(con = default_path, map = defaults)
          exists <- TRUE
        }, error = function(){})
      }

      if (!exists && file.exists(default_path_backup)) {
        tryCatch({
          load_yaml(default_path_backup, map = defaults)
          exists <- TRUE
        }, error = function(){})
      }

      ndots <- ...length()
      if(ndots == 0) {
        simplify <- FALSE
      }
      re <- defaults[...]
      re <- structure(
        names = names(re),
        lapply(re, function(val) {
          if(inherits(val, "raveio-subject-entry")) {
            return(attr(val, "entry_value"))
          } else {
            return(val)
          }
        })
      )

      if(simplify && length(re) == 1){
        re <- re[[1]]
      }
      re
    },

    #' @description get summary table of all the key-value pairs used by 'RAVE'
    #' modules for the subject
    #' @param namespaces namespaces for the entries; see method
    #' \code{get_default} or \code{set_default}. Default is all possible
    #' namespaces
    #' @param include_history whether to include history entries; default is
    #' false
    #' @returns A data frame with four columns: \code{'namespace'} for the group
    #' name of the entry (entries within the same namespace usually share same
    #' module), \code{'timestamp'} for when the entry was registered.
    #' \code{'entry_name'} is the name of the entry. If \code{include_history}
    #' is true, then multiple entries with the same \code{'entry_name'} might
    #' appear since the obsolete entries are included. \code{'entry_value'}
    #' is the value of the corresponding entry.
    get_note_summary = function(namespaces, include_history = FALSE) {

      if(missing(namespaces)) {
        # get all possible namespaces
        namespaces <- list.files(
          path = self$note_path,
          pattern = "\\.(json|yaml)$",
          all.files = TRUE,
          recursive = FALSE,
          full.names = FALSE,
          ignore.case = TRUE,
          include.dirs = FALSE,
          no.. = TRUE
        )
        namespaces <- unique(gsub(pattern = "\\.(json|yaml)$", replacement = "",
                                  x = namespaces, ignore.case = TRUE))
        if("default" %in% namespaces) {
          namespaces <- unique(c("default", namespaces))
        }
      }

      entries <- fastqueue2()
      extract_entries <- function(entry, name, namespace, is_root = FALSE) {
        if(inherits(entry, "raveio-subject-entry")) {
          timestamp <- attr(entry, "timestamp")
          if(!inherits(timestamp, "POSIXct")) {
            timestamp <- NA
          }
          item <- list(
            namespace = namespace,
            name = name,
            timestamp = timestamp,
            value = attr(entry, "entry_value"),
            status = ifelse(is_root, "current", "obsolete")
          )
        } else {
          item <- list(
            namespace = namespace,
            name = name,
            timestamp = NA,
            value = entry,
            status = ifelse(is_root, "current", "obsolete")
          )
        }

        entries$add(item)

        # add previous record
        if( include_history ) {
          previous_entry <- attr(entry, "previous_value")
          if(!is.null(previous_entry)) {
            Recall(entry = previous_entry, name = name, namespace = namespace, is_root = FALSE)
          }
        }
      }

      lapply(namespaces, function(namespace) {
        default_path <- file.path(self$note_path, sprintf("%s.json", namespace))
        default_path_backup <- file.path(self$note_path, sprintf("%s.yaml", namespace))

        defaults <- fastmap2(missing_default = default_if_missing)

        exists <- FALSE

        if(file.exists(default_path)){
          try({
            load_json(con = default_path, map = defaults)
            exists <- TRUE
          })
        }

        if (!exists && file.exists(default_path_backup)) {
          try({
            load_yaml(default_path_backup, map = defaults)
            exists <- TRUE
          })
        }

        if(!exists) { return(NULL) }

        for(nm in names(defaults)) {
          if(nm != "") {
            try({ extract_entries(defaults[[nm]], name = nm, namespace = namespace, is_root = TRUE) })
          }
        }
        return()
      })
      notes <- entries$as_list()

      notes_df <- data.frame(
        timestamp = as.POSIXct(sapply(notes, "[[", "timestamp"), origin = "1960-01-01"),
        namespace = vapply(notes, "[[", "namespace", FUN.VALUE = ""),
        entry_name = vapply(notes, "[[", "name", FUN.VALUE = "")
      )
      notes_df$entry_value <- lapply(notes, "[[", "value")
      notes_df$status <- vapply(notes, "[[", "status", FUN.VALUE = "")

      notes_df
    },


    #' @description check and get subject's epoch information
    #' @param epoch_name epoch name, depending on the subject's meta files
    #' @param as_table whether to convert to \code{\link{data.frame}}; default
    #' is false
    #' @param trial_starts the start of the trial relative to epoch time;
    #' default is 0
    #' @returns If \code{as_table} is \code{FALSE}, then returns as
    #' \code{\link{RAVEEpoch}} instance; otherwise returns epoch table; will
    #' raise errors when file is missing or the epoch is invalid.
    get_epoch = function(epoch_name = "default", as_table = FALSE, trial_starts = 0){
      if(length(epoch_name) != 1){
        stop("Only one epoch is allowed at a time.")
      }
      if(!isTRUE(epoch_name %in% self$epoch_names)){
        stop("Subject ", self$subject_id, " has no epoch name called: ",
             sQuote(epoch_name), "\n  Please check folder\n    ",
             self$meta_path, "\n  and make sure ",
             sQuote(sprintf("epoch_%s.csv", epoch_name)), " exists.")
      }
      epoch <- RAVEEpoch$new(subject = self, name = epoch_name)
      if(!length(epoch$trials)){
        stop("Cannot load epoch file correctly: \n  > Epoch file is missing or corrupted, or there is no trial in the epoch file. \nA typical RAVE-epoch file contains 4 columns (case-sensitive): \n  Block (characters), Time (numerical), Trial (integer), Condition (characters).")
      }
      # trial starts from -1 sec but only 0.5 seconds are allowed
      invalid_trials <- unlist(lapply(epoch$trials, function(ii){
        info <- epoch$trial_at(ii, df = FALSE)
        if(info$Time + trial_starts < 0){
          return(ii)
        }
        return()
      }))

      if(any(invalid_trials)){
        stop("Trial ", deparse_svec(invalid_trials), " start too soon after the beginning of the sessions (less than ", sprintf("%.2f seconds", -trial_starts), "). Please adjust the trial start time (i.e. ", sQuote("Pre"), " if you are using the RAVE application).")
      }

      if( as_table ){
        epoch <- epoch$table
      }
      epoch
    },

    #' @description check and get subject's reference information
    #' @param reference_name reference name, depending on the subject's meta
    #' file settings
    #' @param simplify whether to only return the reference column
    #' @returns If \code{simplify} is true, returns a vector of reference
    #' electrode names, otherwise returns the whole table; will
    #' raise errors when file is missing or the reference is invalid.
    get_reference = function(reference_name, simplify = FALSE){
      if(length(reference_name) != 1){
        stop("Only one reference is allowed at a time.")
      }
      if(!isTRUE(reference_name %in% self$reference_names)){
        stop("Subject ", self$subject_id, " has no reference name called: ", sQuote(reference_name), "\n  Please check folder\n    ", self$meta_path, "\n  and make sure ", sQuote(sprintf("reference_%s.csv", reference_name)), " exists.")
      }

      reference_table <- self$meta_data(meta_type = 'reference', meta_name = reference_name)

      if(!is.data.frame(reference_table)){
        stop("Cannot load reference file correctly. A typical RAVE-reference file contains 4 columns (case-sensitive): Electrode (integer), Group (characters), Reference (characters), Type (characters).")
      }

      if(simplify){
        return(reference_table$Reference)
      }
      reference_table
    },

    #' @description check and get subject's electrode table with electrodes
    #' that are load-able
    #' @param electrodes characters indicating integers such as
    #' \code{"1-14,20-30"}, or integer vector of electrode numbers
    #' @param reference_name see method \code{get_reference}
    #' @param subset whether to subset the resulting data table
    #' @param simplify whether to only return electrodes
    #' @param warn whether to warn about missing electrodes; default is true
    #' @returns If \code{simplify} is true, returns a vector of electrodes
    #' that are valid (or won't be excluded) under given reference; otherwise
    #' returns a table. If \code{subset} is true, then the table will be
    #' subset and only rows with electrodes to be loaded will be kept.
    get_electrode_table = function(electrodes, reference_name,
                                   subset = FALSE, simplify = FALSE, warn = TRUE){
      preproc <- self$preprocess_settings
      all_electrodes <- self$electrodes
      if(missing(reference_name) || !length(reference_name)) {
        valid_electrodes <- all_electrodes
        reference_table <- NULL
      } else {
        valid_electrodes <- self$valid_electrodes(reference_name = reference_name)
        reference_table <- self$get_reference(reference_name, simplify = FALSE)
      }

      if(missing(electrodes)){
        load_electrodes <- valid_electrodes
      } else {
        # Get electrodes to be loaded
        load_electrodes <- parse_svec(electrodes)
      }

      # 1. get electrodes to be truly loaded
      load_electrodes <- load_electrodes[load_electrodes %in% valid_electrodes]
      if(!length(load_electrodes)) {
        stop("There is no valid electrodes to be loaded. The valid electrodes are: ", deparse_svec(valid_electrodes), ".")
      }
      imcomplete <- all_electrodes[
        all_electrodes %in% load_electrodes &
          !preproc$has_wavelet &
          self$electrode_types %in% "LFP"
      ]
      if(length(imcomplete)){
        stop("The following electrodes do not have power spectrum: \n  ", deparse_svec(imcomplete),
             "\nPlease run wavelet module first.")
      }

      electrode_table <- self$meta_data("electrodes", strict = FALSE)

      if(!is.data.frame(electrode_table)){

        if(length(self$electrodes)) {

          if(private$impl@project@format_standard == "bids") {
            bids_table <- get_bids_electrodes_table(self)
            if(nrow(bids_table) >= max(self$electrodes)) {
              bids_table <- bids_table[self$electrodes, ]
              bids_table$SignalType <- self$electrode_types
              electrode_table <- bids_table
            }
          }

          if(!is.data.frame(electrode_table)) {
            electrode_table <- data.frame(
              Electrode = self$electrodes,
              Coord_x = 0,
              Coord_y = 0,
              Coord_z = 0,
              Label = "NoLabel",
              SignalType = self$electrode_types
            )
          }
          save_meta2(electrode_table, meta_type = "electrodes",
                     project_name = self$project_name,
                     subject_code = self$subject_code)
          electrode_table <- self$meta_data("electrodes")
          if(warn) {
            ravepipeline::logger("Cannot load electrode.csv correctly. A basic RAVE-electrode file contains 5 columns (case-sensitive): Electrode (integer), Coord_x (numerical), Coord_y (numerical), Coord_y (numerical), Label (characters). Creating a blank electrode file.", level = "warning")
          }
        } else {
          stop("Cannot load electrode.csv correctly. A basic RAVE-electrode file contains 5 columns (case-sensitive): Electrode (integer), Coord_x (numerical), Coord_y (numerical), Coord_y (numerical), Label (characters).")
        }

      }

      if(!is.null(reference_table)){
        electrode_table <- merge(electrode_table, reference_table, by = 'Electrode', all.x = TRUE, all.y = FALSE)
        electrode_table$isLoaded <- electrode_table$Electrode %in% load_electrodes
        if(subset){
          electrode_table <- electrode_table[electrode_table$isLoaded, ]
        }
      }

      if(simplify){
        return(electrode_table$Electrode)
      }
      electrode_table
    },

    #' @description check and get subject's frequency table, time-frequency
    #' decomposition is needed.
    #' @param simplify whether to simplify as vector
    #' @returns If \code{simplify} is true, returns a vector of frequencies;
    #' otherwise returns a table.
    get_frequency = function(simplify = TRUE){
      frequency_table <- self$meta_data('frequencies')
      if(!is.data.frame(frequency_table)){
        stop("Cannot load frequency table. Please run wavelet first.")
      }
      if(simplify){
        return(frequency_table$Frequency)
      }
      frequency_table
    },


    #' @description list saved pipelines
    #' @param pipeline_name pipeline ID
    #' @param cache whether to use cache registry to speed up
    #' @param check whether to check if the pipelines exist
    #' @param all whether to list all pipelines; default is false; pipelines
    #' with the same label but older time-stamps will be hidden
    #' @returns A table of pipeline registry
    list_pipelines = function(pipeline_name, cache = FALSE, check = TRUE, all = FALSE) {

      registry <- NULL
      if( cache ) {
        # use cached registry
        registry_path <- file.path(self$pipeline_path, "pipeline-registry.csv")
        if(file.exists(registry_path)) {
          registry <- tryCatch({
            registry <- data.table::fread(
              registry_path,
              stringsAsFactors = FALSE,
              colClasses = c(
                project = "character",
                subject = "character",
                pipeline_name = "character",
                timestamp = "POSIXct",
                label = "character",
                directory = "character"
              ), na.strings = "n/a"
            )
            stopifnot(all(c("project", "subject", "pipeline_name", "timestamp", "label", "directory") %in% names(registry)))
            if(nrow(registry)) {
              if(!length(registry$policy)) {
                registry$policy <- "default"
              }
              if(!length(registry$version)) {
                registry$version <- "0.0.0.9000"
              }
            }
            registry
          }, error = function(...) { NULL })
          if(nrow(registry)) {
            if(check) {
              registry <- registry[registry$pipeline_name == pipeline_name, ]
              registry <- registry[dir.exists(file.path(self$pipeline_path, registry$pipeline_name, registry$directory)), ]
            }
          }
        }
      }

      if(!is.data.frame(registry) || nrow(registry) == 0) {
        pipeline_paths <- file.path(self$pipeline_path, pipeline_name)
        prefix <- sprintf("^%s-", pipeline_name)
        re <- list.files(
          pipeline_paths,
          pattern = prefix,
          include.dirs = TRUE,
          all.files = FALSE,
          ignore.case = TRUE,
          no.. = TRUE,
          recursive = FALSE,
          full.names = FALSE
        )
        if( check && length(re) ) {
          re <- re[dir.exists(file.path(pipeline_paths, re))]
        }
        re <- lapply(re, function(name) {
          tryCatch({
            item <- strsplit(gsub(prefix, "", name, ignore.case = TRUE), "-", fixed = TRUE)[[1]]
            idx <- length(item)
            timestamp <- as.POSIXct(strptime(paste(item[idx], collapse = "-"), "%Y%m%dT%H%M%S"))
            label <- paste(item[-idx], collapse = "-")
            # check fork policy
            path_fork_info <- file.path(pipeline_paths, name, "_fork_info")
            if(file.exists(path_fork_info)) {
              info <- readRDS(path_fork_info)
            } else {
              info <- list(
                policy = "default",
                version = "0.0.0.9000"
              )
            }
            list(
              project = self$project_name,
              subject = self$subject_code,
              pipeline_name = pipeline_name,
              timestamp = timestamp,
              label = label,
              directory = name,
              policy = info$policy,
              version = info$version
            )
          }, error = function(...) { NULL })
        })

        registry <- data.table::rbindlist(re)
      }

      if(!all && is.data.frame(registry) && nrow(registry) > 0) {
        # remove duplicated labels
        registry <- registry[order(registry$label, registry$timestamp, decreasing = TRUE, na.last = TRUE), ]
        registry <- data.table::rbindlist(lapply(split(registry, registry$label), function(sub) {
          sub[1, ]
        }), use.names = FALSE)
      }

      registry
    },

    #' @description load saved pipeline
    #' @param directory pipeline directory name
    #' @returns A \code{PipelineTools} instance
    load_pipeline = function(directory) {

      # directory <- "power_explorer-NA-20240822T184419"
      pipeline_name <- strsplit(directory, "-", fixed = TRUE)[[1]][[1]]
      pipeline_path <- file.path(self$pipeline_path, pipeline_name, directory)
      if(!file.exists(pipeline_path)) {
        stop("Unable to find pipeline [", directory, "] from subject ", self$subject_id)
      }
      ravepipeline::pipeline_from_path(pipeline_path)
    }

  ),
  active = list(

    #' @field @impl the internal object
    `@impl` = function() {
      private$impl
    },

    #' @field project project instance of current subject; see
    #' \code{\link{RAVEProject}}
    project = function(){
      as_rave_project(private$impl@project, strict = FALSE)
    },

    #' @field project_name character string of project name
    project_name = function(){
      format(private$impl@project)
    },

    #' @field subject_code character string of subject code
    subject_code = function(){
      private$impl@subject_raw@code
    },

    #' @field subject_id subject ID: \code{"project/subject"}
    subject_id = function(){
      private$impl@id
    },

    #' @field path subject root path
    path = function(){
      rave_path(private$impl, storage = "project_subject")
    },

    #' @field rave_path 'rave' directory under subject root path
    rave_path = function(){
      file_path(rave_path(private$impl, storage = "project_subject"), "rave")
    },

    #' @field meta_path meta data directory for current subject
    meta_path = function(){
      rave_path(private$impl, storage = "meta")
    },

    #' @field imaging_path root path to imaging processing folder
    imaging_path = function() {
      rave_path(private$impl, storage = "rave_imaging")
    },

    #' @field freesurfer_path 'FreeSurfer' directory for current subject. If
    #' no path exists, values will be \code{NA}
    freesurfer_path = function(){
      rave_path(private$impl, storage = "freesurfer")
    },

    #' @field preprocess_path preprocess directory under subject 'rave' path
    preprocess_path = function(){
      rave_path(private$impl, storage = "preprocess")
    },

    #' @field data_path data directory under subject 'rave' path
    data_path = function(){
      rave_path(private$impl, storage = "signals")
    },

    #' @field cache_path path to 'FST' copies under subject 'data' path
    cache_path = function(){
      rave_path(private$impl, storage = "cache")
    },

    #' @field pipeline_path path to pipeline scripts under subject's folder
    pipeline_path = function(){
      rave_path(private$impl, storage = "pipeline")
    },

    #' @field note_path path that stores 'RAVE' related subject notes
    note_path = function(){
      rave_path(private$impl, storage = "notes")
    },

    #' @field epoch_names possible epoch names
    epoch_names = function(){
      regexp <- '^epoch_([a-zA-Z0-9_]+)\\.[cC][sS][vV]$'
      fs <- list.files(self$meta_path, pattern = regexp, ignore.case = TRUE)
      match_info <- regexec(regexp, fs)
      nms <- unlist(lapply(regmatches(fs, match_info), function(m) { m[[2]] }))
      nms <- sort(unique(nms))
      if(length(nms)) {
        nms <- nms[!startsWith(nms, "_")]
      }
      nms
    },

    #' @field reference_names possible reference names
    reference_names = function(){
      regexp <- '^reference_([a-zA-Z0-9_]+)\\.[cC][sS][vV]$'
      fs <- list.files(self$meta_path, pattern = regexp, ignore.case = TRUE)
      match_info <- regexec(regexp, fs)
      nms <- unlist(lapply(regmatches(fs, match_info), function(m) { m[[2]] }))
      nms <- sort(unique(nms))
      if(length(nms)) {
        nms <- nms[!startsWith(nms, "_")]
      }
      nms
    },

    #' @field reference_path reference path under 'rave' folder
    reference_path = function(){
      rave_path(private$impl, storage = "reference")
    },

    #' @field preprocess_settings preprocess instance; see
    #' \code{\link{RAVEPreprocessSettings}}
    preprocess_settings = function(){
      private$.preprocess
    },

    #' @field blocks subject experiment blocks in current project
    blocks = function(){
      private$.preprocess$blocks
    },

    #' @field electrodes all electrodes, no matter excluded or not
    electrodes = function(){
      private$.preprocess$electrodes
    },

    #' @field raw_sample_rates voltage sample rate
    raw_sample_rates = function(){
      private$.preprocess$sample_rates
    },

    #' @field power_sample_rate power spectrum sample rate
    power_sample_rate = function(){
      private$.preprocess$wavelet_params$downsample_to
    },

    #' @field has_wavelet whether electrodes have wavelet transforms
    has_wavelet = function(){
      private$.preprocess$has_wavelet
    },

    #' @field notch_filtered whether electrodes are Notch-filtered
    notch_filtered = function(){
      private$.preprocess$notch_filtered
    },

    #' @field electrode_types electrode signal types
    electrode_types = function(){
      private$.preprocess$electrode_types
    },

    #' @field electrode_composed composed electrode channels, not actual
    #' physically contacts, but is generated from those physically ones
    electrode_composed = function() {
      private$.preprocess$electrode_composed
    }
  )
)



#' Get \code{\link{RAVESubject}} instance from character
#' @param subject_id character in format \code{"project/subject"}
#' @param strict whether to check if subject directories exist or not
#' @param reload whether to reload (update) subject information, default is true
#' @returns \code{\link{RAVESubject}} instance
#' @seealso \code{\link{RAVESubject}}
#' @export
as_rave_subject <- function(subject_id, strict = TRUE, reload = TRUE){
  if(inherits(subject_id, 'RAVESubject')){
    if(reload) {
      return(restore_subject_instance(subject_id$subject_id, strict = strict))
    } else {
      return(subject_id)
    }
  } else if(inherits(subject_id, 'RAVEPreprocessSettings')) {
    if(reload) {
      return(restore_subject_instance(subject_id$subject$subject_id, strict = strict))
    } else {
      return(subject_id$subject)
    }
  } else {
    return(restore_subject_instance(subject_id, strict = strict))
  }

}

restore_subject_instance <- function(subject_id, strict = FALSE) {
  if(inherits(subject_id, 'RAVESubject')){
    return(subject_id)
  } else {
    if(inherits(subject_id, "Subject")) {
      # RAVE 1.0 subject instance
      stopifnot2(is.character(subject_id$id),
                 msg = "`as_rave_subject`: Cannot find subject ID from the given input")
      subject_id <- subject_id$id
    }
    # if(startsWith(subject_id, "@meta_analysis")) {
    #   subject_id <- gsub("^@meta_analysis/", "", subject_id)
    #   return( RAVEMetaSubject$new(subject_id) )
    # } else {
      return( RAVESubject$new(subject_id, strict = strict) )
    # }
  }
}


as_bids_subject <- function(subject, strict = FALSE) {
  subject <- restore_subject_instance(subject, strict = strict)
  bids_subject <- bidsr::bids_subject(
    project = subject$`@impl`@project@parent_path,
    subject_code = subject$subject_code,
    strict = strict
  )
  bids_subject
}

get_bids_electrodes_table <- function(subject) {
  bids_subject <- as_bids_subject(subject)
  bids_raw_path <- bidsr::resolve_bids_path(bids_subject, storage = "raw")
  if(!dir_exists(bids_raw_path)) { return(NULL) }

  query <- bidsr::query_bids(bids_subject, list(
    storage = "raw",
    sidecars = TRUE,
    data_types = c("ieeg"),
    suffixes = c("electrodes", "coordsystem", "channels")
  ))

  # find electrodes.tsv
  sel <- tolower(query$extension) == "tsv" & tolower(query$suffix) == "electrodes"
  if(!any(sel)) { return(NULL) }

  electrode_file <- query$parsed[sel][[1]]
  electrode_file <- bidsr::resolve_bids_path(x = bids_subject@project,
                                             format(electrode_file),
                                             storage = "raw")

  electrode_tabular <- bidsr::as_bids_tabular(electrode_file)

  # # now find channels
  # sel <- tolower(query$extension) == "tsv" & tolower(query$suffix) == "channels"
  # if(any(sel)) {
  #   channel_file <- bidsr::resolve_bids_path(x = bids_subject@project,
  #                                            format(query$parsed[sel][[1]]),
  #                                            storage = "raw")
  #   channel_tabular <- bidsr::as_bids_tabular(channel_file)
  #   if(is.integer(channel_tabular$content$name)) {
  #     # Label of the channel. When a corresponding electrode is specified in
  #     # _electrodes.tsv, the name of that electrode MAY be specified here and
  #     # the reference electrode name MAY be provided in the reference column.
  #     electrodes <- channel_tabular$content$name
  #   }
  # }
  content <- electrode_tabular$content
  tbl <- data.frame(
    Electrode = seq_len(nrow(content)),
    T1R = content$x,
    T1A = content$y,
    T1S = content$z,
    Label = content$name,
    Radius = content$size / 2
  )
  brain <- rave_brain(subject, include_electrodes = FALSE)
  if(length(brain)) {
    brain$set_electrodes(electrodes = tbl, coord_sys = "scannerRAS")
    tbl <- brain$electrodes$raw_table
  } else {
    tbl$Coord_x <- 0
    tbl$Coord_y <- 0
    tbl$Coord_z <- 0
  }
  tbl
}

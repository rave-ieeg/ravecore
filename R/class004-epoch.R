#' @title Definition for epoch class
#' @description Trial epoch, contains the following information: \code{Block}
#' experiment block/session string; \code{Time} trial onset within that block;
#' \code{Trial} trial number; \code{Condition} trial condition. Other optional
#' columns are \code{Event_xxx} (starts with "Event"). Column
#' \code{ExcludedHint} (logical, always present; missing or blank values are
#' \code{FALSE}) marks trials that analyses may exclude; see
#' \code{exclude_trials} and \code{save}. An epoch whose name ends with
#' \code{"_OutlierRemoved"} is a trimmed copy: marked trials are removed,
#' trials are renumbered from 1, and \code{OriginalTrial} holds the trial
#' numbers of the epoch it was generated from.
#' @examples
#'
#' # Please download DemoSubject ~700MB from
#' # https://github.com/beauchamplab/rave/releases/tag/v0.1.9-beta
#'
#' if(has_rave_subject("demo/DemoSubject")) {
#'
#' # Load meta/epoch_auditory_onset.csv from subject demo/DemoSubject
#' epoch <-RAVEEpoch$new(subject = 'demo/DemoSubject',
#'                       name = 'auditory_onset')
#'
#' # first several trials
#' head(epoch$table)
#'
#' # query specific trial
#' old_trial1 <- epoch$trial_at(1)
#'
#' # Create new trial or change existing trial
#' epoch$set_trial(Block = '008', Time = 10,
#'                 Trial = 1, Condition = 'AknownVmeant')
#' new_trial1 <- epoch$trial_at(1)
#'
#' # Compare new and old trial 1
#' list(old_trial1, new_trial1)
#'
#' # To get updated trial table, must update first
#' epoch$update_table()
#' head(epoch$table)
#'
#' # Mark trials 1 and 3; pipelines decide whether to drop them
#' epoch$exclude_trials(1, 3)
#' epoch$excluded_trials
#'
#' }
#'
#' @export
RAVEEpoch <- R6::R6Class(
  classname = "RAVEEpoch",
  lock_objects = FALSE,
  class = TRUE,
  portable = TRUE,
  inherit = RAVESerializable,
  private = list(
    # Reads `meta/epoch_<name>.csv` with `ExcludedHint` as logical: values
    # `as.logical()` understands, or 1/0; blank and `NA` are FALSE. A column
    # with any other value is ignored (all FALSE) with a warning.
    read_table = function(name) {
      table <- self$subject$meta_data("epoch", name)
      hint <- trimws(table$ExcludedHint)
      if (!length(hint)) {
        # missing column: treated as blank
        hint <- rep("", nrow(table))
      }
      excluded <- as.logical(hint)
      excluded[hint %in% "1"] <- TRUE
      excluded[hint %in% "0"] <- FALSE
      if (any(is.na(excluded) & !is.na(hint) & !hint %in% c("", "NA"))) {
        ravepipeline::logger(
          sprintf("Epoch [%s]: column `ExcludedHint` must be logical (TRUE/FALSE, 1/0, or blank). Ignoring this column: no trial is marked as excluded.", name),
          level = "warning"
        )
        excluded[] <- FALSE
      }
      excluded[is.na(excluded)] <- FALSE
      table$ExcludedHint <- excluded
      if (!is.null(table$OriginalTrial)) {
        table$OriginalTrial <- suppressWarnings(as.integer(table$OriginalTrial))
      }
      table
    },

    # Drops trials marked `ExcludedHint` and renumbers trials from 1; the hint
    # column is removed. `OriginalTrial` is set from `Trial` when
    # `set_original` is TRUE, otherwise kept as is.
    trim_table = function(table, set_original = FALSE) {
      table <- as.data.frame(table)
      if (set_original && !length(table$OriginalTrial)) {
        table$OriginalTrial <- as.integer(table$Trial)
      }
      table <- table[!table$ExcludedHint, , drop = FALSE]
      table <- table[order(table$Trial), , drop = FALSE]
      table$Trial <- seq_len(nrow(table))
      table$ExcludedHint <- NULL
      rownames(table) <- NULL
      table
    },

    # (Re)loads the trials from `meta/epoch_<name>.csv`. `*_OutlierRemoved`
    # epochs are always trimmed: rows marked `ExcludedHint` in the file (hand
    # edits) are dropped.
    load = function() {
      name <- self$name
      self$data$`@reset`()
      self$.columns <- character(0)
      if (name %in% self$subject$epoch_names) {
        table <- private$read_table(name)
        if (endsWith(name, "_OutlierRemoved")) {
          if (any(table$ExcludedHint)) {
            ravepipeline::logger(
              sprintf("Epoch [%s]: dropping %d trial(s) marked as `ExcludedHint` in the file",
                      name, sum(table$ExcludedHint)),
              level = "info"
            )
          }
          table <- private$trim_table(table)
        }
        table <- data.table::as.data.table(table)
        if (nrow(table)) {
          # One 1-row table per trial. ravecore is not data.table-aware, so
          # `table[ii, ]` ends up in `[.data.frame` anyway; calling it directly
          # gives the same rows without data.table's per-call dispatch.
          self$data[as.character(table$Trial)] <- lapply(
            seq_len(nrow(table)), function(ii) `[.data.frame`(table, ii, ))
        }
        cnames <- names(table)
        cnames <- cnames[!cnames %in% c(BASIC_EPOCH_TABLE_COLUMNS, "ExcludedHint", "X")]
        cnames <- cnames[!grepl("^X\\.[0-9]+$", cnames)]
        self$.columns <- cnames
      }
      self$update_table()
    }
  ),
  public = list(

    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      list(
        namespace = "ravecore",
        r6_generator = "RAVEEpoch",
        data = list(
          # subject = self$subject$subject_id,#`@marshal`(),
          subject = self$subject$`@marshal`(),
          name = self$name,
          table = self$update_table()
        )
      )
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(identical(object$r6_generator, "RAVEEpoch"))
      # subject <- RAVESubject$new(object$data$subject)
      subject <- RAVESubject$public_methods$`@unmarshal`(object$data$subject)
      epoch <- RAVEEpoch$new(subject = subject, name = "__placeholder__")

      epoch$name <- object$data$name
      table <- data.table::as.data.table(object$data$table)
      epoch$data$`@reset`()
      if (nrow(table)) {
        # same rows as `table[ii, ]`, see `private$load()`
        epoch$data[as.character(table$Trial)] <- lapply(
          seq_len(nrow(table)), function(ii) `[.data.frame`(table, ii, ))
      }
      cnames <- names(table)
      cnames <- cnames[!cnames %in% c(BASIC_EPOCH_TABLE_COLUMNS, "ExcludedHint", "X")]
      cnames <- cnames[!grepl("^X\\.[0-9]+$", cnames)]
      epoch$.columns <- cnames
      epoch$update_table()
      return(epoch)
    },

    #' @field name epoch name, character
    name = character(0),

    #' @field subject \code{RAVESubject} instance
    subject = NULL,

    #' @field data a list of trial information, internally used
    data = NULL,

    #' @field table trial epoch table
    table = NULL,

    #' @field .columns epoch column names, internally used
    .columns = character(0),

    #' @description constructor
    #' @param subject \code{RAVESubject} instance or character
    #' @param name character, make sure \code{"epoch_<name>.csv"} is in meta
    #' folder
    initialize = function(subject, name) {

      stopifnot2(
        grepl("^[a-zA-Z0-9_]", name),
        msg = "epoch name can only contain letters[a-zA-Z] digits[0-9] and underscore[_]")

      self$subject <- RAVESubject$new(subject, strict = FALSE)
      self$name <- name

      self$data <- fastmap2()
      private$load()

    },

    #' @description get \code{ith} trial
    #' @param i trial number
    #' @param df whether to return as data frame or a list
    trial_at = function(i, df = TRUE) {
      cnames <- self$columns
      is_event <- grepl("^Event_.+$", cnames)

      re <- as.list(self$data[[as.character(i)]])
      if (length(re)) {
        # trials added without the column are not excluded
        re[["ExcludedHint"]] <- isTRUE(as.logical(re[["ExcludedHint"]]))
      }
      re <- re[cnames]
      if (any(is_event)) {
        re[is_event] <- as.numeric(re[is_event])
      }
      if (df) {
        re <- data.table::as.data.table(re, stringsAsFactors = FALSE)
      }
      re
    },

    #' @description manually update table field
    #' @returns \code{self$table}
    update_table = function() {
      cnames <- self$columns
      re <- unname(self$data[as.character(self$trials)])

      if (length(re)) {
        suppressWarnings({
          re <- data.table::rbindlist(re, use.names = TRUE, fill = TRUE, ignore.attr = FALSE)
        })
        for (cname in setdiff(cnames, names(re))) {
          data.table::set(re, j = cname, value = NA)
        }
        # trials added without the column are not excluded
        excluded <- as.logical(re[["ExcludedHint"]])
        excluded[is.na(excluded)] <- FALSE
        data.table::set(re, j = "ExcludedHint", value = excluded)
        for (cname in cnames[grepl("^Event_.+$", cnames)]) {
          data.table::set(re, j = cname, value = as.numeric(re[[cname]]))
        }
        re <- re[, cnames, with = FALSE]

        self$table <- as.data.frame(re)
      } else {
        self$table <- data.frame(
          Block = character(),
          Time = numeric(),
          Trial = integer(),
          Condition = character(),
          ExcludedHint = logical()
        )
      }

      re
    },

    #' @description set one trial
    #' @param Block block string
    #' @param Time time in second
    #' @param Trial positive integer, trial number
    #' @param Condition character, trial condition
    #' @param ... other key-value pairs corresponding to other optional columns;
    #' \code{ExcludedHint} defaults to \code{FALSE}
    set_trial = function(Block, Time, Trial, Condition, ...) {
      Trial <- as.integer(Trial)
      stopifnot2(isTRUE(Trial > 0), msg = "Invalid trial number, must be positive integer")

      stopifnot2(is.numeric(Time), msg = "Time must be numerical")

      stopifnot2(Block %in% self$subject$blocks, msg = sprintf("Invalid block [%s]", Block))
      row <- list(Block = Block, Time = Time, Trial = Trial, Condition = Condition, ...)
      stopifnot2(length(row[["ExcludedHint"]]) <= 1,
                 msg = "`ExcludedHint` must be a single logical value")
      row[["ExcludedHint"]] <- isTRUE(as.logical(row[["ExcludedHint"]]))
      self$data[[as.character(Trial)]] <- row

      dotnames <- ...names()
      more_cols <- setdiff(dotnames, c(self$.columns, "ExcludedHint"))
      if (length(more_cols)) {
        self$.columns <- c(self$.columns, more_cols)
      }
      self$trial_at(Trial)
    },

    #' @description Mark trials as excluded through the \code{ExcludedHint}
    #' column. Nothing is removed: pipelines decide whether to drop marked
    #' trials. Use \code{save} to write the marks to disk.
    #' @param ... trial numbers; flattened with \code{unlist(list(...))}
    #' @param add whether to add to the trials already marked (default);
    #' \code{FALSE} replaces them, so \code{exclude_trials(add = FALSE)}
    #' clears every mark
    #' @returns The epoch instance, invisibly
    exclude_trials = function(..., add = TRUE) {
      trials <- unlist(list(...))
      stopifnot2(
        !is.logical(trials) || !length(trials),
        msg = "`exclude_trials` expects trial numbers, not a logical mask"
      )
      all_trials <- self$trials
      trial_numbers <- suppressWarnings(as.integer(trials))
      found <- !is.na(trial_numbers) & trial_numbers %in% all_trials
      if (!all(found)) {
        ravepipeline::logger(
          sprintf("Epoch [%s]: ignoring trial(s) not found in the epoch: %s",
                  self$name, paste(unique(trials[!found]), collapse = ", ")),
          level = "warning"
        )
      }
      trial_numbers <- unique(trial_numbers[found])
      targets <- if (add) trial_numbers else all_trials
      if (length(targets)) {
        keys <- as.character(targets)
        self$data[keys] <- .mapply(function(row, excluded) {
          row <- as.list(row)
          row[["ExcludedHint"]] <- excluded
          row
        }, list(self$data[keys], targets %in% trial_numbers), NULL)
      }
      self$update_table()
      invisible(self)
    },

    #' @description Save the epoch to the subject's meta folder. A regular
    #' epoch writes \code{epoch_<name>.csv} with \code{ExcludedHint} (the
    #' existing file is renamed to a time-stamped backup first), then rebuilds
    #' \code{epoch_<name>_OutlierRemoved.csv}: the existing copy is always
    #' backed up and removed, and a new one is written when some (not all)
    #' trials are marked, without them, with trials renumbered from 1 and
    #' \code{OriginalTrial} holding the trial numbers of
    #' \code{epoch_<name>.csv}. An epoch whose name ends with
    #' \code{_OutlierRemoved} is saved trimmed the same way: its file is
    #' backed up and removed, then the unmarked trials are written. Its marked
    #' trials, including rows marked by hand in the replaced file, are also
    #' marked in the upstream epoch (the name without the suffix) when that
    #' epoch exists and the trial still matches by \code{Block} and
    #' \code{Time}; otherwise only the trimmed file is saved.
    #' @returns Paths of the written files, invisibly
    save = function() {
      name <- self$name
      stopifnot2(
        length(name) == 1 && grepl("^[a-zA-Z0-9][a-zA-Z0-9_]*$", name),
        msg = "Epoch name must start with a letter or digit and contain only letters, digits, and underscores"
      )
      meta_path <- self$subject$meta_path
      dir_create2(meta_path)
      path <- file_path(meta_path, sprintf("epoch_%s.csv", name))
      self$update_table()
      table <- self$table

      if (!endsWith(name, "_OutlierRemoved")) {
        stopifnot2(nrow(table) > 0, msg = sprintf("Epoch [%s] has no trial to save", name))
        paths <- safe_write_csv(table, file = path, row.names = FALSE)

        outlier_path <- file_path(meta_path, sprintf("epoch_%s_OutlierRemoved.csv", name))
        backup_file(outlier_path, remove = TRUE)
        if (all(table$ExcludedHint)) {
          ravepipeline::logger(
            sprintf("Epoch [%s]: every trial is marked as excluded; no `_OutlierRemoved` copy is written", name),
            level = "warning"
          )
        } else if (any(table$ExcludedHint)) {
          utils::write.csv(private$trim_table(table, set_original = TRUE),
                           file = outlier_path, row.names = FALSE)
          paths <- c(paths, outlier_path)
        }
        return(invisible(paths))
      }

      # trials to mark upstream: marked here, plus rows marked by hand in the
      # file about to be replaced (dropped when it was loaded)
      marked <- table[table$ExcludedHint, , drop = FALSE]
      if (file.exists(path)) {
        on_disk <- private$read_table(name)
        marked <- data.table::rbindlist(
          list(on_disk[on_disk$ExcludedHint, , drop = FALSE], marked),
          use.names = TRUE, fill = TRUE
        )
      }

      backup_file(path, remove = TRUE)
      paths <- character(0)
      trimmed <- private$trim_table(table)
      if (nrow(trimmed)) {
        utils::write.csv(trimmed, file = path, row.names = FALSE)
        paths <- path
      } else {
        ravepipeline::logger(
          sprintf("Epoch [%s]: every trial is marked as excluded; its file is backed up and removed", name),
          level = "warning"
        )
      }

      upstream_name <- sub("_OutlierRemoved$", "", name)
      if (nrow(marked) && upstream_name %in% self$subject$epoch_names) {
        # the trimmed file is saved regardless of the upstream epoch
        tryCatch({
          upstream <- RAVEEpoch$new(subject = self$subject, name = upstream_name)
          original <- marked$OriginalTrial
          if (is.null(original)) {
            original <- rep(NA_integer_, nrow(marked))
          }
          matched <- vapply(seq_len(nrow(marked)), function(ii) {
            if (is.na(original[[ii]])) { return(FALSE) }
            row <- upstream$data[[as.character(original[[ii]])]]
            !is.null(row) &&
              identical(as.character(row$Block), as.character(marked$Block[[ii]])) &&
              isTRUE(abs(as.numeric(row$Time) - as.numeric(marked$Time[[ii]])) < 1e-3)
          }, FALSE)
          if (!all(matched)) {
            ravepipeline::logger(
              sprintf("Epoch [%s]: %d excluded trial(s) cannot be matched in epoch [%s] (no `OriginalTrial`, or Block/Time changed); not marked there",
                      name, sum(!matched), upstream_name),
              level = "warning"
            )
          }
          previous <- upstream$excluded_trials
          upstream$exclude_trials(original[matched])
          if (!setequal(previous, upstream$excluded_trials)) {
            upstream_path <- file_path(meta_path, sprintf("epoch_%s.csv", upstream_name))
            paths <- c(paths, safe_write_csv(upstream$table, file = upstream_path, row.names = FALSE))
          }
        }, error = function(e) {
          ravepipeline::logger(
            sprintf("Epoch [%s]: cannot update epoch [%s] (%s); only the trimmed file is saved",
                    name, upstream_name, conditionMessage(e)),
            level = "warning"
          )
        })
      }

      # the in-memory epoch follows the saved file
      private$load()
      invisible(paths)
    },

    #' @description Get epoch column name that represents the desired event
    #' @param event a character string of the event, see
    #' \code{$available_events} for all available events; set to
    #' \code{"trial onset"}, \code{"default"}, or blank to use the default
    #' @param missing what to do if event is missing; default is to warn
    #' @returns If \code{event} is one of \code{"trial onset"},
    #' \code{"default"}, \code{""}, or \code{NULL}, then the result will be
    #' \code{"Time"} column; if the event is found, then return will be the
    #' corresponding event column. When the event is not found and
    #' \code{missing} is \code{"error"}, error will be raised; default is
    #' to return \code{"Time"} column, as it's trial onset and is mandatory.
    get_event_colname = function(event = "",
                                 missing = c("warning", "error", "none")) {
      missing <- match.arg(missing)
      event <- trimws(tolower(paste(event, collapse = " ")))
      if (event %in% c("trial onset", "", "default")) {
        return("Time")
      }
      cname <- sprintf(c("Event_%s", "Event%s"), event)
      cnames <- self$columns
      re <- cnames[tolower(cnames) %in% tolower(cname)]
      if ( length(re) ) {
        return(re[[1]])
      }
      msg <- sprintf("Cannot find event `%s`. Returning default `Time`.", event)
      switch(
        missing,
        "warning" = ravepipeline::logger(msg, level = "warning"),
        "error" = ravepipeline::logger(msg, level = "fatal")
      )
      return("Time")
    },

    #' @description Get condition column name that represents the desired
    #' condition type
    #' @param condition_type a character string of the condition type, see
    #' \code{$available_condition_type} for all available condition types;
    #' set to \code{"default"} or blank to use the default
    #' @param missing what to do if condition type is missing; default is to
    #' warn if the condition column is not found.
    #' @returns If \code{condition_type} is one of
    #' \code{"default"}, \code{""}, or \code{NULL}, then the result will be
    #' \code{"Condition"} column; if the condition type is found, then return
    #' will be the corresponding condition type column. When the condition type
    #' is not found and \code{missing} is \code{"error"}, error will be raised;
    #' default is to return \code{"Condition"} column, as it's the default
    #' and is mandatory.
    get_condition_colname = function(condition_type = "default",
                                     missing = c("error", "warning", "none")) {
      stopifnot(length(condition_type) == 1)
      missing <- match.arg(missing)
      condition_type <- tolower(condition_type)
      if ( condition_type %in% c("", "default") ) {
        return("Condition")
      }
      cname <- sprintf(c("Condition_%s", "Condition%s"), condition_type)
      cnames <- self$columns
      re <- cnames[tolower(cnames) %in% tolower(cname)]
      if ( length(re) ) {
        return(re[[1]])
      }
      msg <- sprintf("Cannot find condition type `%s`. Returning default `Condition`", condition_type)
      switch(
        missing,
        "warning" = ravepipeline::logger(sprintf("Cannot find condition type `%s`; returning default `Condition`", condition_type), level = "warning"),
        "error" = ravepipeline::logger(sprintf("Cannot find condition type `%s`", condition_type), level = "fatal")
      )
      return("Condition")
    }

  ),
  active = list(

    #' @field columns columns of trial table
    columns = function() {
      unique(c(BASIC_EPOCH_TABLE_COLUMNS, self$.columns, "ExcludedHint"))
    },

    #' @field n_trials total number of trials
    n_trials = function() {
      length(self$data)
    },

    #' @field trials trial numbers
    trials = function() {
      sort(as.integer(names(self$data)))
    },

    #' @field excluded_trials trial numbers whose \code{ExcludedHint} is
    #' \code{TRUE}
    excluded_trials = function() {
      trials <- self$trials
      rows <- self$data[as.character(trials)]
      is_excluded <- vapply(rows, function(row) {
        isTRUE(as.logical(row[["ExcludedHint"]]))
      }, FALSE, USE.NAMES = FALSE)
      trials[is_excluded]
    },

    #' @field available_events available events other than trial onset
    available_events = function() {
      cnames <- self$columns
      cnames <- cnames[startsWith(cnames, "Event")]
      if (!length(cnames)) { return("") }
      unique(c("", gsub("^Event[_]{0,1}", "", cnames)))
    },

    #' @field available_condition_type available condition type other than
    #' the default
    available_condition_type = function() {
      cnames <- self$columns
      cnames <- cnames[startsWith(cnames, "Condition")]
      if (!length(cnames)) { return("") }
      unique(c("", gsub("^Condition[_]{0,1}", "", cnames)))
    }
  )
)

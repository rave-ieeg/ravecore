#' Definition for epoch class
#' @description Trial epoch, contains the following information: \code{Block}
#' experiment block/session string; \code{Time} trial onset within that block;
#' \code{Trial} trial number; \code{Condition} trial condition. Other optional
#' columns are \code{Event_xxx} (starts with "Event").
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
#' }
#'
#' @export
RAVEEpoch <- R6::R6Class(
  classname = 'RAVEEpoch',
  lock_objects = FALSE,
  class = TRUE,
  portable = TRUE,
  inherit = RAVESerializable,
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
      lapply(seq_len(nrow(table)), function(ii) {
        row <- table[ii, ]
        epoch$data[[as.character(row$Trial)]] <- row
        return()
      })
      cnames <- names(table)
      cnames <- cnames[!cnames %in% c(BASIC_EPOCH_TABLE_COLUMNS, 'X')]
      cnames <- cnames[grepl('^X\\.[0-9]+$', cnames)]
      epoch$.columns <- cnames
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
    initialize = function(subject, name){

      stopifnot2(
        grepl('^[a-zA-Z0-9_]', name),
        msg = 'epoch name can only contain letters[a-zA-Z] digits[0-9] and underscore[_]')

      self$subject <- RAVESubject$new(subject, strict = FALSE)
      self$name <- name

      self$data <- fastmap2()

      if(name %in% self$subject$epoch_names){
        # load epoch
        table <- data.table::as.data.table(self$subject$meta_data('epoch', name))
        lapply(seq_len(nrow(table)), function(ii) {
          row <- table[ii,]
          self$data[[as.character(row$Trial)]] <- row
          return()
        })
        cnames <- names(table)
        cnames <- cnames[!cnames %in% c(BASIC_EPOCH_TABLE_COLUMNS, 'X')]
        cnames <- cnames[!grepl('^X\\.[0-9]+$', cnames)]
        self$.columns <- cnames
      }
      self$update_table()

    },

    #' @description get \code{ith} trial
    #' @param i trial number
    #' @param df whether to return as data frame or a list
    trial_at = function(i, df = TRUE){
      cnames <- self$columns
      is_event <- grepl('^Event_.+$', cnames)

      re <- as.list(self$data[[as.character(i)]])[cnames]
      if(any(is_event)) {
        re[is_event] <- as.numeric(re[is_event])
      }
      if(df){
        re <- data.table::as.data.table(re, stringsAsFactors = FALSE)
      }
      re
    },

    #' @description manually update table field
    #' @returns \code{self$table}
    update_table = function(){
      cnames <- self$columns
      is_event <- grepl('^Event_.+$', cnames)
      re <- unname(self$data[as.character(self$trials)])

      if(length(re)) {
        re <- lapply(re, function(item) {
          item <- as.list(item)[cnames]
          item[is_event] <- as.numeric(item[is_event])
          item
        })
        suppressWarnings({
          re <- data.table::rbindlist(re, use.names = TRUE, fill = TRUE, ignore.attr = FALSE)
          re <- re[, cnames, with = FALSE]
        })

        self$table <- as.data.frame(re)
      } else {
        self$table <- data.frame(
          Block = character(),
          Time = numeric(),
          Trial = integer(),
          Condition = character()
        )
      }

      re
    },

    #' @description set one trial
    #' @param Block block string
    #' @param Time time in second
    #' @param Trial positive integer, trial number
    #' @param Condition character, trial condition
    #' @param ... other key-value pairs corresponding to other optional columns
    set_trial = function(Block, Time, Trial, Condition, ...){
      Trial <- as.integer(Trial)
      stopifnot2(isTRUE(Trial > 0), msg = "Invalid trial number, must be positive integer")

      stopifnot2(is.numeric(Time), msg = "Time must be numerical")

      stopifnot2(Block %in% self$subject$blocks, msg = sprintf('Invalid block [%s]', Block))
      self$data[[as.character(Trial)]] <- list(Block = Block, Time = Time, Trial = Trial, Condition = Condition, ...)

      dotnames <- ...names()
      more_cols <- setdiff(dotnames, self$.columns)
      if(length(more_cols)){
        self$.columns <- c(self$.columns, more_cols)
      }
      self$trial_at(Trial)
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
      if(event %in% c("trial onset", "", "default")) {
        return("Time")
      }
      cname <- sprintf(c("Event_%s", "Event%s"), event)
      cnames <- self$columns
      re <- cnames[tolower(cnames) %in% tolower(cname)]
      if( length(re) ) {
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
      if( condition_type %in% c("", "default") ) {
        return("Condition")
      }
      cname <- sprintf(c("Condition_%s", "Condition%s"), condition_type)
      cnames <- self$columns
      re <- cnames[tolower(cnames) %in% tolower(cname)]
      if( length(re) ) {
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
    columns = function(){
      unique(c(BASIC_EPOCH_TABLE_COLUMNS, self$.columns))
    },

    #' @field n_trials total number of trials
    n_trials = function(){
      length(self$data)
    },

    #' @field trials trial numbers
    trials = function(){
      sort(as.integer(names(self$data)))
    },

    #' @field available_events available events other than trial onset
    available_events = function() {
      cnames <- self$columns
      cnames <- cnames[startsWith(cnames, "Event")]
      if(!length(cnames)) { return("") }
      unique(c("", gsub("^Event[_]{0,1}", "", cnames)))
    },

    #' @field available_condition_type available condition type other than
    #' the default
    available_condition_type = function() {
      cnames <- self$columns
      cnames <- cnames[startsWith(cnames, "Condition")]
      if(!length(cnames)) { return("") }
      unique(c("", gsub("^Condition[_]{0,1}", "", cnames)))
    }
  )
)

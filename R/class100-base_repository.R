#' 'RAVE' class for base repository
#' @description
#' The class is for creating child classes, to instantiate the class,
#' please use \code{\link{prepare_subject_bare0}} to create base repository.
#'
#' @seealso \code{\link{prepare_subject_bare0}}
#'
#' @export
RAVESubjectBaseRepository <- R6::R6Class(
  # historical issue :}
  classname = "RAVESubjectRepository",
  portable = TRUE,
  inherit = RAVESerializable,
  lock_objects = TRUE,
  lock_class = TRUE,
  cloneable = TRUE,

  private = list(
    .project = NULL,
    .subject = NULL,
    .intended_electrode_list = integer(),
    .electrode_list = integer(),
    .reference_name = character(),


    update_electrode_list = function() {
      reference_table <- self$reference_table
      electrodes <- private$.intended_electrode_list
      included_electrodes <- as.integer(reference_table$Electrode[reference_table$Reference != ''])
      included_electrodes <- included_electrodes[included_electrodes %in% electrodes]
      included_electrodes <- sort(unique(included_electrodes))

      if(!setequal(included_electrodes, electrodes)) {
        ravepipeline::logger("The following electrodes are removed from loading because they are either missing or marked as `excluded`: ", deparse_svec(electrodes[!electrodes %in% included_electrodes]), level = "info")
      }
      private$.electrode_list <- included_electrodes
    },
    update_subject = function() {
      private$.subject <- as_rave_subject(private$.subject, strict = FALSE, reload = TRUE)
      private$update_electrode_list()
    }
  ),
  public = list(

    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      structure(
        list(
          namespace = "ravecore",
          r6_generator = "RAVESubjectBaseRepository",
          data = structure(
            class = "RAVESubjectBaseRepository_marshal",
            list(
              subject = private$.subject$`@marshal`(),
              intended_electrode_list = private$.intended_electrode_list,
              reference_name = private$.reference_name,
              repository_id = self$repository_id,
              quiet = self$quiet
            )
          )
        )
      )
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectBaseRepository_marshal"))
      return(RAVESubjectBaseRepository$new(
        subject = RAVESubject$public_methods$`@unmarshal`(object$data$subject),
        electrodes = object$data$intended_electrode_list,
        reference_name = object$data$reference_name,
        quiet = TRUE,
        repository_id = object$data$repository_id,
        strict = FALSE
      ))
    },

    #' @field repository_id repository identifier, typically generated with
    #' random string
    repository_id = character(),

    #' @field quiet whether to suppress update warning messages, when
    #' requested electrodes are not fully processed or excluded
    quiet = TRUE,

    #' @description constructor
    #' @param subject 'RAVE' subject
    #' @param electrodes string or integers indicating electrodes to load
    #' @param reference_name name of the reference table
    #' @param quiet see field \code{quiet}
    #' @param repository_id see field \code{repository_id}
    #' @param strict whether the mode should be strict; default is true and
    #' errors out when subject is missing
    #' @param ... reserved, currently ignored
    #' @param .class internally used, do not set, even if you know what this is
    initialize = function(subject, electrodes = NULL, reference_name = NULL, ...,
                          quiet = TRUE, repository_id = NULL, strict = TRUE, .class = NULL) {

      if(is.null(.class)) {
        .class <- "rave_prepare_subject_bare0"
      }
      class(self) <- unique(c(.class, 'rave_repository', class(self)))

      self$quiet <- quiet
      repository_id <- paste(repository_id, collapse = "")
      if(nzchar(repository_id)) {
        self$repository_id <- repository_id
      } else {
        self$repository_id <- rand_string(4)
      }

      subject <- as_rave_subject(subject, strict = strict, reload = TRUE)
      private$.project <- subject$project
      private$.subject <- subject

      if(missing(electrodes) || is.null(electrodes)){
        electrodes <- subject$get_default("electrodes", default_if_missing = subject$electrodes)
        ravepipeline::logger("No electrodes specified, trying to load all the electrodes: ",
                             deparse_svec(electrodes))
      } else {
        electrodes <- parse_svec(electrodes)
      }
      private$.intended_electrode_list <- as.integer(sort(unique(electrodes)))

      available_reference_names <- subject$reference_names
      reference_warn <- FALSE
      if(missing(reference_name) || !length(reference_name) || is.na(reference_name[[1]])) {
        reference_warn <- TRUE
        reference_name <- "default"
      }
      reference_name <- as.character(reference_name[[1]])
      if(!isTRUE(reference_name %in% available_reference_names)) {
        if(length(available_reference_names)) {
          reference_name <- available_reference_names[[1]]
        } else {
          reference_name <- "noref"
        }
        reference_warn <- TRUE
      }
      if(reference_warn) {
        ravepipeline::logger("No reference specified, using `{reference_name}`",
                             use_glue = TRUE)
      }
      private$.reference_name <- reference_name

      private$update_electrode_list()

    }

  ),
  active = list(

    #' @field needs_update write-only attribute when subject needs to be
    #' reloaded from the disk and reference table needs to be updated, use
    #' \code{repo$needs_update <- TRUE}
    needs_update = function(v) {
      if(!missing(v) && v) {
        private$update_subject()
      }
      invisible()
    },

    #' @field project project instance, see \code{\link{RAVEProject}}
    project = function() {
      private$.project
    },

    #' @field subject subject instance, see \code{\link{RAVESubject}}
    subject = function() {
      private$.subject
    },

    #' @field electrode_list integer vector of electrodes included
    electrode_list = function() {
      private$.electrode_list
    },

    #' @field electrode_table the entire electrode table
    electrode_table = function() {
      private$.subject$get_electrode_table()
    },

    #' @field electrode_signal_types more accurate name should be "channel"
    #' signal types: currently returns \code{'LFP'}, \code{'Auxiliary'}, or
    #' \code{'Spike'}, for each channel
    electrode_signal_types = function() {
      subject <- private$.subject
      imported_electrodes <- subject$electrodes
      sel <- imported_electrodes %in% self$electrode_list
      imported_electrodes <- imported_electrodes[sel]
      o <- order(imported_electrodes)
      electrode_signal_types <- subject$electrode_types[sel][o]
      imported_electrodes <- imported_electrodes[o]
      structure(
        electrode_signal_types,
        names = imported_electrodes
      )
    },

    #' @field electrode_instances electrode channel instance helpers for
    #' loading electrode data
    electrode_instances = function() {
      subject <- self$subject
      electrode_list <- self$electrode_list
      electrode_signal_types <- self$electrode_signal_types
      reference_table <- self$reference_table
      reference_instances <- self$reference_instances

      electrode_instances <- structure(lapply(seq_along(electrode_list), function(ii){
        e <- electrode_list[[ii]]
        signal_type <- electrode_signal_types[[ii]]
        ref_name <- reference_table$Reference[reference_table$Electrode == e][[1]]
        ref_name <- sprintf("%s_%s", ref_name, signal_type)
        ref <- reference_instances[[ref_name]]

        el <- new_electrode(subject = subject, number = e, signal_type = signal_type, quiet = self$quiet)
        el$set_reference(ref)
        el
      }), names = sprintf("e_%d", electrode_list))
      electrode_instances
    },

    #' @field reference_name name of reference table
    reference_name = function(v) {
      if(!missing(v)) {
        subject <- private$.subject
        if(v == "noref" || v %in% subject$reference_names) {
          private$.reference_name <- v
          private$update_electrode_list()
        } else {
          ravepipeline::logger("Unable to find reference table with name ",
                               sQuote(v), level = "fatal")
        }
      }
      private$.reference_name
    },

    #' @field reference_table reference table
    reference_table = function() {
      subject <- private$.subject
      reference_name <- self$reference_name
      if(!length(subject$electrodes)) {
        stop("No electrode/channel found under this subject. Please import data first.")
      }
      if(!isTRUE(reference_name %in% subject$reference_names)) {
        if(identical(tolower(self$reference_name), "noref")) {
          reference_table <- data.frame(
            Electrode = subject$electrodes,
            Group = "default",
            Reference = "noref",
            Type = "No Reference"
          )
          safe_write_csv(
            reference_table,
            file = file.path(subject$meta_path, "reference_noref.csv"),
            row.names = FALSE
          )
          return(reference_table)
        } else {
          stop(sprintf("Unable to match/find reference table named %s. Please check the subject meta folder on whether reference_%s.csv exists", sQuote(self$reference_name), self$reference_name))
        }
      }
      reference_table <- subject$get_reference(reference_name)
      return(reference_table)
    },

    #' @field references_list a vector of reference channel names, used together
    #' with \code{reference_instances}
    references_list = function() {
      reference_table <- self$reference_table
      ref_table <- reference_table[reference_table$Electrode %in% self$electrode_list, ]
      sort(unique(ref_table$Reference))
    },

    #' @field reference_instances instances of reference channels, for
    #' referencing on the fly, used for \code{electrode_instances}
    reference_instances = function() {
      # load reference electrodes
      reference_table <- self$reference_table
      electrode_list <- self$electrode_list
      electrode_signal_types <- self$electrode_signal_types
      subject <- private$.subject

      ref_table <- reference_table[reference_table$Electrode %in% electrode_list, ]

      ref_mat <- unique(cbind(
        ref_table$Reference,
        electrode_signal_types
      ))

      reference_instances <- structure(
        lapply(seq_len(nrow(ref_mat)), function(ii){
          y <- ref_mat[ii, ]
          new_reference(subject = subject, number = y[[1]], signal_type = y[[2]], quiet = self$quiet)
        }),
        names = sprintf("%s_%s", ref_mat[, 1], ref_mat[, 2])
      )
      drop_nulls(reference_instances)
    },

    #' @field digest_key a list of repository data used to generate
    #' repository signature
    digest_key = function() {
      list(
        subject_id = self$subject$subject_id,
        reference_table = self$reference_table,
        electrodes = self$electrode_list,
        electrode_signal_types = unname(self$electrode_signal_types)
      )
    },

    #' @field signature signature of the repository, two repositories might
    #' share the same signature if their contents are the same (even with
    #' different identifiers); generated from \code{digest_key}
    signature = function() {
      digest_key <- self$digest_key
      if(is.null(digest_key)) {
        digest_key <- self$`@marshal`()$data
      }
      structure(
        ravepipeline::digest(digest_key),
        contents = names(digest_key)
      )
    }

  )

)

#' @title 'RAVE' repository: basic
#' @returns A \code{\link{RAVESubjectBaseRepository}} instance
#' @param subject 'RAVE' subject
#' @param electrodes string or integers indicating electrodes to load
#' @param reference_name name of the reference table
#' @param quiet see field \code{quiet}
#' @param repository_id see field \code{repository_id}
#' @param ... passed to \code{\link{RAVESubjectBaseRepository}} constructor
#' @examples
#'
#' \dontrun{
#'
#'
#' repository <- prepare_subject_bare0("demo/DemoSubject",
#'                                     electrodes = 14:16,
#'                                     reference_name = "default")
#'
#' print(repository)
#'
#' repository$subject
#' repository$subject$raw_sample_rates
#'
#' repository$electrode_table
#'
#' repository$reference_table
#'
#' electrodes <- repository$electrode_instances
#'
#' # Channel 14
#' e <- electrodes$e_14
#'
#' # referenced voltage
#' voltage <- e$load_blocks("008", "voltage")
#'
#' ravetools::diagnose_channel(voltage, srate = 2000)
#'
#'
#' }
#'
#' @export
prepare_subject_bare0 <- function(subject, electrodes = NULL,
                                  reference_name = NULL, ...,
                                  quiet = TRUE, repository_id = NULL) {

  RAVESubjectBaseRepository$new(
    subject = subject,
    electrodes = electrodes,
    reference_name = reference_name,
    quiet = quiet,
    repository_id = repository_id,
    ...
  )
}


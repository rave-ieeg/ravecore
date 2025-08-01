#' @title 'RAVE' class for epoch repository - raw voltage
#' @description
#' The repository inherits \code{link{RAVESubjectEpochRepository}}, with epoch
#' trials, and is intended for loading raw (without any processing or reference)
#' voltage signals.
#' Use \code{\link{prepare_subject_raw_voltage_with_epoch}} to create an
#' instance.
#'
#' @export
RAVESubjectEpochRawVoltageRepository <- R6::R6Class(
  classname = "RAVESubjectEpochRawVoltageRepository",
  portable = TRUE,
  inherit = RAVESubjectEpochRepository,
  lock_objects = TRUE,
  lock_class = TRUE,
  cloneable = TRUE,

  public = list(
    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectEpochRawVoltageRepository"
      class(object$data) <- c("RAVESubjectEpochRawVoltageRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectEpochRawVoltageRepository_marshal"))
      return(RAVESubjectEpochRawVoltageRepository$new(
        subject = RAVESubject$public_methods$`@unmarshal`(object$data$subject),
        electrodes = object$data$intended_electrode_list,
        reference_name = "noref",
        epoch_name = object$data$epoch_name,
        quiet = TRUE,
        repository_id = object$data$repository_id,
        time_windows = object$data$time_windows,
        stitch_events = object$data$stitch_events,
        strict = FALSE
      ))
    },

    #' @description constructor
    #' @param subject 'RAVE' subject
    #' @param electrodes string or integers indicating electrodes to load
    #' @param epoch_name name of the epoch trial table
    #' @param time_windows numeric vector with even lengths, the time start
    #' and end of the trials, for example, \code{c(-1, 2)} means load
    #' 1 second before the trial onset and 2 seconds after trial onset
    #' @param stitch_events events where the \code{time_windows} is based;
    #' default is trial onset (\code{NULL})
    #' @param quiet see field \code{quiet}
    #' @param repository_id see field \code{repository_id}
    #' @param strict whether the mode should be strict; default is true and
    #' errors out when subject is missing
    #' @param eager_load whether to call \code{mount_data} immediately;
    #' default is true
    #' @param ... reserved, currently ignored
    initialize = function(subject, electrodes = NULL,
                          epoch_name = NULL, time_windows = NULL,
                          stitch_events = NULL, ..., quiet = FALSE,
                          repository_id = NULL, strict = TRUE,
                          eager_load = TRUE) {
      subject <- as_rave_subject(subject, strict = strict)
      super$initialize(
        subject = subject,
        electrodes = electrodes,
        epoch_name = epoch_name,
        time_windows = time_windows,
        stitch_events = stitch_events,
        reference_name = "noref",
        quiet = quiet,
        repository_id = repository_id
      )

      class(self) <- unique(c(
        "rave_prepare_subject_raw_voltage_with_epoch",
        class(self)
      ))

      if( eager_load ) {
        self$mount_data()
      }

    },

    #' @description function to mount raw voltage signals
    mount_data = function() {

      # self <- RAVESubjectEpochVoltageRepository$new(
      #   subject = "demo/DemoSubject",
      #   electrodes = 13:16,
      #   reference_name = "default",
      #   epoch_name = "auditory_onset",
      #   time_windows = c(-1, 2),
      #   stitch_events = NULL
      # )

      electrode_instances <- self$electrode_instances

      data_list <- ravepipeline::lapply_jobs(
        electrode_instances,
        function(inst) {
          ravepipeline::RAVEFileArray$new(
            inst$load_data(type = "raw-voltage")
          )
        }, callback = function(inst) {
          sprintf("Loading Original Raw Voltage | Electrode %s", inst$number)
        }
      )
      # Clear progress finish line
      cat("          \r")

      data_list <- structure(
        names = names(electrode_instances),
        lapply(data_list, "[[", "@impl")
      )

      # construct dim and dimnames
      dim <- dim(data_list[[1]])
      dim[[3]] <- length(data_list)

      dimnames <- dimnames(data_list[[1]])
      dimnames[[3]] <- self$electrode_list

      private$.data$`@mset`(
        data_list = data_list,
        dim = dim,
        dimnames = dimnames,
        signature = self$signature
      )

      self
    }

  ),
  active = list(

    #' @field digest_key a list of repository data used to generate
    #' repository signature
    digest_key = function() {
      list(
        rave_data_type = "raw-voltage",
        subject_id = self$subject$subject_id,
        epoch_table = self$epoch$table,
        electrode_list = self$electrode_list,
        sample_rate = self$sample_rate,
        electrode_signal_types = unname(self$electrode_signal_types),
        time_windows = self$time_windows,
        stitch_events = self$stitch_events
      )
    },

    #' @field raw_voltage a named map of raw voltage data, mounted by
    #' \code{mount_data}
    raw_voltage = function() {
      private$.data
    }

  )
)


#' @title 'RAVE' class for epoch repository - voltage
#' @description
#' The repository inherits \code{link{RAVESubjectEpochRepository}}, with epoch
#' trials, and is intended for loading processed and referenced voltage
#' signals.
#' Use \code{\link{prepare_subject_voltage_with_epoch}} to create an
#' instance.
#'
#' @export
RAVESubjectEpochVoltageRepository <- R6::R6Class(
  classname = "RAVESubjectEpochVoltageRepository",
  portable = TRUE,
  inherit = RAVESubjectEpochRepository,
  lock_objects = TRUE,
  lock_class = TRUE,
  cloneable = TRUE,

  public = list(
    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectEpochVoltageRepository"
      class(object$data) <- c("RAVESubjectEpochVoltageRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectEpochVoltageRepository_marshal"))
      return(RAVESubjectEpochVoltageRepository$new(
        subject = RAVESubject$public_methods$`@unmarshal`(object$data$subject),
        electrodes = object$data$intended_electrode_list,
        reference_name = object$data$reference_name,
        epoch_name = object$data$epoch_name,
        quiet = TRUE,
        repository_id = object$data$repository_id,
        time_windows = object$data$time_windows,
        stitch_events = object$data$stitch_events,
        strict = FALSE
      ))
    },


    #' @description constructor
    #' @param subject 'RAVE' subject
    #' @param electrodes string or integers indicating electrodes to load
    #' @param epoch_name name of the epoch trial table
    #' @param time_windows numeric vector with even lengths, the time start
    #' and end of the trials, for example, \code{c(-1, 2)} means load
    #' 1 second before the trial onset and 2 seconds after trial onset
    #' @param stitch_events events where the \code{time_windows} is based;
    #' default is trial onset (\code{NULL})
    #' @param reference_name name of the reference table
    #' @param quiet see field \code{quiet}
    #' @param repository_id see field \code{repository_id}
    #' @param strict whether the mode should be strict; default is true and
    #' errors out when subject is missing
    #' @param eager_load whether to call \code{mount_data} immediately;
    #' default is true
    #' @param ... reserved, currently ignored
    initialize = function(subject, electrodes = NULL,
                          reference_name = NULL, epoch_name = NULL,
                          time_windows = NULL, stitch_events = NULL, ...,
                          quiet = FALSE, repository_id = NULL, strict = TRUE,
                          eager_load = TRUE) {
      subject <- as_rave_subject(subject, strict = strict)
      super$initialize(
        subject = subject,
        electrodes = electrodes,
        epoch_name = epoch_name,
        time_windows = time_windows,
        stitch_events = stitch_events,
        reference_name = reference_name,
        quiet = quiet,
        repository_id = repository_id
      )

      class(self) <- unique(c(
        "rave_prepare_subject_voltage_with_epoch",
        class(self)
      ))

      if( eager_load ) {
        self$mount_data()
      }

    },

    #' @description function to mount processed and referenced voltage signals
    mount_data = function() {

      # self <- RAVESubjectEpochVoltageRepository$new(
      #   subject = "demo/DemoSubject",
      #   electrodes = 13:16,
      #   reference_name = "default",
      #   epoch_name = "auditory_onset",
      #   time_windows = c(-1, 2),
      #   stitch_events = NULL
      # )

      reference_instances <- self$reference_instances
      electrode_instances <- self$electrode_instances
      if(length(reference_instances)) {
        ravepipeline::lapply_jobs(
          reference_instances,
          function(inst) {
            inst$load_data("voltage")
            return()
          }, callback = function(inst) {
            sprintf("Loading Reference | %s", inst$number)
          }
        )
      }

      data_list <- ravepipeline::lapply_jobs(
        electrode_instances,
        function(inst) {
          ravepipeline::RAVEFileArray$new(
            inst$load_data(type = "voltage")
          )
        }, callback = function(inst) {
          sprintf("Loading Voltage | Electrode %s", inst$number)
        }
      )
      # Clear progress finish line
      cat("          \r")

      data_list <- structure(
        names = names(electrode_instances),
        lapply(data_list, "[[", "@impl")
      )

      # construct dim and dimnames
      dim <- dim(data_list[[1]])
      dim[[3]] <- length(data_list)

      dimnames <- dimnames(data_list[[1]])
      dimnames[[3]] <- self$electrode_list

      private$.data$`@mset`(
        data_list = data_list,
        dim = dim,
        dimnames = dimnames,
        signature = self$signature
      )

      self
    }

  ),
  active = list(


    #' @field digest_key a list of repository data used to generate
    #' repository signature
    digest_key = function() {
      list(
        rave_data_type = "voltage",
        subject_id = self$subject$subject_id,
        epoch_table = self$epoch$table,
        reference_table = self$reference_table,
        electrode_list = self$electrode_list,
        sample_rate = self$sample_rate,
        electrode_signal_types = unname(self$electrode_signal_types),
        time_windows = self$time_windows,
        stitch_events = self$stitch_events
      )
    },

    #' @field voltage a named map of voltage data, mounted by
    #' \code{mount_data}
    voltage = function() {
      private$.data
    }

  )

)


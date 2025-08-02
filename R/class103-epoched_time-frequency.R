#' @title 'RAVE' class for epoch repository - spectrogram
#' @description
#' The repository inherits \code{link{RAVESubjectEpochRepository}}, with epoch
#' trials, and is intended for loading processed and referenced time-frequency
#' coefficients.
#' Use \code{\link{prepare_subject_time_frequency_coefficients}} to create an
#' instance.
#'
#' @export
RAVESubjectEpochTimeFreqCoefRepository <- R6::R6Class(
  classname = "RAVESubjectEpochTimeFreqCoefRepository",
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
      object$r6_generator <- "RAVESubjectEpochTimeFreqCoefRepository"
      class(object$data) <- c("RAVESubjectEpochTimeFreqCoefRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectEpochTimeFreqCoefRepository_marshal"))
      return(RAVESubjectEpochTimeFreqCoefRepository$new(
        subject = RAVESubject$public_methods$`@unmarshal`(object$data$subject),
        electrodes = object$data$intended_electrode_list,
        reference_name = object$data$reference_name,
        epoch_name = object$data$epoch_name,
        quiet = TRUE,
        repository_id = object$data$repository_id,
        time_windows = object$data$time_windows,
        stitch_events = object$data$stitch_events,
        strict = TRUE,
        lazy_load = TRUE
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
    #' @param lazy_load whether to delay \code{mount_data};
    #' default is false
    #' @param ... reserved, currently ignored
    initialize = function(subject, electrodes = NULL,
                          reference_name = NULL, epoch_name = NULL,
                          time_windows = NULL, stitch_events = NULL, ...,
                          quiet = FALSE, repository_id = NULL, strict = TRUE,
                          lazy_load = FALSE) {
      subject <- as_rave_subject(subject, strict = strict)
      super$initialize(
        subject = subject,
        electrodes = electrodes,
        epoch_name = epoch_name,
        time_windows = time_windows,
        stitch_events = stitch_events,
        reference_name = reference_name,
        quiet = quiet,
        repository_id = repository_id,
        lazy_load = lazy_load,
        ...
      )

      class(self) <- unique(c(
        "rave_prepare_time_frequency_coefficients",
        class(self)
      ))

    },

    #' @description function to mount processed and referenced voltage signals
    mount_data = function() {

      # self <- RAVESubjectEpochTimeFreqCoefRepository$new(
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
            inst$load_data("wavelet-coefficient")
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
            inst$load_data(type = "wavelet-coefficient")
          )
        }, callback = function(inst) {
          sprintf("Loading Time-Frequency Coefficients | Electrode %s", inst$number)
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
      dim[[4]] <- length(data_list)

      dimnames <- dimnames(data_list[[1]])
      dimnames[[4]] <- self$electrode_list

      private$.data$`@mset`(
        data_list = data_list,
        dim = structure(dim, names = names(dimnames)),
        dimnames = dimnames,
        signature = self$signature
      )

      self
    }

  ),
  active = list(

    #' @field sample_rate time-frequency coefficient sample rate
    sample_rate = function() {
      self$subject$power_sample_rate
    },

    #' @field frequency frequencies where the time-frequency coefficients
    #' are evaluated
    frequency = function() {
      self$subject$preprocess_settings$wavelet_params$frequencies
    },

    #' @field time time in seconds for each trial
    time = function() {
      srate <- self$sample_rate
      tidx <- unlist(lapply(self$time_windows, function(x){
        x <- round(x * srate)
        seq(x[1], x[2])
      }))
      tidx / srate
    },

    #' @field time_points see \code{time} field
    time_points = function() {
      ravepipeline::logger(
        level = "warning",
        "Please use `repository$time` instead of `repository$time_points`. The latter one will be phased out soon."
      )
      self$time
    },

    #' @field signal_type do not use
    signal_type = function() {
      ravepipeline::logger(
        level = "warning",
        "Please do NOT use `repository$signal_type`: will be phased out soon."
      )
      "LFP"
    },


    #' @field digest_key a list of repository data used to generate
    #' repository signature
    digest_key = function() {
      list(
        rave_data_type = "wavelet-coefficient",
        subject_id = self$subject$subject_id,
        epoch_table = self$epoch$table,
        reference_table = self$reference_table,
        electrode_list = self$electrode_list,
        sample_rate = self$sample_rate,
        electrode_signal_types = unname(self$electrode_signal_types),
        time_windows = self$time_windows,
        stitch_events = self$stitch_events,
        frequency_table = self$subject$preprocess_settings$wavelet_params
      )
    },

    #' @field coefficients a named map of time-frequency coefficient data,
    #' mounted by \code{mount_data}
    coefficients = function() {
      if(private$.data$`@size`() == 0) {
        self$mount_data()
      }
      private$.data
    },

    #' @field wavelet not used anymore, see \code{coefficients}
    wavelet = function() {
      ravepipeline::logger(
        level = 'warning',
        "Please use `repository$coefficients` instead of `repository$wavelet`"
      )
      self$coefficients
    }

  )

)

#' @rdname prepare_subject_with_epoch
#' @export
prepare_subject_time_frequency_coefficients <- function(
    subject, electrodes = NULL, reference_name = NULL,
    epoch_name = NULL, time_windows = NULL,
    stitch_events = NULL, ..., quiet = FALSE,
    repository_id = NULL, strict = TRUE) {
  RAVESubjectEpochTimeFreqCoefRepository$new(
    subject = subject,
    electrodes = electrodes,
    reference_name = reference_name,
    epoch_name = epoch_name,
    time_windows = time_windows,
    stitch_events = stitch_events,
    quiet = quiet,
    repository_id = repository_id,
    strict = strict,
    ...
  )
}

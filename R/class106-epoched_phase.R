

#' @title 'RAVE' class for epoch repository - time-frequency phase
#' @description
#' The repository inherits \code{link{RAVESubjectEpochTimeFreqBaseRepository}}, with epoch
#' trials, and is intended for loading processed and referenced time-frequency
#' coefficients.
#' Use \code{\link{prepare_subject_phase_with_epochs}} to create an
#' instance.
#' @export
RAVESubjectEpochPhaseRepository <- R6::R6Class(
  classname = "RAVESubjectEpochPhaseRepository",
  portable = TRUE,
  inherit = RAVESubjectEpochTimeFreqBaseRepository,
  lock_objects = TRUE,
  lock_class = TRUE,
  cloneable = TRUE,

  public = list(
    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectEpochPhaseRepository"
      class(object$data) <- c("RAVESubjectEpochPhaseRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectEpochPhaseRepository_marshal"))
      return(RAVESubjectEpochPhaseRepository$new(
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
    #' @param ... passed to \code{\link{RAVESubjectEpochTimeFreqBaseRepository}} constructor
    #' @param .class internally used, do not set, even if you know what this is
    initialize = function(subject, electrodes = NULL, reference_name = NULL,
                          epoch_name = NULL, time_windows = NULL,
                          stitch_events = NULL, ..., quiet = FALSE,
                          repository_id = NULL, strict = TRUE, lazy_load = FALSE,
                          .class = NULL) {
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
        data_type = "phase",
        .class = c(.class, "rave_prepare_phase"),
        ...
      )

    }

  ),
  active = list(

    #' @field phase a named map of time-frequency coefficient phase,
    #' mounted by \code{mount_data}
    phase = function() {
      self$get_container()
    }

  )

)


#' @rdname prepare_subject_with_epochs
#' @export
prepare_subject_phase_with_epochs <- function(
    subject, electrodes = NULL, reference_name = NULL,
    epoch_name = NULL, time_windows = NULL,
    stitch_events = NULL, ..., quiet = FALSE,
    repository_id = NULL, strict = TRUE) {
  RAVESubjectEpochPhaseRepository$new(
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


prepare_subject_phase <- function(subject, electrodes = NULL, reference_name = NULL,
                                  epoch_name = NULL, time_windows = NULL,
                                  stitch_events = NULL, ..., quiet = FALSE,
                                  repository_id = NULL, strict = TRUE) {
  ravepipeline::logger("Function name `prepare_subject_phase` is no longer recommended for its ambiguity. Use `prepare_subject_phase_with_epochs` (same implementation) instead.", level = "warning")

  RAVESubjectEpochPhaseRepository$new(
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

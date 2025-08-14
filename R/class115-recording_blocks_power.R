#' 'RAVE' class for loading 'spectrogram' power components
#' @description
#' Loads time-frequency power
#'
#' @seealso \code{\link{prepare_subject_power_with_blocks}}
#' @export
RAVESubjectRecordingBlockPowerRepository <- R6::R6Class(
  classname = "RAVESubjectRecordingBlockPowerRepository",
  portable = TRUE,
  inherit = RAVESubjectRecordingBlockTimeFreqBaseRepository,
  lock_objects = FALSE,
  lock_class = TRUE,
  cloneable = TRUE,

  private = list(
    .data_type = "power"
  ),
  public = list(


    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectRecordingBlockPowerRepository"
      class(object$data) <- c("RAVESubjectRecordingBlockPowerRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectRecordingBlockPowerRepository_marshal"))
      repo <- RAVESubjectRecordingBlockPowerRepository$new(
        subject = RAVESubject$public_methods$`@unmarshal`(object$data$subject),
        electrodes = object$data$intended_electrode_list,
        reference_name = object$data$reference_name,
        blocks = object$data$blocks,
        quiet = TRUE,
        repository_id = object$data$repository_id,
        strict = TRUE,
        lazy_load = TRUE
      )
      return(repo)
    },

    #' @description constructor
    #' @param subject 'RAVE' subject
    #' @param electrodes string or integers indicating electrodes to load
    #' @param blocks name of the recording blocks to load
    #' @param reference_name name of the reference table
    #' @param quiet see field \code{quiet}
    #' @param repository_id see field \code{repository_id}
    #' @param strict whether the mode should be strict; default is true and
    #' errors out when subject is missing
    #' @param lazy_load whether to delay (lazy) the evaluation \code{mount_data}
    #' @param ... passed to \code{\link{RAVESubjectBaseRepository}} constructor
    #' @param .class internally used, do not set, even if you know what this is
    initialize = function(subject, electrodes = NULL,
                          reference_name = NULL, blocks = NULL, ...,
                          quiet = FALSE, repository_id = NULL, strict = TRUE,
                          lazy_load = FALSE, .class = NULL) {

      .class <- c(.class,
                  "prepare_subject_power_with_blocks",
                  "RAVESubjectRecordingBlockPowerRepository")

      subject <- as_rave_subject(subject, strict = strict)
      super$initialize(subject = subject, electrodes = electrodes,
                       reference_name = reference_name, quiet = quiet,
                       repository_id = repository_id, blocks = blocks,
                       lazy_load = lazy_load,
                       .class = .class)
    }


  ),
  active = list(

    #' @field power data container, alias of \code{get_container}
    power = function() {
      self$get_container()
    }

  )

)

#' @rdname prepare_subject_with_blocks
#' @export
prepare_subject_power_with_blocks <- function(
    subject, electrodes = NULL, blocks = NULL,
    reference_name = NULL, ...,
    quiet = FALSE, repository_id = NULL, strict = TRUE) {
  RAVESubjectRecordingBlockPowerRepository$new(
    subject = subject, electrodes = electrodes,
    reference_name = reference_name, blocks = blocks, ...,
    quiet = quiet, repository_id = repository_id, strict = strict)
}







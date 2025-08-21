#' @title 'RAVE' class for epoch repository - time-frequency (internal)
#' @description
#' The repository inherits \code{link{RAVESubjectEpochRepository}}, with epoch
#' trials, and is intended for loading processed and referenced time-frequency
#' coefficients.
RAVESubjectEpochTimeFreqBaseRepository <- R6::R6Class(
  classname = "RAVESubjectEpochTimeFreqBaseRepository",
  portable = TRUE,
  inherit = RAVESubjectEpochRepository,
  lock_objects = TRUE,
  lock_class = TRUE,
  cloneable = TRUE,

  private = list(
    .data_type = character()
  ),

  public = list(
    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectEpochTimeFreqBaseRepository"
      class(object$data) <- c("RAVESubjectEpochTimeFreqBaseRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectEpochTimeFreqBaseRepository_marshal"))
      repo <- RAVESubjectEpochTimeFreqBaseRepository$new(
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
      )
      repo$`@restored` <- TRUE
      return(repo)
    },


    #' @description constructor
    #' @param subject 'RAVE' subject
    #' @param electrodes string or integers indicating electrodes to load
    #' @param reference_name name of the reference table
    #' @param epoch_name name of the epoch trial table
    #' @param time_windows numeric vector with even lengths, the time start
    #' and end of the trials, for example, \code{c(-1, 2)} means load
    #' 1 second before the trial onset and 2 seconds after trial onset
    #' @param stitch_events events where the \code{time_windows} is based;
    #' default is trial onset (\code{NULL})
    #' @param data_type for child classes to fill; data type (power,
    #' phase, or complex time-frequency coefficients)
    #' @param quiet see field \code{quiet}
    #' @param repository_id see field \code{repository_id}
    #' @param strict whether the mode should be strict; default is true and
    #' errors out when subject is missing
    #' @param lazy_load whether to delay \code{mount_data};
    #' default is false
    #' @param ... passed to \code{\link{RAVESubjectEpochRepository}} constructor
    #' @param .class internally used, do not set, even if you know what this is
    initialize = function(subject, electrodes = NULL, reference_name = NULL,
                          epoch_name = NULL, time_windows = NULL,
                          stitch_events = NULL, data_type = NULL, ...,
                          quiet = FALSE, repository_id = NULL, strict = TRUE,
                          lazy_load = FALSE, .class = NULL) {
      subject <- as_rave_subject(subject, strict = strict)
      stopifnot2(
        length(data_type) <= 1,
        msg = "Cannot have more than one `data_type`"
      )
      private$.data_type <- as.character(data_type)
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
        .class = c(.class, "rave_prepare_time_frequency"),
        ...
      )
    },

    #' @description function to mount processed and referenced 'spectrogram'
    #' @param force force update data; default is true
    #' @param electrodes electrodes to update for expert-use use; default is
    #' \code{NULL} (all electrode channels will be mounted)
    #' @param ... reserved
    mount_data = function(..., force = TRUE, electrodes = NULL) {

      # self <- RAVESubjectEpochPowerRepository$new(
      #   subject = "demo/DemoSubject",
      #   electrodes = 13:16,
      #   reference_name = "default",
      #   epoch_name = "auditory_onset",
      #   time_windows = c(-1, 2),
      #   stitch_events = NULL,
      #   lazy_load = TRUE
      # )
      # private <- self$.__enclos_env__$private
      # private$.data
      # private$.data$`@reset`()
      # self$mount_data(electrodes = 13)
      # private$.data$data_list
      # private$.data$`@reset`()
      # force = FALSE
      # electrodes <- 13

      workers <- 0
      if(self$`@restored`) { workers <- 1 }

      data_type <- private$.data_type
      if(length(data_type) == 0) { return(self) }

      # determine electrodes to load
      if(length(electrodes) == 1 && is.na(electrodes)) {
        # Do not load data, just fill in the meta
        electrodes <- integer()
      } else {
        electrodes <- parse_svec(electrodes)
        electrodes <- electrodes[electrodes %in% self$electrode_list]
        if(!length(electrodes)) {
          electrodes <- self$electrode_list
        }
      }

      # check data_list
      nms <- sprintf("e_%d", electrodes)

      if( length(private$.data) > 0 && !force ) {
        exist_list <- names(private$.data$data_list)
        if(all(nms %in% exist_list)) { return(self) }
      }

      all_electrode_instances <- self$electrode_instances
      electrode_instances <- self$electrode_instances[nms]

      # fine references to load
      ref_names <- lapply(electrode_instances, function(inst) {
        if(isTRUE(inst$reference_name %in% c("noref", ""))) { return(NULL) }
        sprintf("%s_%s", inst$reference_name, inst$type)
      })
      ref_names <- unlist(unique(ref_names))
      ref_names <- ref_names[ref_names %in% names(self$reference_instances)]

      reference_instances <- self$reference_instances[ref_names]

      # Load reference first
      if(length(reference_instances)) {
        ravepipeline::lapply_jobs(
          reference_instances,
          function(inst) {
            inst$load_data_with_epochs(data_type)
            return()
          }, callback = function(inst) {
            sprintf("Loading Reference | %s", inst$number)
          },
          .globals = list(data_type = data_type),
          .workers = workers
        )
      }

      # Load data next
      data_list <- ravepipeline::lapply_jobs(
        electrode_instances,
        function(inst) {
          ravepipeline::RAVEFileArray$new(
            inst$load_data_with_epochs(type = data_type)
          )
        }, callback = function(inst) {
          sprintf("Loading Time-Frequency Components | Electrode %s", inst$number)
        },
        .globals = list(data_type = data_type),
        .workers = workers
      )
      # Clear progress finish line
      cat("          \r")

      # Construct dataset
      data_list <- structure(
        names = nms,
        lapply(data_list, "[[", "@impl")
      )

      # construct dim and dimnames
      inst <- all_electrode_instances[[1]]
      dimnames <- inst$load_dimnames_with_epochs(type = data_type)
      dimnames$Electrode <- self$electrode_list

      dim <- vapply(dimnames, length, 0L)

      private$.data$`@mset`(
        # data_list = data_list,
        dim = structure(dim, names = names(dimnames)),
        dimnames = dimnames,
        signature = self$signature
      )

      private$.data$data_list <- as.list(private$.data$data_list)
      if(length(data_list)) {
        private$.data$data_list[nms] <- data_list[nms]
      }

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

    #' @field time_points see \code{time} field, existed for backward
    #' compatibility
    time_points = function() {
      # ravepipeline::logger(
      #   level = "warning",
      #   "Please use `repository$time` instead of `repository$time_points`. The latter one will be phased out soon."
      # )
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
        rave_data_type = private$.data_type,
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
    }

  )

)


#' @title 'RAVE' class for epoch repository - spectrogram
#' @description
#' The repository inherits \code{link{RAVESubjectEpochTimeFreqBaseRepository}}, with epoch
#' trials, and is intended for loading processed and referenced time-frequency
#' coefficients.
#' Use \code{\link{prepare_subject_time_frequency_coefficients_with_epochs}} to create an
#' instance.
#'
#' @export
RAVESubjectEpochTimeFreqCoefRepository <- R6::R6Class(
  classname = "RAVESubjectEpochTimeFreqCoefRepository",
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
        data_type = "wavelet-coefficient",
        .class = c(.class, "rave_prepare_time_frequency_coefficients"),
        ...
      )

    }

  ),
  active = list(

    #' @field coefficients a named map of time-frequency coefficient data,
    #' mounted by \code{mount_data}
    coefficients = function() {
      self$get_container()
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


#' @rdname prepare_subject_with_epochs
#' @export
prepare_subject_time_frequency_coefficients_with_epochs <- function(
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


# self <- prepare_subject_power_with_epochs(
#     "demo/DemoSubject", electrodes = 14:16,
#     reference_name = "default", epoch_name = "auditory_onset",
#     time_windows = c(-1, 2))
# self$export_matlab()

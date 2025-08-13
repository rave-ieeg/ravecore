#' @title 'RAVE' class for epoch repository - time-frequency (internal)
#' @description
#' The repository inherits \code{link{RAVESubjectEpochRepository}}, with epoch
#' trials, and is intended for loading processed and referenced time-frequency
#' coefficients.
#' Use \code{\link{prepare_subject_time_frequency_coefficients}} to create an
#' instance.
#'
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
      return(RAVESubjectEpochTimeFreqBaseRepository$new(
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

    #' @description function to mount processed and referenced voltage signals
    mount_data = function() {

      data_type <- private$.data_type
      if(length(data_type) == 0) { return(self) }

      # self <- RAVESubjectEpochTimeFreqBaseRepository$new(
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
            inst$load_data(data_type)
            return()
          }, callback = function(inst) {
            sprintf("Loading Reference | %s", inst$number)
          },
          .globals = list(data_type = data_type)
        )
      }

      data_list <- ravepipeline::lapply_jobs(
        electrode_instances,
        function(inst) {
          ravepipeline::RAVEFileArray$new(
            inst$load_data(type = data_type)
          )
        }, callback = function(inst) {
          sprintf("Loading Time-Frequency Components | Electrode %s", inst$number)
        },
        .globals = list(data_type = data_type)
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
    },

    #' @description Export the repository to 'Matlab' for future analysis
    #' @param ... reserved for child classes
    #' @param verbose print progresses
    #' @returns The root directory where the files are stored.
    export_matlab = function(..., verbose = TRUE) {
      # self <- prepare_subject_time_frequency_coefficients(
      #     "demo/DemoSubject", electrodes = 14:16,
      #     reference_name = "default", epoch_name = "auditory_onset",
      #     time_windows = c(-1, 2))
      # root_path <- self$export_matlab()
      # private <- self$.__enclos_env__$private

      root_path <- super$export_matlab(..., verbose = verbose)
      summary_path <- file_path(root_path, "summary.yaml")
      summary <- load_yaml(summary_path)

      data_type <- private$.data_type

      if(verbose) {
        callback <- function(ii) {
          sprintf("Exporting %s|Electrode channel %d", data_type, self$electrode_list[[ii]])
        }
      } else {
        callback <- NULL
      }

      data_path <- file.path(root_path, data_type)
      dir_create2(data_path)

      data_list <- private$.data$data_list

      ravepipeline::lapply_jobs(
        seq_along(data_list),
        function(ii) {
          # ii <- 1
          nm <- names(data_list)[[ii]]
          electrode_instance <- self$electrode_instances[[nm]]
          ref_name <- electrode_instance$reference_name
          if(length(ref_name)) {
            ravecore <- asNamespace("ravecore")
            ref_name <- ravecore$parse_svec(gsub("^ref_", "", ref_name))
          }
          ref_name <- as.matrix(as.integer(ref_name))

          arr <- data_list[[nm]]
          dnames <- dimnames(arr)
          arr <- arr[dimnames = FALSE]
          ch <- as.integer(dnames$Electrode)
          ieegio::io_write_mat(
            list(
              description = sprintf("Time-frequency %s: frequency x time x trial", data_type),
              electrode = as.matrix(dnames$Electrode),
              trial_number = as.matrix(dnames$Trial),
              time_in_secs = as.matrix(dnames$Time),
              frequency = as.matrix(dnames$Frequency),
              reference = ref_name,
              data = arr
            ),
            con = file.path(data_path, sprintf("ch%04d.mat", ch))
          )
        },
        .globals = list(self = self,
                        data_path = data_path,
                        data_type = data_type,
                        data_list = data_list),
        callback = callback
      )

      summary$time_frequency_shape <- as.integer(private$.data$dim)
      summary$time_frequency_margin <- paste(names(private$.data$dimnames), collapse = " x ")
      summary$contains[["Time frequency"]] <- data_type

      save_yaml(summary, file = summary_path, sorted = TRUE)

      return(root_path)
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
#' Use \code{\link{prepare_subject_time_frequency_coefficients}} to create an
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


#' @title 'RAVE' class for epoch repository - time-frequency power
#' @description
#' The repository inherits \code{link{RAVESubjectEpochTimeFreqBaseRepository}}, with epoch
#' trials, and is intended for loading processed and referenced time-frequency
#' coefficients.
#' Use \code{\link{prepare_subject_power}} to create an
#' instance.
#' @export
RAVESubjectEpochPowerRepository <- R6::R6Class(
  classname = "RAVESubjectEpochPowerRepository",
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
      object$r6_generator <- "RAVESubjectEpochPowerRepository"
      class(object$data) <- c("RAVESubjectEpochPowerRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectEpochPowerRepository_marshal"))
      return(RAVESubjectEpochPowerRepository$new(
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
        data_type = "power",
        .class = c(.class, "rave_prepare_power"),
        ...
      )

    }

  ),
  active = list(

    #' @field power a named map of time-frequency power spectrogram,
    #' mounted by \code{mount_data}
    power = function() {
      if(private$.data$`@size`() == 0) {
        self$mount_data()
      }
      private$.data
    }

  )

)



#' @title 'RAVE' class for epoch repository - time-frequency phase
#' @description
#' The repository inherits \code{link{RAVESubjectEpochTimeFreqBaseRepository}}, with epoch
#' trials, and is intended for loading processed and referenced time-frequency
#' coefficients.
#' Use \code{\link{prepare_subject_phase}} to create an
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
      if(private$.data$`@size`() == 0) {
        self$mount_data()
      }
      private$.data
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

#' @rdname prepare_subject_with_epoch
#' @export
prepare_subject_power <- function(
    subject, electrodes = NULL, reference_name = NULL,
    epoch_name = NULL, time_windows = NULL,
    stitch_events = NULL, ..., quiet = FALSE,
    repository_id = NULL, strict = TRUE) {
  RAVESubjectEpochPowerRepository$new(
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

#' @rdname prepare_subject_with_epoch
#' @export
prepare_subject_phase <- function(
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

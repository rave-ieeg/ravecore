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
    #' @param ... passed to \code{\link{RAVESubjectEpochRepository}} constructor
    #' @param .class internally used, do not set, even if you know what this is
    initialize = function(subject, electrodes = NULL,
                          reference_name = NULL, epoch_name = NULL,
                          time_windows = NULL, stitch_events = NULL, ...,
                          quiet = FALSE, repository_id = NULL, strict = TRUE,
                          lazy_load = FALSE, .class = NULL) {
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
        .class = c(.class, "rave_prepare_subject_voltage_with_epoch"),
        ...
      )

    },

    #' @description function to mount referenced voltage signals
    #' @param force force update data; default is true
    #' @param electrodes electrodes to update for expert-use use; default is
    #' \code{NULL} (all electrode channels will be mounted)
    #' @param ... reserved
    mount_data = function(..., force = TRUE, electrodes = NULL) {

      # self <- RAVESubjectEpochVoltageRepository$new(
      #   subject = "demo/DemoSubject",
      #   electrodes = 13:16,
      #   reference_name = "default",
      #   epoch_name = "auditory_onset",
      #   time_windows = c(-1, 2),
      #   stitch_events = NULL, lazy_load = TRUE
      # )
      # private <- self$.__enclos_env__$private
      # private$.data
      # self$mount_data(electrodes = 13)
      # private$.data
      # private$.data$data_list

      electrodes <- parse_svec(electrodes)
      electrodes <- electrodes[electrodes %in% self$electrode_list]
      if(!length(electrodes)) {
        electrodes <- self$electrode_list
      }

      # check data_list
      nms <- sprintf("e_%d", electrodes)

      if( !force ) {
        exist_list <- names(private$.data$data_list)
        if(all(nms %in% exist_list)) { return(self) }
      }

      electrode_instances <- self$electrode_instances[nms]

      ref_names <- lapply(electrode_instances, function(inst) {
        if(isTRUE(inst$reference_name %in% c("noref", ""))) { return(NULL) }
        sprintf("%s_%s", inst$reference_name, inst$type)
      })
      ref_names <- unlist(unique(ref_names))
      ref_names <- ref_names[ref_names %in% names(self$reference_instances)]

      reference_instances <- self$reference_instances[ref_names]

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
        names = nms,
        lapply(data_list, "[[", "@impl")
      )

      # construct dim and dimnames
      dim <- dim(data_list[[1]])
      dim[[3]] <- length(self$electrode_list)

      dimnames <- dimnames(data_list[[1]])
      dimnames[[3]] <- self$electrode_list

      private$.data$`@mset`(
        # data_list = data_list,
        dim = structure(dim, names = names(dimnames)),
        dimnames = dimnames,
        signature = self$signature
      )

      private$.data$data_list <- as.list(private$.data$data_list)
      private$.data$data_list[nms] <- data_list[nms]

      self
    },

    #' @description Export the repository to 'Matlab' for future analysis
    #' @param ... reserved for child classes
    #' @param verbose print progresses
    #' @returns The root directory where the files are stored.
    export_matlab = function(..., verbose = TRUE) {
      # self <- prepare_subject_voltage_with_epoch(
      #     "demo/DemoSubject", electrodes = 14:16,
      #     reference_name = "default", epoch_name = "auditory_onset",
      #     time_windows = c(-1, 2))
      # root_path <- self$export_matlab()

      root_path <- super$export_matlab(..., verbose = verbose)
      summary_path <- file_path(root_path, "summary.yaml")
      summary <- load_yaml(summary_path)

      data_path <- file.path(root_path, "voltage")
      dir_create2(data_path)

      if(verbose) {
        callback <- function(ii) {
          sprintf("Exporting re-referenced voltage|Electrode channel %d", self$electrode_list[[ii]])
        }
      } else {
        callback <- NULL
      }

      ravepipeline::lapply_jobs(
        seq_along(self$electrode_list),
        function(ii) {
          electrode_channel <- self$electrode_list[[ii]]
          self$mount_data(force = FALSE, electrodes = electrode_channel)
          nm <- sprintf("e_%d", electrode_channel)

          electrode_instance <- self$electrode_instances[[nm]]
          ref_name <- electrode_instance$reference_name
          if(length(ref_name)) {
            ravecore <- asNamespace("ravecore")
            ref_name <- ravecore$parse_svec(gsub("^ref_", "", ref_name))
          }
          ref_name <- as.matrix(as.integer(ref_name))

          arr <- self$voltage$data_list[[nm]]
          dnames <- dimnames(arr)
          arr <- arr[dimnames = FALSE, drop = FALSE]

          data <- list(
            description = "Re-referenced voltage: time x trial",
            electrode = as.matrix(dnames$Electrode),
            trial_number = as.matrix(dnames$Trial),
            time_in_secs = as.matrix(dnames$Time),
            reference_channels = ref_name,
            data = arr
          )
          if(length(electrode_instance$reference)) {
            reference <- electrode_instance$reference$load_data("voltage")
            data$reference <- reference[dimnames = FALSE, drop = FALSE]
          } else {
            data$reference <- as.matrix(0)
          }

          ieegio::io_write_mat(data, con = file.path(data_path, sprintf("ch%04d.mat", electrode_channel)))
        },
        .globals = list(self = self, data_path = data_path),
        callback = callback
      )

      summary$voltage_shape <- as.integer(self$voltage$dim)
      summary$voltage_margin <- paste(names(self$voltage$dimnames), collapse = " x ")
      summary$contains[["Voltage"]] <- "voltage"

      save_yaml(summary, file = summary_path, sorted = TRUE)

      return(root_path)
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
      self$get_container()
    }

  )

)


#' @rdname prepare_subject_with_epoch
#' @export
prepare_subject_voltage_with_epoch <- function(
    subject, electrodes = NULL, reference_name = NULL,
    epoch_name = NULL, time_windows = NULL,
    stitch_events = NULL, ..., quiet = FALSE,
    repository_id = NULL, strict = TRUE) {
  RAVESubjectEpochVoltageRepository$new(
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

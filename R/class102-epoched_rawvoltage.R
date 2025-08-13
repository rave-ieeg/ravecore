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
    #' @param quiet see field \code{quiet}
    #' @param repository_id see field \code{repository_id}
    #' @param strict whether the mode should be strict; default is true and
    #' errors out when subject is missing
    #' @param lazy_load whether to delay calling \code{mount_data};
    #' default is false
    #' @param reference_name ignored, always \code{'noref'} for raw voltage
    #' data
    #' @param ... passed to \code{\link{RAVESubjectEpochRepository}} constructor
    #' @param .class internally used, do not set, even if you know what this is
    initialize = function(subject, electrodes = NULL, epoch_name = NULL,
                          time_windows = NULL, stitch_events = NULL, ...,
                          quiet = FALSE, repository_id = NULL, strict = TRUE,
                          lazy_load = FALSE, reference_name = "noref",
                          .class = NULL) {
      subject <- as_rave_subject(subject, strict = strict)
      super$initialize(
        subject = subject,
        electrodes = electrodes,
        epoch_name = epoch_name,
        time_windows = time_windows,
        stitch_events = stitch_events,
        reference_name = "noref",
        quiet = quiet,
        repository_id = repository_id,
        lazy_load = lazy_load,
        .class = c(.class, "rave_prepare_subject_raw_voltage_with_epoch"),
        ...
      )
      # reinforce
      private$.reference_name <- "noref"

    },

    #' @description function to mount raw voltage signals
    #' @param force force update data; default is true
    #' @param electrodes electrodes to update for expert-use use; default is
    #' \code{NULL} (all electrode channels will be mounted)
    #' @param ... reserved
    mount_data = function(..., force = TRUE, electrodes = NULL) {

      # self <- RAVESubjectEpochRawVoltageRepository$new(
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
      # self <- prepare_subject_raw_voltage_with_epoch(
      #     "demo/DemoSubject", electrodes = 14:16,
      #     reference_name = "default", epoch_name = "auditory_onset",
      #     time_windows = c(-1, 2))
      root_path <- super$export_matlab(..., verbose = verbose)
      summary_path <- file_path(root_path, "summary.yaml")
      summary <- load_yaml(summary_path)

      data_path <- file.path(root_path, "raw_voltage")
      dir_create2(data_path)

      if(verbose) {
        callback <- function(ii) {
          sprintf("Exporting raw voltage|Electrode channel %d", self$electrode_list[[ii]])
        }
      } else {
        callback <- NULL
      }

      ravepipeline::lapply_jobs(
        seq_along(self$raw_voltage$data_list),
        function(ii) {
          arr <- self$raw_voltage$data_list[[ii]]
          dnames <- dimnames(arr)
          arr <- arr[dimnames = FALSE]
          ch <- as.integer(dnames$Electrode)
          ieegio::io_write_mat(
            list(
              description = "Raw voltage: no reference, time x trial",
              electrode = as.matrix(dnames$Electrode),
              trial_number = as.matrix(dnames$Trial),
              time_in_secs = as.matrix(dnames$Time),
              reference = as.matrix(integer()),
              data = arr
            ),
            con = file.path(data_path, sprintf("ch%04d.mat", ch))
          )
        },
        .globals = list(self = self, data_path = data_path),
        callback = callback
      )

      summary$raw_voltage_shape <- as.integer(self$raw_voltage$dim)
      summary$raw_voltage_margin <- paste(names(self$raw_voltage$dimnames), collapse = " x ")
      summary$contains[["Raw voltage"]] <- "raw_voltage"

      save_yaml(summary, file = summary_path, sorted = TRUE)

      return(root_path)
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
      if(private$.data$`@size`() == 0) {
        self$mount_data()
      }
      private$.data
    },

    #' @field reference_table reference table, all channels will be marked as no reference
    reference_table = function() {
      subject <- private$.subject
      if(!length(subject$electrodes)) {
        stop("No electrode/channel found under this subject. Please import data first.")
      }
      reference_name <- "noref"
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
        }
      }
      reference_table <- subject$get_reference(reference_name)
      return(reference_table)
    }

  )
)


#' @rdname prepare_subject_with_epoch
#' @export
prepare_subject_raw_voltage_with_epoch <- function(
    subject, electrodes = NULL,
    epoch_name = NULL, time_windows = NULL,
    stitch_events = NULL, ..., quiet = TRUE,
    repository_id = NULL, strict = TRUE) {
  RAVESubjectEpochRawVoltageRepository$new(
    subject = subject,
    electrodes = electrodes,
    epoch_name = epoch_name,
    time_windows = time_windows,
    stitch_events = stitch_events,
    quiet = quiet,
    repository_id = repository_id,
    strict = strict,
    ...
  )
}

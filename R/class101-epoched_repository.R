#' 'RAVE' class for epoch repository
#' @description
#' Compared to \code{\link{RAVESubjectBaseRepository}}, this repository
#' requires epoch information. please use
#' \code{\link{prepare_subject_with_epochs}} to instantiate this repository.
#'
#' @seealso \code{\link{prepare_subject_with_epochs}}
#' @export
RAVESubjectEpochRepository <- R6::R6Class(
  classname = "RAVESubjectEpochRepository",
  portable = TRUE,
  inherit = RAVESubjectBaseRepository,
  lock_objects = FALSE,
  lock_class = TRUE,
  cloneable = TRUE,

  private = list(
    .epoch_name = character(),
    .epoch = NULL,
    .stitch_events = NULL,
    .time_windows = NULL,
    .data = NULL,
    .data_type = character()
  ),
  public = list(


    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectEpochRepository"
      object$data$epoch_name <- private$.epoch_name
      object$data$time_windows <- private$.time_windows
      object$data$stitch_events <- private$.stitch_events
      class(object$data) <- c("RAVESubjectEpochRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectEpochRepository_marshal"))
      repo <- RAVESubjectEpochRepository$new(
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
    #' @param lazy_load whether to delay (lazy) the evaluation \code{mount_data}
    #' @param ... passed to \code{\link{RAVESubjectBaseRepository}} constructor
    #' @param .class internally used, do not set, even if you know what this is
    initialize = function(subject, electrodes = NULL,
                          reference_name = NULL, epoch_name = NULL,
                          time_windows = NULL, stitch_events = NULL, ...,
                          quiet = FALSE, repository_id = NULL, strict = TRUE,
                          lazy_load = FALSE, .class = NULL) {

      .class <- c(.class, "rave_prepare_with_epoch", "RAVESubjectEpochRepository")

      subject <- as_rave_subject(subject, strict = strict)
      super$initialize(subject = subject, electrodes = electrodes,
                       reference_name = reference_name, quiet = quiet,
                       repository_id = repository_id,
                       .class = .class)

      private$.data <- fastmap2()

      self$time_windows <- time_windows
      self$set_epoch(epoch_name, stitch_events)

      if(!lazy_load) {
        self$mount_data()
      }
    },

    #' @description Export the repository to 'Matlab' for future analysis
    #' @param ... reserved for child classes
    #' @param verbose print progresses
    #' @returns The root directory where the files are stored.
    export_matlab = function(..., verbose = TRUE) {
      # self <- prepare_subject_with_epochs(
      #     "demo/DemoSubject", electrodes = 14:16,
      #     reference_name = "default", epoch_name = "auditory_onset",
      #     time_windows = c(-1, 2))
      root_path <- super$export_matlab(..., verbose = verbose)
      summary_path <- file_path(root_path, "summary.yaml")
      summary <- load_yaml(summary_path)

      summary$epoch_name <- self$epoch_name
      summary$time_windows <- self$time_windows
      summary$stitch_events <- self$stitch_events
      summary$sample_rates <- self$sample_rates

      epoch_root <- dir_create2(file_path(root_path, "with_epochs"))

      epoch_path <- file.path(epoch_root, "epoch.csv")
      export_table(x = self$epoch_table, file = epoch_path)
      summary$contains[["Epoch table"]] <- "with_epochs/epoch.csv"


      # ---- save data
      # self <- prepare_subject_raw_voltage_with_epochs(
      #     "demo/DemoSubject", electrodes = 14:16,
      #     reference_name = "default", epoch_name = "auditory_onset",
      #     time_windows = c(-1, 2))
      data_type <- private$.data_type
      if(length(data_type)) {
        data_path <- dir_create2(file_path(epoch_root, data_type))

        if(verbose) {
          callback <- function(ii) {
            sprintf("Exporting %s with epochs|Electrode channel %d", data_type, self$electrode_list[[ii]])
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

            container <- self$get_container()
            arr <- container$data_list[[nm]]
            dnames <- dimnames(arr)
            arr <- arr[dimnames = FALSE, drop = FALSE]

            if(length(electrode_instance$reference)) {
              reference <- electrode_instance$reference$load_data_with_epochs(data_type)
              reference <- reference[dimnames = FALSE, drop = FALSE]
            } else {
              reference <- as.matrix(0)
            }
            data <- list(
              description = sprintf("RAVE repository type: %s (epoched)", data_type),
              electrode = as.matrix(dnames$Electrode),
              trial_number = as.matrix(dnames$Trial),
              time_in_secs = as.matrix(dnames$Time),
              reference_channels = ref_name,
              data = arr,
              reference = reference
            )
            if("Frequency" %in% names(dnames)) {
              data$frequency <- dnames$Frequency
            }

            ieegio::io_write_mat(data, con = file.path(data_path, sprintf("ch%04d.mat", electrode_channel)))
          },
          .globals = list(self = self, data_path = data_path, data_type = data_type),
          callback = callback
        )

        container <- self$get_container()
        dames <- container$dimnames
        summary[[data_type]] <- list(
          path = sprintf('with_epochs/%s', data_type),
          names = names(container$dimnames),
          shape = as.integer(unname(container$dim))
        )
        summary$contains[["RAVE Repository Types"]] <- c(summary$contains[["RAVE Repository Types"]], data_type)

      }

      save_yaml(summary, file = summary_path, sorted = TRUE)

      return(root_path)
    },

    #' @description change trial epoch profiles
    #' @param epoch_name name of epoch table
    #' @param stitch_events events to stitch
    set_epoch = function(epoch_name, stitch_events = NULL) {
      subject <- self$subject
      available_epoch_names <- subject$epoch_names
      time_windows <- self$time_windows

      if(missing(epoch_name) || !length(epoch_name) || !length(available_epoch_names)){
        if(!length(available_epoch_names)){
          ravepipeline::logger("No epoch file found in this subject. Please check meta folder and make sure you have generated epoch_*.csv")
        }
        epoch_name <- subject$get_default('epoch_name', default_if_missing = available_epoch_names[[1]])
        ravepipeline::logger(
          "Epoch name is unspecified, choosing a default one: ", epoch_name,
          ". This is not recommended. Please specify epoch names explicitly!",
          level = "warning")
      }
      epoch_name <- epoch_name[[1]]
      if(!epoch_name %in% available_epoch_names){
        ravepipeline::logger(
          sprintf(
            "Epoch name %s does not exist. Please make sure the epoch table exists under the subject meta folder, with name `epoch_%s.csv`",
            sQuote(epoch_name), epoch_name
          ),
          level = "fatal"
        )
      }

      private$.epoch_name <- epoch_name
      private$.epoch <- subject$get_epoch(
        epoch_name = epoch_name,
        trial_starts = min(unlist(time_windows)),
        as_table = FALSE
      )


      if(length(stitch_events)) {
        # check if the events are in epochs
        available_events <- private$.epoch$available_events
        if(length(stitch_events) == 1) {
          stitch_events <- c(stitch_events, stitch_events)
        } else {
          stitch_events <- stitch_events[c(1, 2)]
        }
        stitch_events[tolower(stitch_events) %in% c("trial onset")] <- ""
        if(!all(stitch_events %in% available_events)) {
          ravepipeline::logger(
            "Cannot find events to stitch: ",
            paste(sQuote(stitch_events[!stitch_events %in% available_events]), collapse = ", "),
            ". Available events: ",
            paste(sQuote(available_events), collapse = ", "),
            level = "warning"
          )
        }
        stitch_events_start <- stitch_events[[1]] %OF% available_events
        stitch_events_end <- stitch_events[[2]] %OF% available_events
        stitch_events <- c(stitch_events_start, stitch_events_end)
      } else {
        stitch_events <- NULL
      }
      private$.stitch_events <- stitch_events

      invisible(self)
    },

    #' @description function to mount data, not doing anything in this
    #' class, but may be used by child classes
    #' @param force force update data; default is true
    #' @param electrodes electrodes to update; default is \code{NULL} (all
    #' electrode channels)
    #' @param ... reserved
    mount_data = function(..., force = TRUE, electrodes = NULL) {

    },

    #' @description get container where loaded data are stored
    #' @param ... passed to \code{mount_data}
    #' @returns A named map, typically with data arrays, shape/dimension
    #' information
    get_container = function(...) {
      if(private$.data$`@size`() == 0) {
        self$mount_data(...)
      }
      private$.data
    }
  ),
  active = list(

    #' @field needs_update write-only attribute when subject needs to be
    #' reloaded from the disk and reference table needs to be updated, use
    #' \code{repo$needs_update <- TRUE}
    needs_update = function(v) {
      if(!missing(v) && v) {
        private$update_subject()
        self$set_epoch(private$.epoch_name, private$.stitch_events)
        self$mount_data()
      }
      invisible()
    },

    #' @field sample_rates a named list of sampling frequencies; the names
    #' are signal types (\code{'LFP'}, \code{'Auxiliary'}, or \code{'Spike'})
    #' and the values are the sampling frequencies
    sample_rates = function() {
      electrodes <- self$subject$electrodes
      sel <- electrodes %in% self$electrode_list
      electrodes <- electrodes[sel]
      raw_sample_rates <- self$subject$raw_sample_rates[sel]
      electrode_types <- self$subject$electrode_types[sel]

      df <- unique(data.frame(
        type = electrode_types,
        sample_rate = raw_sample_rates
      ))
      structure(
        names = df$type,
        as.list(as.double(df$sample_rate))
      )
    },

    #' @field sample_rate a single number of the sample rate; if the electrode
    #' channels contain local-field potential \code{'LFP'} signal type, then
    #' the sample rate is the \code{'LFP'} sample rate; otherwise the
    #' sample rate is \code{'Spike'} channel sample rate, if exists, or whatever
    #' comes first. This field is for backward compatibility support, use
    #' \code{sample_rates} for more accurate number
    sample_rate = function() {
      sample_rates <- self$sample_rates
      if("LFP" %in% names(sample_rates)) {
        return(sample_rates[["LFP"]])
      }
      if("Spike" %in% names(sample_rates)) {
        return(sample_rates[["Spike"]])
      }
      sample_rates[[1]]
    },

    #' @field epoch_name name of the epoch table
    epoch_name = function() {
      private$.epoch_name
    },

    #' @field epoch \code{\link{RAVEEpoch}} instance
    epoch = function() {
      private$.epoch
    },

    #' @field epoch_table epoch table, equivalent to
    #' \code{repository$epoch$table}
    epoch_table = function() {
      self$epoch$table
    },

    #' @field stitch_events events where \code{time_windows} are based on
    stitch_events = function() {
      private$.stitch_events
    },

    #' @field time_windows list of time ranges to load; the time is relative
    #' to \code{stitch_events}; default is trial onset
    time_windows = function(v) {
      subject <- self$subject
      if(!missing(v)) {
        time_windows <- v
        if(length(time_windows)) {
          time_windows <- validate_time_window(time_windows)
          private$.time_windows <- time_windows
        }
      } else {
        time_windows <- private$.time_windows
      }
      if(!length(time_windows)) {
        time_windows <- subject$get_default("time_windows", default_if_missing = list(c(-1, 2)))
        ravepipeline::logger("No time_windows specified, using default: ",
                             deparse(time_windows), level = "info")
        time_windows <- validate_time_window(time_windows)
        private$.time_windows <- time_windows
      }
      private$.time_windows
    },

    #' @field electrode_table the entire electrode table with reference information
    electrode_table = function() {
      self$subject$get_electrode_table(
        electrodes = self$electrode_list,
        reference_name = self$reference_name)
    },

    #' @field electrode_instances electrode channel instance helpers for
    #' loading electrode data
    electrode_instances = function() {
      electrode_instances <- super$electrode_instances
      structure(
        names = names(electrode_instances),
        lapply(electrode_instances, function(inst) {
          inst$set_epoch(self$epoch, stitch_events = self$stitch_events)
          inst$trial_intervals <- self$time_windows
          inst
        })
      )
    },

    #' @field reference_instances instances of reference channels, for
    #' referencing on the fly, used for \code{electrode_instances}
    reference_instances = function() {
      reference_instances <- super$reference_instances
      structure(
        names = names(reference_instances),
        lapply(reference_instances, function(inst) {
          inst$set_epoch(self$epoch, stitch_events = self$stitch_events)
          inst$trial_intervals <- self$time_windows
          inst
        })
      )
    },

    #' @field digest_key a list of repository data used to generate
    #' repository signature
    digest_key = function() {
      list(
        subject_id = self$subject$subject_id,
        epoch_table = self$epoch$table,
        reference_table = self$reference_table,
        electrode_list = self$electrode_list,
        electrode_signal_types = unname(self$electrode_signal_types),
        time_windows = self$time_windows,
        stitch_events = self$stitch_events
      )
    }

  )

)

#' @title 'RAVE' repository: with epochs
#' @returns A \code{\link{RAVESubjectEpochRepository}} instance
#' @inheritParams prepare_subject_bare0
#' @param epoch_name name of the epoch trial table
#' @param time_windows numeric vector with even lengths, the time start
#' and end of the trials, for example, \code{c(-1, 2)} means load
#' 1 second before the trial onset and 2 seconds after trial onset
#' @param stitch_events events where the \code{time_windows} is based;
#' default is trial onset (\code{NULL})
#' @param strict whether to check existence of subject before loading data;
#' default is true
#' @examples
#'
#' \dontrun{
#'
#'
#' repository <- prepare_subject_with_epochs(
#'   "demo/DemoSubject", electrodes = 14:16,
#'   reference_name = "default", epoch_name = "auditory_onset",
#'   time_windows = c(-1, 2))
#'
#' print(repository)
#'
#' head(repository$epoch$table)
#'
#' electrodes <- repository$electrode_instances
#'
#' # Channel 14
#' e <- electrodes$e_14
#'
#' # referenced voltage
#' voltage <- e$load_data_with_epochs("voltage")
#'
#' # 6001 time points (2000 sample rate)
#' # 287 trials
#' # 1 channel
#' dim(voltage)
#'
#' ravetools::plot_signals(t(voltage[, 1:10, 1]),
#'                         sample_rate = 2000,
#'                         ylab = "Trial",
#'                         main = "First 10 trials")
#'
#' }
#' @export
prepare_subject_with_epochs <- function(
    subject, electrodes = NULL,
    reference_name = NULL, epoch_name = NULL,
    time_windows = NULL, stitch_events = NULL, ...,
    quiet = FALSE, repository_id = NULL, strict = TRUE) {
  RAVESubjectEpochRepository$new(
    subject = subject, electrodes = electrodes,
    reference_name = reference_name, epoch_name = epoch_name,
    time_windows = time_windows, stitch_events = stitch_events, ...,
    quiet = quiet, repository_id = repository_id, strict = strict)
}







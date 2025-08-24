restore_block_container_from_snapshot = function(container, snapshot) {
  if(is.null(snapshot)) { return(invisible()) }
  tryCatch({

    block_names <- names(snapshot)
    lapply(block_names, function(block_name) {
      block_snapshot <- snapshot[[block_name]]
      signal_types <- names(block_snapshot)
      block_data <- structure(
        names = signal_types,
        lapply(signal_types, function(signal_type) {
          signal_data <- block_snapshot[[signal_type]]
          data_array <- ravepipeline::RAVEFileArray$public_methods$`@unmarshal`(signal_data$data)
          signal_data$data <- data_array$`@impl`
          n_timepoints <- signal_data$dimnames$Time
          signal_data$dimnames$Time <- seq(0, by = 1 / signal_data$sample_rate,
                                           length.out = n_timepoints)
          signal_data
        })
      )
      container[[block_name]] <- block_data
      return()
    })

  }, error = function(e) {
  })
  invisible()
}

#' 'RAVE' class for loading entire recording block repository
#' @description
#' Compared to \code{\link{RAVESubjectBaseRepository}}, this repository
#' requires specifying block information. please use
#' \code{\link{prepare_subject_with_blocks}} to instantiate this repository.
#'
#' @seealso \code{\link{prepare_subject_with_blocks}}
RAVESubjectRecordingBlockRepository <- R6::R6Class(
  classname = "RAVESubjectRecordingBlockRepository",
  portable = TRUE,
  inherit = RAVESubjectBaseRepository,
  lock_objects = FALSE,
  lock_class = TRUE,
  cloneable = TRUE,

  private = list(
    .blocks = character(),
    .data = NULL,
    .data_type = character()
  ),
  public = list(

    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectRecordingBlockRepository"
      object$data$blocks <- private$.blocks
      if(length(private$.data)) {
        block_names <- names(private$.data)
        container_snapshot <- structure(
          names = block_names,
          lapply(block_names, function(block_name) {
            block_data <- private$.data[[block_name]]
            signal_types <- names(block_data)
            structure(
              names = signal_types,
              lapply(signal_types, function(signal_type) {
                signal_data <- block_data[[signal_type]]
                data_array <- ravepipeline::RAVEFileArray$new(signal_data$data,
                                                              temporary = FALSE)
                signal_data$data <- data_array$`@marshal`()
                signal_data$dimnames$Time <- length(signal_data$dimnames$Time)
                signal_data
              })
            )
          })
        )
        object$data$container_snapshot <- container_snapshot
      }
      class(object$data) <- c("RAVESubjectRecordingBlockRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectRecordingBlockRepository_marshal"))
      repo <- RAVESubjectRecordingBlockRepository$new(
        subject = RAVESubject$public_methods$`@unmarshal`(object$data$subject),
        electrodes = object$data$intended_electrode_list,
        reference_name = object$data$reference_name,
        blocks = object$data$blocks,
        quiet = TRUE,
        repository_id = object$data$repository_id,
        strict = TRUE,
        lazy_load = TRUE
      )
      restore_block_container_from_snapshot(
        container = repo$`@get_container`(),
        snapshot = object$data$container_snapshot
      )
      repo$`@restored` <- TRUE
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

      .class <- c(.class, "prepare_subject_with_blocks", "RAVESubjectRecordingBlockRepository")

      subject <- as_rave_subject(subject, strict = strict)

      if(is.null(blocks)) { blocks <- subject$blocks }
      blocks <- blocks[blocks %in% subject$blocks]
      if(!length(blocks)) {
        stop("No block is chosen. Please load one or more blocks: ", paste(sQuote(subject$blocks), collapse = ", "))
      }

      super$initialize(subject = subject, electrodes = electrodes,
                       reference_name = reference_name, quiet = quiet,
                       repository_id = repository_id,
                       .class = .class)

      private$.data <- fastmap2()

      private$.blocks <- blocks

      if(!lazy_load) {
        self$mount_data(force = FALSE)
      }
    },

    #' @description Export the repository to 'Matlab' for future analysis
    #' @param ... reserved for child classes
    #' @param verbose print progresses
    #' @returns The root directory where the files are stored.
    export_matlab = function(..., verbose = TRUE) {
      # self <- prepare_subject_voltage_with_blocks(
      #     "demo/DemoSubject", electrodes = 14:16,
      #     reference_name = "default")
      # private <- self$.__enclos_env__$private

      root_path <- super$export_matlab(..., verbose = verbose)
      summary_path <- file_path(root_path, "summary.yaml")
      summary <- load_yaml(summary_path)

      blocks <- self$blocks
      summary$recording_blocks <- blocks
      summary$sample_rates <- self$sample_rates

      block_root <- dir_create2(file_path(root_path, "with_blocks"))

      # save data
      data_type <- private$.data_type

      if(length(data_type)) {
        electrode_list <- self$electrode_list
        data_path <- file_path(block_root, data_type)

        if(verbose) {
          callback <- function(block) {
            sprintf("Exporting %s with blocks|Block %s", data_type, block)
          }
        } else {
          callback <- NULL
        }

        self$mount_data(force = FALSE)
        container <- self$get_container()

        ravepipeline::lapply_jobs(
          x = blocks,
          fun = function(block) {
            # block <- blocks[[1]]
            ravecore <- asNamespace("ravecore")
            file_path <- ravecore$file_path
            dir_create2 <- ravecore$dir_create2

            block_path <- dir_create2(file_path(data_path, block))
            block_data <- container[[block]]

            for (stype in names(block_data)) {
              stype_path <- file_path(block_path, sprintf("%s.mat", stype))
              stype_data <- block_data[[stype]]
              signal_data <- subset(stype_data$data,
                                    Electrode ~ Electrode %in% electrode_list,
                                    drop = FALSE)
              dimnames(signal_data) <- NULL

              data <- list(
                description = sprintf("RAVE repository type: %s (recording blocks)", data_type),
                electrode = as.matrix(stype_data$dimnames$Electrode),
                block_name = block,
                time_in_secs = as.matrix(stype_data$dimnames$Time),
                # reference_channels =
                # reference =
                data = signal_data
              )

              if ("Frequency" %in% names(stype_data$dimnames)) {
                data$frequency <- stype_data$dimnames$Frequency
              }

              ieegio::io_write_mat(data, con = stype_path)

            }
          },
          .globals = list(
            data_path = data_path,
            container = container,
            data_type = data_type,
            electrode_list = electrode_list
          ),
          callback = callback,
          # Main session, no parallel
          .workers = 1L,
          .always = FALSE
        )

        summary[[data_type]] <- list(
          path = sprintf('with_blocks/%s', data_type)
        )
        summary$contains[["RAVE Repository Types"]] <- c(summary$contains[["RAVE Repository Types"]], data_type)
      }

      save_yaml(summary, file = summary_path, sorted = TRUE)

      return(root_path)
    },

    #' @description get container where loaded data are stored
    #' @param force,... passed to \code{mount_data}
    #' @returns A named map, typically with data arrays, shape/dimension
    #' information
    get_container = function(force = FALSE, ...) {
      if(private$.data$`@size`() == 0) {
        self$mount_data(force = force, ...)
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
        self$mount_data(force = FALSE)
      }
      invisible()
    },

    # field sample_rates a named list of sampling frequencies; the names
    # are signal types (\code{'LFP'}, \code{'Auxiliary'}, or \code{'Spike'})
    # and the values are the sampling frequencies
    # sample_rates = function() {
    #   electrodes <- self$subject$electrodes
    #   sel <- electrodes %in% self$electrode_list
    #   electrodes <- electrodes[sel]
    #   raw_sample_rates <- self$subject$raw_sample_rates[sel]
    #   electrode_types <- self$subject$electrode_types[sel]
    #
    #   df <- unique(data.frame(
    #     type = electrode_types,
    #     sample_rate = raw_sample_rates
    #   ))
    #   structure(
    #     names = df$type,
    #     as.list(as.double(df$sample_rate))
    #   )
    # },

    #' @field blocks names of recording blocks
    blocks = function() {
      private$.blocks
    },

    #' @field electrode_table the entire electrode table with reference information
    electrode_table = function() {
      self$subject$get_electrode_table(
        electrodes = self$electrode_list,
        reference_name = self$reference_name)
    },


    #' @field digest_key a list of repository data used to generate
    #' repository signature
    digest_key = function() {
      list(
        subject_id = self$subject$subject_id,
        blocks = self$blocks,
        rave_data_type = private$.data_type,
        reference_table = self$reference_table,
        electrode_list = self$electrode_list,
        electrode_signal_types = unname(self$electrode_signal_types)
      )
    }

  )

)

#' @name prepare_subject_with_blocks
#' @title 'RAVE' repository: with entire recording blocks
#' @description
#' Loads recording blocks - continuous recording chunks, typically a run
#' of minutes.
#' @returns A \code{\link{RAVESubjectRecordingBlockRepository}} instance
#' @inheritParams prepare_subject_bare0
#' @param blocks names of the recording blocks to load, can be queried via
#' \code{subject$blocks}
#' @param strict whether to check existence of subject before loading data;
#' default is true
#' @param downsample positive integer or \code{NA}, indicating whether the
#' signals should be down-sampled during loading, for voltage traces only;
#' default is \code{NA}, meaning no down-sampling
#'
#' @details
#' \code{prepare_subject_with_blocks} does not actually load any signal data.
#' Its existence is simply for backward compatibility. It instantiates a
#' super-class of the rest of methods. Therefore, please refer to the rest of
#' the methods for loading specific data types.
#'
#' If you do not need to analyze super high-frequency signals,
#' it is recommended to set a proper \code{downsample} value to down-sample
#' the signals while loading voltage traces. This helps optimizing the data
#' storage and speed up computation (significantly). For example, suppose
#' you have 200 channels sampled at 30,000 Hz, a 30-minute recording will
#' cost around 80+ gigabyte memory only to store, let along the storage needed
#' to compute analyses and time needed to perform those analyses. Down-sampling
#' the channels helps a lot. If you are mostly interested in signals below
#' 100 Hz, then down-sampling voltage traces to 400 Hz will preserve the
#' frequency components needed, and it takes 1.2 gigabytes to hold the same
#' recording in memory.
#'
#' Due to the large-data nature of blocks of signals, the repository will
#' prepare cache files for all the channels, allowing users to load the
#' cached data later without needing to reload
#'
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' # ---- An use-case example ------------------------------------------------
#' # Install subject via install_subject("DemoSubject")
#' subject <- as_rave_subject("demo/DemoSubject")
#'
#' # list all blocks
#' subject$blocks
#'
#' repository <- prepare_subject_with_voltage_blocks(
#'   subject,
#'   electrodes = 13:16,
#'   blocks = "008",
#'   reference = "default"
#' )
#'
#' print(repository)
#'
#' repository$blocks
#'
#' # get data
#' container <- repository$get_container()
#'
#' # block data
#' container$`008`
#' lfp_list <- container$`008`$LFP
#' channel_sample_rate <- lfp_list$sample_rate
#'
#' # Even we only load channels 14-16, all the channels are here for
#' # in case we want to use the cache for future purposes
#' lfp_list$dimnames$Electrode
#'
#' # Plot all loaded channels
#' channel_sel <- lfp_list$dimnames$Electrode %in% c(14, 15, 16)
#' channel_signals <- lfp_list$data[, channel_sel,
#'                                  drop = FALSE,
#'                                  dimnames = FALSE]
#'
#' ravetools::plot_signals(t(channel_signals),
#'                         sample_rate = channel_sample_rate,
#'                         channel_names = 14:16)
#'
#' # Load channel 14 and plot pwelch
#' channel_sel <- lfp_list$dimnames$Electrode == 14
#'
#' channel_signals <- lfp_list$data[, channel_sel,
#'                                  drop = TRUE,
#'                                  dimnames = FALSE]
#'
#' ravetools::diagnose_channel(channel_signals,
#'                             srate = channel_sample_rate,
#'                             name = "Channel 14",
#'                             nclass = 30)
#'
#' # ---- Use cache ---------------------------------------------------
#'
#' subject <- as_rave_subject("demo/DemoSubject")
#'
#' # Lazy-load block 008
#' repository <- prepare_subject_with_voltage_blocks(
#'   subject,
#'   electrodes = 13:16,
#'   blocks = "008",
#'   reference = "default",
#'   lazy_load = TRUE  # <-- trick
#' )
#'
#' # Immediately load data with force=FALSE to use cache if exists
#' repository$mount_data(force = FALSE)
#'
#' # ---- More examples ---------------------------------------------
#'
#'
#' subject <- as_rave_subject("demo/DemoSubject")
#' repository <- prepare_subject_power_with_blocks(
#'   subject,
#'   electrodes = 14,
#'   blocks = "008",
#'   reference_name = "default"
#' )
#'
#' block_008 <- repository$power$`008`$LFP
#'
#' channel_sel <- block_008$dimnames$Electrode == 14
#'
#' # Drop electrode margin
#' power <- block_008$data[, , channel_sel,
#'                         drop = TRUE, dimnames = FALSE]
#'
#' # global baseline
#' power_baselined_t <- 10 * log10(t(power))
#' power_baselined_t <- power_baselined_t - rowMeans(power_baselined_t)
#'
#' ravetools::plot_signals(
#'   power_baselined_t,
#'   sample_rate = block_008$sample_rate,
#'   channel_names = block_008$dimnames$Frequency,
#'   space = 1,
#'   start_time = 20,
#'   duration = 30, ylab = "Frequency",
#'   main = "Channel 14 - Power with Global Baseline (20-50 sec)"
#' )
#'
#'
#' }
#' @export
prepare_subject_with_blocks <- function(
    subject, electrodes = NULL, blocks = NULL,
    reference_name = NULL, ...,
    quiet = FALSE, repository_id = NULL, strict = TRUE) {
  ravepipeline::logger("Function `prepare_subject_with_blocks` is for backward compatibility considerations. Please use other functions instead; see `?prepare_subject_with_blocks` for more information.", level = "warning")
  RAVESubjectRecordingBlockRepository$new(
    subject = subject, electrodes = electrodes,
    reference_name = reference_name, blocks = blocks, ...,
    quiet = quiet, repository_id = repository_id, strict = strict)
}







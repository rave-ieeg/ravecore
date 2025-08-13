#' 'RAVE' class for loading entire block of 'spectrogram' coefficients
#' @seealso \code{\link{prepare_subject_with_blocks}}
#' @export
RAVESubjectRecordingBlockTimeFreqCoefRepository <- R6::R6Class(
  classname = "RAVESubjectRecordingBlockTimeFreqCoefRepository",
  portable = TRUE,
  inherit = RAVESubjectRecordingBlockRepository,
  lock_objects = FALSE,
  lock_class = TRUE,
  cloneable = TRUE,

  private = list(
    .data = NULL,
    .data_type = "wavelet-coefficient"
  ),
  public = list(


    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectRecordingBlockTimeFreqCoefRepository"
      class(object$data) <- c("RAVESubjectRecordingBlockTimeFreqCoefRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectRecordingBlockTimeFreqCoefRepository_marshal"))
      repo <- RAVESubjectRecordingBlockTimeFreqCoefRepository$new(
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
                  "prepare_subject_time_frequency_coefficients_with_blocks",
                  "RAVESubjectRecordingBlockTimeFreqCoefRepository")

      subject <- as_rave_subject(subject, strict = strict)
      super$initialize(subject = subject, electrodes = electrodes,
                       reference_name = reference_name, quiet = quiet,
                       repository_id = repository_id, blocks = blocks,
                       lazy_load = lazy_load,
                       .class = .class)
    },

    #' @description Export the repository to 'Matlab' for future analysis
    #' @param ... reserved for child classes
    #' @param verbose print progresses
    #' @returns The root directory where the files are stored.
    export_matlab = function(..., verbose = TRUE) {
      # self <- prepare_subject_with_blocks(
      #     "demo/DemoSubject", electrodes = 14:16,
      #     reference_name = "default", epoch_name = "auditory_onset",
      #     time_windows = c(-1, 2))
      root_path <- super$export_matlab(..., verbose = verbose)
      summary_path <- file_path(root_path, "summary.yaml")
      summary <- load_yaml(summary_path)

      summary$blocks <- self$blocks
      summary$sample_rates <- self$sample_rates

      save_yaml(summary, file = summary_path, sorted = TRUE)

      return(root_path)
    },

    #' @description function to mount data, not doing anything in this
    #' class, but may be used by child classes
    #' @param force force update data; default is true; set to false
    #' to use cache
    #' @param electrodes electrodes to update; default is \code{NULL} (all
    #' electrode channels)
    #' @param ... reserved
    mount_data = function(..., force = TRUE, electrodes = NULL) {

      # self <- RAVESubjectRecordingBlockTimeFreqCoefRepository$new(subject = "demo/DemoSubject", lazy_load = TRUE)
      # private <- self$.__enclos_env__$private
      # private$.data
      # electrodes <- 13
      # self$mount_data(electrodes = 16, force = FALSE)

      data_type <- private$.data_type
      if(!length(data_type)) { return(self) }

      blocks <- self$blocks
      subject <- self$subject
      sample_rates <- self$sample_rates
      signal_types <- unique(self$electrode_signal_types)
      reference_table <- self$reference_table
      reference_table <- reference_table[order(reference_table$Electrode), ]
      frequencies <- as.double(subject$preprocess_settings$wavelet_params$frequencies)

      # determine electrodes to load
      electrodes <- parse_svec(electrodes)
      electrodes <- electrodes[electrodes %in% self$electrode_list]
      if(!length(electrodes)) {
        electrodes <- self$electrode_list
      }

      missing_wavelet <- subject$electrodes[!subject$has_wavelet]
      missing_wavelet <- missing_wavelet[missing_wavelet %in% electrodes]
      if(length(missing_wavelet)) {
        stop("Cannot find time-frequency coefficients from channel ", deparse_svec(missing_wavelet))
      }

      # check data_list
      nms <- sprintf("e_%d", electrodes)
      electrode_instances <- self$electrode_instances[nms]

      cache_path <- dir_create2(file_path(
        cache_root(),
        subject$project_name,
        subject$subject_code,
        "_whole_block_",
        self$reference_name,
        data_type
      ))

      all_electrodes <- as.integer(subject$electrodes)

      if(force || length(electrode_instances) * length(blocks) > 10) {
        callback <- function(block) {
          sprintf("Loading Recording Blocks | Recording block %s", block)
        }
      } else {
        callback <- NULL
      }

      # Initialize
      block_data <- ravepipeline::lapply_jobs(blocks, function(block) {
        block_cache <- file.path(cache_path, block)

        cached_arrays <- list()
        lapply(electrode_instances, function(inst) {
          # inst <- electrode_instances[[1]]

          stype <- inst$type

          if(is.null(cached_arrays[[stype]])) {
            # this is a sample electrode channel, load anyway
            sample_signal <- inst$load_blocks(blocks = block, type = data_type, simplify = TRUE)
            dm <- dim(sample_signal)
            if(!length(dm)) { dm <- length(sample_signal) }
            array_dimension <- c(dm, length(all_electrodes))

            # length(array_dimension) is 2 for voltage
            dnames <- list(
              Time = seq(0, by = 1 / sample_rates[[stype]], length.out = array_dimension[[1]]),
              Frequency = frequencies,
              Electrode = all_electrodes
            )

            cached_arrays[[stype]] <<- list(
              dim = structure(array_dimension, names = names(dnames)),
              dimnames = dnames,
              sample_rate = sample_rates[[stype]],
              data = filearray::filearray_load_or_create(
                filebase = file.path(cache_path, block, stype),
                dimension = array_dimension,
                type = storage.mode(sample_signal),
                mode = "readwrite",
                symlink_ok = FALSE,
                partition_size = 1L,
                project = inst$subject$project_name,
                subject = inst$subject$subject_code,
                block = block,
                rave_data_type = data_type,
                channels = all_electrodes,
                initialize = FALSE,
                verbose = FALSE,
                auto_set_headers = TRUE,
                signal_type = stype,
                references = reference_table$Reference,
                on_missing = function(arr) {
                  dimnames(arr) <- dnames
                  arr
                }
              )
            )
          }

          item <- cached_arrays[[stype]]
          dnames <- item$dimnames
          farray <- item$data
          sel <- dnames$Electrode == inst$number

          if(force || is.na(farray[1, 1, sel])) {
            # Channel needs to be loaded
            signal <- inst$load_blocks(blocks = block, type = data_type, simplify = TRUE)
            farray[, , sel] <- signal
          }
          return()
        })

        # For better serialization
        for(ii in seq_along(cached_arrays)) {
          cached_arrays[[ii]]$data <- ravepipeline::RAVEFileArray$new( cached_arrays[[ii]]$data )
        }
        cached_arrays
      }, .globals = list(
        cache_path = cache_path,
        electrode_instances = electrode_instances,
        data_type = data_type,
        sample_rates = sample_rates,
        all_electrodes = all_electrodes,
        reference_table = reference_table,
        force = force,
        frequencies = frequencies
      ), callback = callback)

      # Clear progress finish line
      cat("          \r")


      block_data <- structure(
        names = blocks,
        lapply(block_data, function(data_list) {
          for(stype in names(data_list)) {
            item <- data_list[[stype]]
            item$data <- item$data$`@impl`
            item$data$.mode <- "readonly"
            data_list[[stype]] <- item
          }
          data_list
        })
      )

      list_to_fastmap2(block_data, map = private$.data)

      self
    }

  ),
  active = list(

    #' @field sample_rates a named list of sampling frequencies; the names
    #' are signal types (\code{'LFP'}, \code{'Auxiliary'}, or \code{'Spike'})
    #' and the values are the sampling frequencies
    sample_rates = function() {
      list(LFP = self$subject$power_sample_rate)
    },

    #' @field sample_rate numeric sample rate for 'spectrogram'
    sample_rate = function() {
      self$subject$power_sample_rate
    },

    #' @field coefficients data container, alias of \code{get_container}
    coefficients = function() {
      self$get_container()
    }

  )

)

#' @rdname prepare_subject_with_blocks
#' @export
prepare_subject_time_frequency_coefficients_with_blocks <- function(
    subject, electrodes = NULL, blocks = NULL,
    reference_name = NULL, ...,
    quiet = FALSE, repository_id = NULL, strict = TRUE) {
  RAVESubjectRecordingBlockTimeFreqCoefRepository$new(
    subject = subject, electrodes = electrodes,
    reference_name = reference_name, blocks = blocks, ...,
    quiet = quiet, repository_id = repository_id, strict = strict)
}







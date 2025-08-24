#' 'RAVE' class for blocks of voltage repository
#' @description
#' Compared to \code{\link{RAVESubjectBaseRepository}}, this repository
#' loads the entire voltage traces for selected blocks; use
#' \code{\link{prepare_subject_voltage_with_blocks}} to instantiate this
#' repository.
#'
#' @seealso \code{\link{prepare_subject_voltage_with_blocks}}
#' @export
RAVESubjectRecordingBlockVoltageRepository <- R6::R6Class(
  classname = "RAVESubjectRecordingBlockVoltageRepository",
  portable = TRUE,
  inherit = RAVESubjectRecordingBlockRepository,
  lock_objects = FALSE,
  lock_class = TRUE,
  cloneable = TRUE,

  private = list(
    .data_type = "voltage",
    .downsample = NA
  ),
  public = list(


    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectRecordingBlockVoltageRepository"
      class(object$data) <- c("RAVESubjectRecordingBlockVoltageRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectRecordingBlockVoltageRepository_marshal"))
      repo <- RAVESubjectRecordingBlockVoltageRepository$new(
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
    #' @param downsample down-sample rate by this integer number to save space
    #' and speed up computation; typically 'ERP' signals do not need super
    #' high sampling frequencies to load; default is \code{NA} and no
    #' down-sample is performed.
    #' @param reference_name name of the reference table
    #' @param quiet see field \code{quiet}
    #' @param repository_id see field \code{repository_id}
    #' @param strict whether the mode should be strict; default is true and
    #' errors out when subject is missing
    #' @param lazy_load whether to delay (lazy) the evaluation \code{mount_data}
    #' @param ... passed to \code{\link{RAVESubjectBaseRepository}} constructor
    #' @param .class internally used, do not set, even if you know what this is
    initialize = function(subject, electrodes = NULL,
                          reference_name = NULL, blocks = NULL,
                          downsample = NA, ...,
                          quiet = FALSE, repository_id = NULL, strict = TRUE,
                          lazy_load = FALSE, .class = NULL) {

      downsample <- as.integer(downsample)[[1]]
      if(!is.na(downsample) && (isTRUE(downsample < 1) || !is.finite(downsample))) {
        ravepipeline::logger("`downsample` must be a positive integer; reset to NA", level = "warning")
      }

      .class <- c(.class, "prepare_subject_voltage_with_blocks", "RAVESubjectRecordingBlockVoltageRepository")

      subject <- as_rave_subject(subject, strict = strict)
      super$initialize(subject = subject, electrodes = electrodes,
                       reference_name = reference_name, quiet = quiet,
                       repository_id = repository_id, blocks = blocks,
                       lazy_load = lazy_load,
                       .class = .class)
      private$.downsample <- downsample
    },

    #' @description function to mount data
    #' @param force force update data; default is true; set to false
    #' to use cache
    #' @param electrodes electrodes to update; default is \code{NULL} (all
    #' electrode channels)
    #' @param ... reserved
    mount_data = function(..., force = TRUE, electrodes = NULL) {

      quiet_old <- self$quiet
      self$quiet <- TRUE
      on.exit({ self$quiet <- quiet_old })

      # self <- RAVESubjectRecordingBlockVoltageRepository$new(subject = "demo/DemoSubject", lazy_load = TRUE)
      # private <- self$.__enclos_env__$private
      # private$.data
      # data_type <- "voltage"
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
      downsample <- as.integer(private$.downsample)
      if(is.na(downsample)) {
        downsample <- 1L
      }

      # determine electrodes to load
      electrodes <- parse_svec(electrodes)
      electrodes <- electrodes[electrodes %in% self$electrode_list]
      if(!length(electrodes)) {
        electrodes <- self$electrode_list
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
        data_type,
        sprintf("downsample-%d", downsample)
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
        # if(force && file_exists(block_cache)) {
        #   unlink(block_cache, recursive = TRUE)
        # }

        cached_arrays <- list()
        lapply(electrode_instances, function(inst) {
          # inst <- electrode_instances[[1]]

          stype <- inst$type

          if(stype == "LFP") {
            data_type <- "voltage"
          } else {
            # Special treatment: Spike and Aux channels dont need reference
            data_type <- "raw-voltage"
          }

          if(is.null(cached_arrays[[stype]])) {
            # this is a sample electrode channel, load anyway
            sample_dim_info <- inst$load_dim_with_blocks(blocks = block, type = data_type)[[block]]
            dm <- unname(sample_dim_info$dim)
            dm[[2]] <- length(all_electrodes)

            if(isTRUE(downsample > 1)) {
              dm[[1]] <- length(ravetools::decimate(seq_len(dm[[1]]), downsample))
            } else {
              downsample <- 1L
            }
            array_dimension <- as.integer(dm)

            # length(array_dimension) is 2 for voltage
            sample_rate <- as.double(sample_rates[[stype]])
            dnames <- list(
              Time = seq(0, by = downsample / sample_rate,
                         length.out = array_dimension[[1]]),
              Electrode = all_electrodes
            )

            cached_arrays[[stype]] <<- list(
              dim = structure(array_dimension, names = names(dnames)),
              dimnames = dnames,
              sample_rate = sample_rate,
              downsample = downsample,
              data = filearray::filearray_load_or_create(
                filebase = file.path(cache_path, block, stype),
                dimension = array_dimension,
                type = "float",
                mode = "readwrite",
                symlink_ok = FALSE,
                partition_size = 1L,
                project = inst$subject$project_name,
                subject = inst$subject$subject_code,
                block = block,
                sample_rate = sample_rate,
                downsample = as.integer(downsample),
                rave_data_type = "voltage",
                channels = all_electrodes,
                initialize = FALSE,
                verbose = TRUE,
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

          if(force || is.na(farray[1, sel])) {
            # Channel needs to be loaded
            signal <- inst$load_data_with_blocks(blocks = block,
                                                 type = data_type,
                                                 simplify = TRUE)
            if(isTRUE(downsample > 1)) {
              signal <- ravetools::decimate(signal, downsample)
            }
            farray[, sel] <- signal
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
        sample_rates = sample_rates,
        all_electrodes = all_electrodes,
        reference_table = reference_table,
        force = force,
        downsample = downsample
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
      electrodes <- self$subject$electrodes
      sel <- electrodes %in% self$electrode_list
      electrodes <- electrodes[sel]
      raw_sample_rates <- self$subject$raw_sample_rates[sel]
      electrode_types <- self$subject$electrode_types[sel]

      downsample <- private$.downsample
      if(!isTRUE(downsample > 1)) {
        downsample <- 1
      }

      df <- unique(data.frame(
        type = electrode_types,
        sample_rate = raw_sample_rates / downsample
      ))
      structure(
        names = df$type,
        as.list(as.double(df$sample_rate))
      )
    },

    #' @field voltage data container, alias of \code{get_container}
    voltage = function() {
      self$get_container()
    }

  )

)

#' @rdname prepare_subject_with_blocks
#' @export
prepare_subject_voltage_with_blocks <- function(
    subject, electrodes = NULL, blocks = NULL,
    reference_name = NULL, ...,
    quiet = FALSE, repository_id = NULL, strict = TRUE) {
  RAVESubjectRecordingBlockVoltageRepository$new(
    subject = subject, electrodes = electrodes,
    reference_name = reference_name, blocks = blocks, ...,
    quiet = quiet, repository_id = repository_id, strict = strict)
}







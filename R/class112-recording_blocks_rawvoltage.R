#' 'RAVE' class for blocks of voltage repository
#' @description
#' Compared to \code{\link{RAVESubjectBaseRepository}}, this repository
#' loads the entire voltage traces for selected blocks; use
#' \code{\link{prepare_subject_raw_voltage_with_blocks}} to instantiate this
#' repository.
#'
#' @seealso \code{\link{prepare_subject_raw_voltage_with_blocks}}
#' @export
RAVESubjectRecordingBlockRawVoltageRepository <- R6::R6Class(
  classname = "RAVESubjectRecordingBlockRawVoltageRepository",
  portable = TRUE,
  inherit = RAVESubjectRecordingBlockRepository,
  lock_objects = FALSE,
  lock_class = TRUE,
  cloneable = TRUE,

  private = list(
    .data_type = "raw-voltage",
    .downsample = integer()
  ),
  public = list(


    #' @description Internal method
    #' @param ... internal arguments
    `@marshal` = function(...) {
      object <- super$`@marshal`()
      object$r6_generator <- "RAVESubjectRecordingBlockRawVoltageRepository"
      class(object$data) <- c("RAVESubjectRecordingBlockRawVoltageRepository_marshal", class(object$data))
      object
    },

    #' @description Internal method
    #' @param object,... internal arguments
    `@unmarshal` = function(object, ...) {
      stopifnot(identical(object$namespace, "ravecore"))
      stopifnot(inherits(object$data, "RAVESubjectRecordingBlockRawVoltageRepository_marshal"))
      repo <- RAVESubjectRecordingBlockRawVoltageRepository$new(
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
    #' @param reference_name always \code{'noref'} (no reference); trying to
    #' set to other values will result in a warning
    #' @param blocks name of the recording blocks to load
    #' @param downsample down-sample rate by this integer number to save space
    #' and speed up computation; typically 'ERP' signals do not need super
    #' high sampling frequencies to load; default is \code{NA} and no
    #' down-sample is performed.
    #' @param quiet see field \code{quiet}
    #' @param repository_id see field \code{repository_id}
    #' @param strict whether the mode should be strict; default is true and
    #' errors out when subject is missing
    #' @param lazy_load whether to delay (lazy) the evaluation \code{mount_data}
    #' @param ... passed to \code{\link{RAVESubjectBaseRepository}} constructor
    #' @param .class internally used, do not set, even if you know what this is
    initialize = function(subject, electrodes = NULL,
                          reference_name = "noref", blocks = NULL,
                          downsample = NA, ...,
                          quiet = TRUE, repository_id = NULL, strict = TRUE,
                          lazy_load = FALSE, .class = NULL) {

      downsample <- as.integer(downsample)[[1]]
      if(!is.na(downsample) && (isTRUE(downsample < 1) || !is.finite(downsample))) {
        ravepipeline::logger("`downsample` must be a positive integer; reset to NA", level = "warning")
      }

      .class <- c(.class, "prepare_subject_raw_voltage_with_blocks", "RAVESubjectRecordingBlockRawVoltageRepository")

      if(!identical(reference_name, "noref")) {
        ravepipeline::logger("RAVESubjectRecordingBlockRawVoltageRepository: `reference_name` must be 'noref', coerce this setting", level = "warning")
        reference_name <- "noref"
      }

      subject <- as_rave_subject(subject, strict = strict)
      super$initialize(subject = subject, electrodes = electrodes,
                       reference_name = "noref", quiet = quiet,
                       repository_id = repository_id, blocks = blocks,
                       lazy_load = TRUE,
                       .class = .class, ...)

      private$.downsample <- downsample

      # Has to call this explicitly because downsampling is needed!
      if( !lazy_load ) {
        self$mount_data(force = FALSE)
      }
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

      # self <- RAVESubjectRecordingBlockRawVoltageRepository$new(subject = "test/PAV058", lazy_load = TRUE)
      # private <- self$.__enclos_env__$private
      # private$.data
      # data_type <- "voltage"
      # electrodes <- 13
      # self$mount_data(electrodes = 16, force = FALSE)

      workers <- 0
      if( self$`@restored` ) { workers <- 1 }

      # raw-voltage
      data_type <- private$.data_type
      if(!length(data_type)) { return(self) }

      blocks <- self$blocks
      subject <- self$subject

      # sample_rates is already down-sampled
      sample_rates <- self$sample_rates
      signal_types <- unique(self$electrode_signal_types)
      downsample <- as.integer(private$.downsample)
      if(!isTRUE(downsample > 1)) {
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
        "_noref_raw_",
        data_type,
        sprintf("downsample-%d", downsample)
      ))

      all_electrodes <- as.integer(subject$electrodes)

      if(force || length(electrode_instances) > 4) {
        callback <- function(inst) {
          sprintf("Loading Recording Blocks | Channel %s", inst$number)
        }
      } else {
        callback <- NULL
      }

      # get all signal types needed
      signal_types <- vapply(electrode_instances, function(inst) { inst$type }, "")
      signal_types_unique <- unique(signal_types)

      # Initialize the arrays
      block_preparation <- structure(
        names = blocks,
        lapply(blocks, function(block) {
          # block <- blocks[[1]]
          block_cache <- file.path(cache_path, block)
          if(force && file_exists(block_cache)) {
            unlink(block_cache, recursive = TRUE)
          }
          cached_arrays <- list()

          # load dimensional information and initialize the arrays
          structure(
            names = signal_types_unique,
            lapply(signal_types_unique, function(stype) {
              # stype <- signal_types_unique[[1]]
              # get one instance
              inst <- electrode_instances[[which(signal_types == stype)[[1]]]]

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
                # sample_rates is already down-sampled so no need to multiply
                Time = seq(0, by = 1 / sample_rate,
                           length.out = array_dimension[[1]]),
                Electrode = all_electrodes
              )

              file_array_path <- file.path(cache_path, block, stype)
              dir_create2(dirname(file_array_path))

              data_array <- filearray::filearray_load_or_create(
                filebase = file_array_path,
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
                rave_data_type = "raw-voltage",
                channels = all_electrodes,
                initialize = FALSE,
                verbose = FALSE,
                auto_set_headers = TRUE,
                signal_type = stype,
                on_missing = function(arr) {
                  dimnames(arr) <- dnames
                  arr
                }
              )

              dnames$Time <- c(0, 1 / sample_rate, array_dimension[[1]])
              list(
                dim = structure(array_dimension, names = names(dnames)),
                dimnames = dnames,
                sample_rate = sample_rate,
                downsample = downsample,
                data = ravepipeline::RAVEFileArray$new(data_array, temporary = FALSE)
              )
            })
          )

        })
      )

      # load electrode data
      ravepipeline::lapply_jobs(
        electrode_instances,
        function(inst) {
          # inst <- electrode_instances[[1]]

          stype <- inst$type
          # data_type <- "raw-voltage"

          lapply(blocks, function(block) {

            item <- block_preparation[[block]][[stype]]
            sel <- item$dimnames$Electrode == inst$number
            farray <- item$data$`@impl`

            if(is.na(farray[1, sel])) {
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

          return()
        },
        .globals = list(
          block_preparation = block_preparation,
          blocks = blocks,
          downsample = downsample,
          data_type = data_type
        ),
        callback = callback
      )

      # Clear progress finish line
      if(is.function(callback)) { cat("          \r") }


      block_data <- structure(
        names = blocks,
        lapply(blocks, function(block) {
          # block <- blocks[[1]]
          block_info <- block_preparation[[block]]
          signal_types <- names(block_info)

          structure(
            names = signal_types,
            lapply(signal_types, function(stype) {
              stype_info <- block_info[[stype]]

              # expand time dimname back
              time_params <- stype_info$dimnames$Time
              stype_info$dimnames$Time <- seq(time_params[[1]], by = time_params[[2]], length.out = time_params[[3]])
              stype_info$data <- stype_info$data$`@impl`
              stype_info$data$.mode <- "readonly"
              stype_info
            })
          )
        })
      )

      list_to_fastmap2(block_data, map = private$.data)

      self
    }

  ),
  active = list(

    #' @field reference_name name of reference table; always \code{'noref'}
    reference_name = function(v) {
      if(identical(private$.reference_name, "noref")) {
        private$.reference_name <- "noref"
      }
      "noref"
    },

    #' @field reference_table reference table; a reference table with
    #' \code{'noref'} on all channels
    reference_table = function() {
      subject <- private$.subject
      data.frame(
        Electrode = subject$electrodes,
        Group = "default",
        Reference = "noref",
        Type = "No Reference"
      )
    },

    #' @field references_list a vector of reference channel names;
    #' always \code{'noref'}
    references_list = function() {
      "noref"
    },

    #' @field reference_instances instances of reference channels, empty
    #' in this type of repositories
    reference_instances = function() { list() },

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

    #' @field raw_voltage data container, alias of \code{get_container}
    raw_voltage = function() {
      self$get_container()
    }

  )

)

#' @rdname prepare_subject_with_blocks
#' @export
prepare_subject_raw_voltage_with_blocks <- function(
    subject, electrodes = NULL, blocks = NULL,
    reference_name = "noref", downsample = NA, ...,
    quiet = FALSE, repository_id = NULL, strict = TRUE) {
  RAVESubjectRecordingBlockRawVoltageRepository$new(
    subject = subject, electrodes = electrodes,
    reference_name = "noref", blocks = blocks, ..., downsample = downsample,
    quiet = quiet, repository_id = repository_id, strict = strict)
}







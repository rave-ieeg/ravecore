
validate_raw_h5_mat_per_block <- function(subject, blocks, electrodes, check_content = TRUE, ...) {

  # DIPSAUS DEBUG START
  # subject <- "test/DemoSubject"
  # blocks = c("008", "010")
  # electrodes = "13-16"
  # check_content = TRUE

  blocks <- unique(blocks)
  block_info <- fastmap2()
  validation_errors <- fastqueue2()
  with_validation <- function(expr) {
    tryCatch({
      force(expr)
    }, error = function(e) {
      validation_errors$add(sprintf("Unexpected error during validation: %s", paste(e$message, collapse = "")))
      NULL
    })
  }


  if(missing(electrodes)){
    electrodes <- NULL
  } else {
    electrodes <- parse_svec(electrodes)
  }


  with_validation({

    subject <- restore_subject_instance(subject, strict = FALSE)

    # native or bids
    format_standard <- subject$preprocess_settings$raw_path2_type

  })

  data_paths <- switch (
    format_standard,
    'bids' = {
      lapply(blocks, function(block) {
        # Parse block
        with_validation({

          bids_subject <- as_bids_subject(subject, strict = FALSE)

          query_results <- bidsr::query_bids(
            bids_subject,
            search_params = list(
              storage = "raw",
              sidecars = FALSE,
              data_types = "ieeg"
            )
          )
          sel <- block_names_from_bids_entities(query_results$parsed) %in% block
          if(!any(sel)) {
            validation_errors$add(sprintf("No data file for BIDS entity collection/RAVE block is found: `%s`", block))
            return()
          }
          query_results <- query_results[sel, ]

          is_datafile <- tolower(query_results$extension) %in% c("mat", "h5")

          if(!any(is_datafile)) {
            validation_errors$add(sprintf("Files with BIDS entities `%s` found, but only one matlab (.mat) or HDF5 (.h5) file is allowed for each block.", block))
            return()
          }
          query_results <- query_results[is_datafile, ]

          blockfiles <- lapply(query_results$parsed, function(parsed) {
            bidsr::resolve_bids_path(bids_subject@project, format(parsed), storage = "raw")
          })
          blockfiles <- unlist(blockfiles)[[1]]
          return(blockfiles)
        })

        return(NULL)
      })
    }, {

      raw_root <- subject$preprocess_settings$raw_path2

      # Parse block
      lapply(blocks, function(block) {
        # block <- blocks[[1]]

        with_validation({

          bpath <- file_path(raw_root, block)
          files <- list.files(bpath, pattern = '\\.(mat|h5)$', ignore.case = TRUE)

          if(length(files) != 1) {
            stop(sprintf("Block `%s` must only contain one matlab (.mat) or HDF5 (.h5) file.", block))
          }
          return(file_path(bpath, files))
        })
      })
    }
  )
  names(data_paths) <- blocks

  progress <- ravepipeline::rave_progress(
    title = 'Validating channel files',
    max = length(blocks) + 1,
    shiny_auto_close = FALSE
  )
  on.exit(progress$close())

  # read in and check contents
  with_validation({
    validation <- structure(
      names = blocks,
      lapply(blocks, function(block) {
        # block <- blocks[[1]]

        progress$inc(block)


        # data_paths = list('008' = '~/rave_data/raw_dir/DemoSubject/008/DemoSubjectDatafile008_ch13.mat')

        path <- data_paths[[block]]
        if(length(path) == 0) { return(FALSE) }

        path_data <- read_mat2(path, ram = FALSE)

        possible_names <- guess_raw_trace(path_data, electrodes = electrodes, is_vector = FALSE)

        if(length(possible_names) > 1) {
          stop("Block %s has more than one dataset in the file. RAVE does not know which matrix contains the signal traces: %s",
               block, paste(sQuote(possible_names), collapse = ", "))
        }

        if(length(possible_names) == 0 ) {
          stop(sprintf("Block %s has no matrix that qualifies as signal trace matrix. Please make sure this file contains one matrix with the correct channel size (along the shorted margin of the matrix)", block))
        }

        data_name <- possible_names[[1]]

        block_data <- path_data[[data_name]]
        dm <- dim(block_data)
        max_elec <- min(dm)

        info <- list(
          paths = path,
          data_name = data_name,
          channels = seq_len(max_elec),
          time_points = max(dm)
        )

        if(length(electrodes) > 0) {
          mis_e <- electrodes[!electrodes %in% info$channels]
          if(length(mis_e)) {
            stop("Found data matrix with shape %s from block %s. However, this matrix is too small that it does not contain channel %s",
                 paste(dm, collapse = "x"), block, deparse_svec(mis_e))
          }
        }

        return(info)
      })
    )
  })

  progress$inc("Collecting results...")

  if(length(validation_errors)) {
    return(list(
      passed = FALSE,
      errors = unlist(validation_errors$as_list())
    ))
  }
  return(list(
    passed = TRUE,
    results = validation
  ))

}

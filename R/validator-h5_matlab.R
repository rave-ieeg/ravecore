guess_raw_trace <- function(dat, electrodes = NULL, is_vector = TRUE){
  nms <- names(dat)
  for(nm in nms){
    x <- dat[[nm]]
    if( inherits(x, "LazyH5") ) {
      type <- tryCatch({
        x$get_type(stay_open = FALSE)
      }, error = function(e){ "unknown" })
      if(!type %in% c("integer", "double")) { next }
    } else if(!is.numeric(x) || mode(x) != "numeric"){ next }


    if(is_vector){
      # should be vector
      dm <- dim(x)
      if((is.null(dm) || length(dm) == 1) && length(x) > 1){
        return(nm)
      } else if (length(dm) %in% c(2) && min(dm) == 1){
        return(nm)
      }
    } else {
      if(!is.matrix(x)){ next }
      dm <- dim(x)
      d1 <- min(dm)

      # d2 is the time points, d1 should be electrodes
      if(d1 < max(electrodes, 1)){ next }
      return(nm)
    }
  }
  return(NULL)
}

validate_raw_h5_mat_per_channel <- function(subject, blocks, electrodes, check_content = TRUE, ...) {

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
    format_standard <- subject$preprocess_settings$raw_path_type

  })

  data_paths <- switch (
    format_standard,
    'bids' = {
      lapply(blocks, function(block) {
        # Parse block
        with_validation({

          bids_subject <- bidsr::bids_subject(
            project = subject$`@impl`@project@parent_path,
            subject_code = subject$subject_code
          )

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

          is_headerfile <- tolower(query_results$extension) %in% c("nev")

          if(!any(sel)) {
            validation_errors$add(sprintf("Files with BIDS entities `%s` found, but no NeuroEvent files (.nev) found.", block))
            return()
          }
          query_results <- query_results[is_headerfile, ]


          blockfiles <- lapply(query_results$parsed, function(parsed) {
            bidsr::resolve_bids_path(bids_subject@project, format(parsed), storage = "raw")
          })
          return(blockfiles)
        })

        return(NULL)
      })
    }, {

      raw_root <- subject$preprocess_settings$raw_path

      # Parse block
      lapply(blocks, function(block) {
        # block <- blocks[[1]]

        with_validation({

          bpath <- file_path(raw_root, block)
          files <- list.files(bpath, pattern = '[0-9]+\\.(mat|h5)$', ignore.case = TRUE)

          if(!length(files)) {
            validation_errors$add(sprintf(
              "No Matlab/HDF5 file (.mat/.h5) in block `%s`",
              block
            ))
            return()
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
  validation <- structure(
    names = blocks,
    lapply(blocks, function(block) {

      progress$inc(block)

      with_validation({

        paths <- data_paths[[block]]
        if(length(paths) == 0) { return(FALSE) }

        match <- regexec("(^|[^0-9])([0-9]+)\\.(h5|mat)$", paths, ignore.case = TRUE)
        path_info <- do.call("rbind", lapply(seq_along(paths), function(ii) {
          c(paths[[ii]], regmatches(paths[[ii]], match[ii])[[1]][[3]])
        }))

        channel_numbers <- as.integer(path_info[, 2])
        path_info <- path_info[!is.na(channel_numbers), ]

        info <- list(
          paths = path_info[, 1],
          channels = as.integer(path_info[, 2])
        )
        valid <- TRUE
        if( length(electrodes) > 0 ) {

          channel_exist <- electrodes %in% info$channels
          if(!all(channel_exist)) {
            validation_errors$add(sprintf(
              "Channel %s are missing from block %s",
              deparse_svec(electrodes[!channel_exist]), block
            ))

            valid <- FALSE
            attributes(valid) <- info
            return(valid)
          }

          sel <- info$channels %in% electrodes
          matched_channels <- info$channels[sel]
          dups <- duplicated(matched_channels)
          if(any(dups)) {
            validation_errors$add(sprintf(
              "Channel [%s] have multiple files in block `%s`. RAVE does not know which file to read from.",
              deparse_svec(matched_channels[dups]), block
            ))
            valid <- FALSE
            attributes(valid) <- info
            return(valid)
          }

          if( check_content ) {

            get_time_points <- function(e) {

              path <- info$paths[info$channels == e]
              if(endsWith(tolower(path), "mat")) {
                mat <- ieegio::io_read_mat(path)
              } else {
                mat <- read_mat2(path)
              }

              dname <- guess_raw_trace(mat, electrodes = length(electrodes), is_vector = TRUE)
              if(!length(mat) || !length(dname)) {
                validation_errors$add(sprintf(
                  "Cannot determine data name for channel `%s` from block `%s`",
                  e, block
                ))
                return(NA)
              }

              length(mat[[dname]])

            }

            utils::capture.output({
              get_time_points(electrodes[[1]])

              time_points <- lapply(electrodes, get_time_points)
            })

            unique_tpoints <- unique(unlist(time_points))
            if(anyNA(unique_tpoints)) {
              valid <- FALSE
              attributes(valid) <- info
              return(valid)
            }

            # within 10 time points might be OK?
            if(max(unique_tpoints) - min(unique_tpoints) > 10) {
              validation_errors$add(sprintf(
                "Channel data from block `%s` have inconsistent time points (min=%d, max=%d)",
                block, min(unique_tpoints), max(unique_tpoints)
              ))
              valid <- FALSE
              attributes(valid) <- info
              return(valid)
            }
            info$time_points <- min(unique_tpoints)

          }

        }

        return(info)
      })
    })
  )

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

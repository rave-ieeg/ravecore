validate_raw_brainvision <- function(subject, blocks, electrodes, check_content = TRUE, ...) {

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

          is_brainvision <- tolower(query_results$extension) %in% c("dat", "eeg", "vhdr", "vmrk")

          if(!any(sel)) {
            validation_errors$add(sprintf("Files with BIDS entities `%s` found, but no BrainVision files (.dat, .eeg, .vhdr, .vmrk) found.", block))
            return()
          }
          query_results <- query_results[is_brainvision, ]


          blockfiles <- lapply(query_results$parsed, function(parsed) {
            bidsr::resolve_bids_path(bids_subject@project, format(parsed), storage = "raw")
          })
          return(blockfiles)
        })

        return(NULL)
      })
    }, {

      raw_root <- subject$preprocess_settings$raw_path

      lapply(blocks, function(block) {
        # Parse block

        with_validation({

          bpath <- file_path(raw_root, block)
          files <- list.files(bpath, pattern = '\\.(vhdr)$', ignore.case = TRUE)

          if(!length(files)) {
            validation_errors$add(sprintf(
              "No BrainVision file (.vhdr) in block `%s`",
              block
            ))
            return()
          }
          if(length(files) > 1) {
            validation_errors$add(sprintf(
              'Found more than one BrainVision (.vhdr) file in the block `%s`. Please reduce to one file per block folder',
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

  # read in and check contents
  validation <- structure(
    names = blocks,
    lapply(blocks, function(block) {

      with_validation({

        paths <- data_paths[[block]]
        if(length(paths) == 0) { return(FALSE) }
        info <- list(paths = paths)
        valid <- TRUE
        path <- paths[[1]]
        if( check_content || length(electrodes) > 0 ){

          header_info <- ieegio::read_brainvis(file = path, extract_path = nullfile(), header_only = TRUE, verbose = TRUE)
          header_info$ChannelInfos

          info$header <- header_info

          if( length(electrodes) > 0 ) {
            n_channels <- header_info$CommonInfos$NumberOfChannels
            channel_exist <- electrodes %in% seq_len(n_channels)

            info$channel_exist <- channel_exist

            if(!all(channel_exist)) {
              validation_errors$add(sprintf(
                "Channel %s are missing from block %s",
                deparse_svec(electrodes[!channel_exist]), block
              ))

              valid <- FALSE
              attributes(valid) <- info
              return(valid)
            }

            info$sample_rate <- unique(header_info$ChannelInfos$SampleRate[electrodes])
            info$units <- unique(header_info$ChannelInfos$Unit[electrodes])
          }
        }

        return(info)
      })
    })
  )

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

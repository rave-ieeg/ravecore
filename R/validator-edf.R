validate_raw_edf <- function(subject, blocks, electrodes, check_content = TRUE, ...) {

  # DIPSAUS DEBUG START
  # subject <- "demo@bids:ds005574/02"
  # blocks = c('sub-02_task-podcast')
  # electrodes = 1:10
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

          blockfiles <- subject$preprocess_settings$get_block_paths(block)
          if(!length(blockfiles)) {
            validation_errors$add(sprintf("No data file for BIDS entity collection/RAVE block is found: `%s`", block))
            return()
          }

          blockfiles <- blockfiles[endsWith(tolower(blockfiles), ".edf")]

          if(!length(blockfiles)) {
            validation_errors$add(sprintf("Files with BIDS entities `%s` found, but no EDF(+) files (.edf) found.", block))
            return()
          }
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
          files <- list.files(bpath, pattern = '\\.(edf)$', ignore.case = TRUE)

          if(!length(files)) {
            validation_errors$add(sprintf(
              "No EDF(+) file (.edf) in block `%s`",
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

  temporary_path <- dir_create2(file_path(subject$cache_path, 'edf'))

  progress <- ravepipeline::rave_progress(
    title = 'Validating EDF files',
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
        info <- list(paths = paths)
        valid <- TRUE
        if( check_content && length(electrodes) > 0 ){

          header_info <- ieegio::read_edf(
            paths[[1]], extract_path = temporary_path,
            header_only = TRUE,
            verbose = TRUE,
            cache_ok = TRUE
          )

          # Remove annotations
          channel_table <- header_info$channel_table
          channel_table <- channel_table[!channel_table$Annotation, ]

          info$header <- channel_table

          if( length(electrodes) > 0 ) {
            channel_exist <- electrodes %in% channel_table$Channel

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

            sel <- channel_table$Channel %in% electrodes

            channels <- channel_table$Channel[sel]
            dup_channels <- duplicated(channels)
            if(any(dup_channels)) {
              validation_errors$add(sprintf(
                "Channel [%s] have duplicated entries in block `%s`. RAVE does not know which .nev/ns3/ns5 file to read the channel data",
                deparse_svec(channels[dup_channels]), block
              ))
              valid <- FALSE
              attributes(valid) <- info
              return(valid)
            }
            info$sample_rate <- unique(channel_table$SampleRate[sel])
            info$units <- unique(channel_table$Unit[sel])
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

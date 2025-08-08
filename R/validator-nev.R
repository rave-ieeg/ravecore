validate_raw_nevnsx <- function(subject, blocks, electrodes, check_content = TRUE, ...) {

  # DIPSAUS DEBUG START
  # subject <- "test/YFM"
  # blocks = c('EMU-0095_subj-YFM_task-BlockPodcast_run-02', "EMU-0099_subj-YFM_task-Check_timing_run-01")
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

      raw_root <- subject$preprocess_settings$raw_path2

      # Parse block
      lapply(blocks, function(block) {
        # block <- blocks[[1]]

        with_validation({

          bpath <- file_path(raw_root, block)
          files <- list.files(bpath, pattern = '\\.(nev)$', ignore.case = TRUE)

          if(!length(files)) {
            validation_errors$add(sprintf(
              "No NeuroEvent file (.nev) in block `%s`",
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

  temporary_path <- dir_create2(file_path(subject$cache_path, 'neuroevent'))

  progress <- ravepipeline::rave_progress(
    title = 'Validating NEV/NSx files',
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

          channel_table <- lapply(paths, function(path) {
            digest <- ravepipeline::digest(file = path)
            header_info <- ieegio::read_nsx(
              file = path,
              extract_path = file_path(temporary_path, digest),
              header_only = FALSE,
              verbose = TRUE,
              cache_ok = TRUE,
              include_waveform = FALSE
            )
            channel_table <- header_info$get_channel_table()
            channel_table
          })
          channel_table <- data.table::rbindlist(channel_table)

          info$header <- channel_table

          if( length(electrodes) > 0 ) {
            channel_exist <- electrodes %in% channel_table$original_channel

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

            sel <- channel_table$original_channel %in% electrodes

            channels <- channel_table$original_channel[sel]
            dup_channels <- duplicated(channels)
            if(any(dup_channels) && length(paths) > 1) {
              validation_errors$add(sprintf(
                "Channel [%s] have duplicated entries in block `%s`. RAVE does not know which .nev/ns3/ns5 file to read the channel data",
                deparse_svec(channels[dup_channels]), block
              ))
              valid <- FALSE
              attributes(valid) <- info
              return(valid)
            }
            info$sample_rate <- unique(channel_table$sampling_frequency[sel])
            info$units <- unique(channel_table$units[sel])
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

import_from_brainvis <- function(subject, blocks, electrodes, sample_rate, add = FALSE, data_type = 'LFP', ...) {

  # DIPSAUS DEBUG START
  # subject <- "demo/sub-01"
  # blocks <- "ses-01"
  # electrodes <- 1:10
  # sample_rate <- 1000
  # list2env(list(conversion = NA, add = FALSE, data_type = 'LFP'), .GlobalEnv)
  # skip_validation <- FALSE

  subject <- restore_subject_instance(subject, strict = FALSE)
  pretools <- subject$preprocess_settings
  electrodes <- parse_svec(electrodes)
  format_standard <- subject$preprocess_settings$raw_path_type

  # ---- Validation ---------------------------------------------

  if(!add && isTRUE(pretools$`@freeze_lfp_ecog`)){
    # LFP has been imported, just stop
    stop(sprintf('Subject `%s` has been imported previously. Double-import channels to subject is prohibited in RAVE as it will break the data integrity. Please consider either removing the subject or changing to another project', subject$subject_id))
  }

  validation <- validate_raw_brainvision(
    subject = subject,
    blocks = blocks,
    electrodes = electrodes,
    check_content = TRUE
  )

  if(!validation$passed) {
    error_messages <- validation$errors
    if(!length(error_messages)) {
      stop("RAVE encountered unknown error during the validation. Please report this issue to RAVE.")
    }
    stop(paste(
      c(
        "Found the following issues while validating the subject raw data:",
        error_messages
      ),
      collapse = "\n  * "
    ))
  }
  # ---- Initialize subject and reload -------------------------------------------
  # Reload pretools with read_only FALSE
  pretools <- RAVEPreprocessSettings$new(subject = subject$subject_id, read_only = FALSE)
  if(!add){
    pretools$set_blocks(blocks = blocks)
  }
  pretools$set_electrodes(electrodes, type = data_type, add = add)
  pretools$set_sample_rates(sample_rate, type = data_type)
  pretools$subject$initialize_paths(include_freesurfer = FALSE)
  pretools$save()

  progress <- ravepipeline::rave_progress(
    title = sprintf('Importing %s', subject$subject_id),
    max = length(blocks) + 2,
    shiny_auto_close = FALSE
  )
  on.exit({
    progress$close()
  }, add = TRUE, after = TRUE)


  # ---- Extract data ------------------------------------------------------------

  temporary_path <- dir_create2(file_path(subject$cache_path, 'brainvision'))
  block_info <- validation$results

  block_unpacked <- structure(
    names = blocks,
    lapply(blocks, function(block) {
      progress$inc(detail = sprintf("Unpacking %s", block))
      paths <- block_info[[block]]$paths
      ieegio::read_brainvis(
        file = paths[[1]],
        extract_path = temporary_path,
        header_only = FALSE,
        cache_ok = TRUE,
        verbose = FALSE
      )
    })
  )

  # ---- Import signal data ------------------------------------------------------

  save_path <- file.path(subject$preprocess_path, 'voltage')
  save_path <- dir_create2(save_path)

  progress$inc(detail = "Writing files...")

  ravepipeline::lapply_jobs(electrodes, function(e) {
    cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))

    for (block in blocks) {
      block_data <- block_unpacked[[block]]
      channel_data <- block_data$get_channel(e)

      # TODO: convert physical units if inconsistent, but I haven't encountered
      # such issue so this is an edge issue, unless you put EEG with iEEG
      # signals... or something... weird!
      channel_data$info$Unit

      # sample rate ratio
      sample_ratio <- channel_data$info$SampleRate / sample_rate

      # Need resample
      if (abs(sample_ratio - 1) > 1e-5) {
        if (abs(sample_ratio - round(sample_ratio)) < 1e-5) {
          # use decimate
          signal <- ravetools::decimate(x = channel_data$value,
                                        q = sample_ratio,
                                        ftype = "fir")
        } else {
          resample_factor <- c(sample_rate, channel_data$info$SampleRate)
          for (i in seq(0, 3)) {
            fct <- resample_factor * 10^i
            if (max(abs(fct - round(fct))) < 1e-5) {
              resample_factor <- fct
              break
            }
          }
          resample_factor <- round(resample_factor)
          signal <- ravetools::resample(x = channel_data$value,
                                        p = resample_factor[[1]],
                                        q = resample_factor[[2]])
        }

      } else {
        signal <- channel_data$value
      }

      ieegio::io_write_h5(
        x = channel_data$info$Unit,
        file = cfile,
        name = sprintf("/units/%s", block),
        chunk = 1L,
        replace = TRUE,
        quiet = TRUE,
        ctype = "character"
      )
      ieegio::io_write_h5(
        x = as.double(signal),
        file = cfile,
        name = sprintf("/raw/%s", block),
        chunk = 16384L,
        replace = TRUE,
        quiet = TRUE,
        level = 9L,
        ctype = "numeric"
      )
    }


    #   progress$inc(paste('Writing', b, '- electrode', e))
    #   chunk = 1, replace = TRUE, quiet = TRUE, ctype = 'character')
    invisible()

  }, .globals = list(
    save_path = save_path,
    blocks = blocks,
    sample_rate = sample_rate,
    block_unpacked = block_unpacked
  ), callback = function(e) {
    sprintf('Writing data|Channel %s', e)
  })

  progress$inc(detail = "Finalizing...")

  # Now set user conf
  for(e in electrodes){
    pretools$data[[e]]$data_imported <- TRUE
  }
  pretools$data$format <- which(unname(IMPORT_FORMATS) == "native_brainvis")
  pretools$save()

  # Reload subject instance
  subject <- restore_subject_instance(subject$subject_id, strict = FALSE)

  subject$set_default("Import", value = list(
    subject = subject$subject_id,
    format = "BrainVision",
    blocks = blocks,
    electrodes = electrodes,
    sample_rate = sample_rate,
    add = add,
    data_type = data_type,
    format_standard = format_standard
  ), namespace = "preprocess")

  # ---- Import electrode coordinates --------------------------------------------

  # Always check if subject FreeSurfer path exists, used for coordinate transforms
  freesurfer_path <- subject$freesurfer_path
  brain_model_exists <-
    length(freesurfer_path) == 1 &&
    !is.na(freesurfer_path) &&
    dir_exists(freesurfer_path) &&
    threeBrain::check_freesurfer_path(fs_subject_folder = freesurfer_path,
                                      autoinstall_template = FALSE,
                                      check_volume = TRUE)


  saved <- FALSE

  saved <- tryCatch({
    # When electrodes are imported correctly
    electrode_table <- subject$get_electrode_table(simplify = FALSE, warn = FALSE)

    if(all(subject$electrodes %in% electrode_table$Electrode)) {

      saved <- TRUE

      if(!any(electrode_table$Electrode %in% subject$electrodes)) {
        electrode_table <- electrode_table[electrode_table$Electrode %in% subject$electrodes, , drop = FALSE]
        electrode_table <- electrode_table[order(electrode_table$Electrode), ]
        save_meta2(
          data = electrode_table,
          meta_type = "electrodes",
          project_name = subject$project_name,
          subject_code = subject$subject_code
        )
      }

      # Try to import
      if( brain_model_exists ) {
        raveio::import_electrode_table(
          path = file.path(subject$meta_path, 'electrodes.csv'),
          subject = subject, use_fs = brain_model_exists)
      }

    }

    saved

  }, error = function(e) {
    saved
  })


  if(!saved) {
    # catgl("Cannot import from existing electrodes.csv, creating a new one", level = "INFO")
    tbl <- data.frame(
      Electrode = subject$electrodes,
      Coord_x = 0, Coord_y = 0, Coord_z = 0,
      Label = "NoLabel",
      SignalType = subject$electrode_types,
      LocationType = "iEEG",
      Hemisphere = "auto"
    )
    save_meta2(
      data = tbl,
      meta_type = "electrodes",
      project_name = subject$project_name,
      subject_code = subject$subject_code
    )
  }



}

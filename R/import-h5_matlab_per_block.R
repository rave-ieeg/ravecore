# validate_raw_h5_mat_per_channel <- function(subject, blocks, electrodes, check_content = TRUE, ...)

# ravecore:::import_h5_matlab(subject, blocks, electrodes, sample_rate, skip_validation = T)

# s <- NULL
# for(ch in c(13,14,15,16,24)) {
#   d <- read_mat(sprintf("~/rave_data/raw_dir/DemoSubject/008/DemoSubjectDatafile008_ch%d.mat", ch))
#   s <- rbind(s, d$analogTraces)
# }
# ieegio::io_write_mat(list(data = s), "~/rave_data/raw_dir/DemoSubjectBlock/008/data.mat")



#' @rdname import-signals
#' @export
import_from_h5_mat_per_block <- function(
    subject, blocks, electrodes, sample_rate, add = FALSE,
    data_type = 'LFP', skip_validation = FALSE, ...) {

  # DIPSAUS DEBUG START
  # subject <- "test@bids:ds005953/03"
  # blocks = c("sub-03_ses-01_task-visual_run-01")
  # electrodes = "1-5"
  # check_content = TRUE
  # skip_validation = FALSE
  # sample_rate <- 2000
  # list2env(list(add = FALSE, data_type = 'LFP'), .GlobalEnv)

  subject <- restore_subject_instance(subject, strict = FALSE)
  pretools <- subject$preprocess_settings
  electrodes <- parse_svec(electrodes)
  format_standard <- subject$preprocess_settings$raw_path2_type

  # ---- Validation ---------------------------------------------

  if(!add && isTRUE(pretools$`@freeze_lfp_ecog`)){
    # LFP has been imported, just stop
    stop(sprintf('Subject `%s` has been imported previously. Double-import channels to subject is prohibited in RAVE as it will break the data integrity. Please consider either removing the subject or changing to another project', subject$subject_id))
  }

  validation <- validate_raw_h5_mat_per_block(
    subject = subject,
    blocks = blocks,
    electrodes = electrodes,
    check_content = !skip_validation
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
    max = 2 + length(blocks),
    shiny_auto_close = FALSE
  )
  on.exit({
    progress$close()
  }, add = TRUE, after = TRUE)


  # ---- Extract data ------------------------------------------------------------

  block_info <- validation$results

  # ---- Import signal data ------------------------------------------------------

  save_path <- file.path(subject$preprocess_path, 'voltage')
  save_path <- dir_create2(save_path)


  lapply(blocks, function(block) {

    # block <- blocks[[1]]

    progress$inc(message = "Processing recording block", detail = block)

    progress2 <- ravepipeline::rave_progress(
      title = 'Importing electrode channels',
      max = length(electrodes),
      shiny_auto_close = FALSE
    )
    on.exit({
      progress2$close()
    }, add = TRUE, after = FALSE)

    block_data <- block_info[[block]]
    time_points <- block_data$time_points

    block_signals <- read_mat2(block_data$paths, ram = FALSE)[[block_data$data_name]][drop = FALSE]
    block_signals_dim <- dim(block_signals)

    lapply(electrodes, function(e) {
      # e <- electrodes[[1]]

      progress2$inc(sprintf("Writing channel %s", e))

      cfile <- file.path(save_path, sprintf('electrode_%d.h5', e))
      path <- block_data$paths

      if(which.min(block_signals_dim) == 1) {
        signal <- block_signals[block_data$channels == e, , drop = TRUE]
      } else {
        signal <- block_signals[, block_data$channels == e, drop = TRUE]
      }


      ieegio::io_write_h5(
        x = "NA",
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

    })

  })


  progress$inc(detail = "Finalizing...")

  # Now set user conf
  for(e in electrodes){
    pretools$data[[e]]$data_imported <- TRUE
  }
  pretools$data$format <- which(unname(IMPORT_FORMATS) == "native_matlab2")
  pretools$save()

  # Reload subject instance
  subject <- restore_subject_instance(subject$subject_id, strict = FALSE)

  subject$set_default("Import", value = list(
    subject = subject$subject_id,
    format = "Single Matlab/HDF5",
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
        import_electrode_table(
          path = file.path(subject$meta_path, 'electrodes.csv'),
          subject = subject, use_fs = brain_model_exists)
      }

    }

    saved

  }, error = function(e) {
    saved
  })


  if(!saved) {
    ravepipeline::logger("Cannot import from existing electrodes.csv, creating a new one", level = "info")
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

  invisible()
}

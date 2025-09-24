use_spikeinterface <- function(repository, signal_type = "Spike") {

  # repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = "test/PAV067", electrodes = 1:6); signal_type = "Spike"

  container <- repository$get_container()
  blocks <- sort(repository$blocks)

  n_timepoints <- vapply(blocks, function(block) {
    as.double(container[[block]][[signal_type]]$dim[[1]])
  }, FUN.VALUE = NA_real_)


  ravecorepy <- load_ravecorepy()

  # get channel locations
  electrodes <- sort(repository$electrode_list)
  electrode_table <- repository$subject$get_electrode_table()
  electrode_table <- electrode_table[electrode_table$Electrode %in% electrodes, ]

  electrode_coords <- as.matrix(electrode_table[, c("Coord_x", "Coord_y", "Coord_z")])
  dimnames(electrode_coords) <- NULL

  if(nrow(electrode_coords) > 2) {
    pca <- stats::prcomp(electrode_coords, scale. = FALSE, retx = TRUE, center = TRUE, rank. = 2)
    electrode_2d <- pca$x
    dimnames(electrode_2d) <- NULL
  } else {
    electrode_2d <- electrode_coords[, c(1, 2, drop = FALSE)]
  }

  zeros <- rowSums(electrode_2d^2) == 0
  if(any(zeros)) {
    electrode_2d[zeros, 2] <- seq_len(sum(zeros)) * 10 + 1000
  }

  dups <- duplicated(electrode_2d)
  if(any(dups)) {
    electrode_2d[dups, 2] <- electrode_2d[dups, 2] + stats::runif(sum(dups), min = -0.0001, 0.0001)
  }

  iter_tbl <- expand.grid(block = blocks, channel = electrodes)
  n_iters <- nrow(iter_tbl)

  n_workers <- ravepipeline::raveio_getopt("max_worker")
  if(!isTRUE(n_workers > 0)) { n_workers <- 1L }

  if(n_iters < 10) {
    callback <- NULL
  } else {
    callback <- function(iter) {
      sprintf("Prepare spike sorting|Iteration %s", iter)
    }
  }

  chunk_size <- ceiling(n_iters / n_workers)

  paths <- ravepipeline::lapply_jobs(seq_len(n_workers), function(iter) {

    ravecore <- asNamespace("ravecore")
    ravecorepy <- ravecore$load_ravecorepy()

    row_ii <- seq_len(chunk_size) + iter * (chunk_size - 1)
    row_ii <- row_ii[row_ii <= n_iters]
    if(!length(row_ii)) { return() }

    container <- repository$get_container(electrodes = unique(iter_tbl$channel[row_ii]))

    lapply(row_ii, function(ii) {
      row <- iter_tbl[ii, ]
      block <- row$block
      ch <- row$channel

      data_list <- container[[block]][[signal_type]]
      data <- data_list$data
      sample_rate <- data_list$sample_rate
      if(!length(data)) { return(NULL) }

      filebase <- data$.filebase
      si_root <- file.path(filebase, "spikeinterface", fsep = "/")
      if(!dir.exists(si_root)) {
        dir.create(si_root, showWarnings = FALSE, recursive = TRUE)
      }
      si_path <- file.path(si_root, ch, fsep = "/")
      re <- list(
        block = block,
        channel = ch,
        path = si_path
      )
      if(dir.exists(si_path)) {
        tryCatch(
          {
            rec <- ravecorepy$spike$load_recording(path = si_path)
            return(re)
          },
          error = function(e) {
            unlink(si_path, recursive = TRUE)
          }
        )
      }

      data_ii <- structure(subset(data, Electrode ~ Electrode == ch, drop = FALSE), names = NULL)
      rec <- ravecorepy$spike$make_recording(traces = data_ii, fs_hz = sample_rate, path = si_path)
      return(re)
    })
  }, callback = callback, .workers = n_workers, .globals = list(
    chunk_size = chunk_size,
    n_iters = n_iters,
    iter_tbl = iter_tbl,
    signal_type = signal_type,
    repository = repository

  ))

  # paths <- unlist(paths, recursive = FALSE, use.names = FALSE)
  # paths <- data.table::rbindlist(paths)

  recording_blocks <- lapply(blocks, function(block) {
    # block <- blocks[[1]]; signal_type = "Spike"

    data_list <- container[[block]][[signal_type]]
    data <- data_list$data
    sample_rate <- data_list$sample_rate

    if(!length(data)) { return(NULL) }
    filebase <- data$.filebase
    si_root <- file_path(filebase, "spikeinterface")

    recording_list <- lapply(seq_along(electrodes), function(ii) {
      # ii <- 1
      ch <- electrodes[[ii]]
      # coords <- electrode_coords[ii, , drop = FALSE]
      si_path <- file_path(si_root, ch)
      return(ravecorepy$spike$load_recording(path = si_path))
    })

    recording_list <- lapply(recording_list, function(rec) {
      has_loc <- rpymat::py_to_r(rec$has_channel_location())
      if(isTRUE(has_loc)) {
        rec$clear_channel_locations(list(0L))
      }
      rec
    })

    recordings <- ravecorepy$spike$combine_recording_by_channel(recording_list, channel_ids = repository$electrode_list)

    return(recordings)

  })


  recordings <- ravecorepy$spike$combine_recording_by_time(recording_blocks)

  recordings$set_channel_locations(locations = electrode_2d)

  electrode_groups <- electrode_table$LabelPrefix
  if(length(electrode_groups)) {
    recordings <- ravecorepy$spike$set_channel_groups(recordings, as.integer(as.factor(electrode_groups)))
  }

  # tmp <- as.data.frame(electrode_coords)
  # names(tmp) <- c("x", "y", "z")
  # tmp$ch <- electrode_table$Electrode
  # groups <- split(tmp, electrode_groups, drop = FALSE)
  #
  # lapply(seq_along(groups), function(ii) {
  #   # ii <- 1
  #   sub <- groups[[ii]]
  #   channel_ids <- matrix(as.integer(sub$ch), ncol = 1)
  #   positions <- cbind(sub$x, sub$y, sub$z)
  #   ravecorepy$spike$new_probe(positions, channel_ids)
  #
  # })




  return(list(recordings = recordings, n_timepoints = n_timepoints))
}


spike_sort_py <- function(repository, sorter_name = 'mountainsort5', verbose = TRUE,
                          save_path = tempfile(pattern = "ravecore_spikeinterface_")) {

  # DIPSAUS DEBUG START
  # repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = "test/PAV067", electrodes = 1:8)
  # save_path = tempfile(pattern = "ravecore_spikeinterface_")
  # verbose = TRUE
  # sorter_name = 'mountainsort5'

  prep_results <- use_spikeinterface(repository)
  recordings <- prep_results$recordings
  n_timepoints <- prep_results$n_timepoints

  ravecorepy <- load_ravecorepy()
  sorted <- ravecorepy$spike$run_sorter(recording = recordings, sorter_name = sorter_name, folder = save_path, verbose = verbose)

  non_empty_units <- sorted$get_non_empty_unit_ids()

  # Run analyzers
  si <- rpymat::import("spikeinterface")
  analyzer = si$create_sorting_analyzer(
    sorting=sorted,
    recording=recordings,
    folder=file_path(save_path, "analyzers"),
    overwrite=TRUE
  )

  # 2. Run waveform extraction
  analyzer$compute("random_spikes", method="uniform", max_spikes_per_unit = 600L, seed = 42L)
  # > rpymat::py_to_r(analyzer$get_computable_extensions())
  # [1] "random_spikes"        "waveforms"            "templates"            "noise_levels"         "amplitude_scalings"
  # [6] "correlograms"         "isi_histograms"       "principal_components" "spike_amplitudes"     "spike_locations"
  # [11] "template_metrics"     "template_similarity"  "unit_locations"       "quality_metrics"

  waveforms <- analyzer$compute("waveforms", ms_before=1.0, ms_after=2.0)
  templates <- analyzer$compute("templates")

  # unit x time x channel
  template_waveforms <- rpymat::py_to_r(templates$get_templates(non_empty_units))

  # get spike train
  blocks <- sort(repository$blocks)
  unit_ids <- rpymat::py_to_r(non_empty_units)
  # spike_trains <- structure(names = blocks, lapply(seq_along(blocks), function(ii_seg) {
  #   unit_time <- lapply(unit_ids, function(unit_id) {
  #     time <- sorted$get_unit_spike_train(
  #       unit_id = unit_id,
  #       segment_index = as.integer(ii_seg - 1),
  #       start_frame = 0.0,
  #       end_frame = NULL,
  #       return_times = TRUE
  #     )
  #     rpymat::py_to_r(time)
  #   })
  # }))


  list(
    `@impl` = list(
      sorted = sorted,
      analyzer = analyzer,
      waveforms = waveforms
    ),
    blocks = blocks,
    n_timepoints = n_timepoints,
    unit_ids = unit_ids,
    waveform_mean = template_waveforms
    # spike_trains = spike_trains
  )

}

extract_spike_train <- function(repository, sorted_results, epoch_name, epoch_window, signal_type = c("Spike", "LFP", "Auxiliary")) {
  signal_type <- match.arg(signal_type)
  windows <- ravecore::validate_time_window(epoch_window)
  sample_rate <- repository$sample_rates[[signal_type]]

  if(is.character(epoch_name)) {
    epoch <- repository$subject$get_epoch(epoch_name)
    epoch_table <- epoch$table
  } else if (is.data.frame(epoch_name)) {
    epoch_table <- epoch_name
  } else if (inherits(epoch_name, "RAVEEpoch")) {
    epoch_table <- epoch_name$table
  }

  n_timepoints <- sorted_results$n_timepoints
  blocks <- sorted_results$blocks

  spike_train <- lapply(split(epoch_table, epoch_table$Block), function(sub) {
    # sub <- split(epoch_table, epoch_table$Block)[[1]]
    block <- sub$Block[[1]]
    cond <- sub$Condition[[1]]
    if(!isTRUE(block %in% blocks)) { return() }
    segment <- which(blocks == block)

    if(segment == 1) {
      pad_timepoints <- 0
    } else {
      pad_timepoints <- sum(n_timepoints[seq_len(segment - 1)])
    }

    re <- lapply(seq_len(nrow(sub)), function(ii) {
      row <- sub[ii, ]
      time <- row$Time + pad_timepoints / sample_rate
      trial <- row$Trial
      cond <- row$Condition

      re <- lapply(sorted_results$unit_ids, function(unit_id) {

        lapply(windows, function(window) {
          # window <- windows[[1]]; unit_id <- 1
          window <- window + time

          spike_train <- rpymat::py_to_r(sorted_results$`@impl`$sorted$get_unit_spike_train(
            unit_id = unit_id,
            segment_index = 0L,
            start_frame = floor(window[[1]] * sample_rate),
            end_frame = ceiling(window[[2]] * sample_rate),
            return_times = TRUE
          ))

          if(!length(spike_train)) { return(NULL) }


          list(
            Trial = trial,
            Condition = cond,
            Unit_id = unit_id,
            Time = spike_train - time
          )
        })

      })

      re <- unlist(re, recursive = FALSE, use.names = FALSE)
      if(!length(re)) { return(NULL) }
      re
    })

    re <- unlist(re, recursive = FALSE, use.names = FALSE)
    if(!length(re)) { return(NULL) }
    re <- data.table::rbindlist(re)
    re
  })

  spike_train <- data.table::rbindlist(spike_train)

  spike_train
}

# DIPSAUS DEBUG START
# repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = "test/PAV067", electrodes = 1:8)
# save_path = tempfile(pattern = "ravecore_spikeinterface_")
# verbose = TRUE
# sorter_name = 'mountainsort5'
# sorted_results <- spike_sort_py(repository, save_path = save_path)
# epoch_name <- "UTO_PAV067_5modality_ALL"
#
# epoch <- repository$subject$get_epoch(epoch_name)
# epoch_table$Block <- repository$blocks
# epoch_table <- epoch$table
# epoch_table$Block <- repository$blocks
# epoch_window <- c(-1, 2)
#
# spike_train <- extract_spike_train(repository, sorted_results, epoch_table, c(-1, 2))
#
# plot(spike_train$Time, spike_train$Trial, col = as.factor(spike_train$Condition))






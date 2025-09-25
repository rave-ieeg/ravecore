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


spike_sort_py <- function(repository, sorter_name = 'mountainsort5', verbose = TRUE, force = FALSE,
                          save_path = file_path(tempdir(), sprintf("rave_si_%s", repository$signature))) {

  # DIPSAUS DEBUG START
  # repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = "test/PAV067", electrodes = 1:8)
  # save_path = tempfile(pattern = "ravecore_spikeinterface_")
  # verbose = TRUE
  # sorter_name = 'mountainsort5'

  prep_results <- use_spikeinterface(repository)
  recordings <- prep_results$recordings
  n_timepoints <- prep_results$n_timepoints

  ravecorepy <- load_ravecorepy()
  # bandpass
  recordings_bandpassed <- ravecorepy$spike$bandpass(recordings)

  sorted <- NULL
  if(dir_exists(save_path) && !force) {
    tryCatch({
      sorted <- ravecorepy$spike$load_recording(save_path)
    }, error = function(e) {
    })
  }
  if(is.null(sorted)) {
    sorted <- ravecorepy$spike$run_sorter(
      recording = recordings,
      sorter_name = sorter_name,
      folder = save_path,
      verbose = verbose
    )
  }


  non_empty_units <- sorted$get_non_empty_unit_ids()

  # Run analyzers
  si <- rpymat::import("spikeinterface")
  analyzer = si$create_sorting_analyzer(
    sorting=sorted,
    recording=recordings_bandpassed,
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
  # template_waveforms <- rpymat::py_to_r(templates$get_templates(non_empty_units, ))

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
      waveforms = waveforms,
      templates = templates
    ),
    blocks = blocks,
    n_timepoints = n_timepoints,
    unit_ids = unit_ids
    # waveform_mean = template_waveforms
    # spike_trains = spike_trains
  )

}

epoch_spike_train <- function(repository, sorted_results, epoch_name, epoch_window,
                                signal_type = c("Spike", "LFP", "Auxiliary")) {
  # DIPSAUS DEBUG START
  # repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = "test/PAV067", electrodes = 1:8)
  # save_path = tempfile(pattern = "ravecore_spikeinterface_")
  # verbose = TRUE
  # sorter_name = 'mountainsort5'
  # signal_type <- "Spike"
  # epoch_window <- c(-1, 2)
  # epoch_name <- repository$subject$get_epoch(repository$subject$epoch_names[[1]])$table
  # epoch_name$Block <- repository$blocks[[1]]
  # conditions <- NULL
  # sorted_results <- spike_sort_py(repository)

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

visualize_epoch_spike_train <- function(
    repository, sorted_results, epoch_name, epoch_window,
    unit_id, conditions = NULL,
    waveform_method = c("mean", "quantile"), waveform_ylim = NULL,
    isi_max_time = 500, isi_step_size = 10,
    raster_cex = 0.4, raster_pch = "|",
    firerate_smooth = TRUE,
    use_baseline = FALSE, baseline_window = c(-0.5, 0),
    plot_baseline = use_baseline,
    ...,
    color_palette = NULL, signal_type = c("Spike", "LFP", "Auxiliary")) {
  signal_type <- match.arg(signal_type)
  waveform_method <- match.arg(waveform_method)

  # waveform_method <- "quantile"; isi_max_time = 500; isi_step_size = 10; firerate_smooth = 0.25; color_palette = NULL

  # This is to calculate overhead for bins and firerates
  bin_size <- isi_step_size / 1000
  if(isTRUE(firerate_smooth)) {
    half_bandwidth <- 0.05
    filter <- dnorm(seq(-half_bandwidth, half_bandwidth, by = bin_size), mean = 0, sd = half_bandwidth / 3) * bin_size
  } else if (isTRUE(firerate_smooth > 0)) {
    # firerate_smooth is the half-bandwidth, default is 0.25
    half_bandwidth <- as.double(firerate_smooth)
    filter <- dnorm(seq(-half_bandwidth, half_bandwidth, by = bin_size), mean = 0, sd = half_bandwidth / 3) * bin_size
  } else {
    filter <- 1
  }

  # For baseline firing rate
  bins <- seq(epoch_window[[1]] - half_bandwidth, epoch_window[[2]] + half_bandwidth, by = bin_size)
  firing_rate_time <- (bins[-1] + bins[-length(bins)]) * 0.5
  if(length(use_baseline)) {
    baseline_window <- validate_time_window(baseline_window)
    is_baseline <- rep(FALSE, length(firing_rate_time))
    for(w in baseline_window) {
      is_baseline <- is_baseline | firing_rate_time <= w[[2]] & firing_rate_time >= w[[1]]
    }
  } else {
    baseline_window <- NULL
  }
  if(!any(is_baseline)) {
    warning("Baseline is invalid, falling back to average firing rates")
    use_baseline <- FALSE
  }

  add_baseline <- function(ylim, col = "gray50", alpha = 0.2) {
    if(!plot_baseline) { return() }
    lapply(baseline_window, function(w) {

      graphics::rect(
        xleft = w[[1]],
        xright = w[[2]],
        border = NA,
        ybottom = ylim[[1]],
        ytop = ylim[[2]],
        col = grDevices::adjustcolor(col, alpha.f = alpha)
      )

    })
  }


  spike_train <- epoch_spike_train(
    repository = repository,
    sorted_results = sorted_results,
    epoch_name = epoch_name,
    epoch_window = epoch_window + c(-1, 1) * half_bandwidth,
    signal_type = signal_type
  )
  sample_rate <- repository$sample_rates[[signal_type]]

  all_conditions <- sort(unique(spike_train$Condition))
  if(!length(conditions)) {
    conditions <- all_conditions
  } else {
    conditions <- unique(conditions)
    conditions <- conditions[conditions %in% all_conditions]
  }
  if(!length(conditions)) {
    stop("No matching condition or no epoch found.")
  }

  n_conditions <- length(conditions)

  # color_palette <- ravebuiltins:::group_colors
  # unit_id <- 2

  if(!length(color_palette)) {
    color_palette <- seq_along(conditions)
  } else if (length(color_palette) < n_conditions) {
    color_palette <- rep(color_palette, ceiling(n_conditions / length(color_palette)))
  }

  # FIXME: what if this is empty
  spike_train_subset <- spike_train[spike_train$Unit_id == unit_id & spike_train$Condition %in% conditions, ]
  spike_train_subset$Condition <- factor(spike_train_subset$Condition, levels = conditions, ordered = TRUE)

  trial_info <- unique(spike_train_subset[, c("Condition", "Trial"), with = FALSE])
  trial_info <- trial_info[order(trial_info$Condition, trial_info$Trial), ]
  trial_info$Order <- seq_len(nrow(trial_info))
  trial_info$Color <- color_palette[as.integer(trial_info$Condition)]
  merged <- merge(trial_info, spike_train_subset, by = c("Condition", "Trial"))

  # ---- Layout ----------------------------------------------------------------
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit({ graphics::par(oldpar) })

  graphics::layout(
    matrix(c(3,3,4,4,4,1,1,2,2,5), ncol = 2),
    widths = c(2.5, 1)
  )

  # ---- Plot 1: waveforms -----------------------------------------------------
  # time by channel (full)
  template_waveforms <- sorted_results$`@impl`$templates$get_templates(unit_ids = list(unit_id))
  template_waveforms <- rpymat::py_to_r(template_waveforms)
  # find min
  channel_order <- arrayInd(which.min(template_waveforms), dim(template_waveforms))[[3]]

  # sample x time x channel
  waveform_samples <- sorted_results$`@impl`$waveforms$get_waveforms_one_unit(unit_id = unit_id, force_dense = TRUE)
  waveform_samples <- rpymat::py_to_r(waveform_samples)[, , channel_order, drop = FALSE]
  # -> time x sample
  dim(waveform_samples) <- dim(waveform_samples)[c(1, 2)]
  waveform_samples <- t(waveform_samples)

  waveform_time <- seq(-1, by = 1000 / sample_rate, length.out = nrow(waveform_samples))
  xlim <- range(pretty(waveform_time))

  waveform_ylim <- waveform_ylim[is.finite(waveform_ylim)]
  if(!length(waveform_ylim)) {
    waveform_ylim <- range(pretty(waveform_samples))
  } else if(length(waveform_ylim) == 1) {
    waveform_ylim <- c(-1, 1) * abs(waveform_ylim)
  } else {
    waveform_ylim <- range(waveform_ylim)
  }

  par(mar = c(4.1, 4.1, 3.1, 2.1))
  matplot(waveform_time, waveform_samples, type = 'l', lty = 1, col = "gray90",
          axes = FALSE, xlab = "Time (ms)", ylab = bquote("Voltage (" ~ mu ~ "V)"),
          main = sprintf("Spike unit #%d", unit_id), xaxs = "i", xlim = xlim, ylim = waveform_ylim)

  switch (
    waveform_method,
    "quantile" = {
      waveform_mean <- apply(waveform_samples, 1L, median)
    },
    {
      waveform_mean <- rowMeans(waveform_samples)
    }
  )
  graphics::lines(waveform_time, waveform_mean, lwd = 3)
  axis(1, pretty(xlim))
  axis(2, c(waveform_ylim, 0), las = 1)

  # ---- Plot 1: ISI histogram -------------------------------------------------
  analyzer <- sorted_results$`@impl`$analyzer
  spike_train_in_sec = rpymat::py_to_r(analyzer$sorting$get_unit_spike_train(unit_id=unit_id, return_times = TRUE))
  isis_in_ms <- diff(as.vector(spike_train_in_sec)) * 1000
  max_isis_in_ms <- ceiling(max(isis_in_ms))
  if(max_isis_in_ms <= isi_max_time) {
    max_isis_in_ms <- NULL
  }
  isi_step_size <- abs(isi_step_size)
  isi_hist <- hist(isis_in_ms, breaks = c(seq(0, isi_max_time, by = isi_step_size), max_isis_in_ms), plot = FALSE)

  # plot histogram in log10 scale
  isi_hist$counts[isi_hist$counts == 1] <- 1.4 # log10(1.4) ~= 0.15; log10(2) ~= 0.3;
  isi_hist$counts[isi_hist$counts < 1] <- 1
  isi_hist$counts <- log10(isi_hist$counts)
  log_counts <- pretty(c(0, isi_hist$counts))
  xleft <- isi_hist$breaks[-length(isi_hist$breaks)]
  xright <- isi_hist$breaks[-1]

  graphics::par(mar = c(4.1, 4.1, 3.1, 2.1))

  plot(
    x = isi_hist$mids,
    y = isi_hist$counts,
    type = "n",
    axes = FALSE,

    xlab = "Interspike Internal (ms)",
    xlim = c(-isi_step_size / 2, isi_max_time),
    xaxs = "i",

    ylab = "Frequency",
    yaxs = "i",
    ylim = c(0, max(log_counts)),

    main = sprintf("Histogram (%.0f%% ISI < 1.5 ms)", mean(isis_in_ms < 1.5) * 100)
  )
  graphics::rect(xleft = xleft, xright = xright, ybottom = 0, ytop = isi_hist$counts, col = "gray80", border = par("bg"))
  xat <- pretty(c(0, isi_max_time))
  if(xat[[length(xat)]] < isi_max_time) {
    xat <- c(xat, isi_max_time)
  }
  graphics::axis(1, xat)
  graphics::axis(
    side = 2, log_counts, las = 1,
    labels = parse(text = paste0("10^", log_counts))
  )

  # ---- raster plot -----------------------------------------------------------
  graphics::par(mar = c(0, 4.1, 3.1, 1.1))


  plot(
    x = merged$Time,
    y = merged$Order,
    pch = raster_pch, cex = raster_cex,
    xlim = epoch_window, axes = FALSE,
    ylim = c(0, nrow(trial_info) + 1),
    main = "Over-time plot",
    ylab = "Trial", xlab = "",
    col = merged$Color,
    xaxs = "i", yaxs = "r"
  )


  trial_counts_per_cond <- vapply(conditions, function(cond) {
    as.integer(sum(trial_info$Condition == cond))
  }, FUN.VALUE = NA_integer_)
  tick_pos <- cumsum(c(0, trial_counts_per_cond))
  for(ii in seq_along(conditions)) {
    tick_bd <- tick_pos[c(ii, ii + 1)] + c(1, 0)
    cond <- conditions[[ii]]
    n_trials <- trial_counts_per_cond[[ii]]
    graphics::axis(2, at = tick_bd, labels = c("", ""))
    graphics::axis(2, at = mean(tick_bd), label = sprintf("%s\n(%d)", cond, n_trials), tick = FALSE, las = 1)
  }

  add_baseline(ylim = c(-10, sum(trial_counts_per_cond) + 10), col = "gray50", alpha = 0.2)

  # ---- Firing rates ----------------------------------------------------------

  firing_rate_info <- lapply(split(merged, merged$Condition), function(sub) {
    # sub <- split(merged, merged$Condition)[[1]]

    # time x trial
    firing_rates <- sapply(split(sub, sub$Trial), function(subsub) {
      h <- hist(subsub$Time, bins, plot = FALSE)
      firing_rate <- h$counts / bin_size
      firing_rate
    })

    if(length(use_baseline)) {

      baseline <- colMeans(firing_rates[is_baseline, , drop = FALSE])
      baseline[baseline == 0] <- 1
      firing_rates <- colMeans(t(firing_rates) / baseline)

    } else {
      firing_rates <- rowMeans(firing_rates)
    }

    list(
      condition = sub$Condition[[1]],
      firing_rates = firing_rates,
      color = sub$Color[[1]]
    )
  })

  # time x trial
  firing_rates <- do.call("cbind", lapply(firing_rate_info, "[[", "firing_rates"))


  if(firerate_smooth) {
    firing_rates <- stats::filter(firing_rates, filter)
  }

  firing_rate_colors <- sapply(firing_rate_info, "[[", "color")
  ylim <- range(pretty(c(0, max(firing_rates, na.rm = TRUE))))


  graphics::par(mar = c(4.1, 4.1, 0, 1.1))
  graphics::matplot(
    x = firing_rate_time,
    y = firing_rates,

    type = "l",
    lty = 1,
    col = firing_rate_colors,
    axes = FALSE,
    main = "",

    xlim = epoch_window,
    xlab = "Time (s)",
    xaxs = "i",

    ylab = ifelse(use_baseline, "% Change - Firing Rate (x 100%)", "Firing Rate (Hz)"),
    ylim = ylim,
    yaxs = "i"

  )
  graphics::axis(1, pretty(epoch_window))
  graphics::axis(2, pretty(ylim), las = 1)

  add_baseline(ylim = ylim + c(-1, 1), col = "gray50", alpha = 0.2)

  # ---- Legends --------------------------------------------------------------
  graphics::par(mar = c(0.1, 0.1, 0.1, 0.1))
  plot(c(0, 1), c(0, 1), axes = FALSE, xlab = "", ylab = "", main = "", type = "n")
  if(n_conditions <= 3) {
    ncol <- n_conditions
  } else if(n_conditions == 4) {
    ncol <- 2
  } else {
    ncol <- 3
  }
  graphics::legend("center", conditions, lty = 1, col = color_palette[seq_along(conditions)], horiz = FALSE, ncol = ncol)

  invisible(spike_train)
}


# DIPSAUS DEBUG START
# repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = "test/PAV067", electrodes = 1:8)
# sorted_results <- spike_sort_py(repository)
# epoch_name <- "UTO_PAV067_5modality_ALL"
# epoch <- repository$subject$get_epoch(epoch_name)
# epoch_table <- epoch$table
# epoch_table$Block <- repository$blocks[[1]]
# epoch_window <- c(-1, 2)
# visualize_epoch_spike_train(repository, sorted_results, epoch_table, epoch_window, 2, firerate_smooth = TRUE)
# visualize_epoch_spike_train(repository, sorted_results, epoch_table, epoch_window, 2, firerate_smooth = F, baseline_window = c(-1, 0), use_baseline = TRUE)






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

  # iter_tbl <- expand.grid(block = blocks, channel = electrodes)
  # n_iters <- nrow(iter_tbl)
  #
  #
  # callback <- function(iter) {
  #   sprintf("Prepare spike sorting|Iteration %s", iter)
  # }
  #
  # paths <- ravepipeline::lapply_jobs(seq_len(n_iters), function(iter) {
  #
  #   ravecore <- asNamespace("ravecore")
  #   ravecorepy <- ravecore$load_ravecorepy()
  #
  #   row <- iter_tbl[iter, ]
  #
  #   container <- repository$get_container(electrodes = row$channel)
  #
  #   block <- row$block
  #   ch <- row$channel
  #
  #   data_list <- container[[block]][[signal_type]]
  #   data <- data_list$data
  #   sample_rate <- data_list$sample_rate
  #   if(!length(data)) { return(NULL) }
  #
  #   filebase <- data$.filebase
  #   si_path <- file.path(filebase, "spikeinterface", ch, fsep = "/")
  #   re <- list(
  #     block = block,
  #     channel = ch,
  #     path = si_path
  #   )
  #   if(dir.exists(si_path)) {
  #     tryCatch(
  #       {
  #         rec <- ravecorepy$spike$load_recording(path = si_path)
  #         return(re)
  #       },
  #       error = function(e) {
  #         unlink(si_path, recursive = TRUE)
  #       }
  #     )
  #   }
  #
  #   data_ii <- structure(subset(data, Electrode ~ Electrode == ch, drop = FALSE), dimnames = NULL)
  #   rec <- ravecorepy$spike$make_recording(traces = data_ii, fs_hz = sample_rate, path = si_path)
  #   return(re)
  # }, callback = callback, .globals = list(
  #   n_iters = n_iters,
  #   iter_tbl = iter_tbl,
  #   signal_type = signal_type,
  #   repository = repository
  # ))

  # paths <- unlist(paths, recursive = FALSE, use.names = FALSE)
  # paths <- data.table::rbindlist(paths)

  recording_blocks <- lapply(blocks, function(block) {
    # block <- blocks[[1]]; signal_type = "Spike"

    data_list <- container[[block]][[signal_type]]
    data <- data_list$data
    sample_rate <- data_list$sample_rate

    sel <- data_list$dimnames$Electrode %in% electrodes

    block_voltage <- data[, sel, drop = FALSE, dimnames = FALSE]
    recordings <- ravecorepy$spike$make_recording(traces = block_voltage, fs_hz = sample_rate)

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


spike_sort_py <- function(repository, sorter_name = 'mountainsort5', verbose = TRUE, use_cache = TRUE,
                          save_path = file_path(tempdir(), sprintf("rave_si_%s", repository$signature)),
                          signal_type = "Spike") {

  # DIPSAUS DEBUG START
  # repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = "test/PAV067", electrodes = 1:8)
  # save_path = tempfile(pattern = "ravecore_spikeinterface_")
  # verbose = TRUE
  # sorter_name = 'mountainsort5'
  # use_cache <- TRUE
  # signal_type <- "Spike"

  # orig/           # BinaryFolderRecording or ZarrRecording
  # mountainsort5/
  #   •	sorting_np/               # NumpyFolderSorting (cached spike trains)
  #   •	analyzer/                 # SortingAnalyzer folder (extensions cache)
  #   •	reports/                  # export_report / export_to_phy, etc.
  #

  ravecorepy <- load_ravecorepy()
  si <- rpymat::import("spikeinterface")

  # construct folder tree
  save_path <- dir_create2(save_path)
  orig_path <- file_path(save_path, "orig")
  meta_path <- file_path(save_path, "meta.yaml")
  repocache_path <- file_path(save_path, "repository.yaml")
  sorter_root <- dir_create2(file_path(save_path, sorter_name))
  preprocess_path <- file_path(sorter_root, "preproc")
  sorter_path <- file_path(sorter_root, "sorting_np")
  analyzer_path <- file_path(sorter_root, "analyzer")

  # validate meta information
  meta <- fastmap2()
  if(use_cache) {
    if( file_exists(meta_path) ) {
      meta <- load_yaml(meta_path, map = meta)
      if(
        !isTRUE(meta$repository_signature == repository$signature) ||
        length(meta$segment_n_timepoints) != length(repository$blocks) ||
        !identical(meta$sorter_name, sorter_name) ||
        !identical(meta$rave_signal_type, signal_type)
      ) {
        use_cache <- FALSE
        meta$`@reset`()
      }
    } else {
      use_cache <- FALSE
    }
  }

  meta$repository_signature <- repository$signature
  meta$sorter_name <- sorter_name
  meta$rave_signal_type <- signal_type

  # Load original recordings
  recordings <- NULL
  if(use_cache && dir_exists(orig_path)) {

    tryCatch({
      recordings <- ravecorepy$spike$load_recording(orig_path)
    }, error = function(e) {
      # do nothing
    })
  }
  if(is.null(recordings)) {
    prep_results <- use_spikeinterface(repository)
    recordings <- prep_results$recordings
    recordings$save_to_folder(folder = orig_path, overwrite = TRUE)
    recordings <- ravecorepy$spike$load_recording(orig_path)
    meta$segment_n_timepoints <- prep_results$n_timepoints
    rm(prep_results)
  }

  n_timepoints <- meta$segment_n_timepoints

  # Sorter
  sorting <- NULL
  if(use_cache && file_exists(sorter_path)) {
    tryCatch({
      sorting <- ravecorepy$spike$load_recording(sorter_path)
    }, error = function(e) {})
  }
  if(is.null(sorting)) {
    sorting <- ravecorepy$spike$run_sorter(
      recording = recordings,
      sorter_name = sorter_name,
      folder = sorter_path,
      verbose = verbose
    )
    # sorters might not be deterministic and do not use cache
    use_cache <- FALSE
  }

  # analyzer
  analyzer <- NULL
  if(use_cache && file_exists(analyzer_path)) {
    tryCatch({
      analyzer <- si$load_sorting_analyzer(analyzer_path)
    }, error = function(e) {
    })
  }

  if(is.null(analyzer)) {
    recordings_bandpassed <- NULL
    if(use_cache && dir_exists(preprocess_path)) {
      tryCatch({
        recordings_bandpassed <- ravecorepy$spike$load_recording(preprocess_path)
      }, error = function(e) {
      })
    }
    if(is.null(recordings_bandpassed)) {
      recordings_bandpassed <- ravecorepy$spike$bandpass(recordings)
      recordings_bandpassed$save_to_folder(folder = preprocess_path, overwrite = TRUE)
      recordings_bandpassed <- ravecorepy$spike$load_recording(preprocess_path)
    }

    analyzer = si$create_sorting_analyzer(
      sorting = sorting,
      recording = recordings_bandpassed,
      folder = analyzer_path,
      format = "binary_folder",
      overwrite = TRUE
    )
  }

  existing_extensions <- NULL
  if(use_cache) {
    existing_extensions <- names(analyzer$extensions)
  }

  if(!"random_spikes" %in% existing_extensions) {
    analyzer$compute("random_spikes", method="uniform", max_spikes_per_unit = 600L, seed = 42L, save = TRUE)
  }

  # Run waveform extraction
  if(!"waveforms" %in% existing_extensions) {
    analyzer$compute("waveforms", ms_before = 1.0, ms_after = 2.0, save = TRUE)
  }

  # get wavelet templates
  if(!"templates" %in% existing_extensions) {
    analyzer$compute("templates", ms_before = 1.0, ms_after = 2.0, save = TRUE)
  }

  # noise levels
  if(!"noise_levels" %in% existing_extensions) {
    analyzer$compute("noise_levels", save = TRUE)
  }

  # noise levels
  if(!"isi_histograms" %in% existing_extensions) {
    analyzer$compute("isi_histograms", window_ms = 50, bin_ms = 1.0, save = TRUE)
  }

  # > rpymat::py_to_r(analyzer$get_computable_extensions())
  # [1] "random_spikes"        "waveforms"            "templates"            "noise_levels"         "amplitude_scalings"
  # [6] "correlograms"         "isi_histograms"       "principal_components" "spike_amplitudes"     "spike_locations"
  # [11] "template_metrics"     "template_similarity"  "unit_locations"       "quality_metrics"

  # unit x time x channel
  # template_waveforms <- rpymat::py_to_r(templates$get_templates(non_empty_units, ))

  # get spike train
  meta$blocks <- sort(repository$blocks)

  non_empty_units <- sorting$get_non_empty_unit_ids()
  unit_ids <- rpymat::py_to_r(non_empty_units)
  meta$unit_ids <- unit_ids

  # save metadata
  save_yaml(meta, file = meta_path)

  # save repository serialized
  save_yaml(repository$`@marshal`(), file = repocache_path)

  res <- fastmap2()
  res$meta <- as.list(meta, sorted = TRUE)
  res$recordings <- recordings
  res$sorting <- sorting
  res$analyzer <- analyzer

  res

}

epoch_spike_train <- function(repository, sorted_results, epoch_name, epoch_window) {
  # DIPSAUS DEBUG START
  # repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = "test/PAV067", electrodes = 1:8)
  # sorted_results <- spike_sort_py(repository)
  # epoch_window <- c(-1, 2)
  # epoch_name <- "UTO_PAV067_5modality_ALL"

  windows <- ravecore::validate_time_window(epoch_window)

  if(is.character(epoch_name)) {
    epoch <- repository$subject$get_epoch(epoch_name)
    epoch_table <- epoch$table
  } else if (is.data.frame(epoch_name)) {
    epoch_table <- epoch_name
  } else if (inherits(epoch_name, "RAVEEpoch")) {
    epoch_table <- epoch_name$table
  }

  signal_type <- sorted_results$meta$rave_signal_type
  sample_rate <- repository$sample_rates[[signal_type]]
  n_timepoints <- sorted_results$meta$segment_n_timepoints
  blocks <- sorted_results$meta$blocks
  unit_ids <- sorted_results$meta$unit_ids

  get_spike_train <- function(unit_id, start_time, end_time) {
    rpymat::py_to_r(
      sorted_results$sorting$get_unit_spike_train(
        unit_id = unit_id,
        segment_index = 0L,
        start_frame = floor(start_time * sample_rate),
        end_frame = ceiling(end_time * sample_rate),
        return_times = TRUE
      )
    )
  }

  # For each block, extract spikes
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

      re <- lapply(unit_ids, function(unit_id) {

        lapply(windows, function(window) {
          # window <- windows[[1]]; unit_id <- 1
          window <- window + time

          spike_train <- get_spike_train(unit_id = unit_id,
                                         start_time = window[[1]],
                                         end_time = window[[2]])
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
    waveform_method = c("mean", "quantile"), waveform_ylim = NULL, waveform_alpha = 0.5,
    isi_max_time = 50, isi_step_size = 1,
    raster_cex = 1, raster_pch = "|", raster_alpha = 1,
    firerate_smooth = TRUE, firerate_bin = 10,
    use_baseline = FALSE, baseline_window = c(-0.5, 0),
    plot_baseline = use_baseline,
    color_palette = NULL) {

  waveform_method <- match.arg(waveform_method)

  # DIPSAUS DEBUG START
  # repository <- ravecore::prepare_subject_raw_voltage_with_blocks(subject = "test/PAV067", electrodes = 1:8)
  # sorted_results <- spike_sort_py(repository)
  # epoch_window <- c(-1, 2)
  # epoch_name <- "UTO_PAV067_5modality_ALL"
  # unit_id <- sorted_results$meta$unit_ids[[1]]
  # list2env(
  #   envir = .GlobalEnv,
  #   list(
  #     conditions = NULL,
  #     waveform_method = "mean", waveform_ylim = NULL, waveform_alpha = 0.5,
  #     isi_max_time = 500, isi_step_size = 10,
  #     raster_cex = 0.4, raster_pch = "|", raster_alpha = 1,
  #     firerate_smooth = TRUE,
  #     use_baseline = FALSE, baseline_window = c(-0.5, 0),
  #     plot_baseline = FALSE,
  #     color_palette = NULL
  #   )
  # )

  # typically Spike unless they use LFP
  signal_type <- sorted_results$meta$rave_signal_type
  sample_rate <- repository$sample_rates[[signal_type]]

  # This is to calculate overhead for bins and firing rates
  bin_size <- firerate_bin / 1000
  if(isTRUE(firerate_smooth)) {
    half_bandwidth <- 0.05
    filter <- stats::dnorm(seq(-half_bandwidth, half_bandwidth, by = bin_size), mean = 0, sd = half_bandwidth / 3) * bin_size
  } else if (isTRUE(firerate_smooth > 0)) {
    # firerate_smooth is the half-bandwidth, default is 0.25
    half_bandwidth <- as.double(firerate_smooth)
    filter <- stats::dnorm(seq(-half_bandwidth, half_bandwidth, by = bin_size), mean = 0, sd = half_bandwidth / 3) * bin_size
  } else {
    filter <- 1
  }

  # For baseline firing rate
  epoch_starts_boundary <- epoch_window[[1]] - half_bandwidth
  epoch_ends_boundary <- epoch_window[[2]] + half_bandwidth

  n_breaks <- ceiling((epoch_ends_boundary - epoch_starts_boundary) / bin_size) + 1
  bins <- seq(epoch_starts_boundary, by = bin_size, length.out = n_breaks)
  firing_rate_time <- (bins[-1] + bins[-length(bins)]) * 0.5
  is_baseline <- rep(FALSE, length(firing_rate_time))
  if(use_baseline) {
    baseline_window <- validate_time_window(baseline_window)
    for(w in baseline_window) {
      is_baseline <- is_baseline | firing_rate_time <= w[[2]] & firing_rate_time >= w[[1]]
    }
    if(!any(is_baseline)) {
      warning("Baseline is invalid, falling back to average firing rates")
      use_baseline <- FALSE
    }
  }
  # Baseline is still used for visualization purposes
  if(!use_baseline) {
    baseline_window <- tryCatch({
      validate_time_window(baseline_window)
    }, error = function(e) {
      NULL
    })
  }

  add_baseline <- function(ylim, col = "gray80", alpha = 0.2) {
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
    epoch_window = epoch_window + c(-1, 1) * half_bandwidth
  )

  # Get conditions to analyze
  all_conditions <- sort(unique(spike_train$Condition))
  if(!length(conditions)) {
    conditions <- all_conditions
  } else {
    conditions <- unique(conditions)
    conditions <- conditions[conditions %in% all_conditions]
  }
  if(!length(conditions)) {
    stop("No matching condition or no epoch found. Available conditions are: ", paste(all_conditions, collapse = ", "), ".")
  }
  n_conditions <- length(conditions)


  # Construct color palettes, make sure the palette number is long enough
  if(!length(color_palette)) {
    color_palette <- seq_along(conditions)
  } else if (length(color_palette) < n_conditions) {
    color_palette <- rep(color_palette, ceiling(n_conditions / length(color_palette)))
  }

  # Strict boundary
  sel <- spike_train$Time > epoch_starts_boundary &
    spike_train$Time < epoch_ends_boundary &
    spike_train$Unit_id == unit_id &
    spike_train$Condition %in% conditions

  # spike_train_subset has the above properties
  # FIXME: what if this is empty
  spike_train_subset <- spike_train[sel, ]


  # If the table is not empty
  spike_train_subset$Condition <- factor(spike_train_subset$Condition, levels = conditions, ordered = TRUE)

  # Construct order of plot and colors
  trial_info <- unique(spike_train_subset[, c("Condition", "Trial"), with = FALSE])
  trial_info <- trial_info[order(trial_info$Condition, trial_info$Trial), ]
  trial_info$Order <- seq_len(nrow(trial_info))
  trial_info$Color <- color_palette[as.integer(trial_info$Condition)]
  merged <- merge(trial_info, spike_train_subset, by = c("Condition", "Trial"))

  # ---- Layout ----------------------------------------------------------------
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit({ graphics::par(oldpar) })

  graphics::layout(
    matrix(c(3,3,3,4,4,1,1,2,2,5), ncol = 2),
    widths = c(2.5, 1)
  )

  # ---- Plot 1: waveforms -----------------------------------------------------
  # time by channel (full)
  templates <- sorted_results$analyzer$get_extension("templates")
  template_waveforms <- rpymat::py_to_r(templates$get_templates(unit_ids = list(as.integer(unit_id))))
  # FIXME: how to find channel that waveform can be calculated? maybe PCA?
  # Currently, this is to find the min value of each template_waveforms, and use the corresponding channel
  channel_order <- arrayInd(which.min(template_waveforms), dim(template_waveforms))[[3]]

  # get waveform samples from the selected channel
  # sample x time x channel
  ext_waveforms <- sorted_results$analyzer$get_extension("waveforms")
  waveform_samples <- ext_waveforms$get_waveforms_one_unit(unit_id = unit_id, force_dense = TRUE)
  waveform_samples <- rpymat::py_to_r(waveform_samples)[, , channel_order, drop = FALSE]
  # transpose -> time x sample
  dim(waveform_samples) <- dim(waveform_samples)[c(1, 2)]
  waveform_samples <- t(waveform_samples)

  # waveform time
  waveforms_params <- rpymat::py_to_r(ext_waveforms$params)
  waveform_time <- seq(
    - waveforms_params$ms_before,
    by = 1000 / sample_rate,
    length.out = nrow(waveform_samples)
  )
  xlim <- range(pretty(waveform_time))
  switch (
    waveform_method,
    "quantile" = {
      waveform_mean <- apply(waveform_samples, 1L, stats::median)
    },
    {
      waveform_mean <- rowMeans(waveform_samples)
    }
  )

  hard_lim <- range(waveform_mean, na.rm = TRUE)
  waveform_ylim <- waveform_ylim[is.finite(waveform_ylim)]
  if(!length(waveform_ylim)) {
    waveform_ylim <- round(stats::quantile(waveform_samples, c(0.001, 0.999)))
  } else if(length(waveform_ylim) == 1) {
    waveform_ylim <- c(-1, 1) * abs(waveform_ylim)
  } else {
    waveform_ylim <- range(waveform_ylim)
  }
  if(waveform_ylim[[1]] > hard_lim[[1]]) {
    waveform_ylim[[1]] <- hard_lim[[1]]
  }
  if(waveform_ylim[[2]] < hard_lim[[2]]) {
    waveform_ylim[[2]] <- hard_lim[[2]]
  }

  graphics::par(mar = c(4.1, 4.1, 3.1, 2.1))
  graphics::matplot(
    waveform_time,
    waveform_samples,

    type = 'l',
    lty = 1,
    col = grDevices::adjustcolor("#E5E5E5", alpha.f = waveform_alpha),
    main = sprintf("Spike unit #%d", unit_id),
    axes = FALSE,

    xlab = "Time (ms)",
    xaxs = "i",
    xlim = xlim,

    ylab = bquote("Voltage (" ~ mu ~ "V)"),
    ylim = waveform_ylim
  )


  graphics::lines(waveform_time, waveform_mean, lwd = 3)
  graphics::axis(1, pretty(xlim))
  graphics::axis(2, c(waveform_ylim, 0, round(hard_lim)), las = 1)

  # ---- Plot 1: ISI histogram -------------------------------------------------
  spike_train_in_sec = rpymat::py_to_r(sorted_results$sorting$get_unit_spike_train(unit_id=unit_id, return_times = TRUE))
  isis_in_ms <- diff(as.vector(spike_train_in_sec)) * 1000
  max_isis_in_ms <- ceiling(max(isis_in_ms))
  if(max_isis_in_ms <= isi_max_time) { max_isis_in_ms <- NULL }
  isi_step_size <- abs(isi_step_size)
  isi_hist <- graphics::hist(isis_in_ms, breaks = c(seq(0, isi_max_time, by = isi_step_size), max_isis_in_ms), plot = FALSE)

  # plot histogram in log10 scale
  isi_hist$counts[isi_hist$counts == 1] <- 1.4 # log10(1.4) ~= 0.15; log10(2) ~= 0.3;
  isi_hist$counts[isi_hist$counts < 1] <- 1
  isi_hist$counts <- log10(isi_hist$counts)
  if(length(max_isis_in_ms)) {
    isi_hist$counts[[length(isi_hist$counts)]] <- 0
  }
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

    main = sprintf("Histogram (%.1f%% ISI < 1.5 ms)", mean(isis_in_ms < 1.5) * 100)
  )
  graphics::rect(xleft = xleft, xright = xright, ybottom = 0, ytop = isi_hist$counts, col = "gray80", border = graphics::par("bg"))
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
    col = grDevices::adjustcolor(merged$Color, alpha = raster_alpha),
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

      h <- graphics::hist(subsub$Time, bins, plot = FALSE)
      firing_rate <- h$counts / bin_size
      firing_rate

    })

    if(use_baseline) {

      baseline <- colMeans(firing_rates[is_baseline, , drop = FALSE])
      baseline[baseline == 0] <- 1
      firing_rates <- colMeans(t(firing_rates) / baseline) - 1

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

  if(use_baseline) {
    ylim <- range(pretty(c(-1, max(firing_rates, na.rm = TRUE))))
    ylab <- "% Change - Firing Rate (x 100%)"
  } else {
    ylim <- range(pretty(c(0, max(firing_rates, na.rm = TRUE))))
    ylab <- "Firing Rate (Hz)"
  }

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

    ylab = ylab,
    ylim = ylim,
    yaxs = "i"

  )
  graphics::axis(1, pretty(epoch_window))
  graphics::axis(2, pretty(ylim), las = 1)

  add_baseline(ylim = ylim + c(-1, 1), col = "gray80", alpha = 0.2)

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
# epoch_window <- c(-1, 2)
# visualize_epoch_spike_train(
#   repository,
#   sorted_results,
#   epoch_name,
#   epoch_window,
#   unit_id = 3,
#   # firerate_smooth = TRUE,
#   # waveform_ylim = c(-150, 50),
#   # raster_pch = 3,
#   raster_cex = 0.8
# )
# visualize_epoch_spike_train(repository, sorted_results, epoch_table, epoch_window, 2, firerate_smooth = F, baseline_window = c(-1, 0), use_baseline = F)






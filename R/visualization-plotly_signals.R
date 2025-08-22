
StreamSignalPlot <- R6::R6Class(
  classname = "StreamSignalPlot",
  portable = FALSE,
  private = list(
    # plotly instance
    .impl = NULL,
    # for shiny output and proxy
    .output_id = character(),

    .start_time = numeric(),
    .sample_rate = numeric(),
    # list of channel signals
    .data = NULL,
    .channel_names = character(),
    .channel_needs_update = logical(),

    .channel_gap = numeric(),
    .xlab = character(),
    .ylab = character()

  ),
  public = list(

    #' @field needs_update integer flag indicating whether the plot needs to
    #' be updated: 0 means no update is needed; 1 means underlying data is
    #' updated; 2 means
    needs_update = FALSE,

    #' @field MAX_POINTS maximum number of points to plot before down-sampling
    #' kicks in
    MAX_POINTS = 100000,

    initialize = function(n_channels, sample_rate,
                          start_time = 0,
                          channel_names = sprintf("ch%04d", seq_len(n_channels)),
                          channel_gap = 0,
                          xlab = "Time (s)", ylab = "Channel") {
      if(!package_installed("plotly")) {
        stop("Signal plot with streaming data requires installing package plotly. Please run `install.packages('plotly')`")
      }

      if(missing(n_channels)) {
        n_channels <- length(channel_names)
      } else {
        stopifnot(length(channel_names) == n_channels)
      }

      stopifnot(sample_rate > 0)
      if(anyDuplicated(channel_names)) {
        stop("Channel names must be unique")
      }

      # DIPSAUS DEBUG START
      # n_channels <- 10
      # sample_rate = 100
      # start_time <- 0
      # channel_names = sprintf("ch%04d", seq_len(n_channels))
      # channel_gap <- 0.0
      # xlab = "Time (s)"
      # ylab = "Channel"
      # private <- list()

      private$.start_time <- start_time
      private$.sample_rate <- sample_rate
      private$.channel_names <- channel_names

      private$.data <- lapply(seq_len(n_channels), function(ch) {
        rep(0.0, 2)
      })

      # automatic gap according to the data median
      private$.channel_gap <- as.double(channel_gap)
      private$.xlab <- xlab
      private$.ylab <- ylab

      private$.impl <- plotly::plot_ly(type = "scattergl", mode = "lines")
      self$needs_update <- TRUE
      private$.channel_needs_update <- rep(TRUE, n_channels)
    },

    get_channel_info = function(n) {
      n_channels <- length(private$.channel_names)
      if(is.numeric(n)) {
        stopifnot(isTRUE(n >= 1 && n <= n_channels))
        list(
          index = n,
          name = private$.channel_names[[n]]
        )
      } else {
        idx <- which(private$.channel_names == n)
        if(!length(idx)) {
          stop("Cannot find channel by name ", paste(n, collapse = ""))
        }
        list(
          index = idx[[1]],
          name = n
        )
      }
    },

    set_channel_data = function(n, data) {
      ch_info <- self$get_channel_info(n)
      private$.data[[ch_info$index]] <- data
      private$.channel_needs_update[[ch_info$index]] <- TRUE
      self$needs_update <- TRUE
      invisible(self)
    },

    get_channel_gap = function() {
      gap <- private$.channel_gap
      if( gap <= 0 ) {
        # automatic
        gap <- quantile(abs(unlist(private$.data)), na.rm = TRUE, probs = 0.999) * 2
      }
      gap
    },

    update = function(proxy) {
      needs_update <- self$needs_update || any(private$.channel_needs_update)
      if(!needs_update) { return(invisible(self)) }

      channel_gap <- self$get_channel_gap()
      channel_names <- private$.channel_names
      n_channels <- length(channel_names)
      sample_rate <- private$.sample_rate
      start_time <- private$.start_time

      # estimate down-sampling
      total_timepoints <- vapply(private$.data, length, 0L)
      n_timepoints <- max(total_timepoints)
      duration <- n_timepoints / sample_rate
      ratio <- ceiling(sum(total_timepoints) / self$MAX_POINTS)

      channels_to_update <- which(private$.channel_needs_update)

      # data
      data <- lapply(channels_to_update, function(ii) {
        s <- private$.data[[ii]]
        if(ratio > 1 && length(s) > 20) {
          s <- ravetools::decimate(s, ratio)
        }
        s + channel_gap * ii
      })

      # time
      time_base <- (seq_len(n_timepoints) - 1L) / sample_rate
      time <- lapply(channels_to_update, function(ii) {
        tm <- time_base[seq_along(data[[ii]])]
        if(ratio > 1 && length(tm) > 20) {
          tm <- tm * ratio
        }
        tm + start_time
      })

      # text
      text <- lapply(channels_to_update, function(ii) {
        sprintf("%s <br>%.2f", channel_names[[ii]],
                data[[ii]] - channel_gap * ii)
      })

      # push new y values into first trace
      plotly::plotlyProxyInvoke(
        proxy, "restyle",
        list(
          x = I(time),
          y = I(data),
          text = I(text)
        ),
        as.list(channels_to_update)
      )

      plotly::plotlyProxyInvoke(
        proxy, "relayout",
        list(
          "xaxis.title.text" = private$.xlab,
          # "xaxis.range" = I(c(start_time, start_time + duration)),
          # "xaxis.rangeslider.visible" = TRUE,

          "yaxis.title.text" = private$.ylab,
          "yaxis.tickvals" = seq_len(n_channels) * channel_gap,
          "yaxis.ticktext" = channel_names
        )
      )

      # private$.impl <- plotly::layout(
      #   private$.impl,
      #   showlegend = FALSE,
      #   xaxis = list(title = private$.xlab),
      #   yaxis = list(
      #     title = private$.xlab,
      #     tickmode = "array",
      #     tickvals = seq_len(n_channels) * gap,
      #     ticktext = channel_names
      #   )
      # )
      invisible(self)
    },

    render = function() {
      if(self$needs_update) {
        # prepare initial plot data
        channel_names <- private$.channel_names
        sample_rate <- private$.sample_rate
        channel_gap <- self$get_channel_gap()

        start_time <- private$.start_time
        n_channels <- length(channel_names)

        # estimate down-sampling
        total_timepoints <- vapply(private$.data, length, 0L)
        n_timepoints <- max(total_timepoints)
        ratio <- ceiling(sum(total_timepoints) / self$MAX_POINTS)

        time_base <- (seq_len(n_timepoints) - 1L) / sample_rate

        plot_data <- lapply(seq_len(n_channels), function(ii) {
          channel_name <- channel_names[[ii]]
          channel_data <- private$.data[[ii]]
          if(ratio > 1 && length(channel_data) > 20) {
            channel_data <- ravetools::decimate(channel_data, ratio)
            time <- time_base[seq_along(channel_data)] * ratio
          } else {
            time <- time_base[seq_along(channel_data)]
          }
          list(
            Time = time + start_time,
            Data = channel_data,
            y = channel_data + ii * channel_gap,
            Name = rep(channel_name, length(channel_data))
          )
        })
        plot_data <- data.table::rbindlist(plot_data, use.names = TRUE)
        impl <- plotly::plot_ly(type = "scattergl", mode = "lines")
        impl <- plotly::add_trace(
          impl,
          data = plot_data,
          x = ~ Time,
          y = ~ y,
          split = ~ Name,
          line = list(width = 1),
          text = ~ sprintf("%s <br>%.2f", Name, Data),  # keep original values
          hoverinfo = "text+x"  # show custom text and time only
        )
        impl <- plotly::layout(
          impl,
          showlegend = FALSE,
          xaxis = list(title = private$.xlab),
          yaxis = list(
            title = private$.ylab,
            tickmode = "array",
            tickvals = seq_len(n_channels) * channel_gap,
            ticktext = channel_names
            # rangeslider.visible
          )
        )
        private$.impl <- impl
        self$needs_update <- FALSE
      }
      private$.impl
    }

  )
)



#' @name glimpse-repository
#' @title Visualizes repositories with interactive plots
#' @description
#' Requires optional package \code{'plotly'}; please install the package prior
#' to launching the viewer.
#'
#' @param repository 'RAVE' repository
#' @param initial_block initial recording block to select
#' @param channels channels to visualize; default is all
#' @param epoch_name additional epoch to annotation
#' @param start_time,duration,channel_gap initial start time, duration, and
#' channel gap (can be changed later)
#' @param highpass_freq,lowpass_freq filter to apply when visualizing the
#' signals, useful when signals have 'DC' shift
#' @param shiny_ns shiny name-space
#'
#' @returns An R-shiny application
#' @examples
#'
#' subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)
#'
#' if(interactive() && file.exists(subject$data_path)) {
#'
#'   repository <- ravecore::prepare_subject_voltage_with_blocks(
#'     subject = subject
#'   )
#'
#'   glimpse_voltage_repository_with_blocks(
#'     repository = repository,
#'     initial_block = "008",
#'     epoch_name = "auditory_onset",
#'     highpass_freq = 0.5
#'   )
#'
#' }
#'
#' @export
glimpse_voltage_repository_with_blocks <- function(
    repository, initial_block = NULL, channels = NULL, epoch_name = NULL,
    start_time = 0, duration = 5, channel_gap = 1000,
    highpass_freq = NA, lowpass_freq = NA, shiny_ns = NULL) {

  if(!package_installed("plotly")) {
    stop("This function requires package `plotly`. Please instal this package first.")
  }

  # repository <- ravecore::prepare_subject_voltage_with_blocks("demo/DemoSubject")
  # epoch_name <- "auditory_onset"
  # channels <- NULL
  # initial_block <- NULL
  # channel_gap <- 1000
  # start_time = 0; duration = 5

  ns <- shiny::NS(shiny_ns)

  if(!length(highpass_freq)) { highpass_freq <- NA }
  if(!length(lowpass_freq)) { lowpass_freq <- NA }
  filter_msg <- NULL
  if(!is.na(highpass_freq)) {
    filter_msg <- sprintf(" HighPass=%g", highpass_freq)
  }
  if(!is.na(lowpass_freq)) {
    filter_msg <- c(filter_msg, sprintf(" LowPass=%g", lowpass_freq))
  }
  filter_msg <- paste(filter_msg, collapse = ",")

  initial_block <- initial_block %OF% repository$blocks
  channels <- parse_svec(channels)
  channels <- repository$electrode_list[repository$electrode_list %in% channels]
  if(!length(channels)) {
    channels <- repository$electrode_list
  }

  if(is.na(channel_gap) || channel_gap < 0) { channel_gap <- 1000 }

  # Get epoch
  if(inherits(epoch_name, "RAVEEpoch")) {
    epoch <- epoch_name
    epoch_name <- epoch$name
  } else if(isTRUE(epoch_name %in% repository$subject$epoch_names)) {
    epoch <- repository$subject$get_epoch(epoch_name = epoch_name, as_table = FALSE)
  } else {
    epoch <- NULL
  }
  if(is.null(epoch)) {
    annotation_table_full <- NULL
  } else {
    # generate annotation table from epoch
    epoch_events <- unique(c("", epoch$available_events))
    annotation_table_full <- data.table::rbindlist(lapply(seq_along(epoch_events), function(ii) {
      event_name <- epoch_events[[ii]]
      event_cname <- epoch$get_event_colname(event = event_name, missing = "warning")
      if(event_name == "") { event_name <- "Onset" }
      annotation_table <- data.frame(
        block = epoch$table$Block,
        time = epoch$table[[event_cname]],
        label = sprintf(
          "%s[%s,t=%.1f]<br>%s",
          event_name,
          epoch$table$Trial,
          epoch$table[[event_cname]],
          epoch$table$Condition
        ),
        group = event_name,
        color = ii + 1
      )
    }))
  }



  # Get sample rates for each channel
  sample_rates <- repository$sample_rates
  electrode_types <- repository$subject$electrode_types[repository$subject$electrodes %in% channels]
  channel_sample_rates <- unname(unlist(sample_rates[electrode_types]))

  # get electrode table
  o <- order(repository$electrode_table$Electrode)
  electrode_table <- repository$electrode_table[o, ]
  electrode_table <- electrode_table[electrode_table$Electrode %in% channels, ]
  electrode_table$SignalType <- electrode_types
  electrode_table$SampleRate <- channel_sample_rates

  # channel names
  channel_names <- sprintf("%s|ch%d", electrode_table$Label, electrode_table$Electrode)

  # initialize the plot object
  stream_plot_container <- StreamSignalPlot$new(
    n_channels = length(channels),
    sample_rates = channel_sample_rates,
    start_time = start_time,
    channel_names = channel_names,
    channel_gap = channel_gap,
    title = "",
    ylab = "Channel"
  )
  signal_container <- repository$get_container()
  local_data <- fastmap2()
  local_data$current_block <- initial_block

  update_plot <- function(
    block,
    start_time,
    duration,
    channel_gap,
    quality = c("performance", "balanced", "high-quality"),
    init = FALSE,
    stream_proxy
  ) {

    quality <- match.arg(quality)

    if(length(start_time) != 1 || is.na(start_time)) { start_time <- stream_plot_container$start_time }
    if(length(duration) != 1 || is.na(duration)) { start_time <- stream_plot_container$max_duration }
    if(length(channel_gap) != 1 || is.na(channel_gap)) { channel_gap <- stream_plot_container$channel_gap }

    end_time <- start_time + duration

    current_start_time <- stream_plot_container$start_time
    current_duration <- stream_plot_container$max_duration
    current_data_range <- current_start_time + c(0, current_duration)

    stream_plot_container$channel_gap <- channel_gap
    switch (
      quality,
      "high-quality" = { stream_plot_container$MAX_POINTS <- 2000000 },
      "performance" = { stream_plot_container$MAX_POINTS <- 100000 },
      { stream_plot_container$MAX_POINTS <- 500000 }
    )

    # Whether to load data from disk - performance
    data_needs_update <- init ||
      current_data_range[[1]] > start_time ||
      current_data_range[[2]] < end_time ||
      !identical(local_data$current_block, block)


    # Update stream_plot_container
    if( data_needs_update ) {
      shiny::showNotification("Loading data...", id = "notification")
      local_data$current_block <- block
      recording_block <- block
      block_data <- signal_container[[recording_block]]

      # Set annotations
      if(is.data.frame(annotation_table_full)) {
        stream_plot_container$annotations <- annotation_table_full[annotation_table_full$block == recording_block, ]
      }

      stream_plot_container$title <- sprintf("Recording block: %s%s", recording_block, filter_msg)

      signal_types <- unique(electrode_table$SignalType)

      if( init ) {
        load_start_time <- start_time
        load_duration <- duration
      } else {
        # preload duration
        total_sample_rates <- sum(electrode_table$SampleRate)
        total_timepoints <- duration * total_sample_rates
        if(total_timepoints <= 1e6) {
          # 40 MB from disk
          load_duration <- 1e7 / total_sample_rates
          load_start_time <- start_time - ((load_duration - duration) * 0.5)
          if(load_start_time < 0) {
            load_start_time <- 0
          }
        } else if(total_timepoints <= 1e7){
          # max 100 MB from disk
          load_start_time <- start_time
          load_duration <- duration + ceiling(duration * 0.75)
        } else {
          load_start_time <- start_time
          load_duration <- duration
        }
      }

      # construct filters
      filters <- list()
      if(!is.na(highpass_freq) || !is.na(lowpass_freq)) {
        filters <- structure(
          names = signal_types,
          lapply(signal_types, function(signal_type) {
            sample_rate <- repository$sample_rates[[signal_type]]
            max_order <- floor(load_duration * sample_rate / 3) - 1
            ravetools::design_filter(
              sample_rate = sample_rate,
              method = "firls",
              high_pass_freq = highpass_freq,
              low_pass_freq = lowpass_freq,
              filter_order = min(1600, max_order)
            )
          })
        )

      }

      lapply(signal_types, function(signal_type) {
        row_selector <- which(electrode_table$SignalType == signal_type)
        signal_info <- block_data[[signal_type]]

        channels <- electrode_table$Electrode[row_selector]

        signal_data <- unname(subset(
          signal_info$data,
          Electrode ~ Electrode %in% channels,
          Time ~ Time >= load_start_time & Time <= (load_start_time + load_duration),
          drop = FALSE, .env = environment()
        ))

        dimnames(signal_data) <- NULL

        filter <- filters[[signal_type]]
        if(length(filter)) {
          signal_data <- ravetools::filtfilt(b = filter$b, a = filter$a, x = signal_data)
        }

        lapply(seq_along(row_selector), function(ii) {
          stream_plot_container$set_channel_data(row_selector[[ii]], data = signal_data[, ii])
          return()
        })

        return()
      })

      stream_plot_container$start_time <- load_start_time
    }


    if(!init) {
      shiny::showNotification("Updating graphics...", id = "notification")
      stream_plot_container$update(proxy = stream_proxy,
                                   start_time = start_time,
                                   duration = duration)
    }
    shiny::removeNotification(id = "notification")
  }

  # initialize UI
  block_info <- signal_container[[initial_block]]
  preferred_names <- c("LFP", "Spike", "Auxiliary", names(block_info)[[1]])
  preferred_name <- preferred_names[preferred_names %in% names(block_info)][[1]]
  signal_info <- block_info[[preferred_name]]
  n_timepoints <- signal_info$dim[[1]]
  max_duration <- floor(n_timepoints / signal_info$sample_rate)

  module_ui <- shiny::fluidPage(
    shiny::tags$style(
      ".row { margin-left: 0; margin-right: 0; }",
      ".col-sm-1, .col-sm-10, .col-sm-11, .col-sm-12, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9 { padding: 0; }"
    ),
    style = "margin:0; padding:0; height:100vh",
    shiny::sidebarLayout(
      fluid = TRUE,
      shiny::sidebarPanel(

        style = "height:100vh; border-radius:0",
        width = 2,

        shiny::selectInput(
          inputId = ns("block"),
          label = "Block",
          choices = repository$blocks,
          selected = initial_block
        ),

        shiny::selectInput(
          inputId = ns("quality"),
          label = "Quality",
          choices = c("performance", "balanced", "high-quality"),
          selected = "performance"
        ),

        shiny::numericInput(
          inputId = ns("channel_gap"),
          label = "Gap size",
          min = 0, step = 50,
          value = channel_gap
        ),

        shiny::numericInput(
          inputId = ns("start_time"),
          label = "Start time",
          min = 0, max = floor(max_duration - 1), step = 1,
          value = start_time
        ),

        shiny::numericInput(
          inputId = ns("duration"),
          label = "Duration",
          min = 0, step = 0.5,
          value = duration
        ),

        shiny::actionButton(
          inputId = ns("sync"),
          label = "Sync selection"
        )

      ),

      shiny::mainPanel(

        width = 10,

        plotly::plotlyOutput(outputId = ns("stream_plot"), width = "100%", height = "100vh")

      )
    )
  )

  module_server <- function(input, output, session, ...){


    # Local reactive values, used to store reactive event triggers
    local_reactives <- shiny::reactiveValues(
      update_outputs = NULL
    )

    stream_proxy <- plotly::plotlyProxy(outputId = "stream_plot", session = session)

    # input inter-interactions
    shiny::bindEvent(
      shiny::observe({
        # try({
          duration <- input$duration
          if(length(duration) == 1 && !is.na(duration) && isTRUE(duration > 0)) {
            shiny::updateNumericInput(
              session = session,
              inputId = 'start_time',
              step = max(duration * 0.75, min(1, round(duration, 2)))
            )
          }
        # })
      }),
      input$duration,
      ignoreNULL = TRUE, ignoreInit = FALSE
    )

    shiny::bindEvent(
      shiny::observe({
        # try({
          block <- input$block
          ravepipeline::logger("Block ", block, " is selected")
          if(length(block) != 1 || !block %in% repository$block) { return() }
          block_info <- signal_container[[block]]
          preferred_names <- c("LFP", "Spike", "Auxiliary", names(block_info)[[1]])
          preferred_name <- preferred_names[preferred_names %in% names(block_info)][[1]]
          signal_info <- block_info[[preferred_name]]
          n_timepoints <- signal_info$dim[[1]]
          max_duration <- floor(n_timepoints / signal_info$sample_rate) - 1L

          if(isTRUE(input$start_time < max_duration)) {
            start_time <- input$start_time
          } else {
            start_time <- 0
          }

          shiny::updateNumericInput(
            session = session,
            inputId = 'start_time',
            max = max_duration,
            value = start_time
          )
        # })
      }),
      input$block,
      ignoreNULL = TRUE, ignoreInit = TRUE
    )


    shiny::bindEvent(
      shiny::observe({
        update_plot(
          block = input$block,
          start_time = input$start_time,
          duration = input$duration,
          channel_gap = input$channel_gap,
          quality = input$quality,
          init = FALSE,
          stream_proxy = stream_proxy
        )
      }),
      input$block,
      input$start_time,
      input$duration,
      input$channel_gap,
      input$quality,
      ignoreNULL = TRUE, ignoreInit = TRUE
    )


    shiny::bindEvent(
      shiny::observe({
        relayout <- as.list(plotly::event_data("plotly_relayout"))
        start_time <- as.numeric(relayout[["xaxis.range[0]"]])
        end_time <- as.numeric(relayout[["xaxis.range[1]"]])
        if(length(start_time) != 1 || is.na(start_time)) { return() }
        # start_time <- floor(start_time)
        shiny::updateNumericInput(session = session,
                                  inputId = 'start_time',
                                  value = start_time)

        if(length(end_time) != 1 || is.na(end_time)) { return() }
        duration <- end_time - start_time
        shiny::updateNumericInput(session = session,
                                  inputId = 'duration',
                                  value = duration)
      }),
      input$sync,
      ignoreInit = TRUE, ignoreNULL = TRUE
    )

    # Register outputs
    output$stream_plot <- plotly::renderPlotly({

      update_plot(
        block = initial_block,
        start_time = start_time,
        duration = duration,
        channel_gap = channel_gap,
        quality = "performance",
        init = TRUE
      )

      stream_plot_container$render()
    })
  }

  if(rstudio_main_session()) {
    # rstudioapi::viewer
    options <- list(launch.browser = asNamespace("rstudioapi")$viewer)
  } else {
    options <- list()
  }

  shiny::shinyApp(
    ui = module_ui,
    server = function(input, output, session) {
      shiny::moduleServer(id = shiny_ns, module_server)
    },
    options = options
  )
}

# glimpse_voltage_repository_with_blocks(repository, highpass_freq = 1)

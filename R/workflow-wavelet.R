#' Apply 'Morlet' wavelet to subject
#' @description
#' Calculates time-frequency decomposition; not intended for direct use.
#' Please use 'RAVE' pipelines (see 'Examples').
#' @param subject 'RAVE' subject or subject ID
#' @param electrodes electrode channels to apply, must be imported and
#' \code{'LFP'} type
#' @param freqs numeric vector of frequencies to apply
#' @param cycles number of wavelet cycles at each \code{freqs}, integers
#' @param target_sample_rate the resulting 'spectrogram' sampling frequency
#' @param kernels_precision double or single (default) floating precision
#' @param pre_downsample down-sample (integer) priory to the decomposition;
#' set to 1 (default) to avoid
#' @param verbose whether to verbose the progress
#' @details
#' The channel signals are first down-sampled (optional) by a ratio of
#' \code{pre_downsample} via a 'FIR' filter. After the down-sample,
#' 'Morlet' wavelet kernels are applied to the signals to calculate the
#' wavelet coefficients (complex number) at each frequency in \code{freqs}.
#' The number of \code{cycles} at each frequency controls the number of
#' sine and cosine waves, allowing users to balance the time and power
#' accuracy. After the decomposition, the 'spectrogram' is further down-sampled
#' to \code{target_sample_rate}, assuming the brain power is a smooth function
#' over time. This down-sample is done via time-point sampling to preserve the
#' phase information (so the linear functions such as common-average or
#' bi-polar reference can be carried over to the complex coefficients).
#' @returns The decomposition results are stored in 'RAVE' subject data path;
#' the function only returns the wavelet parameters.
#'
#' @examples
#'
#' # Check https://rave.wiki for additional pipeline installation
#'
#' \dontrun{
#'
#' # ---- Recommended usage --------------------------------------------
#'
#'
#' pipeline <- ravepipeline::pipeline("wavelet_module")
#' pipeline$set_settings(
#'   project_name = "demo",
#'   subject_code = "DemoSubject",
#'   precision = "float",
#'   pre_downsample = 4,
#'   kernel_table = ravetools::wavelet_cycles_suggest(
#'     freqs = seq(1, 200, by = 1)),
#'   target_sample_rate = 100
#' )
#'
#' # Internally, the above pipeline includes this function call below
#'
#' # ---- For demonstration use, do not call this function directly ----
#'
#' # Original sample rate: 2000 Hz
#' # Downsample by 4 to 500 Hz first - 250 Hz Nyquist
#' # Wavelet at each 1, 2, ..., 200 Hz
#' #   The number of cycles log-linear from 2 to 20
#' #   The wavelet coefficient sample rate is 500 Hz
#' # Further down-sample to 100 Hz to save storage space
#'
#' run_wavelet(
#'   subject = "demo/DemoSubject",
#'   electrodes = c(13:16, 2),
#'   pre_downsample = 4,
#'   freqs = seq(1, 200, by = 1),
#'   cycles = c(2, 20),
#'   target_sample_rate = 100
#' )
#'
#' }
#'
#'
#' @export
run_wavelet <- function(
    subject, electrodes, freqs, cycles,
    target_sample_rate = 100,
    kernels_precision = "float", pre_downsample = 1,
    verbose = TRUE
) {
  # DIPSAUS DEBUG START
  # list2env(
  #   list(
  #     subject = "YAEL/PAV020",
  #     electrodes = c(1, 2, 3, 4, 5, 6,
  #                    7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20),
  #     freqs = c(2L,
  #               22L, 42L, 62L, 82L, 102L, 122L, 142L, 162L, 182L),
  #     cycles = c(3,
  #                8, 11, 12, 14, 15, 16, 17, 18, 19),
  #     target_sample_rate = 100,
  #     kernels_precision = "float",
  #     pre_downsample = 4,
  #     verbose = TRUE
  #   ),
  #   envir = globalenv()
  # )

  subject <- restore_subject_instance(subject, strict = FALSE)

  # clear subject's cached files
  clear_cached_files(
    subject_code = subject$subject_code
  )

  blocks <- subject$preprocess_settings$blocks
  srates <- subject$preprocess_settings$sample_rates
  srates <- sapply(electrodes, function(e){
    re <- srates[subject$electrodes == e]
    if(!length(re)) {
      stop("Electrode ", e, " does not have sample rate. The data might not be imported correctly and some configurations are missing.")
    }
    re[[1]]
  })
  compress_rates <- srates / target_sample_rate
  pre_decimate <- as.integer(pre_downsample)
  if(is.na(pre_decimate) || pre_decimate < 1) {
    pre_decimate <- 1
  }

  overall_progress <- ravepipeline::rave_progress(
    title = "Wavelet overall progress",
    max = 6,
    shiny_auto_close = TRUE,
    quiet = !verbose
  )

  # Create dir
  overall_progress$inc("Creating directories")
  subject$initialize_paths(include_freesurfer = FALSE)
  dir_create2(file.path(subject$data_path, "power"))
  dir_create2(file.path(subject$data_path, "phase"))
  dir_create2(file.path(subject$data_path, "voltage"))

  # set up ravetools temporary path
  tpath <- dir_create2(
    file.path(cache_root(), "ravetools", rand_string(length = 10))
  )
  ravetools_tdir_opt <- getOption("ravetools.tempdir", default = NULL)
  ravetools_tdir_env <- Sys.getenv("RAVETOOLS_TEMPDIR", unset = "")
  options("ravetools.tempdir" = tpath)
  Sys.setenv("RAVETOOLS_TEMPDIR" = tpath)

  on.exit({
    options("ravetools.tempdir" = ravetools_tdir_opt)
    if(identical(ravetools_tdir_env, "")) {
      Sys.unsetenv("RAVETOOLS_TEMPDIR")
    } else {
      Sys.setenv("RAVETOOLS_TEMPDIR" = ravetools_tdir_env)
    }
    unlink(tpath, recursive = TRUE)
  }, add = TRUE, after = FALSE)

  # prepare kernels
  overall_progress$inc("Generating wavelet kernels")
  sample_file <- file.path(subject$preprocess_path, 'voltage',
                           sprintf('electrode_%d.h5', electrodes[[1]]))
  if(!file.exists(sample_file) || !ieegio::io_h5_valid(sample_file)){
    stop("Electrode file is missing (preprocess, electrode ", electrodes[[1]], ")")
  }
  sample_names <- gsub("^/", "", ieegio::io_h5_names(sample_file))



  ravetools <- asNamespace("ravetools")

  generate_kernel <- ravetools[[sprintf("wavelet_kernels2_%s", kernels_precision)]]

  lapply(unique(srates), function(srate) {
    lapply(blocks, function(block){
      sample_name <- sprintf("notch/%s", block)
      if(!sample_name %in% sample_names) {
        stop(sprintf("I can find the imported signal file for Electrode %s, but cannot find any notch-filtered signal for block %s. The data file might be corrupted.", electrodes[[1]], block))
      }
      ptr <- load_h5(sample_file, name = sample_name, ram = FALSE, read_only = TRUE)
      data_length <- length(ptr)

      if(data_length <= 0) {
        stop(sprintf("Electrode %s has zero-length signal (/notch/%s). The data file might be corrupted.", electrodes[[1]], block))
      }

      if(pre_downsample > 1) {
        sample_data <- ravetools::decimate(ptr[], pre_downsample, ftype = "fir")
        data_length <- length(sample_data)
      }

      ptr$close()

      generate_kernel(freqs = freqs, srate = srate / pre_decimate, wave_num = cycles, data_length = data_length, signature = subject$subject_id)
    })
  })

  # 2. raw channel files and power/phase files
  overall_progress$inc("Removing previously generated wavelet coefficients")
  preproc <- RAVEPreprocessSettings$new(subject = subject$subject_id, read_only = TRUE)

  for(e in electrodes) {
    preproc$data[[as.character(e)]]$has_wavelet <- FALSE
  }
  preproc$save()

  data_root <- subject$data_path
  lapply(electrodes, function(e){
    unlink(file.path(data_root, 'power', sprintf('%d.h5', e)))
    unlink(file.path(data_root, 'phase', sprintf('%d.h5', e)))
    unlink(file.path(data_root, 'voltage', sprintf('%d.h5', e)))
    for(block in blocks){
      unlink(file.path(data_root, 'cache', 'power', 'raw', block, sprintf('%d.fst', e)))
      unlink(file.path(data_root, 'cache', 'phase', 'raw', block, sprintf('%d.fst', e)))
    }
  })



  # load signals
  overall_progress$inc("Applying wavelet (a.k.a. the long step)")
  preprocess_dir <- subject$preprocess_path
  ravepipeline::lapply_jobs(
    seq_along(electrodes), function(ii){

      e <- electrodes[[ii]]
      srate <- srates[[ii]]
      compress_rate <- compress_rates[[ii]]

      if(pre_decimate > 1) {
        compress_rate <- compress_rates[[ii]] / pre_decimate
        srate <- srate / pre_decimate
      }

      for(block in blocks){

        sorig <- ieegio::io_read_h5(
          file = file.path(preprocess_dir, 'voltage', sprintf('electrode_%d.h5', e)),
          name = sprintf('/notch/%s', block),
          ram = TRUE
        )

        if(pre_decimate > 1) {
          s <- ravetools::decimate(sorig, pre_decimate,
                                   ftype = "fir")
        } else {
          s <- sorig
        }
        data_length <- length(s)

        re <- ravetools::morlet_wavelet(
          data = s,
          freqs = freqs,
          srate = srate,
          wave_num = cycles,
          precision = kernels_precision,
          signature = subject$subject_id
        )

        # Subset coefficients to save space
        ind <- floor(seq(1, data_length, by = compress_rate))

        if(kernels_precision == "float"){
          # Load all at once and subset is faster, but one signal is around 1-2GB, so...
          coef <- t(re[ind,,drop = FALSE])

          phase <- Arg(coef)
          power <- Mod(coef)^2

          re$.mode <- "readwrite"
          re$delete()
        } else {
          coef <- t(re$real[ind, , drop = FALSE] + 1i * re$imag[ind, , drop = FALSE])

          phase <- Arg(coef)
          power <- Mod(coef)^2
          re$real$.mode <- "readwrite"
          re$real$delete()
          re$imag$.mode <- "readwrite"
          re$imag$delete()
        }

        # Save power, phase, voltage
        fname <- sprintf('%d.h5', e)
        wavelet_h5chunk <- c(length(freqs), 128)

        # power
        ieegio::io_write_h5(
          x = power,
          file = file.path(data_root, "power", fname),
          name = sprintf('/raw/power/%s', block),
          chunk = wavelet_h5chunk,
          replace = TRUE, quiet = TRUE
        )
        # save_h5(
        #   x = 'noref',
        #   file = file.path(data_root, "power", fname),
        #   name = '/reference',
        #   chunk = 1,
        #   replace = TRUE, size = 1000
        # )

        # phase
        ieegio::io_write_h5(
          x = phase,
          file = file.path(data_root, "phase", fname),
          name = sprintf('/raw/phase/%s', block),
          chunk = wavelet_h5chunk,
          replace = TRUE, quiet = TRUE
        )

        # voltage
        ieegio::io_write_h5(
          x = sorig,
          file = file.path(data_root, "voltage", fname),
          name = sprintf('/raw/voltage/%s', block),
          chunk = 1024,
          replace = TRUE, quiet = TRUE
        )
      }
    },
    callback = function(ii){
      sprintf("Applying wavelet|Electrode - %s", electrodes[[ii]])
    },
    .globals = list(
      electrodes = electrodes,
      srates = srates,
      compress_rates = compress_rates,
      pre_decimate = pre_decimate,
      blocks = blocks,
      freqs = freqs,
      cycles = cycles,
      subject = subject,
      kernels_precision = kernels_precision,
      data_root = data_root,
      preprocess_dir = preprocess_dir
    )
  )

  # reload preprocess settings in case some changes are not captured
  preproc <- RAVEPreprocessSettings$new(subject = subject$subject_id, read_only = TRUE)


  overall_progress$inc("Saving configurations and update log files")

  for(e in electrodes) {
    preproc$data[[as.character(e)]]$notch_filtered <- TRUE
    preproc$data[[as.character(e)]]$has_wavelet <- TRUE
  }


  wavelet_params <- list(
    channels = electrodes,
    electrodes = electrodes,
    downsample_to = target_sample_rate,
    target_srate = target_sample_rate,
    frequencies = freqs,
    wave_num = cycles,
    cycle = cycles,
    precision = kernels_precision,
    pre_downsample = pre_downsample
  )

  wavelet_logs <- as.list(preproc$data$wavelet_logs)
  wavelet_logs[[length(wavelet_logs) + 1]] <- wavelet_params
  preproc$data$wavelet_logs <- wavelet_logs

  wavelet_params <- wavelet_params[c(
    "electrodes", "downsample_to", "frequencies",
    "cycle", "precision", "pre_downsample"
  )]
  wavelet_params$timestamp <- strftime(Sys.time(), usetz = TRUE)
  preproc$data$wavelet_params <- wavelet_params
  preproc$save()

  subject$set_default(
    namespace = "wavelet_module",
    key = "parameters",
    wavelet_params
  )
  # generate reference table
  safe_write_csv(
    file = file.path(subject$meta_path, "reference_noref.csv"),
    row.names = FALSE,
    data.frame(
      Electrode = subject$electrodes,
      Group = "Default",
      Reference = "noref",
      Type = "No Reference"
    )
  )
  # generate frequencies.csv
  utils::write.csv(
    file = file.path(subject$meta_path, "frequencies.csv"),
    row.names = FALSE,
    data.frame(
      Frequency = freqs,
      Cycle = cycles,
      Method = "Wavelet"
    )
  )
  # Finalizing: clear cache
  clear_cached_files(
    subject_code = subject$subject_code
  )

  # also remove the meta/time_points.csv
  tpfile <- file.path(subject$meta_path, "time_points.csv")
  if(file.exists(tpfile)) {
    unlink(tpfile)
  }

  overall_progress$inc("Make sure reference files are up-to-date")

  subject <- restore_subject_instance(subject$subject_id)
  # check subject references
  refs <- unlist(lapply(subject$reference_names, function(refname) {
    tryCatch({
      refs <- unique(subject$get_reference(refname, simplify = TRUE))
      refs <- refs[startsWith(refs, "ref_")]
      if(length(refs)) {
        refs <- refs[vapply(refs, function(ref) {
          isTRUE(length(parse_svec(gsub("^ref_", "", ref))) > 1)
        }, FALSE)]
      }
      return(refs)
    }, error = function(e) { NULL })
  }))
  refs <- unique(refs)

  if(length(refs)) {
    progress <- ravepipeline::rave_progress(title = "Re-generate reference",
                                            max = length(refs),
                                            shiny_auto_close = TRUE)
    lapply(refs, function(ref) {
      progress$inc(ref)
      try({
        ref_electrodes <- parse_svec(gsub("^ref_", "", ref))
        generate_reference(subject = subject$subject_id, electrodes = ref_electrodes)
      })
      return()
    })
  }

  return(wavelet_params)
}

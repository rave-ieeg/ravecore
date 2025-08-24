
#' @name power_baseline
#' @title Calculate power baseline
#' @param x R array, \code{\link[filearray]{filearray}}, or
#' \code{'rave_prepare_power'} object created by
#' \code{\link{prepare_subject_power_with_epochs}}.
#' @param baseline_windows list of baseline window (intervals)
#' @param method baseline method; choices are \code{'percentage'},
#' \code{'sqrt_percentage'}, \code{'decibel'}, \code{'zscore'},
#' \code{'sqrt_zscore'}; see 'Details' in \code{\link[ravetools]{baseline_array}}
#' @param units the unit of the baseline; see 'Details'
#' @param filebase where to store the output; default is \code{NULL} and is
#' automatically determined
#' @param electrodes the electrodes to be included in baseline calculation;
#' for power repository object produced by
#' \code{\link{prepare_subject_power_with_epochs}} only; default is all
#' available electrodes
#' @param ... passed to other methods
#'
#' @returns Usually the same type as the input: for arrays
#' and \code{\link[filearray]{filearray}}, the outputs are
#' also the same type with the same dimensions; for \code{'rave_prepare_power'}
#' repositories, the results will be stored in its \code{'baselined'} element;
#' see 'Examples'.
#'
#' @details The arrays must be four-mode tensor and must have valid named
#' \code{\link{dimnames}}. The dimension names must be \code{'Trial'},
#' \code{'Frequency'}, \code{'Time'}, \code{'Electrode'}, case sensitive.
#'
#' The \code{baseline_windows} determines the baseline windows that are used to
#' calculate time-points of baseline to be included. This can be one
#' or more intervals and must pass the validation function
#' \code{\link{validate_time_window}}.
#'
#' The \code{units} determines the unit of the baseline. It can be one or
#' more of \code{'Trial'}, \code{'Frequency'}, \code{'Electrode'}. The default
#' value is all of them, i.e., baseline for each combination of trial,
#' frequency, and electrode. To share the baseline across trials, please
#' remove \code{'Trial'} from \code{units}. To calculate baseline that should
#' be shared across electrodes (e.g. in some mini-electrodes), remove
#' \code{'Electrode'} from the \code{units}.
#'
#' @examples
#'
#' \dontrun{
#' # The following code need to download additional demo data
#' # Please see https://rave.wiki/ for more details
#'
#' repo <- prepare_subject_power_with_epochs(
#'   subject = "demo/DemoSubject",
#'   time_windows = c(-1, 3),
#'   electrodes = c(14, 15))
#'
#' ##### Direct baseline on the repository
#' power_baseline(x = repo, method = "decibel",
#'                baseline_windows = list(c(-1, 0), c(2, 3)))
#' power_mean <- repo$power$baselined$collapse(
#'   keep = c(2,1), method = "mean")
#' image(power_mean, x = repo$time_points, y = repo$frequency,
#'       xlab = "Time (s)", ylab = "Frequency (Hz)",
#'       main = "Mean power over trial (Baseline: -1~0 & 2~3)")
#' abline(v = 0, lty = 2, col = 'blue')
#' text(x = 0, y = 20, "Aud-Onset", col = "blue", cex = 0.6)
#'
#' ##### Alternatively, baseline on electrode instances
#' baselined <- lapply(repo$power$data_list, function(inst) {
#'   re <- power_baseline(inst, method = "decibel",
#'                        baseline_windows = list(c(-1, 0), c(2, 3)))
#'   collapse2(re, keep = c(2,1), method = "mean")
#' })
#' power_mean2 <- (baselined[[1]] + baselined[[2]]) / 2
#'
#' # Same with precision difference
#' max(abs(power_mean2 - power_mean)) < 1e-6
#'
#'
#' }
#'
#' @export
power_baseline <- function(
    x, baseline_windows,
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
    units = c("Trial", "Frequency", "Electrode"), ...
){
  UseMethod("power_baseline")
}

#' @rdname power_baseline
#' @export
power_baseline.rave_prepare_power <- function(
    x, baseline_windows,
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
    units = c("Frequency", "Trial", "Electrode"),
    electrodes, ...
){
  method <- match.arg(method)
  force(baseline_windows)

  if(missing(electrodes)){
    electrodes <- x$electrode_list
  } else {
    electrodes <- electrodes[electrodes %in% x$electrode_list]
    if(!length(electrodes)) {
      stop("`power_baseline`: none of electrodes specified can be found in the loaded repository")
    }
  }

  sample_rates <- x$sample_rates

  # if(!inherits(x$baselined, "fastmap2")){
  #   x$baselined <- fastmap2()
  # }

  # Prepare global variables
  baseline_windows <- validate_time_window(baseline_windows)
  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% c("Frequency", "Trial", "Electrode"))){
    stop('`units` must contain 1-3 of the followings: "Frequency", "Trial", "Electrode" (case-sensitive)')
  }
  unit_dims <- c(1L, 3L, 4L)[c("Frequency", "Trial", "Electrode") %in% units]

  sel <- x$electrode_list %in% electrodes

  # Make sure the data is properly loaded
  x$mount_data(force = FALSE, electrodes = electrodes)

  sub_list <- x$power$data_list[sprintf("e_%d", x$electrode_list[sel])]
  sub_elec <- x$electrode_list[sel]

  # dm <- dim(sub_list[[1]])
  # dm[[4]] <- length(sub_elec)

  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(x$power$dimnames$Time >= w[[1]] & x$power$dimnames$Time <= w[[2]])
  })))

  # calculate signature
  digest_key <- list(
    input_signature = x$power$signature,
    sample_rates = sample_rates,
    rave_data_type = "power",
    method = method,
    unit_dims = unit_dims,
    time_index = time_index,
    dimension = x$power$dim
  )

  signature <- ravepipeline::digest(digest_key)

  filebase <- file.path(cache_root(), "_baselined_arrays_", x$power$signature)
  res <- tryCatch({
    res <- filearray::filearray_checkload(
      filebase, mode = "readwrite", symlink_ok = FALSE,
      rave_signature = signature,
      sample_rates = sample_rates,
      rave_data_type = "power-baselined",
      ready = TRUE,  # The rest procedure might go wrong, in case failure
      RAVEIO_FILEARRAY_VERSION = RAVEIO_FILEARRAY_VERSION
    )
    ravepipeline::logger("Using existing cache", level = "trace")
    res
  }, error = function(e){
    # message(e$message)
    if(dir.exists(filebase)){ unlink(filebase, recursive = TRUE, force = TRUE) }
    dir_create2(dirname(filebase))
    res <- filearray::filearray_create(
      filebase = filebase,
      dimension = x$power$dim,
      type = "float",
      partition_size = 1
    )
    res$.mode <- "readwrite"
    res$.header$rave_signature <- signature
    res$.header$sample_rates <- sample_rates
    res$.header$rave_data_type <- "power-baselined"
    res$.header$baseline_method <- method
    res$.header$unit_dims <- unit_dims
    res$.header$time_index <- time_index
    res$.header$baseline_windows <- baseline_windows
    res$.header$RAVEIO_FILEARRAY_VERSION <- RAVEIO_FILEARRAY_VERSION
    res$.header$ready <- FALSE
    dimnames(res) <- x$power$dimnames
    # # automatically run
    # res$.save_header()
    res
  })

  if("Electrode" %in% units){
    # Check electrode with baselines
    todo_elec <- sub_elec[!sub_elec %in% res$.header$electrodes]

    if(length(todo_elec)) {

      res$set_header("ready", FALSE)

      input_list <- lapply(todo_elec, function(e){
        idx <- which(x$electrode_list == e)
        list(
          index = idx,
          electrode = e,
          array = ravepipeline::RAVEFileArray$new(
            x$power$data_list[[sprintf("e_%d", e)]], temporary = FALSE)
        )
      })

      res_wrapper <- ravepipeline::RAVEFileArray$new(x = res, temporary = FALSE)

      ravepipeline::lapply_jobs(
        input_list,
        function(el) {
          res <- res_wrapper$`@impl`
          res[, , , el$index] <- ravetools::baseline_array(
            x = el$array[drop = FALSE],
            along_dim = 2L,
            baseline_indexpoints = time_index,
            unit_dims = unit_dims,
            method = method
          )
          NULL
        },
        .globals = list(
          time_index = time_index,
          unit_dims = unit_dims,
          method = method,
          res_wrapper = res_wrapper
        ),
        callback = function(el) {
          sprintf("Baseline correction | %s", el$electrode)
        }
      )

      res$.header$electrodes <- c(
        res$.header$electrodes,
        todo_elec
      )

      res$set_header("ready", TRUE)
    }


    res$.mode <- "readonly"

  } else {

    stop("Baseline across electrode not supported")

  }

  power <- x$power
  power$baselined <- res
  return(x)
  #
  #   if("Electrode" %in% units){
  #
  #
  #
  #   } else {
  #
  #     dir_create2(dirname(bind_base))
  #     bind_array <- filearray::filearray_bind(
  #       .list = sub$data_list,
  #       symlink = symlink_enabled(),
  #       filebase = bind_base,
  #       overwrite = TRUE, cache_ok = TRUE)
  #
  #     res[] <- ravetools::baseline_array(
  #       x = bind_array[,,, which(sel),drop=FALSE],
  #       along_dim = 2L,
  #       baseline_indexpoints = time_index,
  #       unit_dims = unit_dims,
  #       method = method
  #     )
  #   }
  #
  #
  #   res$set_header("ready", TRUE)
  #   x$power$baselined <- res
  #
  #   return(x)
}


#' @rdname power_baseline
#' @export
power_baseline.FileArray <- function(
    x, baseline_windows,
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
    units = c("Frequency", "Trial", "Electrode"),
    filebase = NULL, ...
){
  method <- match.arg(method)
  # x <- filearray::filearray_load('/Users/dipterix/rave_data/cache_dir/_binded_arrays_/75131880730a1e599bbcd63c798f62b6/power/LFP'); baseline_windows <- c(-1,2); units = c("Trial", "Frequency", "Electrode"); data_only = FALSE; filebase = tempfile(); method = 'percentage'
  baseline_windows <- validate_time_window(baseline_windows)
  dnames <- dimnames(x)
  dm <- dim(x)
  dnn <- c("Frequency", "Time", "Trial", "Electrode")
  if(!identical(names(dnames), dnn)){
    stop('The dimension names are inconsistent, should be c("Frequency", "Time", "Trial", "Electrode")')
  }
  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% dnn)){
    stop('`units` must contain 1-3 of the followings: "Frequency", "Trial", "Electrode" (case-sensitive)')
  }

  unit_dims <- c(1L, 3L, 4L)[c("Frequency", "Trial", "Electrode") %in% units]
  dnames$Time <- as.numeric(dnames$Time)
  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(dnames$Time >= w[[1]] & dnames$Time <= w[[2]])
  })))

  # calculate signatures
  sample_rates <- x$get_header("sample_rates")
  rave_data_type <- x$get_header("rave_data_type")
  digest_key <- list(
    input_signature = x$get_header("rave_signature", default = "power"),
    sample_rates = sample_rates,
    rave_data_type = rave_data_type,
    method = method,
    unit_dims = unit_dims,
    time_index = time_index,
    dimension = dm,
    x_header = x$.header
  )
  signature <- ravepipeline::digest(digest_key)

  if(!length(filebase)){
    filebase <- file.path(cache_root(), "_baselined_arrays_", signature)
  }
  dir_create2(dirname(filebase))

  res <- tryCatch({
    res <- filearray::filearray_checkload(
      filebase, mode = "readwrite", symlink_ok = FALSE,
      rave_signature = signature,
      sample_rates = sample_rates,
      rave_data_type = "power-baselined",
      ready = TRUE,  # The rest procedure might go wrong, in case failure
      RAVEIO_FILEARRAY_VERSION = RAVEIO_FILEARRAY_VERSION
    )
    # No need to baseline again, the settings haven't changed
    return(res)
  }, error = function(e){
    if(dir.exists(filebase)){ unlink(filebase, recursive = TRUE, force = TRUE) }
    res <- filearray::filearray_create(filebase, dm, type = "float", partition_size = 1)
    res$.mode <- "readwrite"
    res$.header$rave_signature <- signature
    res$.header$sample_rates <- sample_rates
    res$.header$rave_data_type <- "power-baselined"
    res$.header$baseline_method <- method
    res$.header$unit_dims <- unit_dims
    res$.header$time_index <- time_index
    res$.header$baseline_windows <- baseline_windows
    res$.header$RAVEIO_FILEARRAY_VERSION <- RAVEIO_FILEARRAY_VERSION
    res$.header$ready <- FALSE
    dimnames(res) <- dnames
    # # automatically run
    # res$.save_header()
    res
  })

  res_wrapper <- ravepipeline::RAVEFileArray$new(res, temporary = FALSE)
  x_wrapper <- ravepipeline::RAVEFileArray$new(x, temporary = FALSE)


  if("Electrode" %in% units){

    ravepipeline::lapply_jobs(seq_len(dm[[length(dm)]]), function(ii){
      res <- res_wrapper$`@impl`
      res[, , , ii] <-
        ravetools::baseline_array(
          x = x_wrapper[, , , ii, drop = FALSE],
          along_dim = 2L,
          baseline_indexpoints = time_index,
          unit_dims = unit_dims,
          method = method
        )
      NULL
    }, .globals = list(
      res_wrapper = res_wrapper,
      x_wrapper = x_wrapper,
      time_index = time_index,
      unit_dims = unit_dims,
      method = method
    ))

  } else {

    output <- ravetools::baseline_array(
      x = x[drop = FALSE],
      along_dim = 2L,
      baseline_indexpoints = time_index,
      unit_dims = unit_dims,
      method = method)
    res[] <- output

  }

  res$set_header("ready", TRUE)

  res

}

#' @rdname power_baseline
#' @export
power_baseline.array <- function(
    x, baseline_windows,
    method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore"),
    units = c("Trial", "Frequency", "Electrode"), ...
){
  method <- match.arg(method)
  baseline_windows <- validate_time_window(baseline_windows)
  dm <- dim(x)
  dnames <- dimnames(x)
  dnn <- names(dnames)
  stopifnot2(all(dnn %in% c("Frequency", "Time", "Trial", "Electrode")) && length(dm) == 4,
             msg = 'The dimension names are inconsistent, must contain 4 modes: "Frequency", "Time", "Trial", "Electrode"')

  dnames$Time <- as.numeric(dnames$Time)
  time_index <- unique(unlist(lapply(baseline_windows, function(w){
    which(dnames$Time >= w[[1]] & dnames$Time <= w[[2]])
  })))
  time_margin <- which(dnn == "Time")

  units <- units[!units %in% "Time"]
  if(!length(units) || !all(units %in% dnn)){
    stop('`units` must contain 1-3 of the followings: "Frequency", "Trial", "Electrode" (case-sensitive)')
  }
  unit_dims <- which(dnn %in% units)

  ravetools::baseline_array(
    x = x,
    along_dim = time_margin,
    baseline_indexpoints = time_index,
    unit_dims = unit_dims,
    method = method
  )

}

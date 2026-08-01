#' @title Re-align trials to a given event
#' @description
#' 'RAVE' arrays loaded with a trial epoch are locked to the trial onset.
#' \code{realign_trials} time-locks them to another event instead (for
#' example a button press),
#' shifting each trial along the time margin by
#' \code{event time - trial onset}, such that the event occurs at time zero.
#' Samples exposed at the edges by the shift are filled with \code{NA}.
#'
#' @param x an array, a \code{\link[filearray]{FileArray-class}} instance, a
#' file-array proxy, or a \code{RAVEFileArray}; must contain a time margin and
#' a trial margin (see \code{time_margin} and \code{trial_margin})
#' @param event character, name of the event to align to; the corresponding
#' epoch column is resolved by \code{epoch$get_event_colname}. Blank string
#' \code{""}, \code{"default"}, or \code{"trial onset"} refer to the trial
#' onset, in which case no shift is applied. See \code{epoch$available_events}
#' for the events provided by \code{epoch}.
#' @param epoch a \code{\link{RAVEEpoch}} instance providing the trial onsets
#' and the event times
#' @param sample_rate sampling frequency, in 'Hz'; used to convert the time
#' differences into integer sample shifts
#' @param time_margin,trial_margin integers, which margins of \code{x} store
#' the time points and the trial numbers; the default \code{NA} resolves them
#' from \code{names(dimnames(x))}, matching \code{"Time"} and \code{"Trial"}
#' case-insensitively. Both must be less than \code{length(dim(x))}: the last
#' margin is reserved so the array can be processed in chunks.
#' @param strict whether to be strict about the inputs; when \code{TRUE}, the
#' default, a missing event column, a trial that is absent from the epoch
#' table, or a margin that disagrees with \code{dimnames(x)} raises an error;
#' when \code{FALSE}, these conditions are reported as warnings and the
#' affected trials receive no shift
#' @param .filebase where to store the resulting file array; the path is
#' removed first if it already exists. Passing the file base of \code{x}
#' itself rewrites \code{x} in place.
#'
#' @returns A \code{\link[filearray]{FileArray-class}} instance with the same
#' dimensions and \code{dimnames} as \code{x}, whose samples are shifted such
#' that \code{event} occurs at time zero; the samples exposed at the trial
#' edges are \code{NA}. The result is read-only, unless it was written in
#' place, in which case it keeps the mode of \code{x}.
#'
#' The time \code{dimnames} are \strong{not} relabeled: they remain the
#' original onset-locked time points, now to be read as relative to
#' \code{event}. The following headers are attached, and can be read back with
#' \code{$get_header}:
#' \describe{
#' \item{\code{original_time_range}}{range of the time points, in seconds}
#' \item{\code{time_shift_range}}{range of the applied shifts, in seconds}
#' \item{\code{valid_time_range}}{range of the time points for which no trial
#' is \code{NA}; useful to crop the result before plotting or averaging}
#' \item{\code{sample_rate}}{the \code{sample_rate} used}
#' \item{\code{signature_shift_amount}, \code{signature}}{digests identifying
#' the applied shifts and the result}
#' }
#'
#' @seealso \code{\link{RAVEEpoch}}, \code{\link[ravetools]{shift_array}}
#'
#' @examples
#'
#' # Please download DemoSubject ~700MB from
#' # https://github.com/beauchamplab/rave/releases/tag/v0.1.9-beta
#'
#' if( has_rave_subject("demo/DemoSubject") ) {
#'
#' repository <- prepare_subject_voltage_with_epochs(
#'   "demo/DemoSubject", electrodes = 14,
#'   reference_name = "default", epoch_name = "auditory_onset",
#'   time_windows = c(-1, 2))
#'
#' # The demo epoch only contains the trial onset; add a synthetic
#' # response time, 0.1-0.5 seconds after each onset, to align to
#' epoch <- repository$epoch
#' set.seed(1)
#' epoch$table$Event_response <- epoch$table$Time +
#'   round(runif(nrow(epoch$table), 0.1, 0.5), 3)
#' epoch$.columns <- unique(c(epoch$.columns, "Event_response"))
#'
#' epoch$available_events
#'
#' # Time x Trial x Electrode
#' voltage <- repository$voltage$data_list$e_14
#' dim(voltage)
#'
#' aligned <- realign_trials(
#'   voltage, event = "response", epoch = epoch,
#'   sample_rate = repository$sample_rate)
#'
#' # Trials are now locked to the response instead of the onset
#' aligned$get_header("time_shift_range")
#'
#' # Time points where no trial has been shifted out of range
#' valid_time_range <- aligned$get_header("valid_time_range")
#' valid_time_range
#'
#' # Crop to the valid window before averaging over trials
#' time_points <- as.numeric(dimnames(aligned)$Time)
#' keep <- time_points >= valid_time_range[[1]] &
#'   time_points <= valid_time_range[[2]]
#'
#' plot(time_points[keep], rowMeans(aligned[keep, , 1]), type = "l",
#'      xlab = "Time to response (s)", ylab = "Voltage",
#'      main = "Response-locked average")
#' abline(v = 0, lty = 2)
#'
#' # clean up
#' aligned$.mode <- "readwrite"
#' aligned$delete(force = TRUE)
#'
#' }
#'
#' @export
realign_trials <- function(x, event, epoch, sample_rate, time_margin = NA, trial_margin = NA, strict = TRUE,
                           .filebase = tempfile()) {

  if (inherits(x, "RAVEFileArray")) {
    x <- x$`@impl`
  }

  dnames <- as.list(dimnames(x))
  dnm <- names(dnames)
  dm <- dim(x)
  ndm <- length(dm)

  time_margin <- suppressWarnings(as.integer(time_margin))
  if (length(time_margin) != 1 || is.na(time_margin)) {
    time_margin <- which(tolower(dnm) == "time")
  }
  if (
    length(time_margin) != 1 ||
      is.na(time_margin) ||
      time_margin < 1 ||
      time_margin >= ndm
  ) {
    stop(
      "Cannot find margin for `Time`: `time_margin` must be an integer from ",
      "1 to ", ndm - 1, ". ",
      "Please specify the time margin or assign 'Time' to the array `dimnames` attribute."
    )
  }
  trial_margin <- suppressWarnings(as.integer(trial_margin))
  if (length(trial_margin) != 1 || is.na(trial_margin)) {
    trial_margin <- which(tolower(dnm) == "trial")
  }
  if (
    length(trial_margin) != 1 ||
      is.na(trial_margin) ||
      trial_margin < 1 ||
      trial_margin >= ndm ||
      !isTRUE(trial_margin != time_margin)
  ) {
    stop(
      "Cannot find margin for `Trial`: `trial_margin` must be an integer from ",
      "1 to ", ndm - 1, " except for the time margin ", time_margin, ". ",
      "Please specify the trial margin or assign 'Trial' to the array `dimnames` attribute."
    )
  }

  if (strict) {
    # we also check if dnames contain time/trial columns
    m <- which(tolower(dnm) == "time")
    if (length(m) && !isTRUE(time_margin %in% m)) {
      stop(
        "The `dimnames(x)` contains Time or time margin at ", deparse_svec(m), ". ",
        "This is inconsistent with the user-specified time margin ",
        time_margin
      )
    }
    m <- which(tolower(dnm) == "trial")
    if (length(m) && !isTRUE(trial_margin %in% m)) {
      stop(
        "The `dimnames(x)` contains Trial or trial margin at ", deparse_svec(m), ". ",
        "This is inconsistent with the user-specified trial margin ",
        trial_margin
      )
    }
  }

  # check if the trial margin is consistent with n_trials
  if (length(dnames) >= trial_margin && isTRUE(tolower(dnm[[trial_margin]]) == "trial")) {
    trial_number <- dnames[[trial_margin]]
  } else {
    trial_number <- seq_len(dm[[trial_margin]])
  }

  # Check if we can find all trials from epoch; match against the table directly
  # so we do not rely on `epoch$table` rows being in sorted-trial order
  row_index <- match(trial_number, epoch$table$Trial)

  if (anyNA(row_index)) {
    if (strict) {
      stop(
        "Input array has trial ", deparse_svec(trial_number), ", ",
        "while the epoch contains trial number ", deparse_svec(epoch$trials), ". ",
        "I cannot find the following trial information from the epoch table: ",
        deparse_svec(trial_number[is.na(row_index)]))
    } else {
      ravepipeline::logger(
        "Unable to find the following trials from epoch table: ",
        deparse_svec(trial_number[is.na(row_index)]), ". ",
        "No shift will be applied to these trials",
        level = "warning"
      )
    }
  }


  # Load epoch
  cname <- if (strict) {
    epoch$get_event_colname(event = event, missing = "error")
  } else {
    epoch$get_event_colname(event = event, missing = "warning")
  }

  # get time differences by trial
  time_diff <- (epoch$table[[cname]] - epoch$table$Time)[row_index]
  time_diff[is.na(time_diff)] <- 0

  # calculate shift amount
  shift_amount <- round(sample_rate * time_diff)

  if (length(dnames) >= time_margin && length(dnames[[time_margin]])) {
    time_points <- as.numeric(dnames[[time_margin]])
    time_range <- range(time_points, na.rm = TRUE)
  } else {
    time_range <- c(0, (dm[[time_margin]] - 1) / sample_rate)
  }
  shift_range <- range(time_diff, na.rm = TRUE)

  # `shift_array` maps new[t] = old[t + shift], so a trial shifted by `s` samples
  # exposes `NA` at the tail when `s > 0` and at the head when `s < 0`. Use the
  # rounded `shift_amount` (what is actually applied) so the window is exact to
  # the sample rather than off by up to half a sample.
  shift_seconds <- range(shift_amount, na.rm = TRUE) / sample_rate
  valid_time_range <- c(
    time_range[[1]] - min(0, shift_seconds[[1]]),
    time_range[[2]] - max(0, shift_seconds[[2]])
  )

  # Check if x is file array
  temporary <- !inherits(x, c("FileArray", "FileArrayProxy", "RAVEFileArray"))

  # filearray:fmap
  if (temporary) {
    # This will try to flash the array into .filebase
    if (file_exists(.filebase)) {
      unlink(.filebase, recursive = TRUE)
    }
    x <- filearray::as_filearray(x[drop = FALSE], filebase = .filebase, mode = "readwrite")
    dimnames(x) <- dnames
  } else {
    x <- filearray::as_filearray(x)

    if (file_exists(.filebase)) {
      filebase_new <- normalizePath(.filebase, mustWork = TRUE, winslash = "/")
      filebase_old <- normalizePath(x$.filebase, mustWork = TRUE, winslash = "/")

      if (filebase_new != filebase_old) {
        unlink(.filebase, recursive = TRUE)
      }
    }
  }

  # fmap allows write in-place, but the array needs to be readwrite
  mode <- x$.mode
  on.exit({
    x$.mode <- mode
  })
  x$.mode <- "readwrite"

  # Snapshot the input headers *before* `aligned_array_impl` is resolved: when the
  # result is written in place, `aligned_array_impl` is `x`, and reading the headers
  # afterwards cannot tell the input's own headers apart from the output's.
  input_headers <- x$.header
  input_signatures <- input_headers[startsWith(names(input_headers), "signature_")]

  filebase_new <- normalizePath(.filebase, mustWork = FALSE, winslash = "/")
  filebase_old <- normalizePath(x$.filebase, mustWork = TRUE, winslash = "/")
  if (filebase_new == filebase_old) {
    fresh_created <- FALSE
    aligned_array_impl <- x
  } else {
    fresh_created <- TRUE
    aligned_array_impl <- filearray::filearray_create(
      .filebase,
      dimension = dm,
      type = x$type(),
      partition_size = 1L
    )
  }

  # We apply for each partition, hence trim the last margin
  pdm <- dm[-length(dm)]
  n_iter <- dm[[length(dm)]]

  if (fresh_created || !all(shift_amount == 0)) {
    # shift is needed
    filearray::fmap(
      x = list(x),
      .y = aligned_array_impl,
      .buffer_count = n_iter,
      fun = function(input) {
        slice <- array(input[[1]], pdm)
        ravetools::shift_array(
          x = slice,
          along_margin = time_margin, # shift along time
          shift_amount = shift_amount,
          unit_margin = trial_margin   # per trial
        )
      }
    )
  }

  # Write header
  extra_headers_names <- names(input_headers)
  extra_headers <- input_headers[!extra_headers_names %in% c(names(aligned_array_impl$.header), "dimnames")]
  extra_headers_names <- names(extra_headers)

  # Migrate headers as-is to the new array
  for (nm in extra_headers_names) {
    aligned_array_impl$set_header(nm, extra_headers[[nm]], save = FALSE)
  }

  signature_shift_amount <- ravepipeline::digest(as.integer(shift_amount))
  aligned_array_impl$set_header("signature_shift_amount", signature_shift_amount, save = FALSE)
  aligned_array_impl$set_header("original_time_range", time_range, save = FALSE)
  aligned_array_impl$set_header("time_shift_range", shift_range, save = FALSE)
  aligned_array_impl$set_header("valid_time_range", valid_time_range, save = FALSE)
  aligned_array_impl$set_header("sample_rate", sample_rate, save = FALSE)

  # generate signature; use the snapshot so the result does not depend on whether
  # the array was written in place or into a fresh `.filebase`
  signature <- ravepipeline::digest(c(
    as.list(input_signatures),
    list(
      signature_shift_amount = signature_shift_amount,
      original_time_range = as.double(time_range)
    )
  ))
  aligned_array_impl$set_header("signature", signature, save = FALSE)

  # This will write dimnames and trigger saving headers
  dimnames(aligned_array_impl) <- dnames

  # Set to read-only
  aligned_array_impl$.mode <- "readonly"

  aligned_array_impl

}

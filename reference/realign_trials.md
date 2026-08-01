# Re-align trials to a given event

'RAVE' arrays loaded with a trial epoch are locked to the trial onset.
`realign_trials` time-locks them to another event instead (for example a
button press), shifting each trial along the time margin by
`event time - trial onset`, such that the event occurs at time zero.
Samples exposed at the edges by the shift are filled with `NA`.

## Usage

``` r
realign_trials(
  x,
  event,
  epoch,
  sample_rate,
  time_margin = NA,
  trial_margin = NA,
  strict = TRUE,
  .filebase = tempfile()
)
```

## Arguments

- x:

  an array, a
  [`FileArray-class`](https://dipterix.org/filearray/reference/FileArray-class.html)
  instance, a file-array proxy, or a `RAVEFileArray`; must contain a
  time margin and a trial margin (see `time_margin` and `trial_margin`)

- event:

  character, name of the event to align to; the corresponding epoch
  column is resolved by `epoch$get_event_colname`. Blank string `""`,
  `"default"`, or `"trial onset"` refer to the trial onset, in which
  case no shift is applied. See `epoch$available_events` for the events
  provided by `epoch`.

- epoch:

  a [`RAVEEpoch`](http://rave.wiki/ravecore/reference/RAVEEpoch.md)
  instance providing the trial onsets and the event times

- sample_rate:

  sampling frequency, in 'Hz'; used to convert the time differences into
  integer sample shifts

- time_margin, trial_margin:

  integers, which margins of `x` store the time points and the trial
  numbers; the default `NA` resolves them from `names(dimnames(x))`,
  matching `"Time"` and `"Trial"` case-insensitively. Both must be less
  than `length(dim(x))`: the last margin is reserved so the array can be
  processed in chunks.

- strict:

  whether to be strict about the inputs; when `TRUE`, the default, a
  missing event column, a trial that is absent from the epoch table, or
  a margin that disagrees with `dimnames(x)` raises an error; when
  `FALSE`, these conditions are reported as warnings and the affected
  trials receive no shift

- .filebase:

  where to store the resulting file array; the path is removed first if
  it already exists. Passing the file base of `x` itself rewrites `x` in
  place.

## Value

A
[`FileArray-class`](https://dipterix.org/filearray/reference/FileArray-class.html)
instance with the same dimensions and `dimnames` as `x`, whose samples
are shifted such that `event` occurs at time zero; the samples exposed
at the trial edges are `NA`. The result is read-only, unless it was
written in place, in which case it keeps the mode of `x`.

The time `dimnames` are **not** relabeled: they remain the original
onset-locked time points, now to be read as relative to `event`. The
following headers are attached, and can be read back with `$get_header`:

- `original_time_range`:

  range of the time points, in seconds

- `time_shift_range`:

  range of the applied shifts, in seconds

- `valid_time_range`:

  range of the time points for which no trial is `NA`; useful to crop
  the result before plotting or averaging

- `sample_rate`:

  the `sample_rate` used

- `signature_shift_amount`, `signature`:

  digests identifying the applied shifts and the result

## See also

[`RAVEEpoch`](http://rave.wiki/ravecore/reference/RAVEEpoch.md),
[`shift_array`](https://dipterix.org/ravetools/reference/shift_array.html)

## Examples

``` r

# Please download DemoSubject ~700MB from
# https://github.com/beauchamplab/rave/releases/tag/v0.1.9-beta

if( has_rave_subject("demo/DemoSubject") ) {

repository <- prepare_subject_voltage_with_epochs(
  "demo/DemoSubject", electrodes = 14,
  reference_name = "default", epoch_name = "auditory_onset",
  time_windows = c(-1, 2))

# The demo epoch only contains the trial onset; add a synthetic
# response time, 0.1-0.5 seconds after each onset, to align to
epoch <- repository$epoch
set.seed(1)
epoch$table$Event_response <- epoch$table$Time +
  round(runif(nrow(epoch$table), 0.1, 0.5), 3)
epoch$.columns <- unique(c(epoch$.columns, "Event_response"))

epoch$available_events

# Time x Trial x Electrode
voltage <- repository$voltage$data_list$e_14
dim(voltage)

aligned <- realign_trials(
  voltage, event = "response", epoch = epoch,
  sample_rate = repository$sample_rate)

# Trials are now locked to the response instead of the onset
aligned$get_header("time_shift_range")

# Time points where no trial has been shifted out of range
valid_time_range <- aligned$get_header("valid_time_range")
valid_time_range

# Crop to the valid window before averaging over trials
time_points <- as.numeric(dimnames(aligned)$Time)
keep <- time_points >= valid_time_range[[1]] &
  time_points <= valid_time_range[[2]]

plot(time_points[keep], rowMeans(aligned[keep, , 1]), type = "l",
     xlab = "Time to response (s)", ylab = "Voltage",
     main = "Response-locked average")
abline(v = 0, lty = 2)

# clean up
aligned$.mode <- "readwrite"
aligned$delete(force = TRUE)

}
```

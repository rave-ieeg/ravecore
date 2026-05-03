# 'RAVE' repository: with epochs

'RAVE' repository: with epochs

## Usage

``` r
prepare_subject_with_epochs(
  subject,
  electrodes = NULL,
  reference_name = NULL,
  epoch_name = NULL,
  time_windows = NULL,
  stitch_events = NULL,
  ...,
  quiet = FALSE,
  repository_id = NULL,
  strict = TRUE
)

prepare_subject_raw_voltage_with_epochs(
  subject,
  electrodes = NULL,
  epoch_name = NULL,
  time_windows = NULL,
  stitch_events = NULL,
  ...,
  quiet = TRUE,
  repository_id = NULL,
  strict = TRUE
)

prepare_subject_voltage_with_epochs(
  subject,
  electrodes = NULL,
  reference_name = NULL,
  epoch_name = NULL,
  time_windows = NULL,
  stitch_events = NULL,
  ...,
  quiet = FALSE,
  repository_id = NULL,
  strict = TRUE
)

prepare_subject_time_frequency_coefficients_with_epochs(
  subject,
  electrodes = NULL,
  reference_name = NULL,
  epoch_name = NULL,
  time_windows = NULL,
  stitch_events = NULL,
  ...,
  quiet = FALSE,
  repository_id = NULL,
  strict = TRUE
)

prepare_subject_power_with_epochs(
  subject,
  electrodes = NULL,
  reference_name = NULL,
  epoch_name = NULL,
  time_windows = NULL,
  stitch_events = NULL,
  ...,
  quiet = FALSE,
  repository_id = NULL,
  strict = TRUE
)

prepare_subject_power(
  subject,
  electrodes = NULL,
  reference_name = NULL,
  epoch_name = NULL,
  time_windows = NULL,
  stitch_events = NULL,
  ...,
  quiet = FALSE,
  repository_id = NULL,
  strict = TRUE
)

prepare_subject_phase_with_epochs(
  subject,
  electrodes = NULL,
  reference_name = NULL,
  epoch_name = NULL,
  time_windows = NULL,
  stitch_events = NULL,
  ...,
  quiet = FALSE,
  repository_id = NULL,
  strict = TRUE
)
```

## Arguments

- subject:

  'RAVE' subject

- electrodes:

  string or integers indicating electrodes to load

- reference_name:

  name of the reference table

- epoch_name:

  name of the epoch trial table

- time_windows:

  numeric vector with even lengths, the time start and end of the
  trials, for example, `c(-1, 2)` means load 1 second before the trial
  onset and 2 seconds after trial onset

- stitch_events:

  events where the `time_windows` is based; default is trial onset
  (`NULL`)

- ...:

  passed to
  [`RAVESubjectBaseRepository`](http://rave.wiki/ravecore/reference/RAVESubjectBaseRepository.md)
  constructor

- quiet:

  see field `quiet`

- repository_id:

  see field `repository_id`

- strict:

  whether to check existence of subject before loading data; default is
  true

## Value

A
[`RAVESubjectEpochRepository`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.md)
instance

## Examples

``` r

if ( has_rave_subject("demo/DemoSubject") ) {


repository <- prepare_subject_with_epochs(
  "demo/DemoSubject", electrodes = 14:16,
  reference_name = "default", epoch_name = "auditory_onset",
  time_windows = c(-1, 2))

print(repository)

head(repository$epoch$table)

electrodes <- repository$electrode_instances

# Channel 14
e <- electrodes$e_14

# referenced voltage
voltage <- e$load_data_with_epochs("voltage")

# 6001 time points (2000 sample rate)
# 287 trials
# 1 channel
dim(voltage)

ravetools::plot_signals(t(voltage[, 1:10, 1]),
                        sample_rate = 2000,
                        ylab = "Trial",
                        main = "First 10 trials")

}
```

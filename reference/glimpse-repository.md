# Visualizes repositories with interactive plots

Requires optional package `'plotly'`; please install the package prior
to launching the viewer.

## Usage

``` r
glimpse_voltage_repository_with_blocks(
  repository,
  initial_block = NULL,
  channels = NULL,
  epoch = NULL,
  start_time = 0,
  duration = 5,
  channel_gap = 1000,
  highpass_freq = NA,
  lowpass_freq = NA
)

glimpse_voltage_filearray(
  filearray,
  sample_rate,
  channels = NULL,
  epoch = NULL,
  start_time = 0,
  duration = 5,
  channel_gap = 1000,
  highpass_freq = NA,
  lowpass_freq = NA
)
```

## Arguments

- repository:

  'RAVE' repository

- initial_block:

  initial recording block to select

- channels:

  channels to visualize; default is all

- epoch:

  additional epoch to annotation

- start_time, duration, channel_gap:

  initial start time, duration, and channel gap (can be changed later)

- highpass_freq, lowpass_freq:

  filter to apply when visualizing the signals, useful when signals have
  'DC' shift

- filearray:

  a
  [`as_filearray`](https://dipterix.org/filearray/reference/filearray.html)
  object, must be two dimensional matrix for voltage (time by
  electrode), with [`dimnames`](https://rdrr.io/r/base/dimnames.html)
  being the time in seconds and electrode in label name

- sample_rate:

  sample rate of the file-array

## Value

An R-shiny application container environment; use
[`print`](https://rdrr.io/r/base/print.html) method to launch the
application.

## Examples

``` r

if(has_rave_subject("demo/DemoSubject")) {
  subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)


  repository <- ravecore::prepare_subject_voltage_with_blocks(
    subject = subject)

  if (interactive()) {
    app <- glimpse_voltage_repository_with_blocks(
      repository = repository,
      initial_block = "008",
      epoch = "auditory_onset",
      highpass_freq = 0.5
    )

    print(app)
    close(app)

  }

}

# ---- Example 2 ---------------------------------------------------

# Construct a filearray


sample_rate <- 100
filearray <- filearray::as_filearray(array(rnorm(50000),
                                           dim = c(10000, 5)))

dimnames(filearray) <- list(
  Time = seq_len(10000) / sample_rate,
  Electrode = 1:5
)

if(interactive()) {

  app <- glimpse_voltage_filearray(filearray = filearray,
                                   sample_rate = sample_rate,
                                   channel_gap = 6)

  print(app)
}

```

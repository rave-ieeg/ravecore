# 'RAVE' class for epoch repository - time-frequency

The repository inherits `link{RAVESubjectEpochTimeFreqBaseRepository}`,
with epoch trials, and is intended for loading processed and referenced
time-frequency coefficients. Use
[`prepare_subject_time_frequency_coefficients_with_epochs`](http://rave.wiki/ravecore/reference/prepare_subject_with_epochs.md)
to create an instance.

## Super classes

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `RAVESubjectRepository` -\>
[`RAVESubjectEpochRepository`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.md)
-\>
[`RAVESubjectEpochTimeFreqBaseRepository`](http://rave.wiki/ravecore/reference/RAVESubjectEpochTimeFreqBaseRepository.md)
-\> `RAVESubjectEpochTimeFreqCoefRepository`

## Active bindings

- `coefficients`:

  a named map of time-frequency coefficient data, mounted by
  `mount_data`

- `wavelet`:

  not used anymore, see `coefficients`

## Methods

### Public methods

- [`RAVESubjectEpochTimeFreqCoefRepository$@marshal()`](#method-RAVESubjectEpochTimeFreqCoefRepository-@marshal)

- [`RAVESubjectEpochTimeFreqCoefRepository$@unmarshal()`](#method-RAVESubjectEpochTimeFreqCoefRepository-@unmarshal)

- [`RAVESubjectEpochTimeFreqCoefRepository$new()`](#method-RAVESubjectEpochTimeFreqCoefRepository-initialize)

- [`RAVESubjectEpochTimeFreqCoefRepository$clone()`](#method-RAVESubjectEpochTimeFreqCoefRepository-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)
- `RAVESubjectRepository$@get_container()`
- `RAVESubjectRepository$print()`
- [`RAVESubjectEpochRepository$export_matlab()`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.html#method-export_matlab)
- [`RAVESubjectEpochRepository$get_container()`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.html#method-get_container)
- [`RAVESubjectEpochRepository$set_epoch()`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.html#method-set_epoch)
- [`RAVESubjectEpochTimeFreqBaseRepository$mount_data()`](http://rave.wiki/ravecore/reference/RAVESubjectEpochTimeFreqBaseRepository.html#method-mount_data)

------------------------------------------------------------------------

### `RAVESubjectEpochTimeFreqCoefRepository$@marshal()`

Internal method

#### Usage

    RAVESubjectEpochTimeFreqCoefRepository$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### `RAVESubjectEpochTimeFreqCoefRepository$@unmarshal()`

Internal method

#### Usage

    RAVESubjectEpochTimeFreqCoefRepository$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### `RAVESubjectEpochTimeFreqCoefRepository$new()`

constructor

#### Usage

    RAVESubjectEpochTimeFreqCoefRepository$new(
      subject,
      electrodes = NULL,
      reference_name = NULL,
      epoch_name = NULL,
      time_windows = NULL,
      stitch_events = NULL,
      ...,
      quiet = FALSE,
      repository_id = NULL,
      strict = TRUE,
      lazy_load = FALSE,
      .class = NULL
    )

#### Arguments

- `subject`:

  'RAVE' subject

- `electrodes`:

  string or integers indicating electrodes to load

- `reference_name`:

  name of the reference table

- `epoch_name`:

  name of the epoch trial table

- `time_windows`:

  numeric vector with even lengths, the time start and end of the
  trials, for example, `c(-1, 2)` means load 1 second before the trial
  onset and 2 seconds after trial onset

- `stitch_events`:

  events where the `time_windows` is based; default is trial onset
  (`NULL`)

- `...`:

  passed to
  [`RAVESubjectEpochTimeFreqBaseRepository`](http://rave.wiki/ravecore/reference/RAVESubjectEpochTimeFreqBaseRepository.md)
  constructor

- `quiet`:

  see field `quiet`

- `repository_id`:

  see field `repository_id`

- `strict`:

  whether the mode should be strict; default is true and errors out when
  subject is missing

- `lazy_load`:

  whether to delay `mount_data`; default is false

- `.class`:

  internally used, do not set, even if you know what this is

------------------------------------------------------------------------

### `RAVESubjectEpochTimeFreqCoefRepository$clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVESubjectEpochTimeFreqCoefRepository$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

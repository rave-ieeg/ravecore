# 'RAVE' class for epoch repository - time-frequency (internal)

The repository inherits `link{RAVESubjectEpochRepository}`, with epoch
trials, and is intended for loading processed and referenced
time-frequency coefficients.

## Super classes

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `ravecore::RAVESubjectRepository` -\>
[`ravecore::RAVESubjectEpochRepository`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.md)
-\> `RAVESubjectEpochTimeFreqBaseRepository`

## Active bindings

- `sample_rate`:

  time-frequency coefficient sample rate

- `frequency`:

  frequencies where the time-frequency coefficients are evaluated

- `time`:

  time in seconds for each trial

- `time_points`:

  see `time` field, existed for backward compatibility

- `signal_type`:

  do not use

- `digest_key`:

  a list of repository data used to generate repository signature

## Methods

### Public methods

- [`RAVESubjectEpochTimeFreqBaseRepository$@marshal()`](#method-RAVESubjectEpochTimeFreqBaseRepository-@marshal)

- [`RAVESubjectEpochTimeFreqBaseRepository$@unmarshal()`](#method-RAVESubjectEpochTimeFreqBaseRepository-@unmarshal)

- [`RAVESubjectEpochTimeFreqBaseRepository$new()`](#method-RAVESubjectEpochTimeFreqBaseRepository-new)

- [`RAVESubjectEpochTimeFreqBaseRepository$mount_data()`](#method-RAVESubjectEpochTimeFreqBaseRepository-mount_data)

- [`RAVESubjectEpochTimeFreqBaseRepository$clone()`](#method-RAVESubjectEpochTimeFreqBaseRepository-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)
- [`ravecore::RAVESubjectRepository$@get_container()`](http://rave.wiki/ravecore/reference/RAVESubjectRepository.html#method-@get_container)
- [`ravecore::RAVESubjectRepository$print()`](http://rave.wiki/ravecore/reference/RAVESubjectRepository.html#method-print)
- [`ravecore::RAVESubjectEpochRepository$export_matlab()`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.html#method-export_matlab)
- [`ravecore::RAVESubjectEpochRepository$get_container()`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.html#method-get_container)
- [`ravecore::RAVESubjectEpochRepository$set_epoch()`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.html#method-set_epoch)

------------------------------------------------------------------------

### Method `@marshal()`

Internal method

#### Usage

    RAVESubjectEpochTimeFreqBaseRepository$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### Method `@unmarshal()`

Internal method

#### Usage

    RAVESubjectEpochTimeFreqBaseRepository$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVESubjectEpochTimeFreqBaseRepository$new(
      subject,
      electrodes = NULL,
      reference_name = NULL,
      epoch_name = NULL,
      time_windows = NULL,
      stitch_events = NULL,
      data_type = NULL,
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

- `data_type`:

  for child classes to fill; data type (power, phase, or complex
  time-frequency coefficients)

- `...`:

  passed to
  [`RAVESubjectEpochRepository`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.md)
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

### Method `mount_data()`

function to mount processed and referenced spectrogram

#### Usage

    RAVESubjectEpochTimeFreqBaseRepository$mount_data(
      ...,
      force = TRUE,
      electrodes = NULL
    )

#### Arguments

- `...`:

  reserved

- `force`:

  force update data; default is true

- `electrodes`:

  electrodes to update for expert-use use; default is `NULL` (all
  electrode channels will be mounted)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVESubjectEpochTimeFreqBaseRepository$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

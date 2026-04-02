# 'RAVE' class for epoch repository - voltage

The repository inherits `link{RAVESubjectEpochRepository}`, with epoch
trials, and is intended for loading processed and referenced voltage
signals. Use
[`prepare_subject_voltage_with_epochs`](http://rave.wiki/ravecore/reference/prepare_subject_with_epochs.md)
to create an instance.

## Super classes

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `ravecore::RAVESubjectRepository` -\>
[`ravecore::RAVESubjectEpochRepository`](http://rave.wiki/ravecore/reference/RAVESubjectEpochRepository.md)
-\> `RAVESubjectEpochVoltageRepository`

## Active bindings

- `digest_key`:

  a list of repository data used to generate repository signature

- `voltage`:

  a named map of voltage data, mounted by `mount_data`

## Methods

### Public methods

- [`RAVESubjectEpochVoltageRepository$@marshal()`](#method-RAVESubjectEpochVoltageRepository-@marshal)

- [`RAVESubjectEpochVoltageRepository$@unmarshal()`](#method-RAVESubjectEpochVoltageRepository-@unmarshal)

- [`RAVESubjectEpochVoltageRepository$new()`](#method-RAVESubjectEpochVoltageRepository-new)

- [`RAVESubjectEpochVoltageRepository$mount_data()`](#method-RAVESubjectEpochVoltageRepository-mount_data)

- [`RAVESubjectEpochVoltageRepository$clone()`](#method-RAVESubjectEpochVoltageRepository-clone)

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

    RAVESubjectEpochVoltageRepository$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### Method `@unmarshal()`

Internal method

#### Usage

    RAVESubjectEpochVoltageRepository$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVESubjectEpochVoltageRepository$new(
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

function to mount referenced voltage signals

#### Usage

    RAVESubjectEpochVoltageRepository$mount_data(
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

    RAVESubjectEpochVoltageRepository$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

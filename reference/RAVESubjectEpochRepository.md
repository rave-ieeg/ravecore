# 'RAVE' class for epoch repository

Compared to
[`RAVESubjectBaseRepository`](http://rave.wiki/ravecore/reference/RAVESubjectBaseRepository.md),
this repository requires epoch information. please use
[`prepare_subject_with_epochs`](http://rave.wiki/ravecore/reference/prepare_subject_with_epochs.md)
to instantiate this repository.

## Value

The root directory where the files are stored.

A named map, typically with data arrays, shape/dimension information

## See also

[`prepare_subject_with_epochs`](http://rave.wiki/ravecore/reference/prepare_subject_with_epochs.md)

## Super classes

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `ravecore::RAVESubjectRepository` -\> `RAVESubjectEpochRepository`

## Active bindings

- `needs_update`:

  write-only attribute when subject needs to be reloaded from the disk
  and reference table needs to be updated, use
  `repo$needs_update <- TRUE`

- `meta_info`:

  list of meta information

- `sample_rates`:

  a named list of sampling frequencies; the names are signal types
  (`'LFP'`, `'Auxiliary'`, or `'Spike'`) and the values are the sampling
  frequencies

- `sample_rate`:

  a single number of the sample rate; if the electrode channels contain
  local-field potential `'LFP'` signal type, then the sample rate is the
  `'LFP'` sample rate; otherwise the sample rate is `'Spike'` channel
  sample rate, if exists, or whatever comes first. This field is for
  backward compatibility support, use `sample_rates` for more accurate
  number

- `epoch_name`:

  name of the epoch table

- `epoch`:

  [`RAVEEpoch`](http://rave.wiki/ravecore/reference/RAVEEpoch.md)
  instance

- `epoch_table`:

  epoch table, equivalent to `repository$epoch$table`

- `stitch_events`:

  events where `time_windows` are based on

- `time_windows`:

  list of time ranges to load; the time is relative to `stitch_events`;
  default is trial onset

- `electrode_table`:

  the entire electrode table with reference information

- `electrode_instances`:

  electrode channel instance helpers for loading electrode data

- `reference_instances`:

  instances of reference channels, for referencing on the fly, used for
  `electrode_instances`

- `digest_key`:

  a list of repository data used to generate repository signature

## Methods

### Public methods

- [`RAVESubjectEpochRepository$@marshal()`](#method-RAVESubjectEpochRepository-@marshal)

- [`RAVESubjectEpochRepository$@unmarshal()`](#method-RAVESubjectEpochRepository-@unmarshal)

- [`RAVESubjectEpochRepository$new()`](#method-RAVESubjectEpochRepository-new)

- [`RAVESubjectEpochRepository$export_matlab()`](#method-RAVESubjectEpochRepository-export_matlab)

- [`RAVESubjectEpochRepository$set_epoch()`](#method-RAVESubjectEpochRepository-set_epoch)

- [`RAVESubjectEpochRepository$mount_data()`](#method-RAVESubjectEpochRepository-mount_data)

- [`RAVESubjectEpochRepository$get_container()`](#method-RAVESubjectEpochRepository-get_container)

- [`RAVESubjectEpochRepository$clone()`](#method-RAVESubjectEpochRepository-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)
- [`ravecore::RAVESubjectRepository$@get_container()`](http://rave.wiki/ravecore/reference/RAVESubjectRepository.html#method-@get_container)
- [`ravecore::RAVESubjectRepository$print()`](http://rave.wiki/ravecore/reference/RAVESubjectRepository.html#method-print)

------------------------------------------------------------------------

### Method `@marshal()`

Internal method

#### Usage

    RAVESubjectEpochRepository$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### Method `@unmarshal()`

Internal method

#### Usage

    RAVESubjectEpochRepository$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVESubjectEpochRepository$new(
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
  [`RAVESubjectBaseRepository`](http://rave.wiki/ravecore/reference/RAVESubjectBaseRepository.md)
  constructor

- `quiet`:

  see field `quiet`

- `repository_id`:

  see field `repository_id`

- `strict`:

  whether the mode should be strict; default is true and errors out when
  subject is missing

- `lazy_load`:

  whether to delay (lazy) the evaluation `mount_data`

- `.class`:

  internally used, do not set, even if you know what this is

------------------------------------------------------------------------

### Method `export_matlab()`

Export the repository to 'Matlab' for future analysis

#### Usage

    RAVESubjectEpochRepository$export_matlab(..., verbose = TRUE)

#### Arguments

- `...`:

  reserved for child classes

- `verbose`:

  print progresses

------------------------------------------------------------------------

### Method `set_epoch()`

change trial epoch profiles

#### Usage

    RAVESubjectEpochRepository$set_epoch(epoch_name, stitch_events = NULL)

#### Arguments

- `epoch_name`:

  name of epoch table

- `stitch_events`:

  events to stitch

------------------------------------------------------------------------

### Method `mount_data()`

function to mount data, not doing anything in this class, but may be
used by child classes

#### Usage

    RAVESubjectEpochRepository$mount_data(..., force = TRUE, electrodes = NULL)

#### Arguments

- `...`:

  reserved

- `force`:

  force update data; default is true

- `electrodes`:

  electrodes to update; default is `NULL` (all electrode channels)

------------------------------------------------------------------------

### Method `get_container()`

get container where loaded data are stored

#### Usage

    RAVESubjectEpochRepository$get_container(force = FALSE, ...)

#### Arguments

- `force, ...`:

  passed to `mount_data`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVESubjectEpochRepository$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

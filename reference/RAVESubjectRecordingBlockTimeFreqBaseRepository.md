# 'RAVE' class for loading entire block of time-frequency coefficients

'RAVE' class for loading entire block of time-frequency coefficients

'RAVE' class for loading entire block of time-frequency coefficients

## See also

[`prepare_subject_with_blocks`](http://rave.wiki/ravecore/reference/prepare_subject_with_blocks.md)

## Super classes

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `ravecore::RAVESubjectRepository` -\>
[`ravecore::RAVESubjectRecordingBlockRepository`](http://rave.wiki/ravecore/reference/RAVESubjectRecordingBlockRepository.md)
-\> `RAVESubjectRecordingBlockTimeFreqBaseRepository`

## Active bindings

- `sample_rates`:

  a named list of sampling frequencies; the names are signal types
  (`'LFP'`, `'Auxiliary'`, or `'Spike'`) and the values are the sampling
  frequencies

- `sample_rate`:

  numeric sample rate for 'spectrogram'

## Methods

### Public methods

- [`RAVESubjectRecordingBlockTimeFreqBaseRepository$@marshal()`](#method-RAVESubjectRecordingBlockTimeFreqBaseRepository-@marshal)

- [`RAVESubjectRecordingBlockTimeFreqBaseRepository$@unmarshal()`](#method-RAVESubjectRecordingBlockTimeFreqBaseRepository-@unmarshal)

- [`RAVESubjectRecordingBlockTimeFreqBaseRepository$new()`](#method-RAVESubjectRecordingBlockTimeFreqBaseRepository-new)

- [`RAVESubjectRecordingBlockTimeFreqBaseRepository$mount_data()`](#method-RAVESubjectRecordingBlockTimeFreqBaseRepository-mount_data)

- [`RAVESubjectRecordingBlockTimeFreqBaseRepository$clone()`](#method-RAVESubjectRecordingBlockTimeFreqBaseRepository-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)
- [`ravecore::RAVESubjectRepository$@get_container()`](http://rave.wiki/ravecore/reference/RAVESubjectRepository.html#method-@get_container)
- [`ravecore::RAVESubjectRepository$print()`](http://rave.wiki/ravecore/reference/RAVESubjectRepository.html#method-print)
- [`ravecore::RAVESubjectRecordingBlockRepository$export_matlab()`](http://rave.wiki/ravecore/reference/RAVESubjectRecordingBlockRepository.html#method-export_matlab)
- [`ravecore::RAVESubjectRecordingBlockRepository$get_container()`](http://rave.wiki/ravecore/reference/RAVESubjectRecordingBlockRepository.html#method-get_container)

------------------------------------------------------------------------

### Method `@marshal()`

Internal method

#### Usage

    RAVESubjectRecordingBlockTimeFreqBaseRepository$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### Method `@unmarshal()`

Internal method

#### Usage

    RAVESubjectRecordingBlockTimeFreqBaseRepository$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVESubjectRecordingBlockTimeFreqBaseRepository$new(
      subject,
      electrodes = NULL,
      reference_name = NULL,
      blocks = NULL,
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

- `blocks`:

  name of the recording blocks to load

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

### Method `mount_data()`

function to mount data

#### Usage

    RAVESubjectRecordingBlockTimeFreqBaseRepository$mount_data(
      ...,
      force = TRUE,
      electrodes = NULL
    )

#### Arguments

- `...`:

  reserved

- `force`:

  force update data; default is true; set to false to use cache

- `electrodes`:

  electrodes to update; default is `NULL` (all electrode channels)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVESubjectRecordingBlockTimeFreqBaseRepository$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

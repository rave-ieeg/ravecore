# 'RAVE' class for loading time-frequency phase components

Loads time-frequency phase

## See also

[`prepare_subject_phase_with_blocks`](http://rave.wiki/ravecore/reference/prepare_subject_with_blocks.md)

## Super classes

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `RAVESubjectRepository` -\>
[`RAVESubjectRecordingBlockRepository`](http://rave.wiki/ravecore/reference/RAVESubjectRecordingBlockRepository.md)
-\>
[`RAVESubjectRecordingBlockTimeFreqBaseRepository`](http://rave.wiki/ravecore/reference/RAVESubjectRecordingBlockTimeFreqBaseRepository.md)
-\> `RAVESubjectRecordingBlockPhaseRepository`

## Active bindings

- `phase`:

  data container, alias of `get_container`

## Methods

### Public methods

- [`RAVESubjectRecordingBlockPhaseRepository$@marshal()`](#method-RAVESubjectRecordingBlockPhaseRepository-@marshal)

- [`RAVESubjectRecordingBlockPhaseRepository$@unmarshal()`](#method-RAVESubjectRecordingBlockPhaseRepository-@unmarshal)

- [`RAVESubjectRecordingBlockPhaseRepository$new()`](#method-RAVESubjectRecordingBlockPhaseRepository-initialize)

- [`RAVESubjectRecordingBlockPhaseRepository$clone()`](#method-RAVESubjectRecordingBlockPhaseRepository-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)
- `RAVESubjectRepository$@get_container()`
- `RAVESubjectRepository$print()`
- [`RAVESubjectRecordingBlockRepository$export_matlab()`](http://rave.wiki/ravecore/reference/RAVESubjectRecordingBlockRepository.html#method-export_matlab)
- [`RAVESubjectRecordingBlockRepository$get_container()`](http://rave.wiki/ravecore/reference/RAVESubjectRecordingBlockRepository.html#method-get_container)
- [`RAVESubjectRecordingBlockTimeFreqBaseRepository$mount_data()`](http://rave.wiki/ravecore/reference/RAVESubjectRecordingBlockTimeFreqBaseRepository.html#method-mount_data)

------------------------------------------------------------------------

### `RAVESubjectRecordingBlockPhaseRepository$@marshal()`

Internal method

#### Usage

    RAVESubjectRecordingBlockPhaseRepository$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### `RAVESubjectRecordingBlockPhaseRepository$@unmarshal()`

Internal method

#### Usage

    RAVESubjectRecordingBlockPhaseRepository$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### `RAVESubjectRecordingBlockPhaseRepository$new()`

constructor

#### Usage

    RAVESubjectRecordingBlockPhaseRepository$new(
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

### `RAVESubjectRecordingBlockPhaseRepository$clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVESubjectRecordingBlockPhaseRepository$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

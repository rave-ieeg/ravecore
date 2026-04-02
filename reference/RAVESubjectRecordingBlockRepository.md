# 'RAVE' class for loading entire recording block repository

Compared to
[`RAVESubjectBaseRepository`](http://rave.wiki/ravecore/reference/RAVESubjectBaseRepository.md),
this repository requires specifying block information. please use
[`prepare_subject_with_blocks`](http://rave.wiki/ravecore/reference/prepare_subject_with_blocks.md)
to instantiate this repository.

## Value

The root directory where the files are stored.

A named map, typically with data arrays, shape/dimension information

## See also

[`prepare_subject_with_blocks`](http://rave.wiki/ravecore/reference/prepare_subject_with_blocks.md)

## Super classes

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `ravecore::RAVESubjectRepository` -\>
`RAVESubjectRecordingBlockRepository`

## Active bindings

- `meta_info`:

  list of meta information

- `needs_update`:

  write-only attribute when subject needs to be reloaded from the disk
  and reference table needs to be updated, use
  `repo$needs_update <- TRUE`

- `blocks`:

  names of recording blocks

- `electrode_table`:

  the entire electrode table with reference information

- `digest_key`:

  a list of repository data used to generate repository signature

## Methods

### Public methods

- [`RAVESubjectRecordingBlockRepository$@marshal()`](#method-RAVESubjectRecordingBlockRepository-@marshal)

- [`RAVESubjectRecordingBlockRepository$@unmarshal()`](#method-RAVESubjectRecordingBlockRepository-@unmarshal)

- [`RAVESubjectRecordingBlockRepository$new()`](#method-RAVESubjectRecordingBlockRepository-new)

- [`RAVESubjectRecordingBlockRepository$export_matlab()`](#method-RAVESubjectRecordingBlockRepository-export_matlab)

- [`RAVESubjectRecordingBlockRepository$get_container()`](#method-RAVESubjectRecordingBlockRepository-get_container)

- [`RAVESubjectRecordingBlockRepository$clone()`](#method-RAVESubjectRecordingBlockRepository-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)
- [`ravecore::RAVESubjectRepository$@get_container()`](http://rave.wiki/ravecore/reference/RAVESubjectRepository.html#method-@get_container)
- [`ravecore::RAVESubjectRepository$print()`](http://rave.wiki/ravecore/reference/RAVESubjectRepository.html#method-print)

------------------------------------------------------------------------

### Method `@marshal()`

Internal method

#### Usage

    RAVESubjectRecordingBlockRepository$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### Method `@unmarshal()`

Internal method

#### Usage

    RAVESubjectRecordingBlockRepository$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVESubjectRecordingBlockRepository$new(
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

### Method `export_matlab()`

Export the repository to 'Matlab' for future analysis

#### Usage

    RAVESubjectRecordingBlockRepository$export_matlab(..., verbose = TRUE)

#### Arguments

- `...`:

  reserved for child classes

- `verbose`:

  print progresses

------------------------------------------------------------------------

### Method `get_container()`

get container where loaded data are stored

#### Usage

    RAVESubjectRecordingBlockRepository$get_container(force = FALSE, ...)

#### Arguments

- `force, ...`:

  passed to `mount_data`

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVESubjectRecordingBlockRepository$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

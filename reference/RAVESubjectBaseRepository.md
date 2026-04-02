# 'RAVE' class for base repository

The class is for creating child classes, to instantiate the class,
please use
[`prepare_subject_bare0`](http://rave.wiki/ravecore/reference/prepare_subject_bare.md)
to create base repository.

## Value

The root directory where the files are stored.

## See also

[`prepare_subject_bare0`](http://rave.wiki/ravecore/reference/prepare_subject_bare.md)

## Super class

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `RAVESubjectRepository`

## Public fields

- `@restored`:

  internal flag indicating whether the repository is restored from
  serialization. Repositories restored from serialization will behave
  differently (slightly) for performance considerations

- `repository_id`:

  repository identifier, typically generated with random string

- `quiet`:

  whether to suppress update warning messages, when requested electrodes
  are not fully processed or excluded

## Active bindings

- `auto_exclude`:

  whether to automatically discard channels that are marked as
  "excluded" (such as bad channels or channels that should not be
  analyzed); default is often true

- `meta_info`:

  list of meta information

- `needs_update`:

  write-only attribute when subject needs to be reloaded from the disk
  and reference table needs to be updated, use
  `repo$needs_update <- TRUE`

- `project`:

  project instance, see
  [`RAVEProject`](http://rave.wiki/ravecore/reference/RAVEProject.md)

- `subject`:

  subject instance, see
  [`RAVESubject`](http://rave.wiki/ravecore/reference/RAVESubject.md)

- `electrode_list`:

  integer vector of electrodes included

- `electrode_table`:

  the entire electrode table

- `electrode_signal_types`:

  more accurate name should be "channel" signal types: currently returns
  `'LFP'`, `'Auxiliary'`, or `'Spike'`, for each channel

- `electrode_instances`:

  electrode channel instance helpers for loading electrode data

- `reference_name`:

  name of reference table

- `reference_table`:

  reference table

- `references_list`:

  a vector of reference channel names, used together with
  `reference_instances`

- `reference_instances`:

  instances of reference channels, for referencing on the fly, used for
  `electrode_instances`

- `digest_key`:

  a list of repository data used to generate repository signature

- `signature`:

  signature of the repository, two repositories might share the same
  signature if their contents are the same (even with different
  identifiers); generated from `digest_key`

## Methods

### Public methods

- [`RAVESubjectBaseRepository$@get_container()`](#method-RAVESubjectRepository-@get_container)

- [`RAVESubjectBaseRepository$@marshal()`](#method-RAVESubjectRepository-@marshal)

- [`RAVESubjectBaseRepository$@unmarshal()`](#method-RAVESubjectRepository-@unmarshal)

- [`RAVESubjectBaseRepository$print()`](#method-RAVESubjectRepository-print)

- [`RAVESubjectBaseRepository$new()`](#method-RAVESubjectRepository-new)

- [`RAVESubjectBaseRepository$export_matlab()`](#method-RAVESubjectRepository-export_matlab)

- [`RAVESubjectBaseRepository$clone()`](#method-RAVESubjectRepository-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)

------------------------------------------------------------------------

### Method `@get_container()`

Internal method, do not use it directly

#### Usage

    RAVESubjectBaseRepository$@get_container()

------------------------------------------------------------------------

### Method `@marshal()`

Internal method

#### Usage

    RAVESubjectBaseRepository$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### Method `@unmarshal()`

Internal method

#### Usage

    RAVESubjectBaseRepository$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

User-friendly print method

#### Usage

    RAVESubjectBaseRepository$print()

------------------------------------------------------------------------

### Method `new()`

constructor

#### Usage

    RAVESubjectBaseRepository$new(
      subject,
      electrodes = NULL,
      reference_name = NULL,
      ...,
      auto_exclude = TRUE,
      quiet = TRUE,
      repository_id = NULL,
      strict = TRUE,
      .class = NULL
    )

#### Arguments

- `subject`:

  'RAVE' subject

- `electrodes`:

  string or integers indicating electrodes to load

- `reference_name`:

  name of the reference table

- `...`:

  reserved, currently ignored

- `auto_exclude`:

  whether to automatically discard bad channels

- `quiet`:

  see field `quiet`

- `repository_id`:

  see field `repository_id`

- `strict`:

  whether the mode should be strict; default is true and errors out when
  subject is missing

- `.class`:

  internally used, do not set, even if you know what this is

------------------------------------------------------------------------

### Method `export_matlab()`

Export the repository to 'Matlab' for future analysis

#### Usage

    RAVESubjectBaseRepository$export_matlab(..., verbose = TRUE)

#### Arguments

- `...`:

  reserved for child classes

- `verbose`:

  print progresses

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVESubjectBaseRepository$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

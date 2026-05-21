# Definition for 'RAVE' project class

See
[`as_rave_project`](http://rave.wiki/ravecore/reference/as_rave_project.md)
for creating 'RAVE' project class

## Super class

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `RAVEProject`

## Active bindings

- `path`:

  project folder, absolute path

- `name`:

  project name, character

- `pipeline_path`:

  path to pipeline scripts under project's folder

- `format_standard`:

  storage format, can be either `'native'` or `'bids'`-compliant

- `@impl`:

  the internal object

## Methods

### Public methods

- [`RAVEProject$@marshal()`](#method-RAVEProject-@marshal)

- [`RAVEProject$@unmarshal()`](#method-RAVEProject-@unmarshal)

- [`RAVEProject$print()`](#method-RAVEProject-print)

- [`RAVEProject$format()`](#method-RAVEProject-format)

- [`RAVEProject$new()`](#method-RAVEProject-initialize)

- [`RAVEProject$subjects()`](#method-RAVEProject-subjects)

- [`RAVEProject$has_subject()`](#method-RAVEProject-has_subject)

- [`RAVEProject$group_path()`](#method-RAVEProject-group_path)

- [`RAVEProject$subject_pipelines()`](#method-RAVEProject-subject_pipelines)

- [`RAVEProject$clone()`](#method-RAVEProject-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)

------------------------------------------------------------------------

### `RAVEProject$@marshal()`

Internal method

#### Usage

    RAVEProject$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### `RAVEProject$@unmarshal()`

Internal method

#### Usage

    RAVEProject$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### `RAVEProject$print()`

override print method

#### Usage

    RAVEProject$print(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### `RAVEProject$format()`

override format method

#### Usage

    RAVEProject$format(...)

#### Arguments

- `...`:

  ignored

------------------------------------------------------------------------

### `RAVEProject$new()`

constructor

#### Usage

    RAVEProject$new(project_name, strict = TRUE, parent_path = NULL)

#### Arguments

- `project_name`:

  character

- `strict`:

  whether to check project path

- `parent_path`:

  `NULL`, a path to the project parent folder for native projects, or
  the path to 'BIDS' root directory.

------------------------------------------------------------------------

### `RAVEProject$subjects()`

get all imported subjects within project

#### Usage

    RAVEProject$subjects()

#### Returns

character vector

------------------------------------------------------------------------

### `RAVEProject$has_subject()`

whether a specific subject exists in this project

#### Usage

    RAVEProject$has_subject(subject_code)

#### Arguments

- `subject_code`:

  character, subject name

#### Returns

true or false whether subject is in the project

------------------------------------------------------------------------

### `RAVEProject$group_path()`

get group data path for 'RAVE' module

#### Usage

    RAVEProject$group_path(module_id, must_work = FALSE)

#### Arguments

- `module_id`:

  character, 'RAVE' module ID

- `must_work`:

  whether the directory must exist; if not exists, should a new one be
  created?

------------------------------------------------------------------------

### `RAVEProject$subject_pipelines()`

list saved pipelines

#### Usage

    RAVEProject$subject_pipelines(
      pipeline_name,
      cache = FALSE,
      check = TRUE,
      all = FALSE
    )

#### Arguments

- `pipeline_name`:

  name of the pipeline

- `cache`:

  whether to use cached registry

- `check`:

  whether to check if the pipelines exist as directories

- `all`:

  whether to list all pipelines; default is false; pipelines with the
  same label but older time-stamps will be hidden

#### Returns

A data table of pipeline time-stamps and directories

------------------------------------------------------------------------

### `RAVEProject$clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVEProject$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

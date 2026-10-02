# Definition for epoch class

Trial epoch, contains the following information: `Block` experiment
block/session string; `Time` trial onset within that block; `Trial`
trial number; `Condition` trial condition. Other optional columns are
`Event_xxx` (starts with "Event"). Column `ExcludedHint` (logical,
always present; missing or blank values are `FALSE`) marks trials that
analyses may exclude; see `exclude_trials` and `save`. An epoch whose
name ends with `"_OutlierRemoved"` is a trimmed copy: marked trials are
removed, trials are renumbered from 1, and `OriginalTrial` holds the
trial numbers of the epoch it was generated from.

## Super class

[`ravepipeline::RAVESerializable`](http://dipterix.org/ravepipeline/reference/RAVESerializable.md)
-\> `RAVEEpoch`

## Public fields

- `name`:

  epoch name, character

- `subject`:

  `RAVESubject` instance

- `data`:

  a list of trial information, internally used

- `table`:

  trial epoch table

- `.columns`:

  epoch column names, internally used

## Active bindings

- `columns`:

  columns of trial table

- `n_trials`:

  total number of trials

- `trials`:

  trial numbers

- `excluded_trials`:

  trial numbers whose `ExcludedHint` is `TRUE`

- `available_events`:

  available events other than trial onset

- `available_condition_type`:

  available condition type other than the default

## Methods

### Public methods

- [`RAVEEpoch$@marshal()`](#method-RAVEEpoch-@marshal)

- [`RAVEEpoch$@unmarshal()`](#method-RAVEEpoch-@unmarshal)

- [`RAVEEpoch$new()`](#method-RAVEEpoch-initialize)

- [`RAVEEpoch$trial_at()`](#method-RAVEEpoch-trial_at)

- [`RAVEEpoch$update_table()`](#method-RAVEEpoch-update_table)

- [`RAVEEpoch$set_trial()`](#method-RAVEEpoch-set_trial)

- [`RAVEEpoch$exclude_trials()`](#method-RAVEEpoch-exclude_trials)

- [`RAVEEpoch$save()`](#method-RAVEEpoch-save)

- [`RAVEEpoch$get_event_colname()`](#method-RAVEEpoch-get_event_colname)

- [`RAVEEpoch$get_condition_colname()`](#method-RAVEEpoch-get_condition_colname)

- [`RAVEEpoch$clone()`](#method-RAVEEpoch-clone)

Inherited methods

- [`ravepipeline::RAVESerializable$@compare()`](http://dipterix.org/ravepipeline/reference/RAVESerializable.html#method-@compare)

------------------------------------------------------------------------

### `RAVEEpoch$@marshal()`

Internal method

#### Usage

    RAVEEpoch$@marshal(...)

#### Arguments

- `...`:

  internal arguments

------------------------------------------------------------------------

### `RAVEEpoch$@unmarshal()`

Internal method

#### Usage

    RAVEEpoch$@unmarshal(object, ...)

#### Arguments

- `object, ...`:

  internal arguments

------------------------------------------------------------------------

### `RAVEEpoch$new()`

constructor

#### Usage

    RAVEEpoch$new(subject, name)

#### Arguments

- `subject`:

  `RAVESubject` instance or character

- `name`:

  character, make sure `"epoch_<name>.csv"` is in meta folder

------------------------------------------------------------------------

### `RAVEEpoch$trial_at()`

get `ith` trial

#### Usage

    RAVEEpoch$trial_at(i, df = TRUE)

#### Arguments

- `i`:

  trial number

- `df`:

  whether to return as data frame or a list

------------------------------------------------------------------------

### `RAVEEpoch$update_table()`

manually update table field

#### Usage

    RAVEEpoch$update_table()

#### Returns

`self$table`

------------------------------------------------------------------------

### `RAVEEpoch$set_trial()`

set one trial

#### Usage

    RAVEEpoch$set_trial(Block, Time, Trial, Condition, ...)

#### Arguments

- `Block`:

  block string

- `Time`:

  time in second

- `Trial`:

  positive integer, trial number

- `Condition`:

  character, trial condition

- `...`:

  other key-value pairs corresponding to other optional columns;
  `ExcludedHint` defaults to `FALSE`

------------------------------------------------------------------------

### `RAVEEpoch$exclude_trials()`

Mark trials as excluded through the `ExcludedHint` column. Nothing is
removed: pipelines decide whether to drop marked trials. Use `save` to
write the marks to disk.

#### Usage

    RAVEEpoch$exclude_trials(..., add = TRUE)

#### Arguments

- `...`:

  trial numbers; flattened with `unlist(list(...))`

- `add`:

  whether to add to the trials already marked (default); `FALSE`
  replaces them, so `exclude_trials(add = FALSE)` clears every mark

#### Returns

The epoch instance, invisibly

------------------------------------------------------------------------

### `RAVEEpoch$save()`

Save the epoch to the subject's meta folder. A regular epoch writes
`epoch_<name>.csv` with `ExcludedHint` (the existing file is renamed to
a time-stamped backup first), then rebuilds
`epoch_<name>_OutlierRemoved.csv`: the existing copy is always backed up
and removed, and a new one is written when some (not all) trials are
marked, without them, with trials renumbered from 1 and `OriginalTrial`
holding the trial numbers of `epoch_<name>.csv`. An epoch whose name
ends with `_OutlierRemoved` is saved trimmed the same way: its file is
backed up and removed, then the unmarked trials are written. Its marked
trials, including rows marked by hand in the replaced file, are also
marked in the upstream epoch (the name without the suffix) when that
epoch exists and the trial still matches by `Block` and `Time`;
otherwise only the trimmed file is saved.

#### Usage

    RAVEEpoch$save()

#### Returns

Paths of the written files, invisibly

------------------------------------------------------------------------

### `RAVEEpoch$get_event_colname()`

Get epoch column name that represents the desired event

#### Usage

    RAVEEpoch$get_event_colname(
      event = "",
      missing = c("warning", "error", "none")
    )

#### Arguments

- `event`:

  a character string of the event, see `$available_events` for all
  available events; set to `"trial onset"`, `"default"`, or blank to use
  the default

- `missing`:

  what to do if event is missing; default is to warn

#### Returns

If `event` is one of `"trial onset"`, `"default"`, `""`, or `NULL`, then
the result will be `"Time"` column; if the event is found, then return
will be the corresponding event column. When the event is not found and
`missing` is `"error"`, error will be raised; default is to return
`"Time"` column, as it's trial onset and is mandatory.

------------------------------------------------------------------------

### `RAVEEpoch$get_condition_colname()`

Get condition column name that represents the desired condition type

#### Usage

    RAVEEpoch$get_condition_colname(
      condition_type = "default",
      missing = c("error", "warning", "none")
    )

#### Arguments

- `condition_type`:

  a character string of the condition type, see
  `$available_condition_type` for all available condition types; set to
  `"default"` or blank to use the default

- `missing`:

  what to do if condition type is missing; default is to warn if the
  condition column is not found.

#### Returns

If `condition_type` is one of `"default"`, `""`, or `NULL`, then the
result will be `"Condition"` column; if the condition type is found,
then return will be the corresponding condition type column. When the
condition type is not found and `missing` is `"error"`, error will be
raised; default is to return `"Condition"` column, as it's the default
and is mandatory.

------------------------------------------------------------------------

### `RAVEEpoch$clone()`

The objects of this class are cloneable with this method.

#### Usage

    RAVEEpoch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

# Please download DemoSubject ~700MB from
# https://github.com/beauchamplab/rave/releases/tag/v0.1.9-beta

if(has_rave_subject("demo/DemoSubject")) {

# Load meta/epoch_auditory_onset.csv from subject demo/DemoSubject
epoch <-RAVEEpoch$new(subject = 'demo/DemoSubject',
                      name = 'auditory_onset')

# first several trials
head(epoch$table)

# query specific trial
old_trial1 <- epoch$trial_at(1)

# Create new trial or change existing trial
epoch$set_trial(Block = '008', Time = 10,
                Trial = 1, Condition = 'AknownVmeant')
new_trial1 <- epoch$trial_at(1)

# Compare new and old trial 1
list(old_trial1, new_trial1)

# To get updated trial table, must update first
epoch$update_table()
head(epoch$table)

# Mark trials 1 and 3; pipelines decide whether to drop them
epoch$exclude_trials(1, 3)
epoch$excluded_trials

}
```

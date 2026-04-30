# Validate and normalize a list of user-supplied condition groupings.

Groups with empty or no conditions (after filtering against `epoch`,
when provided) are removed from the result.

## Usage

``` r
validate_condition_groupings(condition_groupings, epoch = NULL)
```

## Arguments

- condition_groupings:

  A list of condition group items. Each item is typically a list with
  elements `label` and `conditions`.

- epoch:

  Optional. A `RAVEEpoch` object (with `$table`) or a `data.frame`
  containing a `Condition` column. When supplied, conditions that do not
  appear in the epoch's `Condition` column are dropped.

## Value

A list with the following elements:

- `n` - the number of valid condition groups remaining after cleaning;

- `groups` - an unnamed list of cleaned group items (see below);

- `group_indexes` - integer vector of the original 1-based positions of
  the kept groups (used to derive a stable color);

- `group_labels` - character vector of the cleaned group labels;

- `unique_conditions` - sorted unique condition names across all groups;

- `use_epoch` - logical, `TRUE` when `epoch` was supplied;

- `n_trials` - (only when `epoch` is supplied) integer vector of the
  number of trials in each group.

Each cleaned group item contains:

- `index` - original 1-based position of the group in the input;

- `label` - cleaned label (auto-filled if missing/empty);

- `conditions` - unique character vector, in input order (not sorted);

- (when `epoch` is supplied)

  - `trials_by_condition` - named list of integer trial numbers from the
    epoch `Trial` column, one element per condition;

  - `trial_count` - integer vector of trial counts per condition;

  - `trials_included` - integer vector of trial numbers ordered by
    condition (concatenated in the condition order of `conditions`);

  - `n_trials` - total number of trials in the group.

## Examples

``` r

condition_groupings <- list(

  # Normal case
  list(
    label = "AOnly",
    conditions = c("drive_a", "known_a", "last_a", "meant_a")
  ),

  # Invalid/empty label
  list(
    label = NA,
    conditions = c("last_av", "drive_av", "known_av", "meant_av")
  ),

  # Empty condition
  list(
    label = "G3"
  ),

  # Missing label
  list(
    conditions = c("known_v", NA)
  ),

  # Invalid condition if epoch is provided
  list(
    conditions = "adafawefwf"
  )
)

# Without epoch
validate_condition_groupings(condition_groupings)
#> $n
#> [1] 4
#> 
#> $use_epoch
#> [1] FALSE
#> 
#> $group_indexes
#> [1] 1 2 4 5
#> 
#> $group_labels
#> [1] "AOnly"  "Group2" "Group4" "Group5"
#> 
#> $unique_conditions
#>  [1] "adafawefwf" "drive_a"    "drive_av"   "known_a"    "known_av"  
#>  [6] "known_v"    "last_a"     "last_av"    "meant_a"    "meant_av"  
#> 
#> $groups
#> $groups[[1]]
#> $groups[[1]]$index
#> [1] 1
#> 
#> $groups[[1]]$label
#> [1] "AOnly"
#> 
#> $groups[[1]]$conditions
#> [1] "drive_a" "known_a" "last_a"  "meant_a"
#> 
#> 
#> $groups[[2]]
#> $groups[[2]]$index
#> [1] 2
#> 
#> $groups[[2]]$label
#> [1] "Group2"
#> 
#> $groups[[2]]$conditions
#> [1] "last_av"  "drive_av" "known_av" "meant_av"
#> 
#> 
#> $groups[[3]]
#> $groups[[3]]$index
#> [1] 4
#> 
#> $groups[[3]]$label
#> [1] "Group4"
#> 
#> $groups[[3]]$conditions
#> [1] "known_v"
#> 
#> 
#> $groups[[4]]
#> $groups[[4]]$index
#> [1] 5
#> 
#> $groups[[4]]$label
#> [1] "Group5"
#> 
#> $groups[[4]]$conditions
#> [1] "adafawefwf"
#> 
#> 
#> 

if (FALSE) { # \dontrun{

# With epoch table: run `ravecore::install_subject("DemoSubject")`
# to install the demo-subject
subject <- ravecore::new_rave_subject("demo", "DemoSubject")
epoch <- subject$get_epoch("auditory_onset")

validate_condition_groupings(condition_groupings, epoch)

} # }

```

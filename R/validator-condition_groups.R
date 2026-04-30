
#' Validate and normalize a list of user-supplied condition groupings.
#' @description
#' Groups with empty or no conditions (after filtering against `epoch`, when
#' provided) are removed from the result.
#'
#' @param condition_groupings A list of condition group items. Each item is
#'   typically a list with elements `label` and `conditions`.
#' @param epoch Optional. A `RAVEEpoch` object (with `$table`) or a
#'   `data.frame` containing a `Condition` column. When supplied, conditions
#'   that do not appear in the epoch's `Condition` column are dropped.
#'
#' @return A list with the following elements:
#'   * `n` - the number of valid condition groups remaining after cleaning;
#'   * `groups` - an unnamed list of cleaned group items (see below);
#'   * `group_indexes` - integer vector of the original 1-based positions
#'     of the kept groups (used to derive a stable color);
#'   * `group_labels` - character vector of the cleaned group labels;
#'   * `unique_conditions` - sorted unique condition names across all groups;
#'   * `use_epoch` - logical, `TRUE` when `epoch` was supplied;
#'   * `n_trials` - (only when `epoch` is supplied) integer vector of the
#'     number of trials in each group.
#'
#' Each cleaned group item contains:
#'   * `index` - original 1-based position of the group in the input;
#'   * `label` - cleaned label (auto-filled if missing/empty);
#'   * `conditions` - unique character vector, in input order (not sorted);
#'   * (when `epoch` is supplied)
#'     - `trials_by_condition` - named list of integer trial numbers from
#'       the epoch `Trial` column, one element per condition;
#'     - `trial_count` - integer vector of trial counts per condition;
#'     - `trials_included` - integer vector of trial numbers ordered by
#'       condition (concatenated in the condition order of `conditions`);
#'     - `n_trials` - total number of trials in the group.
#'
#'
#'
#' @examples
#'
#'
#' condition_groupings <- list(
#'
#'   # Normal case
#'   list(
#'     label = "AOnly",
#'     conditions = c("drive_a", "known_a", "last_a", "meant_a")
#'   ),
#'
#'   # Invalid/empty label
#'   list(
#'     label = NA,
#'     conditions = c("last_av", "drive_av", "known_av", "meant_av")
#'   ),
#'
#'   # Empty condition
#'   list(
#'     label = "G3"
#'   ),
#'
#'   # Missing label
#'   list(
#'     conditions = c("known_v", NA)
#'   ),
#'
#'   # Invalid condition if epoch is provided
#'   list(
#'     conditions = "adafawefwf"
#'   )
#' )
#'
#' # Without epoch
#' validate_condition_groupings(condition_groupings)
#'
#' \dontrun{
#'
#' # With epoch table: run `ravecore::install_subject("DemoSubject")`
#' # to install the demo-subject
#' subject <- ravecore::new_rave_subject("demo", "DemoSubject")
#' epoch <- subject$get_epoch("auditory_onset")
#'
#' validate_condition_groupings(condition_groupings, epoch)
#'
#' }
#'
#'
#' @export
validate_condition_groupings <- function(condition_groupings, epoch = NULL) {
  # Resolve available conditions / trial table, if any
  use_epoch <- FALSE
  available_conditions <- NULL
  epoch_condition <- NULL
  epoch_trial <- NULL
  if (!is.null(epoch)) {
    epoch_table <- NULL
    if (inherits(epoch, "RAVEEpoch")) {
      epoch_table <- epoch$table
    } else if (is.data.frame(epoch)) {
      epoch_table <- epoch
    } else {
      stop("`epoch` must be a `RAVEEpoch` object or a `data.frame` ",
           "with a `Condition` column.")
    }
    if (!"Condition" %in% names(epoch_table)) {
      stop("`epoch` does not have a `Condition` column.")
    }
    if (!"Trial" %in% names(epoch_table)) {
      stop("`epoch` does not have a `Trial` column.")
    }
    use_epoch <- TRUE
    epoch_condition <- as.character(epoch_table$Condition)
    epoch_trial <- as.integer(epoch_table$Trial)
    available_conditions <- unique(epoch_condition)
  }

  if (!length(condition_groupings)) {
    return(list(
      n = 0L,
      groups = list(),
      group_indexes = integer(0),
      group_labels = character(0),
      unique_conditions = character(0),
      use_epoch = use_epoch,
      n_trials = if (use_epoch) integer(0) else NULL
    ))
  }

  cleaned <- lapply(seq_along(condition_groupings), function(ii) {
    item <- condition_groupings[[ii]]
    if (!is.list(item)) { return(NULL) }

    conditions <- item$conditions
    if (is.null(conditions)) { return(NULL) }
    conditions <- as.character(unlist(conditions, use.names = FALSE))
    conditions <- conditions[!is.na(conditions) & nzchar(conditions)]
    conditions <- unique(conditions)

    if (!is.null(available_conditions)) {
      conditions <- conditions[conditions %in% available_conditions]
    }

    if (!length(conditions)) { return(NULL) }

    label <- item$label[!is.na(item$label)]
    label <- trimws(paste(label, collapse = ""))
    if (!nzchar(label)) {
      label <- sprintf("Group%d", ii)
    }

    out <- list(
      index = as.integer(ii),
      label = label,
      conditions = conditions
    )

    if (use_epoch) {
      # For each condition, collect Trial numbers in epoch order, then
      # concatenate by condition order to produce `trials_included`.
      trials_by_condition <- lapply(conditions, function(cond) {
        epoch_trial[epoch_condition == cond]
      })
      names(trials_by_condition) <- conditions
      trial_count <- as.integer(vapply(trials_by_condition, length, integer(1)))
      trials_included <- as.integer(unlist(trials_by_condition,
                                           use.names = FALSE))

      out$trials_by_condition <- trials_by_condition
      out$trial_count <- trial_count
      out$trials_included <- trials_included
      out$n_trials <- sum(trial_count)
    }

    out
  })

  cleaned <- cleaned[!vapply(cleaned, is.null, logical(1))]
  cleaned <- unname(cleaned)

  group_indexes <- vapply(cleaned, `[[`, integer(1), "index")
  group_labels <- vapply(cleaned, `[[`, character(1), "label")

  if (anyDuplicated(group_labels)) {
    dup_labels <- unique(group_labels[duplicated(group_labels)])
    stop(
      "Duplicated condition group labels detected. ",
      "Please check the condition group settings, ",
      "make sure the labels/names for the groups are unique, or empty. ",
      "The duplicated group labels are: ",
      paste(sQuote(dup_labels), collapse = ", ")
    )
  }

  unique_conditions <- sort(unique(unlist(
    lapply(cleaned, `[[`, "conditions"), use.names = FALSE
  )))

  out <- list(
    n = length(cleaned),
    use_epoch = use_epoch,
    group_indexes = group_indexes,
    group_labels = group_labels,
    unique_conditions = unique_conditions
  )

  if (use_epoch) {
    out$n_trials <- vapply(cleaned, `[[`, integer(1), "n_trials")
  }

  out$groups <- cleaned

  out
}

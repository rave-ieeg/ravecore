# RAVEEpoch: ExcludedHint, exclude_trials(), save(), `_OutlierRemoved` epochs.
# Every test uses a throw-away subject under a temporary `data_dir`.

with_epoch_subject <- function(code, blocks = c("008", "009")) {
  root <- tempfile(pattern = "ravecore_epoch_test_")
  dir.create(root, recursive = TRUE)
  old_data_dir <- ravepipeline::raveio_getopt("data_dir")
  ravepipeline::raveio_setopt("data_dir", root, .save = FALSE)
  on.exit({
    ravepipeline::raveio_setopt("data_dir", old_data_dir, .save = FALSE)
    unlink(root, recursive = TRUE)
  }, add = TRUE)

  subject <- RAVESubject$new(project_name = "epochtest/sub01", strict = FALSE)
  dir_create2(subject$meta_path)
  dir_create2(subject$preprocess_path)
  # blocks are needed by `set_trial()`; written before the subject is reloaded
  save_yaml(list(blocks = blocks), file = file.path(subject$preprocess_path, "rave.yaml"))
  code(RAVESubject$new(project_name = "epochtest/sub01", strict = FALSE))
}

write_epoch_csv <- function(subject, name, table) {
  utils::write.csv(table, file.path(subject$meta_path, sprintf("epoch_%s.csv", name)),
                   row.names = FALSE)
}

read_epoch_csv <- function(subject, name) {
  utils::read.csv(file.path(subject$meta_path, sprintf("epoch_%s.csv", name)),
                  colClasses = "character")
}

# Backups made by `backup_file()` vs. by `safe_write_csv()`
count_backups <- function(subject, name, style = c("backup_file", "safe_write_csv")) {
  style <- match.arg(style)
  pattern <- switch(
    style,
    "backup_file" = sprintf("^epoch_%s_\\[backup_[0-9_]+\\]\\.csv$", name),
    "safe_write_csv" = sprintf("^epoch_%s_\\[[0-9_]+\\]\\.csv$", name)
  )
  length(list.files(subject$meta_path, pattern = pattern))
}

five_trials <- function() {
  data.frame(
    Block = "008",
    Time = c(1.5, 3, 4.5, 6, 7.5),
    Trial = 1:5,
    Condition = c("a", "b", "a", "b", "a")
  )
}

# Trimmed copy where trial 3 was already removed and the user then marked
# OriginalTrial 2 by hand
outlier_fixture <- function(subject) {
  write_epoch_csv(subject, "task", five_trials())
  or_tbl <- five_trials()[c(1, 2, 4, 5), ]
  or_tbl$OriginalTrial <- or_tbl$Trial
  or_tbl$Trial <- 1:4
  or_tbl$ExcludedHint <- c("", "TRUE", "", "")
  write_epoch_csv(subject, "task_OutlierRemoved", or_tbl)
}

# ---- reading ----

test_that("ExcludedHint is FALSE when the epoch file has no such column", {
  with_epoch_subject(function(subject) {
    write_epoch_csv(subject, "plain", five_trials())
    epoch <- RAVEEpoch$new(subject, "plain")
    testthat::expect_identical(epoch$table$ExcludedHint, rep(FALSE, 5))
    testthat::expect_false(epoch$trial_at(2)$ExcludedHint)
    testthat::expect_identical(epoch$excluded_trials, integer(0))
  })
})

test_that("ExcludedHint accepts logical strings, 0/1, blank and NA", {
  with_epoch_subject(function(subject) {
    values <- c("TRUE", "T", "true", "1", "FALSE", "F", "0", "", NA)
    tbl <- data.frame(Block = "008", Time = seq_along(values), Trial = seq_along(values),
                      Condition = "a", ExcludedHint = values)
    write_epoch_csv(subject, "hinted", tbl)
    epoch <- RAVEEpoch$new(subject, "hinted")
    testthat::expect_identical(
      epoch$table$ExcludedHint,
      c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
    )
    testthat::expect_identical(epoch$excluded_trials, 1:4)
  })
})

test_that("an ExcludedHint column with invalid values is ignored", {
  with_epoch_subject(function(subject) {
    tbl <- five_trials()
    tbl$ExcludedHint <- c("TRUE", "maybe", "", "", "")
    write_epoch_csv(subject, "badhint", tbl)
    epoch <- RAVEEpoch$new(subject, "badhint")
    testthat::expect_identical(epoch$table$ExcludedHint, rep(FALSE, 5))
  })
})

test_that("set_trial defaults ExcludedHint to FALSE", {
  with_epoch_subject(function(subject) {
    epoch <- RAVEEpoch$new(subject, "fresh")
    testthat::expect_true("ExcludedHint" %in% names(epoch$table))

    epoch$set_trial(Block = "008", Time = 1, Trial = 1, Condition = "a")
    epoch$set_trial(Block = "009", Time = 2, Trial = 2, Condition = "b", ExcludedHint = TRUE)
    epoch$set_trial(Block = "008", Time = 3, Trial = 3, Condition = "a", ExcludedHint = "maybe")
    testthat::expect_false(epoch$trial_at(1)$ExcludedHint)
    testthat::expect_false(epoch$trial_at(3)$ExcludedHint)
    testthat::expect_identical(epoch$excluded_trials, 2L)
    testthat::expect_error(
      epoch$set_trial(Block = "008", Time = 4, Trial = 4, Condition = "a", ExcludedHint = c(TRUE, FALSE))
    )
    epoch$update_table()
    testthat::expect_identical(epoch$table$ExcludedHint, c(FALSE, TRUE, FALSE))
  })
})

test_that("serialization keeps ExcludedHint and optional columns", {
  with_epoch_subject(function(subject) {
    tbl <- five_trials()
    tbl$Event_offset <- tbl$Time + 0.5
    tbl$ExcludedHint <- c("", "TRUE", "", "", "")
    write_epoch_csv(subject, "serial", tbl)
    epoch <- RAVEEpoch$new(subject, "serial")

    raw <- serialize(epoch, NULL, refhook = ravepipeline::rave_serialize_refhook)
    restored <- unserialize(raw, refhook = ravepipeline::rave_unserialize_refhook)

    testthat::expect_identical(restored$columns, epoch$columns)
    testthat::expect_equal(restored$table, epoch$table)
    testthat::expect_identical(restored$excluded_trials, 2L)
  })
})

test_that("loaded epochs keep their table, trial and row classes", {
  with_epoch_subject(function(subject) {
    tbl <- five_trials()
    tbl$Event_offset <- tbl$Time + 0.5
    write_epoch_csv(subject, "classes", tbl)
    epoch <- RAVEEpoch$new(subject, "classes")

    testthat::expect_identical(
      vapply(epoch$table, function(x) class(x)[[1]], ""),
      c(Block = "character", Time = "numeric", Trial = "numeric", Condition = "character",
        Duration = "numeric", ExcludedElectrodes = "character", Event_offset = "numeric",
        ExcludedHint = "logical")
    )
    testthat::expect_identical(class(epoch$update_table()), c("data.table", "data.frame"))
    testthat::expect_identical(class(epoch$trial_at(2)), c("data.table", "data.frame"))
    testthat::expect_identical(class(epoch$trial_at(2, df = FALSE)), "list")

    # trials are stored as 1-row data.tables indexed like data.frame rows
    row <- epoch$data[["2"]]
    testthat::expect_identical(class(row), c("data.table", "data.frame"))
    testthat::expect_identical(attr(row, "row.names"), 2L)
  })
})

test_that("get_epoch rejects trials that start before the recording", {
  with_epoch_subject(function(subject) {
    write_epoch_csv(subject, "onset", five_trials())     # first trial at 1.5 s
    testthat::expect_error(subject$get_epoch("onset", trial_starts = -2), "Trial 1 start too soon")
    epoch <- subject$get_epoch("onset", trial_starts = -1)
    testthat::expect_identical(epoch$trials, 1:5)
  })
})

# ---- exclude_trials() ----

test_that("exclude_trials adds to or replaces the marked trials", {
  with_epoch_subject(function(subject) {
    write_epoch_csv(subject, "ex", five_trials())
    epoch <- RAVEEpoch$new(subject, "ex")

    epoch$exclude_trials(2, c(4, 99))     # 99 is not a trial: ignored with a warning
    testthat::expect_identical(epoch$excluded_trials, c(2L, 4L))
    testthat::expect_identical(epoch$table$ExcludedHint, c(FALSE, TRUE, FALSE, TRUE, FALSE))

    epoch$exclude_trials(list(3))
    testthat::expect_identical(epoch$excluded_trials, 2:4)

    epoch$exclude_trials(5, add = FALSE)
    testthat::expect_identical(epoch$excluded_trials, 5L)

    epoch$exclude_trials(add = FALSE)
    testthat::expect_identical(epoch$excluded_trials, integer(0))

    testthat::expect_error(epoch$exclude_trials(c(TRUE, FALSE)), "logical")
    testthat::expect_identical(epoch$exclude_trials(1), epoch)
  })
})

# ---- save(): regular epochs ----

test_that("save writes ExcludedHint and a trimmed _OutlierRemoved copy", {
  with_epoch_subject(function(subject) {
    write_epoch_csv(subject, "task", five_trials())
    epoch <- RAVEEpoch$new(subject, "task")
    epoch$exclude_trials(2, 4)
    epoch$save()

    main <- read_epoch_csv(subject, "task")
    testthat::expect_identical(as.logical(main$ExcludedHint), c(FALSE, TRUE, FALSE, TRUE, FALSE))

    trimmed <- read_epoch_csv(subject, "task_OutlierRemoved")
    testthat::expect_identical(as.integer(trimmed$Trial), 1:3)
    testthat::expect_identical(as.integer(trimmed$OriginalTrial), c(1L, 3L, 5L))
    testthat::expect_identical(as.numeric(trimmed$Time), c(1.5, 4.5, 7.5))
    testthat::expect_false("ExcludedHint" %in% names(trimmed))
    testthat::expect_true("task_OutlierRemoved" %in% subject$epoch_names)
  })
})

test_that("save backs up the epoch with safe_write_csv and _OutlierRemoved with backup_file", {
  with_epoch_subject(function(subject) {
    write_epoch_csv(subject, "task", five_trials())
    epoch <- RAVEEpoch$new(subject, "task")
    epoch$exclude_trials(2, 4)
    epoch$save()
    # a second save in the same second must not fail
    epoch$save()

    testthat::expect_true(count_backups(subject, "task", "safe_write_csv") >= 1)
    testthat::expect_true(count_backups(subject, "task_OutlierRemoved", "backup_file") >= 1)
    testthat::expect_identical(count_backups(subject, "task_OutlierRemoved", "safe_write_csv"), 0L)
  })
})

test_that("save backs up and removes _OutlierRemoved when nothing is excluded", {
  with_epoch_subject(function(subject) {
    write_epoch_csv(subject, "task", five_trials())
    epoch <- RAVEEpoch$new(subject, "task")
    epoch$exclude_trials(2)
    epoch$save()
    testthat::expect_true("task_OutlierRemoved" %in% subject$epoch_names)

    epoch$exclude_trials(add = FALSE)
    epoch$save()
    testthat::expect_false("task_OutlierRemoved" %in% subject$epoch_names)
    testthat::expect_identical(count_backups(subject, "task_OutlierRemoved", "backup_file"), 1L)
  })
})

test_that("save does not write an empty _OutlierRemoved epoch", {
  with_epoch_subject(function(subject) {
    write_epoch_csv(subject, "task", five_trials())
    epoch <- RAVEEpoch$new(subject, "task")
    epoch$exclude_trials(1:5)
    epoch$save()
    testthat::expect_identical(as.logical(read_epoch_csv(subject, "task")$ExcludedHint), rep(TRUE, 5))
    testthat::expect_false("task_OutlierRemoved" %in% subject$epoch_names)
  })
})

test_that("save drops the row-name column left by write.csv(row.names = TRUE)", {
  with_epoch_subject(function(subject) {
    utils::write.csv(five_trials(), file.path(subject$meta_path, "epoch_rn.csv"))
    RAVEEpoch$new(subject, "rn")$save()
    testthat::expect_false("X" %in% names(read_epoch_csv(subject, "rn")))
  })
})

test_that("save rejects unlisted names and empty epochs", {
  with_epoch_subject(function(subject) {
    testthat::expect_error(RAVEEpoch$new(subject, "empty")$save(), "no trial")
    epoch <- RAVEEpoch$new(subject, "_hidden")
    epoch$set_trial(Block = "008", Time = 1, Trial = 1, Condition = "a")
    testthat::expect_error(epoch$save(), "Epoch name")
  })
})

# ---- `_OutlierRemoved` epochs ----

test_that("loading an _OutlierRemoved epoch drops marked rows and renumbers", {
  with_epoch_subject(function(subject) {
    outlier_fixture(subject)
    epoch <- RAVEEpoch$new(subject, "task_OutlierRemoved")
    testthat::expect_identical(epoch$trials, 1:3)
    testthat::expect_identical(as.integer(epoch$table$OriginalTrial), c(1L, 4L, 5L))
    testthat::expect_identical(epoch$excluded_trials, integer(0))
  })
})

test_that("saving an _OutlierRemoved epoch trims it and marks the upstream epoch", {
  with_epoch_subject(function(subject) {
    outlier_fixture(subject)
    epoch <- RAVEEpoch$new(subject, "task_OutlierRemoved")
    epoch$exclude_trials(3)                 # OriginalTrial 5
    epoch$save()

    trimmed <- read_epoch_csv(subject, "task_OutlierRemoved")
    testthat::expect_identical(as.integer(trimmed$Trial), 1:2)
    testthat::expect_identical(as.integer(trimmed$OriginalTrial), c(1L, 4L))
    testthat::expect_false("ExcludedHint" %in% names(trimmed))
    testthat::expect_false("task_OutlierRemoved_OutlierRemoved" %in% subject$epoch_names)

    # the replaced file is backed up by backup_file(), never by safe_write_csv()
    testthat::expect_identical(count_backups(subject, "task_OutlierRemoved", "backup_file"), 1L)
    testthat::expect_identical(count_backups(subject, "task_OutlierRemoved", "safe_write_csv"), 0L)

    # trial 2: marked by hand in the file; trial 5: exclude_trials()
    upstream <- read_epoch_csv(subject, "task")
    testthat::expect_identical(as.logical(upstream$ExcludedHint), c(FALSE, TRUE, FALSE, FALSE, TRUE))

    testthat::expect_identical(epoch$trials, 1:2)
  })
})

test_that("a restored (serialized) _OutlierRemoved epoch still marks hand-marked trials upstream", {
  with_epoch_subject(function(subject) {
    outlier_fixture(subject)
    epoch <- RAVEEpoch$new(subject, "task_OutlierRemoved")
    raw <- serialize(epoch, NULL, refhook = ravepipeline::rave_serialize_refhook)
    restored <- unserialize(raw, refhook = ravepipeline::rave_unserialize_refhook)

    restored$exclude_trials(3)              # OriginalTrial 5
    restored$save()

    upstream <- read_epoch_csv(subject, "task")
    testthat::expect_identical(as.logical(upstream$ExcludedHint), c(FALSE, TRUE, FALSE, FALSE, TRUE))
  })
})

test_that("upstream trials that no longer match are not marked", {
  with_epoch_subject(function(subject) {
    outlier_fixture(subject)
    epoch <- RAVEEpoch$new(subject, "task_OutlierRemoved")
    moved <- five_trials()
    moved$Time[5] <- 9                      # upstream regenerated after trimming
    write_epoch_csv(subject, "task", moved)
    epoch$exclude_trials(3)                 # OriginalTrial 5 was at 7.5 s
    epoch$save()
    upstream <- read_epoch_csv(subject, "task")
    testthat::expect_identical(as.logical(upstream$ExcludedHint), c(FALSE, TRUE, FALSE, FALSE, FALSE))
  })
})

test_that("an _OutlierRemoved epoch without upstream is saved trimmed and backed up", {
  with_epoch_subject(function(subject) {
    outlier_fixture(subject)
    file.remove(file.path(subject$meta_path, "epoch_task.csv"))
    epoch <- RAVEEpoch$new(subject, "task_OutlierRemoved")
    epoch$exclude_trials(3)
    epoch$save()
    testthat::expect_identical(subject$epoch_names, "task_OutlierRemoved")
    testthat::expect_identical(as.integer(read_epoch_csv(subject, "task_OutlierRemoved")$Trial), 1:2)
    testthat::expect_identical(count_backups(subject, "task_OutlierRemoved", "backup_file"), 1L)
  })
})

test_that("an unreadable upstream does not stop the _OutlierRemoved save", {
  with_epoch_subject(function(subject) {
    outlier_fixture(subject)
    upstream_path <- file.path(subject$meta_path, "epoch_task.csv")
    writeLines('"Block","Time","Trial","Condition"', upstream_path)   # no rows: cannot be loaded
    epoch <- RAVEEpoch$new(subject, "task_OutlierRemoved")
    epoch$exclude_trials(3)
    testthat::expect_no_error(epoch$save())

    testthat::expect_identical(as.integer(read_epoch_csv(subject, "task_OutlierRemoved")$Trial), 1:2)
    testthat::expect_identical(count_backups(subject, "task_OutlierRemoved", "backup_file"), 1L)
    testthat::expect_identical(readLines(upstream_path), '"Block","Time","Trial","Condition"')
  })
})

test_that("an _OutlierRemoved file without OriginalTrial leaves upstream alone", {
  with_epoch_subject(function(subject) {
    write_epoch_csv(subject, "task", five_trials())
    or_tbl <- five_trials()
    or_tbl$ExcludedHint <- c("", "TRUE", "", "", "")
    write_epoch_csv(subject, "task_OutlierRemoved", or_tbl)

    epoch <- RAVEEpoch$new(subject, "task_OutlierRemoved")
    testthat::expect_identical(epoch$trials, 1:4)
    epoch$save()
    testthat::expect_identical(as.integer(read_epoch_csv(subject, "task_OutlierRemoved")$Trial), 1:4)
    testthat::expect_false("ExcludedHint" %in% names(read_epoch_csv(subject, "task")))
  })
})

test_that("trials without OriginalTrial are skipped, the others still mark upstream", {
  with_epoch_subject(function(subject) {
    write_epoch_csv(subject, "task", five_trials())
    or_tbl <- five_trials()[c(1, 2, 4, 5), ]
    or_tbl$OriginalTrial <- c(1L, 2L, NA, 5L)
    or_tbl$Trial <- 1:4
    write_epoch_csv(subject, "task_OutlierRemoved", or_tbl)

    epoch <- RAVEEpoch$new(subject, "task_OutlierRemoved")
    epoch$exclude_trials(3, 4)              # no OriginalTrial; OriginalTrial 5
    epoch$save()
    upstream <- read_epoch_csv(subject, "task")
    testthat::expect_identical(as.logical(upstream$ExcludedHint), c(FALSE, FALSE, FALSE, FALSE, TRUE))
  })
})

test_that("excluding every trial of an _OutlierRemoved epoch retires its file", {
  with_epoch_subject(function(subject) {
    outlier_fixture(subject)
    epoch <- RAVEEpoch$new(subject, "task_OutlierRemoved")
    epoch$exclude_trials(epoch$trials)
    epoch$save()
    testthat::expect_identical(subject$epoch_names, "task")
    testthat::expect_identical(count_backups(subject, "task_OutlierRemoved", "backup_file"), 1L)
    testthat::expect_identical(as.logical(read_epoch_csv(subject, "task")$ExcludedHint),
                               c(TRUE, TRUE, FALSE, TRUE, TRUE))
    testthat::expect_equal(epoch$n_trials, 0)
  })
})

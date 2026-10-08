test_that("RAVESubject formats and prints a summary of its data", {
  subject <- as_rave_subject("demo/DemoSubject", strict = FALSE)
  testthat::skip_if_not(dir.exists(subject$path),
                        "demo/DemoSubject is not installed")

  lines <- subject$format()
  testthat::expect_identical(lines[[1]], "RAVE subject <demo/DemoSubject>")
  has_line <- function(pattern) any(grepl(pattern, lines))
  testthat::expect_true(has_line("^  Blocks: "))
  testthat::expect_true(has_line("^  Electrodes: "))
  testthat::expect_true(has_line("^    LFP: .+ at [0-9.]+ Hz"))
  testthat::expect_true(has_line("^  Epochs: "))
  testthat::expect_true(has_line("^  References: "))
  for (label in c("Native MRI", "MNI normalization", "CT-MRI coregistration",
                  "FreeSurfer")) {
    testthat::expect_true(has_line(sprintf("^  %s: ", label)))
  }

  testthat::expect_identical(format(subject), lines)
  testthat::expect_output(print(subject), "RAVE subject <demo/DemoSubject>",
                          fixed = TRUE)
  printed <- withVisible(print(subject))
  testthat::expect_false(printed$visible)
})

test_that("a subject without data still formats", {
  subject <- as_rave_subject("demo/NoSuchSubject123", strict = FALSE)
  lines <- subject$format()
  testthat::expect_identical(lines[[1]], "RAVE subject <demo/NoSuchSubject123>")
  testthat::expect_true("  Electrodes: none imported" %in% lines)
  testthat::expect_true("  Epochs: none" %in% lines)
  testthat::expect_true("  FreeSurfer: missing" %in% lines)
})

test_that("normalization templates are read from the mapping logs", {
  imaging <- tempfile("rave-imaging-")
  logs <- file.path(imaging, "normalization", "log")
  dir.create(logs, recursive = TRUE)
  # subject codes may contain underscores, which are not valid in BIDS labels
  file.create(file.path(logs, c(
    "sub-LOC3_MW_desc-preproc_template-MNI152NLin2009bAsym_native-T1w_mappings.json",
    "sub-LOC3_MW_desc-preproc_template-fsaverage_native-T1w_mappings.json",
    "notes.txt"
  )))

  testthat::expect_identical(
    subject_normalization_templates(imaging, "LOC3_MW"),
    c("MNI152NLin2009bAsym", "fsaverage")
  )
  testthat::expect_identical(
    subject_normalization_templates(tempfile(), "LOC3_MW"),
    character(0)
  )
})

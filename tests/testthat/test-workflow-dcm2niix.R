test_that("image_source_relpath() gives the path the yael_preprocess loader lists", {
  root <- normalizePath(tempfile("raw_"), winslash = "/", mustWork = FALSE)
  bids <- normalizePath(tempfile("bids_"), winslash = "/", mustWork = FALSE)
  on.exit({ unlink(c(root, bids), recursive = TRUE) }, add = TRUE)

  dir.create(file.path(root, "rave-uploads", "MRI"), recursive = TRUE)
  dir.create(file.path(root, "DICOM", "series1"), recursive = TRUE)
  dir.create(file.path(bids, "ses-preop", "anat"), recursive = TRUE)
  nii <- file.path(root, "rave-uploads", "MRI", "MRI.nii.gz")
  bids_nii <- file.path(bids, "ses-preop", "anat", "sub-01_T1w.nii.gz")
  outside <- tempfile(fileext = ".nii")
  file.create(c(nii, bids_nii, outside))
  on.exit({ unlink(outside) }, add = TRUE)

  roots <- c(root, bids)

  # a file in the raw folder
  expect_equal(image_source_relpath(nii, roots), "rave-uploads/MRI/MRI.nii.gz")
  # a DICOM folder, with or without a trailing slash on the root
  expect_equal(image_source_relpath(file.path(root, "DICOM", "series1"), roots),
               "DICOM/series1")
  expect_equal(image_source_relpath(nii, c(paste0(root, "/"), bids)),
               "rave-uploads/MRI/MRI.nii.gz")
  # a file in the 'BIDS' raw folder
  expect_equal(image_source_relpath(bids_nii, roots),
               "ses-preop/anat/sub-01_T1w.nii.gz")
  # the first root that contains the path wins
  expect_equal(image_source_relpath(nii, c(dirname(dirname(nii)), root)),
               "MRI/MRI.nii.gz")
  # outside every root, or no usable root: absolute
  expect_equal(image_source_relpath(outside, roots),
               normalizePath(outside, winslash = "/"))
  expect_equal(image_source_relpath(nii, c(NA_character_, "")),
               normalizePath(nii, winslash = "/"))
  # a sibling folder sharing the root's name prefix is not inside it
  sibling <- paste0(root, "2")
  dir.create(sibling)
  on.exit({ unlink(sibling, recursive = TRUE) }, add = TRUE)
  file.create(file.path(sibling, "a.nii"))
  expect_equal(image_source_relpath(file.path(sibling, "a.nii"), root),
               normalizePath(file.path(sibling, "a.nii"), winslash = "/"))
})

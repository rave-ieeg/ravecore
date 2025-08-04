
r6_reserved_fields <- c(
  ".__enclos_env__", "clone", "print", "initialize", "private",
  "@unmarshal", "@marshal", "@unserialize_refhook", "@serialize_refhook")

HDF5_EAGERLOAD <- TRUE

BASIC_EPOCH_TABLE_COLUMNS <- c("Block", "Time", "Trial", "Condition")

RAVEIO_FILEARRAY_VERSION <- 1

#' @name ravecore-constants
#' @title 'RAVE' constants
#' @description
#' Constant variables
#'
#' @export
LOCATION_TYPES <- c("iEEG", "sEEG", "ECoG", "EEG", "Others")

#' @rdname ravecore-constants
#' @export
SIGNAL_TYPES <- c("LFP", "Spike", "EKG", "Auxiliary", "Unknown")

#' @rdname ravecore-constants
#' @export
IMPORT_FORMATS <- list(
  `.mat/.h5 file per electrode per block` = "native_matlab",
  `Single .mat/.h5 file per block` = "native_matlab2",
  `Single EDF(+) file per block` = "native_edf",
  `Single BrainVision file (.vhdr+.eeg, .vhdr+.dat) per block` = "native_brainvis",
  `BIDS & EDF(+)` = "bids_edf",
  `BIDS & BrainVision (.vhdr+.eeg, .vhdr+.dat)` = "bids_brainvis",
  `Single BlackRock file (.nev+.nsx) per block` = "native_blackrock"
)

#' @rdname ravecore-constants
#' @export
YAEL_IMAGE_TYPES <- c(
  "T1w", "T2w", "FLAIR", "preopCT", "T1wContrast", "fGATIR",
  "postopT1w", "postopT2w", "postopFLAIR", "CT")

#' @rdname ravecore-constants
#' @export
MNI305_to_MNI152 <- matrix(
  c(0.9975, 0.0146, -0.013, 0,
    -0.0073, 1.0009, -0.0093, 0,
    0.0176, -0.0024, 0.9971, 0,
    -0.0429, 1.5496, 1.184, 1),
  nrow = 4L, byrow = FALSE
)

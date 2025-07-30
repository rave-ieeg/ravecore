
r6_reserved_fields <- c(
  ".__enclos_env__", "clone", "print", "initialize", "private",
  "@unmarshal", "@marshal", "@unserialize_refhook", "@serialize_refhook")

HDF5_EAGERLOAD <- TRUE

BASIC_EPOCH_TABLE_COLUMNS <- c("Block", "Time", "Trial", "Condition")

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



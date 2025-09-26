
#' @title Install data-sets from OpenNeuro
#' @description
#' Enjoy hundreds of open-access data sets from \url{https://openneuro.org} with a
#' simple accession number.
#'
#' @param accession_number \code{'OpenNeuro'} accession number
#' @param subject_codes subject codes, with or without the prefix \code{'sub-'},
#' default is \code{NULL} to download the entire data repository
#' @param tag version number; default is \code{NULL} to download the latest
#' version
#' @param parent_folder parent directory where the data will be downloaded into
#' the data folder name is always the accession number
#' @returns The data folder name on the local disk.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Download Hermes D, Miller KJ, Wandell BA, Winawer J (2015) dataset
#' # from https://openneuro.org/datasets/ds005953
#'
#' install_openneuro('ds005953')
#'
#' # Download subject sub-HUP070 used by Bernabei & Li et al.
#' # from https://openneuro.org/datasets/ds004100
#'
#' install_openneuro('ds004100', subject_codes = "HUP070")
#'
#' # access the downloaded data
#' bids_parent_root <- ravepipeline::raveio_getopt("bids_data_dir")
#'
#' # ---- Example of visualizing electrodes on the fsaverage ----
#' # Load BIDS project
#' proj_ds004100 <- bidsr::bids_project(
#'   file.path(bids_parent_root, "ds004100"))
#'
#' # BIDS-R Subject instance
#' sub_HUP070 <- bidsr::bids_subject(proj_ds004100, "HUP070")
#'
#' # Find BIDS entities with electrodes as suffix
#' electrode <- bidsr::query_bids(sub_HUP070, list(
#'   data_types = "ieeg",
#'   suffixes = "electrodes",
#'   sidecars = TRUE
#' ))
#'
#' # resolve electrode table path
#' electrode_path <- bidsr::resolve_bids_path(
#'   x = proj_ds004100,
#'   format(electrode$parsed[[1]]))
#'
#' # laod electrode coordinate
#' tabular <- bidsr::as_bids_tabular(electrode_path)
#'
#' # Build RAVE electrode table
#' electrode_coordinates <- data.frame(
#'   Electrode = 1:nrow(tabular$content),
#'   x = tabular$content$x,
#'   y = tabular$content$y,
#'   z = tabular$content$z,
#'   Label = tabular$content$name,
#'   Radius = 2,
#'   BIDSSubject = "sub-HUP070"
#' )
#'
#' # Load RAVE brain - fsaverage
#' template <- threeBrain::merge_brain(template_subject = "fsaverage")
#' fsaverage <- template$template_object
#'
#' # This dataset uses surface RAS; see coordsys JSON
#' # tkrRAS: surface RAS
#' # scannerRAS: MRI RAS
#' fsaverage$set_electrodes(electrode_coordinates, coord_sys = "tkrRAS")
#'
#' fsaverage$plot()
#'
#'
#'
#' }
#'
#' @export
install_openneuro <- function(
    accession_number,
    subject_codes = NULL,
    tag = NULL,
    parent_folder = NULL
) {


  if(!dir_exists(rpymat::env_path())) {
    stop("Conda is not configured for RAVE. Please run `ravemanager::configure_python()` to set it up.")
  }


  # Make sure the inputs are correct
  force(accession_number)

  if(length(subject_codes)) {
    subject_codes <- gsub("^sub-", "", subject_codes, ignore.case = TRUE)
    subject_codes <- sprintf("sub-%s", subject_codes)
  }

  if(length(parent_folder)) {
    if(!dir_exists(parent_folder)) {
      stop("Parent folder does not exist: ", parent_folder)
    }
  } else {
    parent_folder <- ravepipeline::raveio_getopt("bids_data_dir", "~/rave_data/bids_dir")
  }


  # Make sure openneuro-py has been installed
  ensure_py_package("openneuro-py", pip = TRUE)
  rpymat::ensure_rpymat(verbose = FALSE)
  openneuro <- rpymat::import("openneuro")

  # Intialize directories
  parent_folder <- dir_create2(parent_folder)

  target_dir <- file_path(parent_folder, accession_number)

  if(length(subject_codes)) {
    openneuro$download(
      dataset = accession_number,
      target_dir = target_dir,
      include = subject_codes,
      tag = tag
    )
  } else {
    openneuro$download(dataset = accession_number,
                       target_dir = target_dir,
                       tag = tag)
  }

  invisible(target_dir)
}

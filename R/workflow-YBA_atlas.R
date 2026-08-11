
#' @title Generate \verb{YBA} atlas for a 'RAVE' subject
#' @description
#' Morphs a \verb{YBA} atlas from the \verb{MNI152} symmetric template to the
#' subject native space, calculates the anatomical labels for the electrode
#' contacts, and optionally creates a quality-control report. This function
#' requires the subject to be normalized to a \verb{MNI152} symmetric template
#' first; see \code{\link{cmd_run_yael_preprocess}}.
#'
#' @param subject 'RAVE' subject instance or character; see
#' \code{\link{as_rave_subject}}
#' @param name atlas name; choices are \verb{"YBA690"} (default) and
#' \verb{"YBA696"}
#' @param template_names template names to search for existing non-linear
#' mappings; default is \code{NULL}, which uses the \verb{MNI152} symmetric
#' templates; the first template with a valid mapping will be used
#' @param radius search radius, in millimeters, when assigning the atlas labels
#' to the electrode contacts; default is \code{2}
#' @param verbose whether to print out the progress; default is \code{TRUE}
#' @param create_report whether to create a quality-control report; default is
#' \code{TRUE}
#' @param disable_viewer whether to omit the embedded three-dimensional viewer
#' in the report; default is \code{TRUE}. Please be aware that enabling the
#' viewer results in a self-contained report that can be hundreds of megabytes
#' large.
#'
#' @returns An invisible named list of
#' \describe{
#' \item{\code{atlas_path}}{path to the atlas volume created under the subject
#' \verb{FreeSurfer} \code{'mri'} folder}
#' \item{\code{label_path}}{path to the electrode label table, or \code{NA} if
#' no electrode contact has been localized}
#' \item{\code{colormap_path}}{path to the color lookup table saved along with
#' the atlas volume}
#' \item{\code{report_path}}{path to the report folder, or \code{NA} if
#' \code{create_report} is false}
#' \item{\code{report_file}}{path to the report file, or \code{NA} if
#' \code{create_report} is false}
#' }
#'
#' @details
#' The workflow requires an existing non-linear normalization from the subject
#' native \verb{T1w} image to a \verb{MNI152} symmetric template. Please run
#' \code{\link{cmd_run_yael_preprocess}} with \code{normalize_template} set to
#' one of \verb{"mni_icbm152_nlin_sym_09a"}, \verb{"mni_icbm152_nlin_sym_09b"},
#' or \verb{"mni_icbm152_nlin_sym_09c"} if such mapping is missing.
#'
#' Given the mapping, the template atlas is inverse-transformed to the native
#' space via \code{\link{generate_atlases_from_template}}. The resulting
#' \verb{NIfTI} volume is stored under the subject \code{'atlases'} imaging
#' folder, with a copy (and its color lookup table) placed under the subject
#' \verb{FreeSurfer} \code{'mri'} folder such that the atlas can be displayed
#' by the \pkg{threeBrain} viewer.
#'
#' When the subject has electrode contacts localized, the anatomical labels are
#' calculated within a sphere of \code{radius} millimeters around each contact
#' center, and are stored as \code{'electrode_atlas_<name>.csv'} under the
#' subject meta folder.
#'
#' @seealso \code{\link{generate_atlases_from_template}},
#' \code{\link{cmd_run_yael_preprocess}}
#'
#' @examples
#'
#' # Please check out https://rave.wiki to configure Python for RAVE
#' # or run ravemanager::configure_python()
#' \dontrun{
#'
#' subject_id <- "YAEL/subject_code"
#'
#' # ---- Step 1: normalization ------------------------------------------
#' # Map the native `T1w` image to a `MNI152` symmetric template. This step
#' # is only needed once per subject; skip it when the mapping is available
#' cmd_run_yael_preprocess(
#'   subject = subject_id,
#'   t1w_path = "/path/to/T1w.nii.gz",
#'
#'   # `YBA` atlases are defined in the `MNI152` symmetric space
#'   normalize_template = "mni_icbm152_nlin_sym_09b",
#'
#'   # set to `TRUE` to also run `FreeSurfer` reconstruction
#'   # if you haven't...
#'   run_recon_all = FALSE
#' )
#'
#' # ---- Step 2: atlas, electrode labels, and report ---------------------
#' # Morph the atlas back to the native space, label the electrode contacts,
#' # and create the quality-control report
#' results <- generate_atlas_YBA(subject_id, name = "YBA696")
#'
#' # ---- Step 3: inspect --------------------------------------------------
#' # Electrode labels: `results$label_path`
#' utils::read.csv(results$label_path)
#'
#' # Quality-control report
#' utils::browseURL(results$report_file)
#'
#' }
#'
#' @export
generate_atlas_YBA <- function(
    subject, name = c("YBA690", "YBA696"), template_names = NULL,
    radius = 2, verbose = TRUE, create_report = TRUE,
    disable_viewer = TRUE) {

  name <- match.arg(name)

  radius <- as.numeric(radius)[[1]]
  if (is.na(radius) || radius < 0) {
    stop("`radius` must be a non-negative number (in millimeters).")
  }

  # Do NOT remove!
  # DIPSAUS DEBUG START
  # subject <- "YAEL/Precision012"
  # name <- "YBA696"
  # template_names <- NULL
  # verbose <- TRUE
  # radius = 2

  TEMPLATE_MNI152_SYM <- c("mni_icbm152_nlin_sym_09b",
                           "mni_icbm152_nlin_sym_09a",
                           "mni_icbm152_nlin_sym_09c")

  TEMPLATE_MNI152_ASYM <- c("mni_icbm152_nlin_asym_09b",
                            "mni_icbm152_nlin_asym_09a",
                            "mni_icbm152_nlin_asym_09c")

  # Initialize subject
  subject <- as_rave_subject(subject, strict = TRUE)
  yael <- as_yael_process(subject)
  t1_path <- yael$get_input_image("T1w")

  # Check if there is any template mapping to MNI152 sym
  template_names <- template_names[!is.na(as.character(template_names))]
  if (!length(template_names)) {
    template_names <- switch(
      name,
      "YBA690" = TEMPLATE_MNI152_SYM,
      "YBA696" = TEMPLATE_MNI152_SYM,
      {
        TEMPLATE_MNI152_ASYM
      }
    )
  }

  has_mapping <- FALSE

  for (template_name in template_names) {
    tryCatch(
      {
        if (verbose) {
          ravepipeline::logger("Checking template: `{template_name}`", use_glue = TRUE, level = "trace")
        }
        mapping <- yael$get_template_mapping(template_name = template_name)
        if (!is.null(mapping)) {
          has_mapping <- TRUE
        }
      },
      error = function(e) {
      }
    )
    if (has_mapping) {
      if (verbose) {
        ravepipeline::logger("Found mapping to `{template_name}`", use_glue = TRUE, level = "success")
      }
      break
    }
  }

  if (!has_mapping) {
    if (length(t1_path) != 1) {
      t1_path <- "/path/to/t1.nii.gz"
    }
    stop(
      "No normalization mapping to MNI152 symmetric template. ",
      "Please run `ravecore::cmd_run_yael_preprocess(...)` with ",
      "`normalize_template` set to one of the following options first: \n",
      "  * 'mni_icbm152_nlin_sym_09b': symmetric, (0.5 mm)^3 voxel size, ~16GB RAM needed;\n",
      "  * 'mni_icbm152_nlin_sym_09a': symmetric, (1 mm)^3 voxel size, ~8GB RAM needed;\n",
      "  * 'mni_icbm152_nlin_sym_09c': symmetric, (1 mm)^3 voxel size, ~8GB RAM needed.\n\n",
      "For example:\n\n",
      "  ravecore::cmd_run_yael_preprocess(\n",
      sprintf("    subject = \"%s\",\n", subject$subject_id),
      sprintf("    t1w_path = \"%s\",\n", t1_path),
      sprintf("    normalize_template = \"%s\",\n", template_names[[1]]),
      "    run_recon_all = FALSE  # Set to `TRUE` if you want to run FreeSurfer\n",
      "  )"
    )
  }

  if (!length(t1_path)) {
    # We need to find T1w somewhere. This is rare but some users do delete the
    # input folder once normalized
    t1_path <- file_path(subject$imaging_path, c(
      "fs/mri/rave_slices.nii.gz",
      "fs/mri/rave_slices.nii",
      "coregistration/MRI_reference.nii.gz",
      "coregistration/MRI_reference.nii"
    ))
    t1_path <- t1_path[file_exists(t1_path)]

    if (length(t1_path)) {
      t1_path <- t1_path[[1]]
      yael$set_input_image(t1_path, type = "T1w")
    } else {
      stop("Unable to find T1w image. Please run normalization step first.")
    }

  }

  # Check if we have the atlas files ready
  atlas_path <- switch(
    name,
    "YBA690" = ieegio::ieegio_sample_data("atlases/YBA/YBA690.nii.gz"),
    "YBA696" = ieegio::ieegio_sample_data("atlases/YBA/YBA696.nii.gz")
  )

  colormap_src_path <- switch(
    name,
    "YBA690" = {
      system.file("palettes", "datacube2", "YBA690ColorLUT.json", package = "threeBrain")
    },
    "YBA696" = {
      system.file("palettes", "datacube2", "YBA696ColorLUT.json", package = "threeBrain")
    }
  )
  colormap <- threeBrain::load_colormap(colormap_src_path)

  # The sample-data folder might contain more than one atlas, and each atlas
  # costs a full non-linear warp; only stage the requested one. The staging
  # folder is named after the original folder so the generated file layout
  # stays the same
  staging_root <- tempfile(pattern = "ravecore_atlas_")
  staging_path <- dir_create2(file.path(staging_root, basename(dirname(atlas_path))))
  on.exit({ unlink(staging_root, recursive = TRUE, force = TRUE) }, add = TRUE)

  if (!file.copy(atlas_path, file.path(staging_path, basename(atlas_path)),
                 overwrite = TRUE)) {
    stop("Unable to stage the atlas file: ", atlas_path)
  }

  # Create atlas from MNI152 -> native brain
  generate_atlases_from_template(
    subject = subject,
    atlas_folders = staging_path,
    template_name = template_name,
    as_job = FALSE,
    surfaces = FALSE
  )

  # Save a copy to rave-imaging/fs/mri/ for visualizations later
  atlas_root <- file.path(subject$imaging_path, "atlases")
  atlas_native <- file.path(atlas_root, basename(atlas_path))
  if (!file.exists(atlas_native)) {
    # The atlas might have been generated under a sub-folder
    candidates <- list.files(
      atlas_root,
      pattern = "\\.(nii|nii\\.gz)$",
      recursive = TRUE,
      include.dirs = FALSE,
      full.names = TRUE
    )
    candidates <- candidates[basename(candidates) == basename(atlas_path)]
    if (!length(candidates)) {
      stop("Unable to find the generated atlas `", basename(atlas_path),
           "`. Please check whether the atlas generation step has failed. ",
           "The expected folder is:\n  ", atlas_root)
    }
    atlas_native <- candidates[[1]]
  }

  freesurfer_path <- subject$freesurfer_path
  if (length(freesurfer_path) != 1 || is.na(freesurfer_path) ||
      !nzchar(freesurfer_path)) {
    stop("Unable to resolve the `FreeSurfer` folder for subject ",
         subject$subject_id, ". Please run ",
         "`ravecore::cmd_run_yael_preprocess` to initialize the imaging ",
         "folder first.")
  }
  freesurfer_mri_path <- dir_create2(file.path(freesurfer_path, "mri"))

  atlas_native2 <- file.path(freesurfer_mri_path, sprintf("%s_aseg.nii.gz", name))
  v <- ieegio::read_volume(atlas_native)
  ieegio::write_volume(v, atlas_native2)

  colormap_path <- file.path(freesurfer_mri_path, sprintf("%s_colormap.json", name))
  threeBrain::save_colormap(colormap, colormap_path)

  # Compute electrode labels
  brain <- rave_brain(subject)
  electrode_table <- brain$electrodes$raw_table
  if (is.data.frame(electrode_table) && nrow(electrode_table)) {

    labels <- brain$electrodes$get_atlas_labels(
      atlas_native2,
      lut = colormap,
      radius = radius
    )

    # `Electrode` goes first so the table is consistent with the report
    labels <- cbind(
      data.frame(Electrode = electrode_table$Electrode),
      labels
    )
    label_path <- file.path(subject$meta_path, sprintf("electrode_atlas_%s.csv", name))
    backup_file(label_path, remove = TRUE, quiet = TRUE)
    utils::write.csv(labels, file = label_path, row.names = FALSE)

    if (verbose) {
      ravepipeline::logger("{name} atlas labels for electrodes saved to -> {label_path}", use_glue = TRUE, level = "info")
    }
  } else {
    labels <- NULL
    label_path <- NA_character_
  }


  # Preview
  if (create_report) {
    # output_name <- format(x = Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    datetime <- format(x = Sys.time(), format = "%y%m%dT%H%M%S")
    report_filename <- sprintf(
      "report-AtlasOverlay_datetime-%s_atlas-%s_ravecore",
      datetime, name
    )
    report_path <- dir_create2(file_path(subject$report_path, report_filename))
    ravepipeline::pkg_build_report(
      "atlas-overlay",
      package = "ravecore",
      params = list(
        subject_id = subject$subject_id,
        disable_viewer = disable_viewer,
        underlay = NULL,
        atlas_name = name,
        atlas_path = sprintf("%s_aseg.nii.gz", name),
        atlas_colormap = colormap_path,
        atlas_search_radius = radius
      ),
      build_path = report_path
    )
    report_file <- file.path(report_path, "report.html")
    if (verbose) {
      ravepipeline::logger("Report has been created at:\n  ", report_path, level = "success")
    }
  } else {
    report_path <- NA_character_
    report_file <- NA_character_
  }

  invisible(list(
    atlas_path = atlas_native2,
    label_path = label_path,
    colormap_path = colormap_path,
    report_path = report_path,
    report_file = report_file
  ))

}

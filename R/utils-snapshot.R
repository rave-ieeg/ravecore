snapshot_table <- function(x, target_path, digit_cnames = NULL, digits = 2, caption = NULL, title = "Table") {
  # cache_table <- function(x, subject, name, digit_cnames = NULL, digits = 2) {

  if(!is.data.frame(x) || !nrow(x)) {
    if(file.exists(target_path)) {
      unlink(target_path)
    }
    return(invisible(0L))
  }

  x <- as.data.frame(x)
  table_names <- names(x)
  table_names <- table_names[!grepl("^X(\\.[0-9]+|)$", table_names)]
  x <- x[, table_names]
  digit_cnames <- digit_cnames[digit_cnames %in% table_names]

  # if DT installed
  if(package_installed("DT")) {
    DT <- asNamespace("DT")
    htmlwidgets <- asNamespace("htmlwidgets")
    htmltools <- asNamespace("htmltools")

    widget <- DT$datatable(
      x,
      class = "compact stripe nowrap",
      caption = caption,
      rownames = FALSE,
      fillContainer = FALSE,
      options = list(
        scrollX = TRUE,        # allow horizontal scrolling
        scrollY = FALSE,
        pageLength = -1,                     # show all rows by default
        lengthMenu = list(c(20, 50, 100, -1), c("20", "50", "100", "All")),
        paging = TRUE
      )
    )
    if(length(digit_cnames)) {
      widget <- DT$formatRound(widget, digits = digits, columns = digit_cnames)
    }
    css <- "
    html, body { height:100%; width:100%; margin:0 !important; padding:0 !important; }
    .dataTables_wrapper { height:100% !important; width:100% !important; box-sizing:border-box; }
    "
    styled_tbl <- htmlwidgets$prependContent(widget, htmltools$tags$style(htmltools$HTML(css)))

    htmlwidgets$saveWidget(
      widget = styled_tbl,
      file = target_path,
      selfcontained = TRUE,
      title = title
    )
  } else {

    str <- knitr::kable(
      x, format = "html", caption = caption, digits = digits,
      table.attr = 'class="table table-striped table-condensed compress stripe nowrap" style="border-collapse:collapse;width:auto;font-size:90%;overflow-x:scroll"')

    writeLines(str, target_path)
  }

  invisible(nrow(x))

}

#' @name snapshot_project
#' @title Create overview report for given project or subject
#' @param x characters or a 'RAVE' project or subject instance
#' @param target_path directory where the snapshot data will be stored; default
#' is under \code{'project_overview'} group data folder, see method
#' \code{group_data} from \code{\link{RAVEProject}} class.
#' @param template_subjects a vector of characters of template brain to be
#' used for generating the group brain; see
#' \code{\link[threeBrain]{available_templates}} for available templates.
#' @param quick whether to skip certain validations and subjects if snapshot
#' reports have already existed
#' @returns \code{snapshot_subject} returns a list of subject summary and
#' the calculated target path; \code{snapshot_project} returns the path to
#' the compiled project report in 'HTML'.
#' @examples
#'
#' \dontrun{
#'
#' # Run in parallel
#' ravepipeline::with_rave_parallel({
#'
#'   snapshot_project("demo")
#'
#' })
#'
#' }
#'
NULL

#' @rdname snapshot_project
#' @export
snapshot_subject <- function(x, target_path = NULL, quick = FALSE) {
  subject <- restore_subject_instance(x)

  if(length(target_path) != 1) {
    target_path <- file_path(subject$project$group_path("project_overview"), "snapshot", subject$subject_code)
  }

  target_abspath <- dir_create2(target_path)

  # brain object
  brain <- rave_brain(subject)
  if(length(brain)) {
    brain$set_electrode_values()
    widget <- brain$plot(controllers = list("Display Data" = "LabelPrefix"))
    threeBrain::save_brain(widget, file.path(target_abspath, "viewer.html"))
  }


  # electrode table
  electrode_table <- tryCatch({subject$get_electrode_table()}, error = function(e) { NULL })
  cnames <- names(electrode_table)
  cnames <- cnames[
    grepl("_[xyz]$", cnames) | grepl("T1[RAS]", cnames) |
      cnames %in% c("DistanceShifted", "DistanceToPial")
  ]
  electrode_table_rows <- snapshot_table(
    x = electrode_table,
    target_path = file_path(target_abspath, "electrode_table.html"),
    digit_cnames = cnames,
    digits = 1,
    caption = "Electrode coordinates: T1RAS are T1w defined RAS coordinate; Coord_xyz are tk-registered surface RAS",
    title = sprintf("Electrode Coordinates - %s", subject$subject_id)
  )

  # Epochs
  epoch_table_rows <- structure(
    names = subject$epoch_names,
    lapply(subject$epoch_names, function(epoch_name) {
      tryCatch(
        {
          epoch_table <- subject$meta_data(meta_type = "epoch", meta_name = epoch_name, strict = TRUE)
          cnames <- "Time"
          cnames <- cnames[cnames %in% names(epoch_table)]
          epoch_table_rows <- snapshot_table(
            x = epoch_table,
            target_path = file_path(target_abspath, sprintf("epoch-%s.html", epoch_name)),
            digit_cnames = cnames,
            digits = 3,
            title = sprintf("Epoch %s - %s", epoch_name, subject$subject_id)
          )
          epoch_table_rows
        },
        error = function(e) {
          0L
        }
      )
    })
  )

  # References
  reference_table_rows <- structure(
    names = subject$reference_names,
    lapply(subject$reference_names, function(reference_name) {
      tryCatch(
        {
          reference_table <- subject$meta_data(meta_type = "references",
                                               meta_name = reference_name,
                                               strict = TRUE)
          snapshot_table(
            x = reference_table,
            target_path = file_path(target_abspath, sprintf("reference-%s.html", reference_name)),
            title = sprintf("Reference %s - %s", reference_name, subject$subject_id)
          )
        },
        error = function(e) {
          0L
        }
      )
    })
  )

  # validation
  if(quick) {
    validation_method <- "basic"
  } else {
    validation_method <- "normal"
  }
  validation_results <- validate_subject(subject, method = validation_method)

  snapshots <- list(
    subject = subject$subject_id,
    # brain = brain,
    meta = list(
      electrodes = electrode_table_rows,
      electrode_table = electrode_table,
      epochs = epoch_table_rows,
      blocks = subject$blocks,
      references = reference_table_rows,
      target_path = target_abspath
    ),
    validation = validation_results
  )

  saveRDS(snapshots, file = file_path(target_abspath, "snapshot.rds"))

  invisible(snapshots)
}

#' @rdname snapshot_project
#' @export
snapshot_project <- function(x, target_path = NULL, template_subjects = NULL, quick = FALSE) {
  # x <- "demo"
  # target_path = tempfile()

  project <- ravecore::as_rave_project(x)
  if(length(target_path) != 1) {
    target_path <- file_path(project$group_path("project_overview"), "snapshot")
  }

  # target_path will be the project folder, e.g. cache/build/demo/
  target_path <- dir_create2(target_path)

  # find all subjects
  subject_codes <- project$subjects()

  # Prepare for the template subject
  if(!length(template_subjects)) {
    template_subjects <- ravepipeline::raveio_getopt("threeBrain_template_subject",
                                                     default = "N27")
  }
  template_paths <- lapply(template_subjects, ensure_threeBrain_template)
  template_subjects <- template_subjects[!vapply(template_paths, is.null, FALSE)]


  # Get subject object
  brain_list <- lapply(
    subject_codes,
    function(subject_code) {
      if (!is.character(subject_code) ||
          is.na(subject_code) ||
          !nzchar(subject_code) || startsWith(subject_code, "_")) {
        return(NULL)
      }
      ravecore <- asNamespace("ravecore")
      subject <- ravecore$new_rave_subject(project_name = project,
                                           subject_code = subject_code,
                                           strict = FALSE)
      target_path_subject <- ravecore$file_path(target_path, subject$subject_code)
      if(!quick || !file_exists(target_path_subject, "snapshot.rds")) {
        snapshot <- ravecore$snapshot_subject(subject, target_path = target_path_subject, quick = quick)
      }

      brain <- ravecore$rave_brain(subject)
      if (is.null(brain)) {
        return(NULL)
      }
      brain$set_electrode_values()
      return(brain)
    }
  )

  # Build template brain
  lapply(template_subjects, function(template_subject) {
    template_brain <- threeBrain::merge_brain(
      brain_list,
      template_subject = template_subject,
      template_surface_types = c("pial", "smoothwm", "inflated", "sphere", "sphere.reg")
    )
    # save cache
    viewer <- template_brain$plot(controllers = list("Display Data" = "[Subject]"))
    threeBrain::save_brain(viewer, file_path(target_path, sprintf("group_viewer-%s.html", template_subject)))
  })

  # Save project snapshot
  saveRDS(list(
    project_name = project$name,
    subject_codes = subject_codes,
    template_subjects = template_subjects,
    timestamp = Sys.time()
  ), file = file_path(target_path, "snapshot.rds"))

  # Now (re)build project report
  report_rmdpath <- file_path(target_path, "project-report.rmd")
  file.copy(
    system.file("reports", "project-snapshot.rmd", package = "ravecore"),
    report_rmdpath, overwrite = TRUE, recursive = FALSE
  )
  file.copy(
    system.file("reports", "project-snapshot_styles.css", package = "ravecore"),
    file_path(target_path, "project-snapshot_styles.css"), overwrite = TRUE, recursive = FALSE
  )

  rmarkdown <- asNamespace("rmarkdown")
  rmarkdown$render(
    input = report_rmdpath,
    output_dir = target_path,
    clean = TRUE,
    knit_root_dir = target_path
  )
  return(file_path(target_path, "project-report.html"))
}

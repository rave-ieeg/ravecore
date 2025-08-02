import_electrode_table <- function (path, subject, use_fs = NA, dry_run = FALSE, ...) {

  subject <- restore_subject_instance(subject, strict = FALSE)
  electrodes <- subject$electrodes
  new_tbl <- utils::read.csv(path, stringsAsFactors = FALSE, ...)
  nms <- names(new_tbl)
  if (!"Electrode" %in% nms) {
    ravepipeline::logger("`import_electrode_table` cannot find column `Electrode` (case-sensitive).", level = "fatal")
  }
  if(length(electrodes)) {
    # subject has electrode channels set
    new_tbl <- new_tbl[new_tbl$Electrode %in% electrodes, ]
  }
  electrodes2 <- electrodes[!electrodes %in% new_tbl$Electrode]
  if (length(electrodes2)) {
    ravepipeline::logger(
      level = "warning",
      sprintf(
        "`import_electrode_table` has some issues with importing from:\n  %s\nThe number of electrodes in file does not match with what's in subject `%s`\n  In the table file: %s\n  In the subject, %s\nAppending electrodes with blank rows",
        path, subject$subject_id, deparse_svec(new_tbl$Electrode),
        deparse_svec(electrodes)
      )
    )

    new_items <- data.frame(
      Electrode = electrodes2, Label = "NoLabel",
      Coord_x = 0, Coord_y = 0, Coord_z = 0,
      T1R = 0, T1A = 0, T1S = 0,
      MNI305_x = 0, MNI305_y = 0, MNI305_z = 0,
      MNI152_x = 0, MNI152_y = 0, MNI152_z = 0,
      Radius = 2, VertexNumber = -1,
      SignalType = subject$electrode_types[electrodes %in% electrodes2],
      SurfaceElectrode = FALSE, SurfaceType = "pial"
    )
    nms1 <- names(new_items)
    nms2 <- names(new_tbl)
    nms1 <- nms1[nms1 %in% nms2]
    nms2 <- nms2[!nms2 %in% nms1]
    for(nm in nms2) {
      if(is.character(new_tbl[[nm]])) {
        new_items[[nm]] <- ""
      } else if(is.numeric(new_tbl[[nm]])) {
        new_items[[nm]] <- 0
      } else {
        new_items[[nm]] <- NA
      }
    }
    new_items <- new_items[,names(new_tbl)]
    new_tbl <- rbind(new_tbl, new_items)
  }
  new_tbl <- new_tbl[order(new_tbl$Electrode), ]
  nms <- names(new_tbl)
  has_tkrRAS <- all(c("Coord_x", "Coord_y", "Coord_z") %in% nms)
  has_T1RAS <- all(c("T1R", "T1A", "T1S") %in% nms)
  has_mni305 <- all(c("MNI305_x", "MNI305_y", "MNI305_z") %in% nms)
  has_mni152 <- all(c("MNI152_x", "MNI152_y", "MNI152_z") %in% nms)
  if (!any(has_tkrRAS, has_T1RAS, has_mni305, has_mni152)) {
    ravepipeline::logger(
      level = 'warning',
      "`import_electrode_table`: No coordinates found. The coordinates are set to the origin. If you want to import coordinates. Please make sure to have at least one of the following coordinates in your file:\n  T1R, T1A, T1S (scanner T1 RAS)\n  Coord_x, Coord_y, Coord_z (FreeSurfer tkrRAS)\n  MNI305_x, MNI305_y, MNI305_z (MNI305 RAS)\n  MNI152_x, MNI152_y, MNI152_z (MNI152 RAS)\nImporting anyway..."
    )
    save_meta2(data = new_tbl, meta_type = "electrodes",
               project_name = subject$project_name,
               subject_code = subject$subject_code)
    ravepipeline::logger(
      level = 'info',
      use_glue = TRUE,
      "`import_electrode_table`: Done importing {subject$subject_id} - meta/electrodes.csv. However, the coordinates are blank."
    )
    return(invisible(NULL))
  }
  brain <- NULL
  if (!isFALSE(use_fs)) {
    brain <- rave_brain(subject = subject)
  }
  has_brain <- !is.null(brain)
  if (!has_brain) {
    if (use_fs) {
      ravepipeline::logger(
        level = "fatal",
        "`import_electrode_table`: `use_fs=TRUE` but FreeSurfer is absent."
      )
    }
    ravepipeline::logger(
      level = "warning",
      "`import_electrodes`: FreeSurfer files are missing. Save the electrodes with minimal editing"
    )
    save_meta2(data = new_tbl, meta_type = "electrodes",
               project_name = subject$project_name,
               subject_code = subject$subject_code)
    return(invisible(NULL))
  }
  if (!has_tkrRAS) {
    ravepipeline::logger(
      level = "info",
      "FreeSurfer tkrRAS are not detected. Trying to calculate using T1, MNI305, or MNI152. \nOrder: T1 > MNI305 > MNI152.\n"
    )
    if (has_T1RAS) {
      ravepipeline::logger("T1 RAS detected! T1 -> tkrRAS")
      tmp <- new_tbl[, c("T1R", "T1A", "T1S")]
      tmp <- t(cbind(data.matrix(tmp), 1))
      invalids <- colSums(tmp == 0) == 3
      tkRAS <- t(brain$Torig %*% solve(brain$Norig) %*%
                   tmp)[, c(1, 2, 3)]
      tkRAS[invalids, ] <- 0
      new_tbl[, paste0("Coord_", c("x", "y", "z"))] <- tkRAS
    } else if (has_mni305) {
      ravepipeline::logger("MNI-305 detected! MNI-305 -> tkrRAS")
      tmp <- new_tbl[, paste0("MNI305_", c("x", "y", "z"))]
      tmp <- t(cbind(data.matrix(tmp), 1))
      invalids <- colSums(tmp == 0) == 3
      tkRAS <- t(brain$Torig %*% solve(brain$Norig) %*%
                   solve(brain$xfm) %*% tmp)[, c(1, 2, 3)]
      tkRAS[invalids, ] <- 0
      new_tbl[, paste0("Coord_", c("x", "y", "z"))] <- tkRAS
    } else if (has_mni152) {
      ravepipeline::logger("MNI-152 detected! MNI-152 -> tkrRAS")
      tmp <- new_tbl[, paste0("MNI152_", c("x", "y", "z"))]
      tmp <- t(cbind(data.matrix(tmp), 1))
      invalids <- colSums(tmp == 0) == 3
      tkRAS <- t(brain$Torig %*% solve(brain$Norig) %*%
                   solve(brain$xfm) %*% solve(MNI305_to_MNI152) %*%
                   tmp)[, c(1, 2, 3)]
      tkRAS[invalids, ] <- 0
      new_tbl[, paste0("Coord_", c("x", "y", "z"))] <- tkRAS
    } else {
      stop("Unexpected error. Please report to RAVE developers")
    }
  }
  tkRAS <- t(cbind(data.matrix(new_tbl[, paste0("Coord_", c("x", "y", "z"))]), 1))
  invalids <- colSums(tkRAS == 0) == 3
  if (!has_T1RAS) {
    T1 <- t(brain$Norig %*% solve(brain$Torig) %*% tkRAS)[, c(1, 2, 3)]
    T1[invalids, ] <- 0
    new_tbl[, c("T1R", "T1A", "T1S")] <- T1
    ravepipeline::logger("T1 has been generated from tkrRAS")
  }
  if (!has_mni305) {
    mni305 <- t(brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*%
                  tkRAS)[, c(1, 2, 3)]
    mni305[invalids, ] <- 0
    new_tbl[, c("MNI305_x", "MNI305_y", "MNI305_z")] <- mni305
    ravepipeline::logger("MNI-305 has been generated from tkrRAS")
  }
  if (!has_mni152) {
    mni305 <- t(cbind(data.matrix(new_tbl[, c("MNI305_x",
                                              "MNI305_y", "MNI305_z")]), 1))
    mni152 <- t(MNI305_to_MNI152 %*% mni305)[, c(1, 2, 3)]
    mni152[invalids, ] <- 0
    new_tbl[, c("MNI152_x", "MNI152_y", "MNI152_z")] <- mni152
    ravepipeline::logger("MNI-152 has been generated from MNI-305")
  }
  if (is.null(new_tbl$Label)) {
    new_tbl$Label <- "NoLabel"
  }
  if (is.null(new_tbl$SignalType)) {
    new_tbl$SignalType <- subject$electrode_types
  }
  brain$set_electrodes(new_tbl)
  new_tbl2 <- tryCatch({
    brain$calculate_template_coordinates()
  }, error = function(e){
    new_tbl
  })
  # Reorder
  nms1 <- c("Electrode", "Coord_x", "Coord_y", "Coord_z", "Label", "SignalType")
  nms <- names(new_tbl2)
  nms1 <- nms1[nms1 %in% nms]
  nms2 <- nms[!nms %in% nms1]
  new_tbl2 <- new_tbl2[, c(nms1, nms2)]
  if( dry_run ) {
    return(new_tbl2)
  }
  save_meta2(new_tbl2, meta_type = "electrodes",
             project_name = subject$project_name,
             subject_code = subject$subject_code)
  invisible(new_tbl2)
}

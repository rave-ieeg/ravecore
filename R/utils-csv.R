safe_read_csv <- function(file, header = TRUE, sep = ',',
         colClasses = NA, skip = 0, quote = "\"",
         ..., stringsAsFactors = FALSE){

  s <- readLines(file, n = skip + 1, ok = TRUE)

  # Reach EOF
  if(length(s) != skip+1){ return(data.frame()) }

  # Parse headers
  s <- s[skip+1]
  s <- strsplit(s, sep)[[1]]

  s <- gsub(quote, "", x = s, fixed = TRUE)

  # If length(s) == 1
  if(length(s) == 1){
    colClasses <- colClasses[1]
    return(utils::read.csv(file = file, header = header, sep = sep,
                           colClasses = colClasses, skip = skip,
                           quote = quote, ..., stringsAsFactors = stringsAsFactors))
  }


  if(!header || s[[1]] != ''){
    col_class <- rep(NA, length(s))
    col_class[seq_along(colClasses)] <- colClasses
    return(utils::read.csv(file = file, header = header, sep = sep,
                           colClasses = col_class, skip = skip,
                           quote = quote, ..., stringsAsFactors = stringsAsFactors))
  }else{
    # first blank header will be rownames
    col_class <- rep(NA, length(s))
    col_class[seq_along(colClasses) + 1] <- colClasses
    dat <- utils::read.csv(file = file, header = header, sep = sep,
                           colClasses = col_class, skip = skip,
                           quote = quote, ..., stringsAsFactors = stringsAsFactors)
    row.names(dat) <- dat[,1]
    dat <- dat[,-1]
    return(dat)
  }

  utils::read.csv(file = file, header = header, sep = sep,
                  colClasses = colClasses, skip = skip,
                  quote = quote, ..., stringsAsFactors = stringsAsFactors)
}

load_electrodes_csv <- function(file) {

  tbl <- safe_read_csv(file)
  tbl_names <- names(tbl)
  if(!'Label' %in% tbl_names){
    tbl$Label <- NA
  }
  na_labels <- is.na(tbl$Label)
  if(any(na_labels)){
    tbl$Label[na_labels] <- paste0('Unlabeled', seq_len(sum(na_labels)))
  }

  if(!"LabelPrefix" %in% tbl_names) {
    tbl$LabelPrefix <- gsub("[ 0-9_-]+$", "", tbl$Label)
  }

  if(!'LocationType' %in% tbl_names){
    tbl$LocationType <- "iEEG"
  }

  if(any(!tbl$LocationType %in% c(LOCATION_TYPES, ""))){
    usp <- unique(tbl$LocationType[!tbl$LocationType %in% LOCATION_TYPES])
    ravepipeline::logger("Unsupported electrode location type(s) found: ",
                         paste(usp, collapse = ", "),
            ". Alter these electrode types to `iEEG`. If you see this warning, it is most likely the `LocationType` column in `electrodes.csv` (subject meta folder) contains invalid elements. I have corrected for you, however, please double-check the file as my correction might be wrong.",
            level = "warning")
    tbl$LocationType[!tbl$LocationType %in% LOCATION_TYPES] <- "iEEG"
  }
  tbl$LocationType[is.na(tbl$LocationType) | tbl$LocationType == ""] <- "iEEG"

  return(tbl)
}


safe_write_csv <- function(x, file, ..., quiet = FALSE){
  if(file.exists(file)){
    oldfile <- gsub('(\\.csv|)$',
                     strftime(Sys.time(), '_[%Y%m%d_%H%M%S].csv'),
                     file,
                     ignore.case = TRUE)
    if(!quiet){
      ravepipeline::logger(sprintf("Renaming file %s\n  >> %s", file, oldfile), level = "info")
    }
    file_move(file, oldfile)
  }
  args <- list(...)
  rn <- isTRUE(args$row.names)

  tryCatch({
    utils::write.csv(x, file, ...)
  }, error = function(e){

    call_args <- list(x = quote(x), file = file)
    call_args$col.names <- if (is.logical(rn) && !rn) TRUE else NA
    call_args$sep <- ","
    call_args$dec <- "."
    call_args$qmethod <- "double"

    do.call(utils::write.table, call_args)
  })
  normalizePath(file)
}



#' Convert electrode table
#' @param subject 'RAVE' subject
#' @param space suggested coordinate space, notice this argument might not be
#' supported when \code{'FreeSurfer'} reconstruction is missing.
#' @returns A list of table in data frame and a list of meta information
#' @examples
#'
#' # Run `install_subject("DemoSubject")` first!
#' \dontrun{
#'
#' convert_electrode_table_to_bids(
#'   "demo/DemoSubject",
#'   space = "ScanRAS"
#' )
#'
#' }
#'
#' @export
convert_electrode_table_to_bids <- function(
    subject,
    space = c("ScanRAS", "MNI305", "fsnative")) {
  subject <- restore_subject_instance(subject)

  space <- match.arg(space)
  electrode_table <- subject$get_electrode_table()

  if( !is.data.frame(electrode_table) ) {
    stop(sprintf("Subject [%s] does not have valid electrode table.", subject$subject_id))
  }

  # https://bids-specification.readthedocs.io/en/stable/appendices/coordinate-systems.html#ieeg-specific-coordinate-systems

  re <- data.frame(
    name = electrode_table$Label
  )
  brain <- rave_brain(subject)
  switch(
    space,
    "ScanRAS" = {
      if(all(c("T1R", "T1A", "T1S") %in% names(electrode_table))) {
        re$x <- electrode_table$T1R
        re$y <- electrode_table$T1A
        re$z <- electrode_table$T1S
      } else if( is.null(brain) ) {
        re$x <- electrode_table$Coord_x
        re$y <- electrode_table$Coord_y
        re$z <- electrode_table$Coord_z
        space <- "fsnative"
      } else {
        tkr_ras <- cbind(data.matrix(electrode_table[, paste0("Coord_", c("x", "y", "z"))]), 1)
        invalid <- rowSums((tkr_ras[, c(1,2,3)])^2) == 0
        ras <- t(brain$Norig %*% solve(brain$Torig) %*% t(tkr_ras))[, c(1,2,3)]
        ras[invalid, ] <- 0
        re$x <- ras[,1]
        re$y <- ras[,2]
        re$z <- ras[,3]
      }
    },
    "MNI305" = {
      if(all(c("MNI305_x", "MNI305_y", "MNI305_z") %in% names(electrode_table))) {
        re$x <- electrode_table$MNI305_x
        re$y <- electrode_table$MNI305_y
        re$z <- electrode_table$MNI305_z
      } else if( is.null(brain) ) {
        re$x <- electrode_table$Coord_x
        re$y <- electrode_table$Coord_y
        re$z <- electrode_table$Coord_z
        space <- "fsnative"
      } else {
        tkr_ras <- cbind(data.matrix(electrode_table[, paste0("Coord_", c("x", "y", "z"))]), 1)
        invalid <- rowSums((tkr_ras[, c(1,2,3)])^2) == 0
        ras <- t(brain$xfm %*% brain$Norig %*% solve(brain$Torig) %*% t(tkr_ras))[, c(1,2,3)]
        ras[invalid, ] <- 0
        re$x <- ras[,1]
        re$y <- ras[,2]
        re$z <- ras[,3]
      }
    },
    {
      re$x <- electrode_table$Coord_x
      re$y <- electrode_table$Coord_y
      re$z <- electrode_table$Coord_z
      space <- "fsnative"
    }
  )

  return(list(
    table = re,
    meta = list(
      iEEGCoordinateSystem = ifelse(space == "fsnative", "Other", space),
      iEEGCoordinateUnits = "mm",
      iEEGCoordinateSystemDescription = list(
        "fsnative" = "FreeSurfer tk-registered RAS",
        "MNI305" = "Linear registered MNI-305 RAS",
        "ScanRAS" = "T1-weighted RAS"
      )[[space]],
      iEEGCoordinateProcessingReference = "Zhengjia Wang, John F. Magnotti, Xiang Zhang and Michael S. Beauchamp
eNeuro 19 October 2023, 10 (10) ENEURO.0328-23.2023. https://doi.org/10.1523/ENEURO.0328-23.2023"
    )
  ))
}

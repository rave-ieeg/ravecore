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

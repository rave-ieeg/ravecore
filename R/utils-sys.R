get_os <- function () {
  os <- R.version$os
  if (grepl("^darwin", os, ignore.case = TRUE)) {
    return("darwin")
  }
  if (grepl("^linux", os, ignore.case = TRUE)) {
    return("linux")
  }
  if (grepl("^solaris", os, ignore.case = TRUE)) {
    return("solaris")
  }
  if (grepl("^win", os, ignore.case = TRUE)) {
    return("windows")
  }
  if (grepl("^(emscr|wasm)", os, ignore.case = TRUE)) {
    return("emscripten")
  }
  return("unknown")
}


#' @noRd
#' @title Check whether current session is running under 'RStudio' environment
#' @description
#' To prevent certain actions or warn the users for alternative methods, as
#' 'RStudio' might crash unexpectedly (when loading 'Python' or forking processes)
#'
rstudio_main_session <- function(
    os = c("darwin", "linux", "solaris", "windows", "emscripten")) {
  if(!interactive()) { return(FALSE) }
  if(!isTRUE(get_os() %in% os)) { return(FALSE) }
  if(!identical(.Platform$GUI, "RStudio")) { return(FALSE) }
  ravepipeline <- asNamespace("ravepipeline")
  ravepipeline$rs_avail(child_ok = FALSE, shiny_ok = TRUE)
}

shiny_is_running <- function() {
  asNamespace("ravepipeline")$shiny_is_running()
}


rstudio_viewer <- function(missing = TRUE) {

  if( rstudio_main_session() ) {
    # This means rstudioapi has been installed
    return(asNamespace("rstudioapi")$viewer)
  }

  return(missing)

}

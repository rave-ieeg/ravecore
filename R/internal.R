# Ported from ravepipeline

fastmap2 <- function(missing_default = NULL) {
  asNamespace("ravepipeline")$fastmap2(missing_default = missing_default)
}

list_to_fastmap2 <- function(li, map = NULL) {
  asNamespace("ravepipeline")$list_to_fastmap2(li, map)
}

fastqueue2 <- function(init = 20L, missing_default = NULL) {
  asNamespace("ravepipeline")$fastqueue2(init = init, missing_default = missing_default)
}

load_yaml <- function(file, ..., map = NULL){
  re <- ieegio::io_read_yaml(con = file, ...)
  if(!inherits(map, 'fastmap2')){
    map <- fastmap2()
  }
  for(nm in names(re)){
    if(nm != ''){
      map[[nm]] <- re[[nm]]
    }
  }
  map
}

ns_ravecore <- function() {
  asNamespace("ravecore")
}

save_yaml <- function(x, file, ..., sorted = FALSE){
  ieegio::io_write_yaml(x = x, con = file, ..., sorted = sorted)
}


load_json <- function(con, ..., map = NULL){

  s <- readLines(con)
  args <- list(...)

  s <- trimws(paste(s, collapse = ""))

  re <- list()
  if(nzchar(s)) {
    ok <- FALSE
    tryCatch({
      withRestarts({
        re <- jsonlite::unserializeJSON(s)
        ok <- TRUE
      }, abort = function() {})
    }, error = function(e) {})
    if(!ok) {
      tryCatch({
        withRestarts({
          args$txt <- s
          re <- do.call(jsonlite::fromJSON, args)
          ok <- TRUE
        }, abort = function() {})
      }, error = function(e) {})
    }
  }

  if(is.list(re) && !is.null(map)){
    if(is.environment(map)){
      list2env(re, map)
    } else if (inherits(map, 'fastmap2')){
      list_to_fastmap2(re, map)
    } else if (inherits(map, "fastmap")){
      map$mset(.list = re)
    }
  }
  re
}

save_json <- function(x, con = stdout(), ...,
         digits = ceiling(-log10(.Machine$double.eps)),
         pretty = TRUE, serialize = TRUE) {

  if(serialize){
    s <- jsonlite::serializeJSON(x, digits = digits, pretty = pretty)
  } else {
    s <- jsonlite::toJSON(x, digits = digits, pretty = pretty, ...)
  }

  writeLines(s, con)
  invisible()
}


cache_root <- function(check = FALSE){
  re <- ravepipeline::raveio_getopt(key = 'tensor_temp_path', default = NULL)
  if(!length(re)){
    re <- '~/rave_data/cache_dir/'
    ravepipeline::raveio_setopt(key = 'tensor_temp_path', value = re)
  }
  if(check){
    re <- dir_create2(re)
  }
  re
}

using_netdrive <- function(){
  if(ravepipeline::raveio_getopt("using_netdrive", FALSE)){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


shiny_is_running <- function() {
  # I don't even want to even suggest shiny
  if(!package_installed("shiny")) { return(FALSE) }

  session <- call_pkg_fun("shiny", "getDefaultReactiveDomain", .if_missing = "none", .missing_default = NULL)
  if(is.null(session)) {
    return(FALSE)
  }

  return(TRUE)
}

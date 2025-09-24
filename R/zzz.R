rpymat_is_setup <- function () {
  return(dir.exists(rpymat::env_path()))
}

.ravecorepy <- local({

  ravecorepy_module <- NULL

  get_ravecorepy <- function(force = FALSE, error_if_missing = TRUE) {
    if(!force && inherits(ravecorepy_module, "python.builtin.module")) {
      return( ravecorepy_module )
    }
    if( !rpymat_is_setup() ) {
      if( error_if_missing ) {
        stop("Please configure environment first. Run the following command:\n  ravecore:::finalize_installa()")
      }
      return( NULL )
    }
    tryCatch({
      rpymat::ensure_rpymat(verbose = FALSE)
      reticulate <- asNamespace('reticulate')
      m <- reticulate$import_from_path(
        "ravecorepy",
        path = system.file("ravecorepy", package = "ravecore"),
        convert = FALSE,
        delay_load = FALSE
      )
      ravecorepy_module <<- m
      return( ravecorepy_module )
    }, error = function(e) {
      if( error_if_missing ) {
        stop(e)
      }
      return(NULL)
    })
  }

  clean_ravecorepy <- function() {
    ravecorepy_module <<- NULL
  }

  list(
    get = get_ravecorepy,
    clean = clean_ravecorepy
  )
})

load_ravecorepy <- .ravecorepy$get

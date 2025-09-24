rpymat_is_setup <- function (env_name = NA) {
  return(dir.exists(rpymat::env_path(env_name = env_name)))
}

# share with ieegio
check_py_flag <- function() {
  if(nchar(Sys.getenv("IEEGIO_NO_PYTHON", unset = "")) > 0) {
    stop("System environment 'IEEGIO_NO_PYTHON' is set, Python is disabled")
  }
}

ensure_py_package <- local({

  installed_pkgs_tbl <- NULL

  function(packages, python_ver = "auto", ...) {
    check_py_flag()
    if(!rpymat_is_setup()) {
      standalone <- !file.exists(rpymat::conda_bin())
      rpymat::configure_conda(python_ver = python_ver, force = TRUE, standalone = standalone)
    }
    rpymat::ensure_rpymat(verbose = FALSE)

    if(is.null(installed_pkgs_tbl) || !is.data.frame(installed_pkgs_tbl) || !all(packages %in% installed_pkgs_tbl$package)) {
      installed_pkgs_tbl <<- rpymat::list_pkgs()
    }

    packages <- packages[!packages %in% installed_pkgs_tbl$package]

    if(length(packages)) {
      tryCatch({
        rpymat::run_command("conda tos accept --channel https://repo.anaconda.com/pkgs/r --channel https://repo.anaconda.com/pkgs/main")
      }, error = function(e) {})
      rpymat::add_packages(packages, ...)
      installed_pkgs_tbl <<- rpymat::list_pkgs()
    }

    invisible()
  }
})


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
      ensure_py_package(c("spikeinterface", "mountainsort5"))

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

read_mat <- function(file, ram = TRUE, engine = c("r", "py")){
  engine <- match.arg(engine)
  file <- normalizePath(file, mustWork = TRUE, winslash = "/")
  # Check if the file is HDF5 format

  if(engine == "r") {
    if( ieegio::io_h5_valid(file) ){

      dset_names <- ieegio::io_h5_names(file)
      re <- sapply(dset_names, function(nm){
        y <- load_h5(file, name = nm, ram = ram)
        y
      }, simplify = FALSE, USE.NAMES = TRUE)
    }else{
      # re <- R.matlab::readMat(file)
      re <- ieegio::io_read_mat(con = file, method = "R.matlab")
    }
  } else {
    # use python
    rpymat::ensure_rpymat(verbose = FALSE, cache = TRUE)
    mat73 <- tryCatch({
      rpymat::import("mat73", convert = FALSE)
    }, error = function(e) {
      rpymat::add_packages("mat73")
      rpymat::import("mat73", convert = FALSE)
    })
    sio <- tryCatch({
      rpymat::import("scipy.io", convert = FALSE)
    }, error = function(e) {
      rpymat::add_packages("scipy")
      rpymat::import("scipy.io", convert = FALSE)
    })

    dat <- tryCatch({
      sio$loadmat(file_name = file)
    }, error = function(e) {
      mat73$loadmat(filename = file)
    })
    re <- fastmap2()
    for(nm in names(dat)) {
      re[[nm]] <- rpymat::py_to_r(dat[[nm]])
    }
  }

  re
}

read_mat2 <- function(file, ram = TRUE, engine = c("r", "py")){
  engine <- match.arg(engine)
  file <- normalizePath(file, mustWork = TRUE, winslash = "/")
  # Check if the file is HDF5 format

  if(engine == "r") {
    if( ieegio::io_h5_valid(file) ){

      dset_names <- ieegio::io_h5_names(file)
      re <- fastmap2()
      lapply(dset_names, function(nm){
        y <- load_h5(file, name = nm, ram = ram)
        nm_path <- strsplit(nm, "/")[[1]]
        d <- re
        for(ii in seq_along(nm_path)){
          nm <- nm_path[[ii]]
          if(ii != length(nm_path)){
            if(!inherits(d[[nm]], 'fastmap2')){
              d[[nm]] <- fastmap2()
            }
            d <- d[[nm]]
          } else {
            d[[nm]] <- y
          }
        }
        NULL
      })
    } else{
      re <- list_to_fastmap2(
        ieegio::io_read_mat(con = file, method = "R.matlab")
      )
    }
  } else {
    # use python
    rpymat::ensure_rpymat(verbose = FALSE, cache = TRUE)

    sio <- tryCatch({
      rpymat::import("scipy.io", convert = FALSE)
    }, error = function(e) {
      rpymat::add_packages("scipy")
      rpymat::import("scipy.io", convert = FALSE)
    })

    dat <- tryCatch({
      sio$loadmat(file)
    }, error = function(e) {
      mat73 <- tryCatch({
        rpymat::import("mat73", convert = FALSE)
      }, error = function(e) {
        rpymat::add_packages("mat73")
        rpymat::import("mat73", convert = FALSE)
      })
      mat73$loadmat(file)
    })
    re <- fastmap2()

    iterate <- function(x, prefix = "") {
      if(!inherits(x, "python.builtin.dict")) {
        re[[ gsub("^/", "", prefix) ]] <- rpymat::py_to_r(x)
        return()
      }
      nms <- names(x)
      for(nm in nms) {
        Recall(x[[nm]], prefix = sprintf("%s/%s", prefix, nm))
      }
    }
    iterate(dat)
  }
  re
}

load_h5 <- function(file, name, read_only = TRUE, ram = FALSE, quiet = FALSE){
  return(ieegio::io_read_h5(
    file = file,
    name = name,
    read_only = read_only,
    ram = ram,
    quiet = quiet
  ))
}

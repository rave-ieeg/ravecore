#' Export data frame to different common formats
#' @description
#' Stores and load data in various of data format. See 'Details' for limitations.
#'
#' @param x data table to be saved to \code{file}
#' @param file file to store the data
#' @param format data storage format, default is \code{'auto'} (infer from the
#' file extension); other choices are \code{'csv'}, \code{'csv.zip'}, \code{'h5'},
#' \code{'fst'}, \code{'json'}, \code{'rds'}, \code{'yaml'}
#' @param ... parameters passed to other functions
#' @returns The normalized path for \code{export_table}, and a
#' \code{\link[data.table]{data.table}} for \code{import_table}
#'
#' @details
#' The format \code{'rds'}, \code{'h5'}, \code{'fst'}, \code{'json'}, and
#' \code{'yaml'} try to preserve the first-level column attributes. Factors
#' will be preserved in these formats. Such property does not exist in
#' \code{'csv'}, \code{'csv.zip'} formats.
#'
#' Open-data formats are \code{'h5'}, \code{'csv'}, \code{'csv.zip'},
#' \code{'json'}, \code{'yaml'}. These formats require the table elements to
#' be native types (numeric, character, factor, etc.).
#'
#' \code{'rds'}, \code{'h5'}, and \code{'fst'} can store large data sets.
#' \code{'fst'} is the best choice is performance and file size are the major
#' concerns. \code{'rds'} preserves all the properties of the table.
#'
#'
#' @examples
#'
#' x <- data.table::data.table(
#'   a = rnorm(10),
#'   b = letters[1:10],
#'   c = 1:10,
#'   d = factor(LETTERS[1:10])
#' )
#'
#' f <- tempfile(fileext = ".csv.zip")
#'
#' export_table(x = x, file = f)
#'
#' y <- import_table(file = f)
#'
#' str(x)
#' str(y)
#'
#' # clean up
#' unlink(f)
#'
#'
#' @export
export_table <- function(x, file, format = c("auto", "csv", "csv.zip", "tsv", "h5", "fst", "json", "rds", "yaml"), ...) {

  format <- match.arg(format)
  if(format == "auto") {
    format <- path_ext(tolower(file))
    if(!format %in% c("csv", "zip", "csv.zip", "tsv", "h5", "fst", "json", "rds", "yaml", "yml")) {
      stop("ravecore::export_table: cannot infer `format` from file name: ", basename(file))
    }
    if(format == "zip") {
      format <- "csv.zip"
    } else if (format == "yml") {
      format <- "yaml"
    }
  }

  switch(
    format,
    "csv" = {
      data.table::fwrite(x = x, file = file, ...)
    },
    "csv.zip" = {
      tf <- tempfile(
        pattern = paste0('data_export_', format(Sys.time(), "%b_%d_%Y_%H_%M")),
        fileext = '.csv'
      )
      zf <- tempfile(fileext = ".csv.zip")
      on.exit({
        unlink(tf)
        unlink(zf)
      })
      data.table::fwrite(x = x, file = tf, ...)
      utils::zip(zipfile = zf, files = tf, extras='-j')
      file_move(zf, file)
    },
    "tsv" = {
      utils::write.table(x = x, file = file, append = FALSE, na = "n/a",
                         row.names = FALSE, fileEncoding = "UTF-8")
    },
    "fst" = {
      # fst::write_fst(x = x, path = file, compress = 99)
      ieegio::io_write_fst(x = x, con = file, compress = 99)
    },
    "h5" = {
      h5file <- tempfile(fileext = '.h5')
      on.exit({
        unlink(h5file)
      })
      # recursive method for saving to h5 file
      save_list_to_h5 <- function(v, nm) {
        if(is.list(v)) {
          re <- mapply(save_list_to_h5, v,
                       paste0(nm, '/', names(v)), USE.NAMES = TRUE, SIMPLIFY = FALSE)
          structure(list(do.call("c", re)), names = nm)
          return()
        } else {
          ctype <- ifelse(is.numeric(v), 'numeric', 'character')
          ieegio::io_write_h5(x=v, file=h5file, name=nm,
                  level=ifelse(is.numeric(v), 4, 9),
                  ctype=ctype,
                  replace = TRUE, quiet = TRUE)
          return(structure(list(ctype), names = nm))
        }
      }

      nms <- names(x)
      mapply(save_list_to_h5, x, nms)
      attri <- structure(names = nms, lapply(nms, function(nm) {
        attributes(x[[nm]])
      }))

      meta <- list(
        columns = nms,
        attributes = attri
      )

      # save meta information
      ieegio::io_write_h5(
        x = jsonlite::serializeJSON(meta),
        file = h5file,
        name = "__meta__",
        replace = TRUE,
        ctype = "character",
        quiet = TRUE
      )

      file_move(h5file, file)
    },
    "rds" = {
      saveRDS(object = x, file = file, ...)
    },
    "json" = {
      nms <- names(x)
      attri <- structure(names = nms, lapply(nms, function(nm) {
        attributes(x[[nm]])
      }))

      jsonlite::write_json(
        x = list(
          data = x,
          columns = nms,
          attributes = attri
        ),
        path = file,
        dataframe = "row",
        factor = "string",
        na = "null",
        auto_unbox = TRUE,
        ...
      )
    },
    "yaml" = {
      nms <- names(x)
      attri <- structure(names = nms, lapply(nms, function(nm) {
        attributes(x[[nm]])
      }))
      save_yaml(x = list(
        data = as.list(x),
        columns = nms,
        attributes = attri
      ), file = file, ...)
    }
  )

  invisible(normalizePath(file, mustWork = FALSE))

}


#' @rdname export_table
#' @export
import_table <- function(file, format = c("auto", "csv", "csv.zip", "tsv", "h5", "fst", "json", "rds", "yaml"), ...) {
  format <- match.arg(format)
  if(format == "auto") {
    format <- path_ext(tolower(file))
    if(!format %in% c("csv", "zip", "csv.zip", "tsv", "h5", "fst", "json", "rds", "yaml", "yml")) {
      stop("ravecore::import_table: cannot infer `format` from file name: ", basename(file))
    }
    if(format == "zip") {
      format <- "csv.zip"
    } else if (format == "yml") {
      format <- "yaml"
    }
  }

  re <- switch(
    format,
    "csv" = {
      data.table::fread(file = file, ...)
    },
    "csv.zip" = {
      td <- dir_create2(tempfile())
      on.exit({ unlink(td, recursive = TRUE) })
      utils::unzip(zipfile = file, exdir = td, overwrite = TRUE)
      f <- list.files(td, pattern = "\\.csv$", ignore.case = TRUE)
      if(length(f) != 1) {
        stop("ravecore::import_table: csv.zip file contains no csv or more than one csv files.")
      }
      data.table::fread(file = file.path(td, f), ...)
    },
    "tsv" = {
      args <- list(...)
      args$file <- file
      if(!length(args$na)) { args$na <- "n/a" }
      if(!length(args$header)) { args$header <- TRUE }
      do.call(utils::read.table, args)
    },
    "fst" = {
      # fst::read_fst(path = file, as.data.table = TRUE, ...)
      ieegio::io_read_fst(con = file, method = "data_table", ...)
    },
    "h5" = {
      nms <- ieegio::io_h5_names(file = file)
      column_attrs <- list()
      if("__meta__" %in% nms) {
        tryCatch({
          meta <- load_h5(file = file, name = "__meta__", ram = TRUE, quiet = TRUE)
          meta <- jsonlite::unserializeJSON(meta)
          if("columns" %in% names(meta)) {
            column_attrs <- meta$columns
            nms <- meta$columns
          } else {
            nms <- nms[!nms %in% "__meta__"]
          }

          if(is.list(meta$attributes)) {
            column_attrs <- meta$attributes
          }
        }, error = function(e) {
          nms <<- nms[!nms %in% "__meta__"]
        })
      }
      re <- structure(lapply(nms, function(nm) {
        re <- as.vector(load_h5(file = file, name = nm, ram = TRUE, quiet = TRUE))
        attri <- column_attrs[[ nm ]]
        if(length(attri) && is.list(attri)) {
          if("factor" %in% attri$class) {
            re <- factor(re, levels = attri$levels)
          }
          attributes(re) <- attri
        }
        re
      }), names = nms)
      data.table::as.data.table(re)
    },
    "rds" = {
      re <- readRDS(file = file, ...)
      stopifnot2(is.data.frame(re), msg = "The object stored at RDS file is not a data.frame")
      data.table::as.data.table(re)
    },
    "json" = {
      re <- jsonlite::read_json(path = file, simplifyVector = TRUE)
      dat <- re$data
      stopifnot2(is.data.frame(dat), msg = "The object stored at JSON file does not contain a data.frame")
      attrs <- re$attributes
      cols <- re$columns
      for(nm in cols) {
        attri <- attrs[[nm]]
        if(length(attri) && is.list(attri)) {
          if('factor' %in% attri$class) {
            dat[[nm]] <- factor(dat[[nm]], levels = attri$levels)
          }
          attributes(dat[[nm]]) <- attri
        }
      }
      data.table::as.data.table(dat)
    },
    "yaml" = {
      re <- ieegio::io_read_yaml(con = file, ...)
      dat <- re$data
      attrs <- re$attributes
      cols <- re$columns
      re <- structure(names = cols, lapply(cols, function(nm) {
        re <- unlist(dat[[nm]])
        attri <- attrs[[nm]]
        if(length(attri) && is.list(attri)) {
          if('factor' %in% attri$class) {
            re <- factor(re, levels = attri$levels)
          }
          attributes(re) <- attri
        }
        re
      }))
      data.table::as.data.table(re)
    }
  )

  re

}



package_installed <- function (pkgs, all = FALSE) {
  re <- sapply(pkgs, function(p) {
    system.file("", package = p) != ""
  })
  if (all) {
    re <- all(re)
  }
  re
}


call_pkg_fun <- function(package, f_name, ...,
         .if_missing = c("error", "warning", "none"),
         .missing_default = NULL,
         .call_pkg_function = TRUE) {

  stopifnot(length(package) == 1)

  if(!package_installed(package)) {
    .if_missing <- match.arg(.if_missing)
    switch(
      .if_missing,
      "error" = {
        ravepipeline::logger("Package ", sQuote(package), " is missing.", level = "fatal")
      },
      "warning" = {
        ravepipeline::logger("Package ", sQuote(package), " is missing.", level = "warning")
      },
      {}
    )
    return(.missing_default)
  }

  ns <- asNamespace(package)
  fun <- ns[[f_name]]

  if( .call_pkg_function ) {
    if(!is.function(fun)) {
      .if_missing <- match.arg(.if_missing)
      switch(
        .if_missing,
        "error" = {
          ravepipeline::logger("Package ", sQuote(package), " does not have function ", sQuote(f_name), level = "fatal")
        },
        "warning" = {
          ravepipeline::logger("Package ", sQuote(package), " does not have function ", sQuote(f_name), level = "warning")
        },
        {}
      )
      return(.missing_default)
    }

    return(fun(...))
  } else {
    return(fun)
  }

}

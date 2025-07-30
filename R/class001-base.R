format.generic <- S7::new_external_generic("base", "format", "x")

#' @name rave_path
#' @title Find file paths based on storage
#' @description
#' A generic function that will be dispatched to using different method based
#' on input \code{x}
#'
#' @param x R object
#' @param storage storage type, different options based on different R objects
#' @param ... additional arguments passed to dispatched method
#' @export
rave_path <- S7::new_generic("rave_path", "x", fun = function(x, storage = NULL, ...) {
  S7::S7_dispatch()
})




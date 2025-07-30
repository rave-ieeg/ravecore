#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom bidsr BIDSProject
#' @importFrom methods getGeneric
#' @importFrom methods setMethod
#' @importFrom ravepipeline RAVESerializable
#' @importFrom R6 R6Class
## usethis namespace: end
NULL

.onLoad <- function(...) {
  S7::methods_register()
}

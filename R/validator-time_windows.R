#' @title Validate time windows to be used
#' @description Make sure the time windows are valid intervals and returns
#' a reshaped window list
#' @param time_windows vectors or a list of time intervals
#' @returns A list of time intervals (ordered, length of 2)
#' @examples
#'
#'
#' # Simple time window
#' validate_time_window(c(-1, 2))
#'
#' # Multiple windows
#' validate_time_window(c(-1, 2, 3, 5))
#'
#' # alternatively
#' validate_time_window(list(c(-1, 2), c(3, 5)))
#' validate_time_window(list(list(-1, 2), list(3, 5)))
#'
#'
#' \dontrun{
#'
#' # Incorrect usage (will raise errors)
#'
#'   # Invalid interval (length must be two for each intervals)
#'   validate_time_window(list(c(-1, 2, 3, 5)))
#'
#'   # Time intervals must be in ascending order
#'   validate_time_window(c(2, 1))
#'
#' }
#'
#'
#' @export
validate_time_window <- function(time_windows){
  if(!is.list(time_windows)){
    time_windows <- unlist(time_windows)
    if(length(time_windows) %% 2 != 0){
      stop("`time_windows` must be a list of time intervals (length 2)")
    }
    time_windows <- matrix(time_windows, nrow = 2, byrow = FALSE)
    time_windows <- as.list(as.data.frame(time_windows))
    time_windows <- unname(time_windows)
  }
  time_windows <- lapply(time_windows, function(x){
    if(is.list(x)){
      x <- unlist(x)
    }
    if(length(x) != 2){
      stop("`time_windows` must be a list of time intervals (length 2)")
    }
    if(!is.numeric(x)){
      stop("`time_windows` must be a list of 'numerical' time intervals")
    }
    if(anyNA(x)){
      stop("`time_windows` cannot contain NAs")
    }
    if(x[[1]] > x[[2]]){
      stop("`time_windows` time intervals must be in ascending order")
    }
    x
  })
  time_windows
}

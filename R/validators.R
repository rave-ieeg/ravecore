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

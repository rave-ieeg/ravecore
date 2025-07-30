drop_nulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

stopifnot2 <- function(..., msg = 'Condition not satisfied'){
  if(!all(c(...))){
    stop(msg)
  }
}

is.blank <- function(x) {
  !nzchar(x)
}

is_valid_ish <- function(x, min_len = 1, max_len = Inf, mode = NA,
         na = TRUE, blank = FALSE, all = FALSE){
  if(!is.na(mode) && mode(x) != mode){
    return(FALSE)
  }
  len <- length(x)
  if(len < min_len || len > max_len){
    return(FALSE)
  }
  if(len == 0){
    return(TRUE)
  }
  if(na){
    if(!all && any(is.na(x))){
      return(FALSE)
    }
    if(all && all(!is.na(x))){
      return(FALSE)
    }
  }
  if(blank && mode(x) == 'character'){
    if(!all && any(is.blank(x), na.rm = TRUE)){
      return(FALSE)
    }
    if(all && all(!is.blank(x), na.rm = TRUE)){
      return(FALSE)
    }
  }
  return(TRUE)
}


deparse_svec <- function (
    nums,
    connect = "-",
    concatenate = TRUE,
    collapse = ",",
    max_lag = 1
)
{
  nums <- nums[is.finite(nums)]
  if (length(nums) == 0) {
    return("")
  }
  alag <- seq_len(max(1, max_lag))
  nums <- sort(unique(nums))
  lg <- c(NA, nums)[seq_len(length(nums))]
  ind <- nums - lg
  ind[1] <- 0
  ind2 <- c(ind[-1], -1)
  re <- apply(cbind(nums[!ind %in% alag], nums[!ind2 %in% alag]), 1, function(x) {
    if (x[1] == x[2]) {
      as.character(x[1])
    }
    else {
      paste(x, collapse = connect)
    }
  })
  if (concatenate) {
    re <- paste(re, collapse = collapse)
  }
  re
}


parse_svec <- function (text, sep = ",", connect = "-:|", sort = FALSE, unique = TRUE) {
  connect <- unique(unlist(strsplit(connect, "")))
  connect[connect %in% c("|", ":", "~")] <- paste0("\\", connect[connect %in% c("|", ":", "~")])
  if ("-" %in% connect) {
    connect <- c(connect[connect != "-"], "-")
  }
  connect <- paste(connect, collapse = "")
  if (length(text) != 1) {
    text <- paste(text, collapse = sep)
  }
  if (length(text) == 0 || !nzchar(trimws(text))) {
    return(NULL)
  }
  if (is.numeric(text)) {
    if (unique) {
      text <- unique(text)
    }
    if (sort) {
      text <- sort(text)
    }
    return(text)
  }
  s <- unlist(strsplit(text, sep, perl = TRUE))
  s <- trimws(s)
  s <- s[s != ""]
  s <- s[grepl(sprintf("^[0-9\\ %s]+$", connect), s)]
  re <- NULL
  for (ss in s) {
    if (grepl(sprintf("[%s]", connect), ss)) {
      ss <- unlist(strsplit(ss, sprintf("[%s]", connect),
                            perl = TRUE))
      ss <- trimws(ss)
      ss <- ss[grepl("^[0-9]+$", ss)]
      ss <- as.numeric(ss)
      ss <- ss[!is.na(ss)]
      if (length(ss) >= 2) {
        re <- c(re, (ss[1]:ss[2]))
      }
    }
    else {
      re <- c(re, as.numeric(ss))
    }
  }
  if (unique) {
    re <- unique(re)
  }
  if (sort) {
    re <- sort(re)
  }
  return(re)
}


rand_string <- function(length = 50){
  pid <- as.integer(Sys.getpid())
  now <- as.numeric(Sys.time() - as.POSIXlt(Sys.Date()), units = "secs")
  now <- sprintf("%.24f", now)
  now <- strsplit(now, "\\.")[[1]]
  now2 <- strsplit(now[[2]], "")[[1]]
  now <- as.integer(c(
    paste(now2[c(1,5,9,13,17,21) + 3], collapse = ""),
    paste(now2[c(1,5,9,13,17,21) + 2], collapse = ""),
    paste(now2[c(1,5,9,13,17,21) + 1], collapse = ""),
    paste(now2[c(1,5,9,13,17,21)], collapse = ""),
    now[[1]]
  ))
  now <- rev(as.integer(now))

  dict0 <- ravepipeline::digest(paste(pid, now), algo = "xxhash32", seed = pid)
  dict1 <- ravepipeline::digest(paste(pid, now, dict0), algo = "xxhash32", seed = now[[1]])
  dict2 <- ravepipeline::digest(paste(pid, now, dict1), algo = "murmur32", seed = sum(now))
  dict3 <- ravepipeline::digest(paste(pid, now, dict1, dict2), algo = "xxhash64",
                           seed = strtoi(sprintf("0x%s", substr(dict2, 1, 7))))

  dict <- strsplit(paste0(dict3, dict2, dict1), "")[[1]]
  # dict <- c(dict, letters, LETTERS, 0:9)

  paste(sample(dict, size = length, replace = TRUE), collapse = "")
  # c(dict1, dict2, dict3)
}

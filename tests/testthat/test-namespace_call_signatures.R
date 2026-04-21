# Static analysis: validate that all `ns$func(...)` calls (where ns is a
# package namespace loaded via asNamespace()) pass arguments that match
# the function's actual formals. R CMD check cannot detect these mismatches
# because `$` access on namespace environments bypasses standard checks.

test_that("namespace call signatures match function formals", {

  testthat::skip_on_cran()

  pkg_root <- normalizePath(testthat::test_path("../.."), winslash = "/")

  # Collect .R files from R/ and inst/, excluding adhoc/
  r_files <- c(
    list.files(file.path(pkg_root, "R"), pattern = "\\.[Rr]$",
               full.names = TRUE, recursive = TRUE),
    list.files(file.path(pkg_root, "inst"), pattern = "\\.[Rr]$",
               full.names = TRUE, recursive = TRUE)
  )
  r_files <- r_files[!grepl("/adhoc/", r_files)]

  # --- helpers ---------------------------------------------------------------

  # Look up a name in a package namespace; returns NULL if not found
  resolve_fn <- function(pkg, name) {
    if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
    tryCatch(get(name, envir = asNamespace(pkg)),
             error = function(e) NULL)
  }

  # Resolve R6 initialize method, walking the inheritance chain
  resolve_r6_init <- function(pkg, class_name) {
    cls <- resolve_fn(pkg, class_name)
    if (!inherits(cls, "R6ClassGenerator")) return(NULL)
    current <- cls
    for (d in seq_len(10L)) {
      if ("initialize" %in% names(current$public_methods)) {
        return(current$public_methods$initialize)
      }
      parent <- current$get_inherit()
      if (is.null(parent) || !inherits(parent, "R6ClassGenerator")) break
      current <- parent
    }
    NULL
  }

  # Given a position right after an opening "(", extract the substring up to
  # its matching ")" across lines. Returns NULL if unbalanced.
  extract_call_args_text <- function(lines, start_line, start_col) {
    depth <- 1L
    buf <- character(0L)
    li <- start_line
    ci <- start_col
    while (li <= length(lines)) {
      ln <- lines[[li]]
      chars <- substring(ln, ci, nchar(ln))
      for (j in seq_len(nchar(chars))) {
        ch <- substr(chars, j, j)
        if (ch == "(") {
          depth <- depth + 1L
        } else if (ch == ")") {
          depth <- depth - 1L
          if (depth == 0L) {
            buf <- c(buf, substr(chars, 1L, j - 1L))
            return(paste(buf, collapse = "\n"))
          }
        }
      }
      buf <- c(buf, chars)
      li <- li + 1L
      ci <- 1L
    }
    NULL
  }

  # Parse an argument string to extract named argument names.
  # Returns a list(names = character(), count = integer()).
  parse_arg_info <- function(args_text) {
    args_text <- trimws(args_text)
    if (!nzchar(args_text)) return(list(names = character(0L), count = 0L))
    # Wrap in a dummy function call so parse() gives us the AST
    dummy <- sprintf("f(%s)", args_text)
    parsed <- tryCatch(parse(text = dummy, keep.source = FALSE)[[1L]],
                       error = function(e) NULL)
    if (is.null(parsed)) return(NULL)
    args <- as.list(parsed)[-1L]
    arg_names <- names(args)
    if (is.null(arg_names)) arg_names <- rep("", length(args))
    list(names = arg_names, count = length(args))
  }

  # Validate call arguments against target function formals.
  # Returns character vector of error messages (empty if OK).
  check_args <- function(arg_info, target_fn, label, loc) {
    if (is.primitive(target_fn)) return(character(0L))
    fmls <- formals(target_fn)
    if (is.null(fmls)) fmls <- list()
    fnames <- names(fmls)

    # If formals include `...`, any extra arguments are valid
    if ("..." %in% fnames) return(character(0L))

    errs <- character(0L)

    # Check each named argument exists in formals (exact or unique prefix match)
    for (nm in arg_info$names[nzchar(arg_info$names)]) {
      if (!nm %in% fnames) {
        partial <- fnames[startsWith(fnames, nm)]
        if (length(partial) != 1L) {
          errs <- c(errs, sprintf(
            "%s: `%s()` unknown argument '%s'; formals: (%s)",
            loc, label, nm, paste(fnames, collapse = ", ")))
        }
      }
    }

    # Check total argument count does not exceed formal count
    if (arg_info$count > length(fnames)) {
      errs <- c(errs, sprintf(
        "%s: `%s()` passed %d args, max %d; formals: (%s)",
        loc, label, arg_info$count, length(fnames),
        paste(fnames, collapse = ", ")))
    }

    errs
  }

  # --- regex patterns --------------------------------------------------------

  # var <- asNamespace("pkg")
  re_assign <- '(\\w+)\\s*(<-|=|<<-)\\s*asNamespace\\(\\s*["\']([^"\']+)["\']\\s*\\)'
  # var$func(  OR  var$Class$method(
  # Capture: (var), (func_or_Class), optional ($method), and note column of "("
  re_call <- '\\b(\\w+)\\$(\\w+)(?:\\$(\\w+))?\\s*\\('
  # asNamespace("pkg")$func(
  re_inline <- 'asNamespace\\(\\s*["\']([^"\']+)["\']\\s*\\)\\$(\\w+)\\s*\\('

  # --- main scan -------------------------------------------------------------

  all_errors <- character(0L)

  for (fpath in r_files) {
    lines <- tryCatch(readLines(fpath, warn = FALSE),
                      error = function(e) character(0L))
    if (!length(lines)) next

    rel <- sub(paste0(pkg_root, "/"), "",
               normalizePath(fpath, winslash = "/"), fixed = TRUE)

    # Phase 1: build var -> pkg namespace map from assignments in this file
    ns_map <- list()
    for (i in seq_along(lines)) {
      m <- regmatches(lines[[i]], regexec(re_assign, lines[[i]]))[[1L]]
      if (length(m) >= 4L) {
        ns_map[[m[[2L]]]] <- m[[4L]]
      }
    }

    # Phase 2: find all ns$func( calls and validate
    for (i in seq_along(lines)) {
      ln <- lines[[i]]

      # Skip comment-only lines
      stripped <- trimws(ln)
      if (startsWith(stripped, "#")) next

      # --- Case A & C: var$func( or var$Class$method( ---
      matches <- gregexpr(re_call, ln, perl = TRUE)[[1L]]
      if (matches[[1L]] > 0L) {
        for (mi in seq_along(matches)) {
          pos <- matches[[mi]]
          mlen <- attr(matches, "match.length")[[mi]]
          match_text <- substr(ln, pos, pos + mlen - 1L)

          parts <- regmatches(match_text,
                              regexec('(\\w+)\\$(\\w+)(?:\\$(\\w+))?\\s*\\(', match_text))[[1L]]
          var_name <- parts[[2L]]
          name2 <- parts[[3L]]
          name3 <- if (nzchar(parts[[4L]])) parts[[4L]] else ""

          if (!var_name %in% names(ns_map)) next
          pkg <- ns_map[[var_name]]

          target <- NULL
          label <- NULL

          if (!nzchar(name3)) {
            # var$func(...)
            obj <- resolve_fn(pkg, name2)
            if (is.function(obj)) {
              target <- obj
              label <- sprintf("%s::%s", pkg, name2)
            }
          } else if (name3 == "new") {
            # var$Class$new(...)
            fn <- resolve_r6_init(pkg, name2)
            if (is.function(fn)) {
              target <- fn
              label <- sprintf("%s::%s$new", pkg, name2)
            }
          }

          if (!is.function(target)) next

          # Extract argument text starting after the "("
          open_col <- pos + mlen
          args_text <- extract_call_args_text(lines, i, open_col)
          if (is.null(args_text)) next

          arg_info <- parse_arg_info(args_text)
          if (is.null(arg_info)) next

          loc <- sprintf("%s:%d", rel, i)
          errs <- check_args(arg_info, target, label, loc)
          if (length(errs)) all_errors <- c(all_errors, errs)
        }
      }

      # --- Case B: asNamespace("pkg")$func( ---
      matches_inline <- gregexpr(re_inline, ln, perl = TRUE)[[1L]]
      if (matches_inline[[1L]] > 0L) {
        for (mi in seq_along(matches_inline)) {
          pos <- matches_inline[[mi]]
          mlen <- attr(matches_inline, "match.length")[[mi]]
          match_text <- substr(ln, pos, pos + mlen - 1L)

          parts <- regmatches(match_text,
                              regexec('asNamespace\\(\\s*["\']([^"\']+)["\']\\s*\\)\\$(\\w+)\\s*\\(',
                                      match_text))[[1L]]
          pkg <- parts[[2L]]
          func_name <- parts[[3L]]

          obj <- resolve_fn(pkg, func_name)
          if (!is.function(obj)) next

          label <- sprintf("%s::%s", pkg, func_name)
          open_col <- pos + mlen
          args_text <- extract_call_args_text(lines, i, open_col)
          if (is.null(args_text)) next

          arg_info <- parse_arg_info(args_text)
          if (is.null(arg_info)) next

          loc <- sprintf("%s:%d", rel, i)
          errs <- check_args(arg_info, obj, label, loc)
          if (length(errs)) all_errors <- c(all_errors, errs)
        }
      }
    }
  }

  if (length(all_errors)) {
    fail(paste(c(
      sprintf("Found %d namespace $ call signature mismatch(es):",
              length(all_errors)),
      all_errors
    ), collapse = "\n  "))
  }
  expect_true(length(all_errors) == 0L)
})

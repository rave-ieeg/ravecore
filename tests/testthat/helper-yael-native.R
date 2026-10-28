# Helpers for the tests of the native 'YAEL' pipeline: a sandbox for the RAVE
# directories, mocks that keep the tests away from 'Python' and from the real
# template folders, and toy images with a known ground truth. Nothing here
# reads real images.

# ---- Sandbox and mocks ---------------------------------------------------------

# Skips unless 'ravetools' has every function and argument of the native
# pipeline ('ravetools' 0.3.3 or later; released 0.3.2 and the development
# builds before 0.3.2.3 lack some of them)
skip_if_no_native_yael <- function() {
  testthat::skip_if_not(
    yael_native_ravetools_ok(),
    message = "ravetools lacks the native YAEL functions (needs ravetools >= 0.3.3)"
  )
}

# Skips with 'testthat' older than 3.1.7, which has no
# `local_mocked_bindings()` (the package only asks for 'testthat' 3.0.0 or
# later). The helpers below that mock functions call it first, and so do the
# tests that mock functions themselves
skip_if_old_testthat <- function() {
  testthat::skip_if_not(
    utils::packageVersion("testthat") >= "3.1.7",
    message = "needs testthat >= 3.1.7 (local_mocked_bindings)"
  )
}

# Points the RAVE directories (raw, data, BIDS, templates and cache) to empty
# temporary folders for the rest of the calling frame, without saving the
# settings, and runs `ravepipeline::lapply_jobs` in this process (one worker:
# child processes would load the real settings); also disables the 'Python'
# support of 'ieegio' (`IEEGIO_NO_PYTHON`), so that nothing can start or
# install 'Python'. Restores the previous settings and environment variable
# and removes the folders on exit. Fails (instead of running against the real
# folders) when a setting cannot be redirected, as in a 'RAVE' job session,
# whose settings come from `options(rave.use_settings)`. Call it before
# building any subject or process
local_yael_sandbox <- function(env = parent.frame()) {
  root <- tempfile(pattern = "ravecore-yael-sandbox-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  folders <- c(raw_data_dir = "raw_dir", data_dir = "data_dir",
               bids_data_dir = "bids_dir", mni_template_root = "templates",
               tensor_temp_path = "cache_dir")
  values <- list(max_worker = 1L)
  keys <- c(names(folders), names(values))
  settings <- get(".settings", envir = asNamespace("ravepipeline"))
  old_temp <- settings$..temp
  was_set <- vapply(keys, .subset2(settings, "has"), FALSE)
  no_python <- Sys.getenv("IEEGIO_NO_PYTHON", unset = NA)
  restore <- function() {
    # temporary values as before; keys that did not exist are removed again
    settings$..temp <- if (is.null(old_temp)) list() else old_temp
    for (key in keys[!was_set]) {
      .subset2(settings, "remove")(key)
    }
    if (is.na(no_python)) {
      Sys.unsetenv("IEEGIO_NO_PYTHON")
    } else {
      Sys.setenv(IEEGIO_NO_PYTHON = no_python)
    }
    unlink(root, recursive = TRUE)
  }
  do.call(base::on.exit, list(as.call(list(restore)), TRUE, FALSE), envir = env)
  Sys.setenv(IEEGIO_NO_PYTHON = "1")
  refuse <- function(key) {
    stop(sprintf(paste(
      "local_yael_sandbox: cannot redirect RAVE option `%s` (RAVE job session?);",
      "refusing to run against the real settings"), key), call. = FALSE)
  }
  for (key in names(folders)) {
    path <- normalizePath(file.path(root, folders[[key]]), winslash = "/",
                          mustWork = FALSE)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    ravepipeline::raveio_setopt(key, path, .save = FALSE)
    # fail closed: the option must now point into the sandbox
    current <- ravepipeline::raveio_getopt(key, default = NULL)
    if (length(current) != 1 || is.na(current) ||
        !identical(normalizePath(current, winslash = "/", mustWork = FALSE), path)) {
      refuse(key)
    }
  }
  for (key in names(values)) {
    ravepipeline::raveio_setopt(key, values[[key]], .save = FALSE)
    if (!isTRUE(all.equal(ravepipeline::raveio_getopt(key, default = NULL),
                          values[[key]]))) {
      refuse(key)
    }
  }
  invisible(root)
}

# Replacement of `call_rpyants` for the tests: the pure 'R' template metadata
# of 'rpyANTs' passes through; `ensure_template` (which may download or copy
# files into the template folders), `load_rpyants` and anything else (which
# would start 'Python') stop. `ensure_template` may be given as a function
yael_rpyants_guard <- function(ensure_template = NULL) {
  function(.name, ...) {
    if (identical(.name, "ensure_template") && is.function(ensure_template)) {
      return(ensure_template(...))
    }
    if (!.name %in% c("BUILTIN_TEMPLATES", "template_urls")) {
      stop(sprintf("`call_rpyants(\"%s\")` is not allowed in the native YAEL tests", .name))
    }
    asNamespace("rpyANTs")[[.name]]
  }
}

# Installs the guard above for the rest of the calling frame
local_yael_rpyants_guard <- function(ensure_template = NULL, env = parent.frame()) {
  skip_if_old_testthat()
  testthat::local_mocked_bindings(
    call_rpyants = yael_rpyants_guard(ensure_template),
    .package = "ravecore", .env = env
  )
}

# A toy template folder in the shape returned by `yael_native_template()`
yael_toy_template_info <- function(root, name = "mni_icbm152_nlin_asym_09a") {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  info <- asNamespace("rpyANTs")$template_urls[[name]]
  list(
    name = name,
    camel = info$name,
    coord_sys = info$coord_sys,
    root = root,
    t1 = file.path(root, "T1.nii.gz"),
    mask = file.path(root, "T1_brainmask.nii.gz"),
    atropos = file.path(root, sprintf("atropos_%d.nii.gz", 0:7)),
    fs_dir = file.path(root, "fs"),
    atlas_dir = file.path(root, "atlases")
  )
}

# Makes `yael_native_template()` return the toy template under the name of
# `mni_icbm152_nlin_asym_09a` (or `name`), and guards `call_rpyants`, for the
# rest of the calling frame
local_yael_toy_template <- function(root, name = "mni_icbm152_nlin_asym_09a",
                                    env = parent.frame()) {
  skip_if_old_testthat()
  info <- yael_toy_template_info(root, name = name)
  testthat::local_mocked_bindings(
    yael_native_template = function(template_name = rpyants_builtin_templates()) {
      info
    },
    call_rpyants = yael_rpyants_guard(),
    .package = "ravecore", .env = env
  )
  invisible(info)
}

# Records the calls of `ravetools::register_volume3d` for the rest of the
# calling frame: the type, number of channels, weights, whether masks are
# given (`masked`: both; `source_masked`, `target_masked`), the initial
# transform, some settings (the convergence stop included), and the resulting
# transform, and with
# `keep_sources` the moving and fixed image of each channel (`sources`,
# `targets`, lists) and the masks (`source_mask`, `target_mask`). The
# registration itself runs unchanged
local_yael_registration_spy <- function(keep_sources = FALSE, env = parent.frame()) {
  skip_if_old_testthat()
  calls <- new.env(parent = emptyenv())
  calls$list <- list()
  register <- ravetools::register_volume3d
  spy <- function(source, target, ...) {
    args <- list(...)
    result <- register(source, target, ...)
    calls$list[[length(calls$list) + 1]] <- list(
      type = args$type,
      channels = if (is.list(source)) length(source) else 1L,
      weights = args$weights,
      masked = !is.null(args$target_mask) && !is.null(args$source_mask),
      source_masked = !is.null(args$source_mask),
      target_masked = !is.null(args$target_mask),
      init_transform = args$init_transform,
      shrink_factors = args$shrink_factors,
      iterations = args$iterations,
      syn_iterations = args$syn_iterations,
      syn_grad_step = args$syn_grad_step,
      syn_sigma = args$syn_sigma,
      syn_convergence = args$syn_convergence,
      syn_convergence_window = args$syn_convergence_window,
      transform = result$transform,
      sources = if (keep_sources) { if (is.list(source)) source else list(source) },
      source_mask = if (keep_sources) { args$source_mask },
      targets = if (keep_sources) { if (is.list(target)) target else list(target) },
      target_mask = if (keep_sources) { args$target_mask }
    )
    result
  }
  testthat::local_mocked_bindings(register_volume3d = spy,
                                  .package = "ravetools", .env = env)
  function() { calls$list }
}

# ---- Toy images ------------------------------------------------------------------

# 'RAS' coordinates (N x 3) of the voxel centers of a grid
yael_toy_grid <- function(dim, vox2ras) {
  ijk <- as.matrix(expand.grid(seq_len(dim[1]) - 1, seq_len(dim[2]) - 1,
                               seq_len(dim[3]) - 1))
  t(vox2ras[1:3, 1:3] %*% t(ijk) + vox2ras[1:3, 4])
}

# Toy head in template 'RAS' (mm): tissue code of every point. Labels follow
# `deep_atropos`: 1 CSF, 2 gray matter, 3 white matter, 4 deep gray matter,
# 5 brain stem, 6 cerebellum; 7 is the skull, 8 the scalp (with a nose-like
# bump), 0 the background. The ventricles and deep gray matter are asymmetric
# so that a rigid motion is well determined
yael_toy_codes <- function(p) {
  ellipsoid <- function(center, radius) {
    ((p[, 1] - center[1]) / radius[1])^2 + ((p[, 2] - center[2]) / radius[2])^2 +
      ((p[, 3] - center[3]) / radius[3])^2 <= 1
  }
  code <- integer(nrow(p))
  code[ellipsoid(c(0, 2, 0), c(46, 56, 44)) | ellipsoid(c(0, 50, -10), c(8, 9, 10))] <- 8L
  code[ellipsoid(c(0, 2, 0), c(42, 52, 40))] <- 7L
  brain <- ellipsoid(c(0, 2, 2), c(38, 48, 36))
  code[brain] <- 1L
  code[ellipsoid(c(0, 2, 2), c(34, 44, 32))] <- 2L
  code[ellipsoid(c(1, 4, 4), c(27, 36, 25))] <- 3L
  code[brain & (ellipsoid(c(-14, -2, 2), c(5, 8, 5)) |
                  ellipsoid(c(13, 0, 3), c(5, 7, 5)))] <- 4L
  code[brain & (ellipsoid(c(-7, 4, 8), c(4, 13, 5)) |
                  ellipsoid(c(7, 2, 7), c(3.5, 11, 4.5)))] <- 1L
  code[brain & ellipsoid(c(0, -6, -20), c(6, 6, 11))] <- 5L
  code[brain & ellipsoid(c(2, -30, -14), c(18, 11, 9))] <- 6L
  code
}

# Intensity of each tissue code (background first) for two contrasts
yael_toy_contrast <- function(contrast = c("T1", "T2", "CT")) {
  contrast <- match.arg(contrast)
  switch(
    contrast,
    # bg, CSF, GM, WM, deep GM, brain stem, cerebellum, skull, scalp
    "T1" = c(0, 30, 70, 100, 85, 92, 78, 12, 120),
    "T2" = c(0, 100, 60, 40, 50, 45, 55, 5, 35),
    # Hounsfield units: air, soft tissues, bright skull; the deep gray matter
    # and the brain stem are bright (like calcifications) so that the head
    # has asymmetric landmarks that mutual information can see
    "CT" = c(-1000, 0, 40, 25, 400, 150, 60, 1200, 40)
  )
}

# Mean intensity over 2 x 2 x 2 sub-voxel points (partial volume); `to_head`
# maps 'RAS' points of the grid to template 'RAS'
yael_toy_image <- function(dim, vox2ras, contrast, to_head = identity) {
  centers <- yael_toy_grid(dim, vox2ras)
  table <- yael_toy_contrast(contrast)
  offsets <- as.matrix(expand.grid(c(-0.25, 0.25), c(-0.25, 0.25), c(-0.25, 0.25)))
  value <- numeric(nrow(centers))
  for (ii in seq_len(nrow(offsets))) {
    shift <- as.vector(vox2ras[1:3, 1:3] %*% offsets[ii, ])
    p <- sweep(centers, 2, shift, "+")
    value <- value + table[yael_toy_codes(to_head(p)) + 1L]
  }
  array(value / nrow(offsets), dim)
}

# Gaussian smoothing along the three axes (`sigma` in voxels); each kernel is
# normalized, so maps that sum to one still do
yael_toy_blur <- function(x, sigma) {
  dm <- dim(x)
  kernel <- function(n) {
    d <- outer(seq_len(n), seq_len(n), "-")
    k <- exp(-d^2 / (2 * sigma^2)) * (abs(d) <= ceiling(3 * sigma))
    k / rowSums(k)
  }
  y <- kernel(dm[1]) %*% matrix(x, dm[1])
  y <- aperm(array(y, dm), c(2, 1, 3))
  y <- kernel(dm[2]) %*% matrix(y, dm[2])
  y <- aperm(array(y, dm[c(2, 1, 3)]), c(2, 1, 3))
  y <- matrix(y, dm[1] * dm[2]) %*% t(kernel(dm[3]))
  array(y, dm)
}

yael_toy_write <- function(x, vox2ras, path) {
  ieegio::write_volume(ieegio::as_ieegio_volume(x, vox2ras = vox2ras), path)
  path
}

# Writes a toy template folder: `T1.nii.gz` (2 mm, 52 x 62 x 50 voxels), a
# binary `T1_brainmask.nii.gz`, and `atropos_0.nii.gz` (labels) to
# `atropos_7.nii.gz` (background and the six tissue probability maps). The
# 'atropos' maps are on a grid twice as fine with the same origin, like
# `mni_icbm152_nlin_asym_09a`, or on the 'T1' grid with `shared_grid`;
# `zero_cerebellum` writes an empty cerebellum map; with `intensity_mask`,
# the brain mask holds the 'T1' intensities inside the brain (about 20 to
# 100) instead of ones, like the mask of `fsaverage`
yael_toy_template <- function(root, shared_grid = FALSE, zero_cerebellum = FALSE,
                              intensity_mask = FALSE, seed = 1L) {
  set.seed(seed)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  dim <- c(52L, 62L, 50L)
  vox2ras <- diag(c(2, 2, 2, 1))
  vox2ras[1:3, 4] <- -(dim - 1)

  t1 <- yael_toy_image(dim, vox2ras, "T1")
  t1 <- abs(t1 + stats::rnorm(length(t1), sd = 0.5))
  yael_toy_write(t1, vox2ras, file.path(root, "T1.nii.gz"))
  codes <- yael_toy_codes(yael_toy_grid(dim, vox2ras))
  mask <- array((codes >= 1 & codes <= 6) * 1L, dim)
  if (intensity_mask) {
    mask <- mask * t1
  }
  yael_toy_write(mask, vox2ras, file.path(root, "T1_brainmask.nii.gz"))

  if (shared_grid) {
    fine_dim <- dim
    fine_vox2ras <- vox2ras
  } else {
    fine_dim <- dim * 2L
    fine_vox2ras <- diag(c(1, 1, 1, 1))
    fine_vox2ras[1:3, 4] <- vox2ras[1:3, 4]
  }
  labels <- yael_toy_codes(yael_toy_grid(fine_dim, fine_vox2ras))
  labels[labels > 6] <- 0L
  yael_toy_write(array(labels, fine_dim), fine_vox2ras,
                 file.path(root, "atropos_0.nii.gz"))
  # 1.5 mm smoothing of the one-hot maps of the background and the tissues
  sigma <- 1.5 / abs(fine_vox2ras[1, 1])
  for (label in 0:6) {
    prob <- yael_toy_blur(array((labels == label) * 1, fine_dim), sigma)
    if (zero_cerebellum && label == 6) {
      prob[] <- 0
    }
    yael_toy_write(prob, fine_vox2ras,
                   file.path(root, sprintf("atropos_%d.nii.gz", label + 1L)))
  }
  list(root = normalizePath(root, winslash = "/"), dim = dim, vox2ras = vox2ras,
       t1 = file.path(root, "T1.nii.gz"),
       mask = file.path(root, "T1_brainmask.nii.gz"),
       atropos = file.path(root, sprintf("atropos_%d.nii.gz", 0:7)))
}

# Rotation (degrees) about axis 1, 2 or 3 as a 4 x 4 matrix
yael_toy_rotation <- function(axis, degree) {
  a <- degree * pi / 180
  m <- diag(4)
  i <- setdiff(1:3, axis)
  m[i, i] <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2)
  m
}

# The smooth non-linear part `w(p)` of the toy subject (native 'RAS', mm):
# three Gaussian bumps of up to about 2.5 mm, which only a deformable
# registration can recover
yael_toy_bumps <- function(p) {
  bump <- function(center, amplitude, s = 14) {
    outer(exp(-rowSums(sweep(p, 2, center)^2) / (2 * s^2)), amplitude)
  }
  bump(c(12, 10, 8), c(2, -1, 0.5)) + bump(c(-14, -12, 0), c(-1, 1.5, -1)) +
    bump(c(0, 22, -8), c(0.5, 0, 1.5))
}

# The known map from native 'RAS' to template 'RAS' of the toy subject:
# q = B(p + w(p)), with `B` a rigid motion that also scales the head (the
# native head is 5 percent smaller) and `w` the bumps above
yael_toy_native_to_template <- function(p) {
  B <- yael_toy_rotation(3, 8) %*% yael_toy_rotation(1, 5) %*% diag(c(1, 1, 1, 1) / c(0.95, 0.95, 0.95, 1))
  B[1:3, 4] <- c(4, -6, 3)
  q <- p + yael_toy_bumps(p)
  t(B[1:3, 1:3] %*% t(q) + B[1:3, 4])
}

# Errors (mm) of points mapped to the template, by the full normalization and
# by its affine part only, where the bumps move the native points by more
# than `threshold` mm: the deformable stage must do clearly better there
yael_toy_bump_errors <- function(native_points, template_points, affine_points,
                                 threshold = 1.5) {
  truth <- yael_toy_native_to_template(native_points)
  bumped <- sqrt(rowSums(yael_toy_bumps(native_points)^2)) > threshold
  error <- function(points) {
    stats::median(sqrt(rowSums((points[bumped, , drop = FALSE] -
                                  truth[bumped, , drop = FALSE])^2)))
  }
  c(n = sum(bumped), full = error(template_points), affine = error(affine_points))
}

# Writes a toy native subject seen through `yael_toy_native_to_template`:
# `T1.nii.gz` (left-to-right flipped 2 x 2.2 x 2 mm grid, raw scanner units,
# with a smooth multiplicative bias and noise), `T2.nii.gz` (another contrast
# on an oblique 2.5 mm grid, as a realigned image), `T2_partial.nii.gz` (the
# same contrast covering only the top of the head) and `T2_moved.nii.gz` (the
# 'T2' scan in a scanner space moved by the rigid transform `t2_motion`, from
# native 'RAS' to the 'RAS' of that scan, like a scan of another session).
# Returns the paths, the grid, `t2_motion`, and the true labels of every 'T1'
# voxel (`deep_atropos` labels)
yael_toy_native <- function(root, seed = 2L) {
  set.seed(seed)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  dim <- c(52L, 60L, 50L)
  vox2ras <- diag(c(-2, 2.2, 2, 1))
  vox2ras[1:3, 4] <- c(47, -60, -52)
  p <- yael_toy_grid(dim, vox2ras)
  bias <- exp(0.3 * p[, 1] / 50 - 0.2 * (p[, 2] / 60)^2 + 0.2 * p[, 3] / 50)
  t1 <- yael_toy_image(dim, vox2ras, "T1", yael_toy_native_to_template)
  t1 <- abs(6 * t1 * bias + stats::rnorm(length(t1), sd = 8))
  yael_toy_write(array(t1, dim), vox2ras, file.path(root, "T1.nii.gz"))
  truth <- yael_toy_codes(yael_toy_native_to_template(p))
  truth[truth > 6] <- 0L

  t2_dim <- c(42L, 52L, 40L)
  t2_vox2ras <- yael_toy_rotation(1, 10) %*% diag(c(2.5, 2.5, 2.5, 1))
  t2_vox2ras[1:3, 4] <- -t2_vox2ras[1:3, 1:3] %*% ((t2_dim - 1) / 2) + c(-3, 4, -2)
  t2 <- yael_toy_image(t2_dim, t2_vox2ras, "T2", yael_toy_native_to_template)
  t2 <- abs(4 * t2 + stats::rnorm(length(t2), sd = 4))
  yael_toy_write(array(t2, t2_dim), t2_vox2ras, file.path(root, "T2.nii.gz"))

  partial_dim <- c(42L, 48L, 14L)
  partial_vox2ras <- diag(c(2.5, 2.5, 2.5, 1))
  partial_vox2ras[1:3, 4] <- c(-51.25, -58.75, 8)
  partial <- yael_toy_image(partial_dim, partial_vox2ras, "T2", yael_toy_native_to_template)
  yael_toy_write(array(partial, partial_dim), partial_vox2ras,
                 file.path(root, "T2_partial.nii.gz"))

  # the T2 grid carried along by the motion (8.3 mm root-mean-square over the
  # brain), so the voxels see the same head as above
  t2_motion <- yael_toy_rotation(3, -7) %*% yael_toy_rotation(2, 5)
  t2_motion[1:3, 4] <- c(-5, -3, 6)
  moved_vox2ras <- t2_motion %*% t2_vox2ras
  moved_to_native <- solve(t2_motion)
  moved <- yael_toy_image(t2_dim, moved_vox2ras, "T2", function(p) {
    yael_toy_native_to_template(
      t(moved_to_native[1:3, 1:3] %*% t(p) + moved_to_native[1:3, 4]))
  })
  moved <- abs(4 * moved + stats::rnorm(length(moved), sd = 4))
  yael_toy_write(array(moved, t2_dim), moved_vox2ras, file.path(root, "T2_moved.nii.gz"))

  list(
    root = normalizePath(root, winslash = "/"), dim = dim, vox2ras = vox2ras,
    t1 = file.path(root, "T1.nii.gz"),
    t2 = file.path(root, "T2.nii.gz"),
    t2_partial = file.path(root, "T2_partial.nii.gz"),
    t2_moved = file.path(root, "T2_moved.nii.gz"),
    t2_motion = t2_motion,
    truth = array(truth, dim)
  )
}

# The noise-free 'T2' intensity of the toy subject at the voxels of the
# native 'T1' grid, to check which 'T2' image a registration was given
yael_toy_native_t2 <- function(native) {
  yael_toy_image(native$dim, native$vox2ras, "T2", yael_toy_native_to_template)
}

# Writes a CT-like image of the toy subject (integers like Hounsfield units,
# bright skull) on its own 2.2 x 2.2 x 2.4 mm grid, in a scanner space moved
# by the rigid transform `A` (native 'RAS' to CT 'RAS'); returns the path
yael_toy_ct <- function(path, A) {
  dim <- c(48L, 56L, 44L)
  vox2ras <- diag(c(2.2, 2.2, 2.4, 1))
  vox2ras[1:3, 4] <- -vox2ras[1:3, 1:3] %*% ((dim - 1) / 2) + A[1:3, 4]
  ct_to_native <- solve(A)
  to_head <- function(p) {
    yael_toy_native_to_template(t(ct_to_native[1:3, 1:3] %*% t(p) + ct_to_native[1:3, 4]))
  }
  ct <- round(yael_toy_image(dim, vox2ras, "CT", to_head))
  storage.mode(ct) <- "integer"
  yael_toy_write(array(ct, dim), vox2ras, path)
}

# Template labels (`deep_atropos` labels, 0 outside the brain) at the voxels of
# a grid of the template
yael_toy_template_labels <- function(dim, vox2ras) {
  labels <- yael_toy_codes(yael_toy_grid(dim, vox2ras))
  labels[labels > 6] <- 0L
  array(labels, dim)
}

# Toy data built once per test session; `name` selects the variant
yael_toy_cache <- new.env(parent = emptyenv())
yael_toy_dataset <- function(name = c("template", "template_shared", "native")) {
  name <- match.arg(name)
  if (is.null(yael_toy_cache[[name]])) {
    root <- file.path(tempdir(), sprintf("ravecore-yael-toy-%s", name))
    unlink(root, recursive = TRUE)
    yael_toy_cache[[name]] <- switch(
      name,
      "template" = yael_toy_template(root),
      "template_shared" = yael_toy_template(root, shared_grid = TRUE,
                                            zero_cerebellum = TRUE,
                                            intensity_mask = TRUE),
      "native" = yael_toy_native(root)
    )
  }
  yael_toy_cache[[name]]
}

# Overlap (Dice) of each label between two label arrays
yael_toy_dice <- function(seg1, seg2, labels = 1:6) {
  vapply(labels, function(label) {
    2 * sum(seg1 == label & seg2 == label) / (sum(seg1 == label) + sum(seg2 == label))
  }, 0.0)
}

# CT/MR phantom with a known rigid transform `A` (MR 'RAS' to CT 'RAS'),
# adapted from `test-workflow-reg3d.R`: MR 48^3 at 1 mm with asymmetric blobs
# and a bright landmark; the CT is the MR moved by `A` on a finer
# 100 x 90 x 60 grid, stored as integers like Hounsfield units
yael_toy_ct_mr <- function(root) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  mr_dim <- c(48L, 48L, 48L)
  mr_v2r <- diag(4)
  mr_v2r[1:3, 4] <- -23.5
  ras <- yael_toy_grid(mr_dim, mr_v2r)
  inside <- function(centre, radius) {
    rowSums(sweep(ras, 2, centre)^2 / matrix(radius^2, nrow(ras), 3, byrow = TRUE)) <= 1
  }
  mr <- array(0, mr_dim)
  mr[inside(c(0, 0, 0), c(18, 14, 11))] <- 100
  mr[inside(c(-7, 4, 3), c(5, 5, 5))] <- 200
  mr[inside(c(9, -5, -4), c(4, 3, 6))] <- 60
  mr[inside(c(8, -6, 5), c(1.6, 1.6, 1.6))] <- 400

  A <- yael_toy_rotation(3, 8) %*% yael_toy_rotation(1, 5)
  A[1:3, 4] <- c(3, -2, 4)

  ct_dim <- c(100L, 90L, 60L)
  ct_v2r <- diag(c(0.5, 0.5, 0.8, 1))
  ct_v2r[1:3, 4] <- -(ct_dim - 1) * c(0.5, 0.5, 0.8) / 2 + A[1:3, 4]
  ct <- ravetools::apply_transform3d(
    mr, mr_v2r, transform = solve(A), reference_dim = ct_dim,
    reference_vox2ras = ct_v2r, interpolation = "trilinear")
  ct <- round(ct * 10) - 1000
  storage.mode(ct) <- "integer"

  ct_path <- yael_toy_write(ct, ct_v2r, file.path(root, "ct.nii.gz"))
  mr_path <- yael_toy_write(mr, mr_v2r, file.path(root, "mr.nii.gz"))
  list(ct_path = ct_path, mr_path = mr_path, A = A,
       brain = rbind(t(ras[mr > 0, , drop = FALSE]), 1))
}

# Rotation angle (degrees) and root-mean-square shift (mm) over the points
# `brain` (4 x N, homogeneous) between an estimated and a true transform
yael_toy_rigid_error <- function(estimate, truth, brain) {
  rot <- estimate[1:3, 1:3] %*% solve(truth[1:3, 1:3])
  angle <- acos(min(1, (sum(diag(rot)) - 1) / 2)) * 180 / pi
  shift <- sqrt(colSums(((estimate - truth) %*% brain)[1:3, , drop = FALSE]^2))
  c(angle = angle, rms = sqrt(mean(shift^2)))
}

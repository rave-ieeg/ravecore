#' @name reg3d_rigid
#' @title Rigidly register a computerized tomography (CT) image to MRI with
#' the built-in \verb{YAEL-reg3d} engine
#' @description Aligns the 'CT' to the 'MRI' with
#' \code{\link[ravetools]{register_volume3d}}, which is built into 'RAVE' and
#' needs no external program. The registration always runs on the
#' full-resolution images. Images with any side of \code{max_side} voxels or
#' more are additionally saved as down-sampled copies (same field of view) so
#' that viewers can load them; the original images are kept. Please use
#' \code{cmd_run_reg3d_rigid} from pipelines: it saves a script that runs
#' \code{reg3d_rigid} and records the results for the electrode localization.
#' @param ct_path,mri_path absolute paths to 'CT' and 'MR' image files
#' @param coreg_path directory where the results are saved; required. An
#' existing directory is backed up (renamed) once the new results are
#' complete, so a failed registration leaves it untouched
#' @param reg_type registration preset; choices are \code{'Rigid'} (three
#' resolution levels, 20 percent of the voxels sampled), \code{'DenseRigid'}
#' (same levels, every voxel sampled; slower), or \code{'FastRigid'} (coarser
#' levels, fewer iterations, 5 percent of the voxels sampled; fast)
#' @param cost cost function; choices are \code{'mattes'} (mutual
#' information, suitable for 'CT' to 'MRI') or \code{'cc'} (normalized
#' cross-correlation, for images of the same modality)
#' @param interp interpolation used to re-sample the saved images; choices
#' are \code{'trilinear'}, \code{'bspline'}, or \code{'nearest'}; the
#' alignment itself does not depend on it
#' @param max_side images with any side of at least this many voxels are also
#' saved as copies whose sides are at most \code{max_side} voxels; default is
#' \code{384}
#' @param verbose whether to print the registration progress
#' @param ... passed to \code{\link[ravetools]{register_volume3d}}; overrides
#' the preset, for example \code{iterations} or \code{sampling_rate}
#' @param subject 'RAVE' subject
#' @param dry_run whether to only build the script without running it;
#' default is false
#' @returns \code{reg3d_rigid} invisibly returns a list with the result
#' directory (\code{coreg_path}) and the names of the files in it:
#' \describe{
#' \item{\code{'CT_RAW.nii.gz'}, \code{'MRI_reference.nii.gz'}}{the 'CT' and
#' 'MR' images; an image with any side of \code{max_side} voxels or more is
#' saved as a down-sampled copy with the same field of view}
#' \item{\code{'CT_ORIG.nii.gz'}, \code{'MRI_ORIG.nii.gz'}}{the original
#' images; only saved when the images above are down-sampled copies}
#' \item{\code{'ct_in_t1.nii.gz'}}{the 'CT' re-sampled onto the grid of
#' \code{'MRI_reference.nii.gz'}}
#' \item{\code{'CT_RAS_to_MR_RAS.txt'}}{transform from the 'CT' scanner
#' 'RAS' to the 'MR' scanner 'RAS' coordinates}
#' \item{\code{'CT_IJK_to_MR_RAS.txt'}}{transform from the voxel indices of
#' \code{'CT_RAW.nii.gz'} (starting from 0) to the 'MR' scanner 'RAS'
#' coordinates}
#' \item{\code{'reg3d/reg3d.dcf'}}{registration record written by
#' \code{\link[ravetools]{save_registration}}, with the 'ANTs'-style affine
#' transform \code{'reg3d/reg3d0GenericAffine.mat'}; it refers to the
#' full-resolution images}
#' }
#' \code{cmd_run_reg3d_rigid} returns a list with the script, its path, the
#' log file, the image paths, and a function \code{execute} that runs the
#' script.
#' @export
reg3d_rigid <- function(
    ct_path, mri_path, coreg_path,
    reg_type = c("Rigid", "DenseRigid", "FastRigid"),
    cost = c("mattes", "cc"),
    interp = c("trilinear", "bspline", "nearest"),
    max_side = 384, verbose = TRUE, ...) {

  ct_path <- normalizePath(ct_path, winslash = "/", mustWork = TRUE)
  mri_path <- normalizePath(mri_path, winslash = "/", mustWork = TRUE)
  if (length(coreg_path) != 1 || is.na(coreg_path) || !nzchar(coreg_path)) {
    stop("`reg3d_rigid`: `coreg_path` must be a directory path.")
  }
  coreg_path <- normalizePath(coreg_path, winslash = "/", mustWork = FALSE)
  reg_type <- match.arg(reg_type)
  cost <- match.arg(cost)
  interp <- match.arg(interp)
  max_side <- as.numeric(max_side)
  if (length(max_side) != 1 || is.na(max_side) || max_side < 2) {
    stop("`reg3d_rigid`: `max_side` must be a number of at least 2.")
  }

  ct <- reg3d_read_image(ct_path)
  mri <- reg3d_read_image(mri_path)

  # register the full-resolution images; `...` overrides the preset
  reg_args <- utils::modifyList(reg3d_preset(reg_type), list(...))
  result <- do.call(ravetools::register_volume3d, c(
    list(
      source = ct$data,
      target = mri$data,
      source_vox2ras = ct$vox2ras,
      target_vox2ras = mri$vox2ras,
      type = "rigid",
      metric = cost,
      interpolation = interp,
      verbose = verbose
    ),
    reg_args
  ))

  # copies with at most `max_side` voxels per side for viewers
  ct_small <- reg3d_cap_image(ct, max_side = max_side, interp = interp)
  mri_small <- reg3d_cap_image(mri, max_side = max_side, interp = interp)

  # write everything into a temporary folder next to `coreg_path`, so an
  # existing result is replaced only when the new one is complete
  dir_create2(dirname(coreg_path))
  tmp_path <- tempfile(pattern = sprintf(".%s-reg3d-", basename(coreg_path)),
                       tmpdir = dirname(coreg_path))
  dir_create2(tmp_path)
  on.exit({
    if (dir.exists(tmp_path)) {
      unlink(tmp_path, recursive = TRUE)
    }
  }, add = TRUE)

  reg3d_write_image(ct, ct_small, file.path(tmp_path, "CT_RAW.nii.gz"),
                    file.path(tmp_path, "CT_ORIG.nii.gz"))
  reg3d_write_image(mri, mri_small, file.path(tmp_path, "MRI_reference.nii.gz"),
                    file.path(tmp_path, "MRI_ORIG.nii.gz"))

  # CT re-sampled onto the grid of the saved MRI_reference.nii.gz
  if (is.null(mri_small)) {
    t1_dim <- dim(mri$data)
    t1_vox2ras <- mri$vox2ras
    ct_in_t1 <- result$image
  } else {
    t1_dim <- dim(mri_small$data)
    t1_vox2ras <- mri_small$vox2ras
    ct_in_t1 <- ravetools::apply_transform3d(
      volume = reg3d_as_double(ct$data),
      vox2ras = ct$vox2ras,
      transform = result$transform,
      reference_dim = t1_dim,
      reference_vox2ras = t1_vox2ras,
      interpolation = interp
    )
  }
  ct_in_t1 <- array(as.numeric(ct_in_t1), dim = t1_dim)
  ieegio::write_volume(
    ieegio::as_ieegio_volume(ct_in_t1, vox2ras = t1_vox2ras),
    con = file.path(tmp_path, "ct_in_t1.nii.gz"),
    version = 1
  )

  # `result$transform` maps MR scanner RAS to CT scanner RAS. The field of
  # view of CT_RAW.nii.gz is that of the original CT, so only the voxel
  # transform depends on which CT image is saved
  ct_ras_to_mri_ras <- solve(result$transform)
  ct_raw_vox2ras <- if (is.null(ct_small)) ct$vox2ras else ct_small$vox2ras
  ct_ijk_to_mri_ras <- ct_ras_to_mri_ras %*% ct_raw_vox2ras
  utils::write.table(
    x = ct_ras_to_mri_ras, sep = "\t", row.names = FALSE, col.names = FALSE,
    file = file.path(tmp_path, "CT_RAS_to_MR_RAS.txt"))
  utils::write.table(
    x = ct_ijk_to_mri_ras, sep = "\t", row.names = FALSE, col.names = FALSE,
    file = file.path(tmp_path, "CT_IJK_to_MR_RAS.txt"))

  # 'ANTs'-style record of the full-resolution registration
  ravetools::save_registration(result, path = file.path(tmp_path, "reg3d", "reg3d.dcf"))

  if (file.exists(coreg_path)) {
    backup_file(coreg_path, remove = TRUE, quiet = !verbose)
  }
  file_move(tmp_path, coreg_path)

  invisible(list(
    coreg_path = coreg_path,
    files = list.files(coreg_path, recursive = TRUE),
    resampled = c(CT = !is.null(ct_small), MRI = !is.null(mri_small))
  ))
}

#' @rdname reg3d_rigid
#' @export
cmd_run_reg3d_rigid <- function(
    subject, ct_path, mri_path,
    reg_type = c("Rigid", "DenseRigid", "FastRigid"),
    cost = c("mattes", "cc"),
    interp = c("trilinear", "bspline", "nearest"),
    verbose = TRUE, dry_run = FALSE) {

  subject <- restore_subject_instance(subject, strict = FALSE)
  work_path <- normalizePath(
    subject$imaging_path,
    winslash = "/", mustWork = FALSE
  )
  ct_path <- normalizePath(ct_path, winslash = "/", mustWork = TRUE)
  mri_path <- normalizePath(mri_path, winslash = "/", mustWork = TRUE)

  reg_type <- match.arg(reg_type)
  cost <- match.arg(cost)
  interp <- match.arg(interp)
  force(dry_run)

  log_path <- normalizePath(
    file.path(subject$imaging_path, "log"),
    mustWork = FALSE, winslash = "/"
  )
  log_file <- "log-rave-reg3d-rigid.R.log"

  template <- readLines(system.file("shell-templates/rave-reg3d-rigid.R", package = "ravecore"))

  cmd <- ravepipeline::glue(
    paste(template, collapse = "\n"),
    .sep = "\n",
    .open = "{{",
    .close = "}}",
    .trim = FALSE,
    .null = ""
  )

  script_path <- normalizePath(
    file.path(subject$imaging_path, "scripts", "cmd-reg3d-rigid.R"),
    mustWork = FALSE, winslash = "/"
  )

  execute <- function(...) {
    dir_create2(log_path)
    log_abspath <- normalizePath(file.path(log_path, log_file),
                                 winslash = "/", mustWork = FALSE)
    cmd_execute(script = cmd, script_path = script_path,
                args = c("--no-save", "--no-restore"),
                command = rscript_path(),
                stdout = log_abspath, stderr = log_abspath, ...)
  }
  re <- list(
    script = cmd,
    script_path = script_path,
    dry_run = dry_run,
    log_file = file.path(log_path, log_file, fsep = "/"),
    mri_path = mri_path,
    ct_path = ct_path,
    execute = execute,
    command = rscript_path()
  )
  if ( verbose ) {
    message(cmd)
  }
  if (dry_run) {
    return(invisible(re))
  }

  execute()

  return(invisible(re))

}

# Parameters of `ravetools::register_volume3d` for each preset
reg3d_preset <- function(reg_type) {
  switch(
    reg_type,
    "DenseRigid" = list(
      shrink_factors = c(4, 2, 1),
      smoothing_sigmas = c(2, 1, 0),
      iterations = c(1000, 500, 250),
      sampling_rate = 1
    ),
    "FastRigid" = list(
      shrink_factors = c(8, 4, 2),
      smoothing_sigmas = c(3, 2, 1),
      iterations = c(200, 150, 100),
      sampling_rate = 0.05
    ),
    list(
      shrink_factors = c(4, 2, 1),
      smoothing_sigmas = c(2, 1, 0),
      iterations = c(1000, 500, 250),
      sampling_rate = 0.2
    )
  )
}

reg3d_read_image <- function(path) {
  volume <- ieegio::read_volume(path)
  data <- volume[]
  dm <- dim(data)
  if (length(dm) > 3) {
    if (any(dm[-c(1, 2, 3)] != 1)) {
      stop("`reg3d_rigid`: image ", basename(path), " is not a 3D volume.")
    }
    dim(data) <- dm[1:3]
  }
  list(
    volume = volume,
    data = data,
    vox2ras = matrix(as.numeric(volume$transforms$vox2ras)[seq_len(16)], nrow = 4L)
  )
}

reg3d_as_double <- function(x) {
  if (!is.double(x)) {
    storage.mode(x) <- "double"
  }
  x
}

# Down-samples every side longer than `max_side` voxels to `max_side`,
# keeping the field of view; returns NULL when nothing changes
reg3d_cap_image <- function(image, max_side, interp) {
  dm <- dim(image$data)
  new_dim <- as.integer(pmin(dm, floor(max_side)))
  if (all(new_dim == dm)) { return(NULL) }

  # 0-based voxel centres: new voxel j sits at old (continuous) index
  # j * s + (s - 1) / 2, so the outer voxel edges stay in place
  s <- dm / new_dim
  scale <- diag(c(s, 1))
  scale[1:3, 4] <- (s - 1) / 2
  vox2ras <- image$vox2ras %*% scale

  x <- image$data
  is_integer <- is.integer(x) || is.logical(x)
  # trilinear and bspline need a double volume
  x <- reg3d_as_double(x)
  data <- ravetools::resample_3d_volume(
    x,
    new_dim = new_dim,
    vox2ras_old = image$vox2ras,
    vox2ras_new = vox2ras,
    na_fill = min(x, na.rm = TRUE),
    interpolation = interp
  )
  data <- array(as.numeric(data), dim = new_dim)
  if (is_integer) {
    data <- round(data)
    storage.mode(data) <- "integer"
  }
  list(data = data, vox2ras = vox2ras)
}

# Writes the image (header kept) as `path`; when a down-sampled copy exists,
# the original goes to `orig_path` and the copy to `path`
reg3d_write_image <- function(image, small, path, orig_path) {
  if (is.null(small)) {
    ieegio::write_volume(image$volume, con = path, version = 1)
  } else {
    ieegio::write_volume(image$volume, con = orig_path, version = 1)
    ieegio::write_volume(
      ieegio::as_ieegio_volume(small$data, vox2ras = small$vox2ras),
      con = path, version = 1
    )
  }
  invisible(path)
}

#' @title Extract the brain from a 'T1'-weighted image (without 'Python')
#' @description
#' The brain mask of a 'T1'-weighted 'MRI', computed in 'R' by the native
#' \verb{YAEL} engine on top of \pkg{ravetools}: the brain mask of the template
#' \code{'mni_icbm152_nlin_asym_09a'}, brought to the image by a registration
#' (or a given initial mask), is adjusted to the subject.
#' @param image the 'T1'-weighted image: a file path, an \code{ieegio_volume},
#' or anything else that \code{ieegio::as_ieegio_volume} accepts
#' @param mask optional initial brain mask in the same scanner space (the
#' voxels above zero), in the same forms as \code{image}; when given, the
#' template and its registration are not used. A mask on another grid, such
#' as a 'FreeSurfer' \code{brainmask.mgz}, is resampled onto the grid of
#' \code{image} (nearest neighbor)
#' @param work_path folder for the working files, which are kept; default
#' \code{NULL} uses a temporary folder that is removed at the end
#' @param verbose whether to log the steps and print the registration progress
#' @returns A binary \code{ieegio_volume} (1 inside the brain) on the grid of
#' \code{image}.
#' @details
#' The steps, with the files they write into \code{work_path} (named after the
#' stages of the native normalization):
#' \enumerate{
#' \item bias-field correction with the settings of \code{abp_n4} in
#' 'ANTsPy' (\code{nu.nii.gz});
#' \item unless \code{mask} is given, the registration of the whole head
#' to the template image without masks, affine and then deformable, as the
#' initial registration (\code{'SyNabp'}) of the native normalization; it is
#' saved by \code{ravetools::save_registration} as \code{SyNabp.dcf} with its
#' \verb{'ANTs'} transform files, and the registered image as
#' \code{SyNabp_warped_mov.nii.gz};
#' \item the initial mask: the template brain mask through that registration,
#' or \code{mask} (\code{initial_mask.nii.gz});
#' \item the brain extraction of the native engine (see the item "Brain
#' extraction" of \code{\link{YAELNativeProcess}}), which keeps the largest
#' connected gray and white matter, leaves out what dark voxels cut off from
#' it (skull marrow, dura or scalp) and thin strands, and keeps the initial
#' mask, with a warning, when the result differs from it by more than 20
#' percent in volume (\code{brainmask.nii.gz}).
#' }
#' Every call computes all the steps again; existing files in
#' \code{work_path} are replaced.
#' @examples
#' \dontrun{
#'
#' # downloads the template on first use, and takes a few minutes
#' mask <- yael_brain_extract("sub-01_T1w.nii.gz", work_path = "brain_extraction")
#' ieegio::write_volume(mask, "sub-01_desc-brain_mask.nii.gz")
#'
#' # adjust a mask of the same image instead
#' mask2 <- yael_brain_extract("sub-01_T1w.nii.gz", mask = "fs/mri/brainmask.mgz")
#'
#' }
#' @export
yael_brain_extract <- function(image, mask = NULL, work_path = NULL, verbose = TRUE) {
  verbose <- isTRUE(verbose)
  free_memory <- function() { invisible(gc(verbose = FALSE)) }
  # a volume as a 3D double array with its "vox2ras" attribute
  as_array <- function(x, what) {
    volume <- ieegio::as_ieegio_volume(x)
    vox2ras <- matrix(as.numeric(volume$transforms$vox2ras)[seq_len(16)], nrow = 4L)
    data <- volume[]
    rm(volume)
    dm <- dim(data)
    if (length(dm) < 3 || prod(dm[-(1:3)]) != 1) {
      stop(sprintf("yael_brain_extract: the %s is not a 3D volume (dimension: %s).",
                   what, paste(dm, collapse = "x")))
    }
    data <- array(as.double(data), dm[1:3])
    attr(data, "vox2ras") <- vox2ras
    data
  }

  if (is.null(work_path)) {
    work_path <- tempfile(pattern = "yael-brain-extract-")
    on.exit({ unlink(work_path, recursive = TRUE) }, add = TRUE)
  }
  work_path <- normalizePath(work_path, winslash = "/", mustWork = FALSE)
  dir_create2(work_path)

  t1 <- as_array(image, "image")
  native_dim <- dim(t1)
  native_vox2ras <- attr(t1, "vox2ras")

  # ---- nu: N4 bias field correction (`abp_n4`) ----
  yael_native_log(verbose, "Brain extraction: N4 bias field correction")
  nu <- ravetools::bias_correction_n4(t1, vox2ras = native_vox2ras)
  rm(t1)
  yael_native_write_volume(nu, file.path(work_path, "nu.nii.gz"), vox2ras = native_vox2ras)

  if (is.null(mask)) {
    # ---- SyNabp: registration of the whole head to the template, without masks ----
    # the settings of the `SyNabp` stage of `yael_native_normalize`
    template_name <- "mni_icbm152_nlin_asym_09a"
    yael_native_log(verbose, sprintf(
      "Brain extraction: registration of the head to template [%s] (affine and deformable)",
      template_name))
    template <- yael_native_template(template_name)
    fixed <- yael_native_read_volume(template$t1)
    template_dim <- dim(fixed)
    template_vox2ras <- attr(fixed, "vox2ras")
    template_mask <- yael_native_resample(
      yael_native_read_mask(template$mask),
      template_dim, template_vox2ras, interpolation = "nearest")
    # a template with more voxels per brain than a 1 mm template is registered
    # on a grid as coarse as at 1 mm, with the update fields smoothed as much
    syn_scale <- yael_native_syn_scale(sum(template_mask > 0))
    coarse <- yael_native_coarsen(fixed, syn_scale)
    coarse_vox2ras <- attr(coarse, "vox2ras")
    rm(fixed)
    linear <- ravetools::register_volume3d(
      source = nu, target = coarse,
      source_vox2ras = native_vox2ras, target_vox2ras = coarse_vox2ras,
      type = "affine", metric = "mattes",
      shrink_factors = c(8, 4, 2, 1), smoothing_sigmas = c(4, 2, 1, 0),
      iterations = c(1000, 500, 250, 100), sampling_rate = 0.25,
      verbose = verbose
    )
    init_transform <- linear$transform
    rm(linear)
    # 'ANTsPy' leaves the convergence of 'SyNabp' to the defaults of
    # 'antsRegistration' (1e-6, window 10)
    registration <- ravetools::register_volume3d(
      source = nu, target = coarse,
      source_vox2ras = native_vox2ras, target_vox2ras = coarse_vox2ras,
      type = "syn_only", metric = "mattes", init_transform = init_transform,
      shrink_factors = pmax(1, round(c(4, 2, 1) / syn_scale)),
      smoothing_sigmas = c(2, 1, 0),
      syn_iterations = c(50, 10, 0), syn_grad_step = 0.1, syn_sigma = 3 * syn_scale,
      syn_total_sigma = 0, syn_cc_radius = 4L,
      syn_convergence = 1e-6, syn_convergence_window = 10L,
      verbose = verbose
    )
    rm(coarse)
    warped <- registration$image
    attr(warped, "vox2ras") <- coarse_vox2ras
    registration$image <- NULL
    registration$images <- NULL
    yael_native_stage_write(
      list(registration = registration, warpedmovout = warped, warpedfixout = NULL),
      name = "SyNabp", kind = "registration", root = work_path)
    rm(warped)
    free_memory()

    # the template mask on the native grid: a template image goes there with
    # the inverse direction (the template is the target) and its own grid
    yael_native_log(verbose, "Brain extraction: template brain mask on the native image")
    registration$forward_field <- NULL
    initial <- ravetools::apply_transform3d_volume(
      template_mask, registration, vox2ras = template_vox2ras,
      reference_dim = native_dim, reference_vox2ras = native_vox2ras,
      direction = "inverse", interpolation = "nearest")
    rm(registration, template_mask)
    free_memory()
  } else {
    initial <- as_array(mask, "mask")
    mask_vox2ras <- attr(initial, "vox2ras")
    initial <- (initial > 0) * 1
    attr(initial, "vox2ras") <- mask_vox2ras
    initial <- yael_native_resample(initial, native_dim, native_vox2ras,
                                    interpolation = "nearest")
  }
  initial <- array((initial > 0) * 1, native_dim)
  attr(initial, "vox2ras") <- native_vox2ras
  yael_native_write_volume(initial, file.path(work_path, "initial_mask.nii.gz"),
                           vox2ras = native_vox2ras)

  # ---- brainmask: the initial mask adjusted to the subject ----
  brain <- yael_native_brain_extraction(nu, initial, native_vox2ras, verbose = verbose)
  rm(nu, initial)
  yael_native_write_volume(brain, file.path(work_path, "brainmask.nii.gz"),
                           vox2ras = native_vox2ras)
  ieegio::as_ieegio_volume(array(as.vector(brain), native_dim), vox2ras = native_vox2ras)
}

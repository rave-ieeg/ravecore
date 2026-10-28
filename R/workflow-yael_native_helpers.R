# Helpers of the native 'YAEL' normalization only (`yael_native_normalize`
# in `workflow-yael_preprocess_native.R`). They assume its tissue labels (0
# outside the brain mask, 1 CSF, 2 gray matter, 3 white matter with deep gray
# matter and brain stem, 4 cerebellum) and its native grids (`vox2ras` maps
# 0-based voxel indices to 'RAS'), and are not designed to be reused by other
# functions

#' Whether the tissue classes are ordered by intensity like their priors
#' @param nu2 intensity-normalized native image
#' @param labels tissue labels on the grid of \code{nu2}
#' @returns Invisibly, whether the median intensities of the \verb{CSF}, gray
#' matter and white matter labels increase; a warning is given otherwise. The
#' cerebellum is not ranked: on \verb{T1} images its intensity lies between the
#' gray and the white matter. The labels keep the order of their priors either
#' way
#' @noRd
yael_native_segmentation_class_order <- function(nu2, labels) {
  medians <- vapply(1:3, function(label) {
    selected <- labels == label
    if (!any(selected)) { return(NA_real_) }
    stats::median(nu2[selected])
  }, 0.0)
  ordered <- !anyNA(medians) && medians[[1]] < medians[[2]] &&
    medians[[2]] < medians[[3]]
  if (!ordered) {
    warning(sprintf(
      "yael_native_normalize: the tissue classes are not ordered by intensity like their priors (median: CSF %.1f, gray matter %.1f, white matter %.1f); the prior order is kept and no white-matter artifact is left out.",
      medians[[1]], medians[[2]], medians[[3]]), call. = FALSE)
  }
  invisible(ordered)
}

#' Gray-matter envelope of the native brain
#' @param labels tissue labels
#' @param vox2ras voxel-to-'RAS' matrix of \code{labels}
#' @param method how the gray-matter surface is wrapped: \code{'none'} keeps
#' the surface as it is (its outer and its inner sheet), \code{'convex_hull'}
#' takes its convex hull (\code{ravetools::vcg_convex_hull} with its
#' defaults); \code{'outer_smoothed'} is reserved for a wrapper of
#' \pkg{ravetools} that is not available yet
#' @returns A \code{'mesh3d'} surface in 'RAS' around the gray matter and the
#' cerebellum
#' @noRd
yael_native_gm_envelope <- function(labels, vox2ras,
                                    method = c("none", "convex_hull", "outer_smoothed")) {
  method <- match.arg(method)
  gray <- array((labels == 2 | labels == 4) * 1, dim(labels))
  if (!any(gray > 0)) {
    stop("yael_native_gm_envelope: the segmentation has no gray matter")
  }
  surface <- ravetools::vcg_isosurface(gray, threshold_lb = 0.5, vox_to_ras = vox2ras)
  switch(
    method,
    "none" = surface,
    "convex_hull" = ravetools::vcg_convex_hull(surface),
    "outer_smoothed" = ravetools::mris_outer_surface(surface = surface)
  )
}

#' Which points may lie outside a surface around a center
#' @param surface \code{'mesh3d'} surface in 'RAS'
#' @param points 'RAS' coordinates, a matrix with 3 columns
#' @param center 'RAS' coordinates of a point inside \code{surface}
#' @returns A logical vector, \code{TRUE} where the ray cast from the point
#' away from \code{center} misses \code{surface}. For a convex surface around
#' \code{center} this is exactly the outside: if that ray from a point outside
#' hit the surface, the segment from \code{center} to the hit, which holds the
#' point, would lie inside. From the white matter under the gray-matter
#' surface (method \code{'none'} of \code{yael_native_gm_envelope}), the ray
#' hits the cortex unless it passes through a gap. A point at \code{center}
#' is inside
#' @noRd
yael_native_check_maybe_outside <- function(surface, points, center) {
  points <- matrix(points, ncol = 3L)
  n_points <- nrow(points)
  outward <- sweep(points, 2L, as.double(center[1:3]))
  zero <- rowSums(outward^2) == 0
  # the rays start a few micrometers off the voxel centers: from the voxel
  # lattice they can start on the cell boundaries of the search grid of
  # `vcg_raycaster`, or run through the vertices and edges of a
  # marching-cubes surface, and then pass the surface undetected
  points <- sweep(points, 2L, c(pi, exp(1), sqrt(2)) * 1e-3, "+")
  hit <- logical(n_points)
  # about a million rays per call: each call builds the search grid of the
  # surface again
  chunk <- 1e6
  starts <- if (n_points > 0) seq.int(1L, n_points, by = chunk) else integer(0)
  for (start in starts) {
    rows <- seq.int(start, min(n_points, start + chunk - 1L))
    hit[rows] <- ravetools::vcg_raycaster(
      surface,
      ray_origin = t(points[rows, , drop = FALSE]),
      ray_direction = t(outward[rows, , drop = FALSE])
    )$has_intersection
  }
  !hit & !zero
}

#' Whether points lie in the cone below a center (the brain stem)
#' @param points 'RAS' coordinates, a matrix with 3 columns
#' @param center 'RAS' coordinates of the apex
#' @returns A logical vector, \code{TRUE} for the points in the cone of 45
#' degrees that points down from \code{center}:
#' \eqn{z - z_0 < -\sqrt{(x - x_0)^2 + (y - y_0)^2}}. A heuristic for the
#' brain stem, which leaves the brain below its center and therefore lies
#' outside the gray-matter envelope; it assumes a head roughly upright in 'RAS'
#' @noRd
yael_native_brainstem_cone <- function(points, center) {
  points <- matrix(points, ncol = 3L)
  points[, 3] - center[[3]] <
    -sqrt((points[, 1] - center[[1]])^2 + (points[, 2] - center[[2]])^2)
}

#' White-matter artifacts outside the gray-matter envelope
#' @param labels tissue labels
#' @param vox2ras voxel-to-'RAS' matrix of \code{labels}
#' @param dgwbs posterior of the white matter with deep gray matter and brain
#' stem (label 3) on the grid of \code{labels}; the centroid of its voxels
#' above 0.5 is the center from which the rays point outward and the apex of
#' \code{yael_native_brainstem_cone}
#' @param method how the gray-matter surface is wrapped, see
#' \code{yael_native_gm_envelope}
#' @param dilate voxels by which the artifacts are dilated (a box)
#' @returns A 0/1 array on the grid of \code{labels} with attributes
#' \code{vox2ras} and \code{artifact_voxels} (the number of voxels before the
#' dilation): the voxels to leave out of the white-matter channel
#' @details Every white-matter voxel outside the cone below the center (the
#' brain stem, see \code{yael_native_brainstem_cone}) casts one ray away from
#' the center; when the ray misses the gray-matter envelope, the voxel is an
#' artifact (see \code{yael_native_check_maybe_outside}). The artifacts are
#' dilated by \code{dilate} voxels, outside that cone
#' @noRd
yael_native_clean_wm_mask <- function(labels, vox2ras, dgwbs,
                                      method = c("none", "convex_hull", "outer_smoothed"),
                                      dilate = 2L) {
  method <- match.arg(method)
  dm <- dim(labels)
  white <- array(labels == 3, dm)
  result <- function(index = integer(0), n_artifacts = 0L) {
    out <- array(0, dm)
    out[index] <- 1
    attr(out, "vox2ras") <- vox2ras
    attr(out, "artifact_voxels") <- n_artifacts
    out
  }
  if (!any(white)) { return(result()) }
  to_ras <- function(index) {
    ijk <- arrayInd(index, dm) - 1
    t(vox2ras[1:3, 1:3] %*% t(ijk) + vox2ras[1:3, 4])
  }

  # the center of the white matter: the rays point away from it, and the
  # brain stem leaves the brain below it
  core <- which(dgwbs > 0.5)
  if (!length(core)) { core <- which(white) }
  center <- colMeans(to_ras(core))
  candidates <- which(white)
  candidates <- candidates[!yael_native_brainstem_cone(to_ras(candidates), center)]
  if (!length(candidates)) { return(result()) }

  # the white matter whose outward ray misses the envelope
  surface <- yael_native_gm_envelope(labels, vox2ras, method = method)
  artifacts <- candidates[
    yael_native_check_maybe_outside(surface, to_ras(candidates), center)]
  if (!length(artifacts)) { return(result()) }

  # dilated by `dilate` voxels (a box), outside the cone
  selected <- array(0, dm)
  selected[artifacts] <- 1
  if (dilate > 0) {
    selected <- ravetools::grow_volume(selected, 2L * dilate + 1L)
  }
  index <- which(selected > 0)
  index <- index[!yael_native_brainstem_cone(to_ras(index), center)]
  result(index, length(artifacts))
}

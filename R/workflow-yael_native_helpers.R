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
#' defaults), and \code{'outer_smoothed'} its smoothed outer surface
#' (\code{ravetools::mris_outer_surface} with its defaults; the method of
#' \code{yael_native_normalize})
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

# `ravetools::morph_volume` with a radius per axis, `NULL` before 'ravetools'
# 0.4.2. Looked up by name since the released 'ravetools' may not export it
# yet; its first version took one radius only
yael_native_morph_volume <- function() {
  morph <- get0("morph_volume", envir = asNamespace("ravetools"),
                mode = "function", inherits = FALSE)
  if (is.null(morph)) { return(NULL) }
  per_axis <- tryCatch({
    morph(array(1L, c(1L, 1L, 1L)), "dilate", c(1L, 1L, 1L))
    TRUE
  }, error = function(e) { FALSE })
  if (per_axis) { morph } else { NULL }
}

#' Radii in voxels of a distance given on an adult human brain
#' @param mm distance in millimeters on a brain of 1.9 liters
#' @param brain_ml volume of the brain mask in milliliters
#' @param voxel_mm voxel size in millimeters, one per axis
#' @returns The distance scaled by the cube root of the ratio of the brain
#' volumes, in voxels along each axis of \code{voxel_mm}; at least one voxel
#' unless \code{mm} is zero
#' @noRd
yael_native_extraction_radius <- function(mm, brain_ml, voxel_mm) {
  if (mm <= 0) { return(rep(0L, length(voxel_mm))) }
  pmax(1L, as.integer(round(mm * (brain_ml / 1900)^(1 / 3) / voxel_mm)))
}

#' Brain extraction: the warped template mask adjusted to the subject
#' @param image bias-corrected native image
#' @param mask warped template brain mask on the grid of \code{image}
#' @param vox2ras voxel-to-'RAS' matrix of \code{image}
#' @param verbose whether to log the change of the mask
#' @returns A 0/1 array on the grid of \code{mask} with attribute
#' \code{vox2ras}
#' @details The local half of \verb{antsBrainExtraction.sh}, since a warp only
#' places the mask to a few millimeters: a 3-class \verb{k-means} segmentation
#' of the subject in the mask grown by 2 mm; the largest connected white and
#' gray matter; an erosion by 2 mm that cuts thin bridges to dura or marrow,
#' the largest component, a dilation by 4 mm and the holes filled; the warped
#' mask added back, and a closing by 5 mm. \verb{antsBrainExtraction.sh} also
#' adds the \verb{CSF} eroded by 10 voxels, which never touches the brain and
#' is dropped by the largest component, so it is left out here.
#'
#' These steps keep all of the warped mask and grow through any intensity,
#' so the mask can still hold skull marrow, dura or scalp. As in the watershed
#' of 'FreeSurfer', the gray and white matter that dark voxels (\verb{CSF} or
#' bone, which a 'T1' image cannot tell apart) cut off from the largest
#' connected white and gray matter then leaves the mask, also where the warped
#' mask held it; beyond the segmented band, the voxels are classed by the
#' midpoints between the class means. The largest component stays, with its
#' holes filled. Marrow as dark as gray matter, or marrow that touches the
#' brain with no dark voxel between, stays.
#'
#' Distances are millimeters on an adult brain, scaled by its volume and
#' converted to voxels along each axis (see
#' \code{yael_native_extraction_radius}), so that they hold on thick slices.
#' The warped mask is kept, with a warning, when a tissue class is empty, the
#' segmentation fails, or the brain differs from the mask by more than 20
#' percent in volume; and with a line in the log before \pkg{ravetools} 0.4.2,
#' which has no \code{morph_volume} with a radius per axis
#' @noRd
yael_native_brain_extraction <- function(image, mask, vox2ras, verbose = TRUE) {
  rough <- array(mask > 0, dim(mask))
  as_mask <- function(brain) {
    brain <- array(brain * 1, dim(mask))
    attr(brain, "vox2ras") <- vox2ras
    brain
  }
  morph <- yael_native_morph_volume()
  if (is.null(morph)) {
    ravepipeline::logger(
      level = "warning", use_glue = FALSE,
      "Brain extraction needs `morph_volume` from a newer `ravetools`; using the warped template mask")
    return(as_mask(rough))
  }
  keep_warped <- function(reason) {
    warning(sprintf(
      "yael_native_brain_extraction: %s; using the warped template mask.", reason),
      call. = FALSE)
    as_mask(rough)
  }
  voxel_ml <- abs(det(vox2ras[1:3, 1:3])) / 1000
  # the voxel size along each axis
  voxel_mm <- sqrt(colSums(vox2ras[1:3, 1:3]^2))
  radius <- function(mm) {
    yael_native_extraction_radius(mm, sum(rough) * voxel_ml, voxel_mm)
  }
  grow <- function(x, mm) { morph(x, "dilate", radius(mm)) > 0 }
  shrink <- function(x, mm) { morph(x, "erode", radius(mm)) > 0 }
  largest <- function(x) { morph(x, "largest_component") > 0 }

  # the subject's tissues around the edge of the warped mask
  band <- largest(grow(rough, 2))
  seg3 <- tryCatch({
    yael_native_kmeans3(image, band * 1, vox2ras, iterations = 3L,
                        stage = "brainmask", verbose = verbose)
  }, error = function(e) { e })
  if (inherits(seg3, "error")) {
    return(keep_warped(sprintf("the segmentation failed (%s)", conditionMessage(seg3))))
  }
  labels <- seg3$segmentation
  means <- vapply(1:3, function(k) { mean(image[labels == k]) }, 0.0)
  if (anyNA(means)) {
    return(keep_warped("a tissue class is empty"))
  }
  classes <- order(means)

  # white and gray matter, without what hangs on them by a thread
  brain <- largest(labels == classes[[3]]) | largest(labels == classes[[2]])
  brain <- grow(largest(shrink(brain, 2)), 4)
  brain <- morph(brain, "fill_holes") > 0
  # the warped mask comes back, closed
  brain <- shrink(grow(brain | rough, 5), 5)

  # what stays is dark or part of the brain: as in the watershed of
  # 'FreeSurfer', gray and white matter that dark voxels (CSF, bone) cut off
  # from the largest components is marrow, dura or scalp, also inside the
  # warped mask. Beyond the band, voxels are classed by the midpoints of the
  # class means, so that the cortex the warped mask missed stays
  tissue <- array(match(labels, classes, nomatch = 0L), dim(mask))
  outside <- brain & !band
  sorted <- means[classes]
  tissue[outside] <- 1L + (image[outside] > (sorted[[1]] + sorted[[2]]) / 2) +
    (image[outside] > (sorted[[2]] + sorted[[3]]) / 2)
  attached <- largest(tissue == 3L) | largest(tissue == 2L)
  cut_off <- brain & tissue >= 2L & !attached
  brain <- morph(largest(brain & !cut_off), "fill_holes") > 0

  ratio <- sum(brain) / sum(rough)
  if (!isTRUE(abs(ratio - 1) <= 0.2)) {
    return(keep_warped(sprintf(
      "the extracted brain is %.0f%% of the volume of the mask", ratio * 100)))
  }
  yael_native_log(verbose, sprintf(
    "Brain extraction: %.1f mL added to the warped template mask, %.1f mL removed; %.1f mL of tissue cut off from the brain (marrow, dura, scalp) left out",
    sum(brain & !rough) * voxel_ml, sum(rough & !brain) * voxel_ml,
    sum(cut_off & !brain) * voxel_ml))
  as_mask(brain)
}

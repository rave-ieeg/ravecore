base64_plot_slice <- function(
    x, overlay = NULL,
    which = c("axial", "coronal", "sagittal"), depth = 0, pixel_width = 0.5,
    underlay_range = NULL, underlay_col = c("black", "white"),
    overlay_range = NULL, overlay_col = underlay_col

) {

  which <- match.arg(which)

  underlay <- ieegio::as_ieegio_volume(x)
  if(is.null(underlay_range)) {
    underlay_range <- range(underlay[], na.rm = TRUE)
  }

  if(!is.null(overlay)) {
    overlay <- ieegio::as_ieegio_volume(overlay)

    if(is.null(overlay_range)) {
      overlay_range <- range(overlay[], na.rm = TRUE)
    }
  }

  which_axis <- switch (
    which,
    "axial" = "S",
    "coronal" = "A",
    "sagittal" = "R"
  )
  position <- c(R = 0, A = 0, S = 0)
  position[[which_axis]] <- depth
  title_text <- sprintf("%s, %s=%.1f", which, which_axis, depth)

  ravepipeline <- asNamespace('ravepipeline')
  base64_plot <- ravepipeline$base64_plot

  underlay_base64 <- base64_plot(
    width = 512, height = 512,
    {
      oldpar1 <- graphics::par(mar = c(0, 0, 0, 0))
      on.exit({ graphics::par(oldpar1) })
      plot(
        underlay, which = which, position = position,
        center_position = TRUE, vlim = underlay_range,
        pixel_width = pixel_width, col = underlay_col,
        crosshair_col = NA, background = "black",
        foreground = "white"
      )
      graphics::text(
        title_text, col = "white", x = -125, y = 125,
        adj = c(0, 1), offset = 0, cex = 2
      )
    }
  )

  overlay_base64 <- NULL
  if(!is.null(overlay)) {
    overlay_base64 <- base64_plot(
      width = 512, height = 512,
      {
        oldpar2 <- graphics::par(mar = c(0, 0, 0, 0))
        on.exit({ graphics::par(oldpar2) })
        plot(overlay, which = which, position = position,
             center_position = TRUE, col = overlay_col,
             vlim = overlay_range, pixel_width = pixel_width, crosshair_col = NA,
             background = "black", foreground = "white")
        graphics::text(
          title_text, col = "white", x = -125, y = 125,
          adj = c(0, 1), offset = 0, cex = 2
        )
      }
    )
  }

  list(
    underlay_base64 = underlay_base64,
    overlay_base64 = overlay_base64
  )
}

#' @title Plot volume slices into 'SVG' images
#' @description
#' Display slices, or interleave with image overlays. Require installing
#' package \code{htmltools}.
#' @param x underlay, objects that can be converted to
#' \code{\link[ieegio]{as_ieegio_volume}}; for example, 'NIfTI' or 'FreeSurfer'
#' volume file path, array, loaded volume instances
#' @param overlay same type as \code{x}, but optional; served as overlay
#' @param depths depth position in millimeters, along the normal to the
#' \code{which} plane
#' @param which which plane to visualize; can be \code{"coronal"},
#' \code{"axial"}, \code{"sagittal"}
#' @param nc number of columns; default is to be determined by total number of
#' images
#' @param col,overlay_col underlay and overlay color keys, must have at least
#' two colors to construct color palettes
#' @param overlay_alpha overlay transparency
#' @param interleave whether to interleave overlay; default is true when
#' \code{overlay_alpha} is unspecified
#' @param interleave_period interleave animation duration per period; default
#' is 3 seconds
#' @param pixel_width pixel width resolution; default is 0.5 millimeters
#' @param underlay_range,overlay_range numeric vectors of two, value ranges
#' of underlay and overlay
#' @param ... passed to internal method
#' @returns A 'SVG' tag object that can be embedded in shiny applications or
#' plotted directly.
#' @examples
#'
#'
#'
#' # toy-example:
#'
#' shape <- c(50, 50, 50)
#' vox2ras <- matrix(
#'   c(1, 1.732, 0, -68.3,
#'     -1.732, 1, 2, -31.7,
#'     0, -2, 0, 50,
#'     0, 0, 0, 1),
#'   nrow = 4, byrow = TRUE
#' )
#'
#' # continuous
#' x <- array(sin(seq_len(125000) / 100), shape)
#'
#' underlay <- ieegio::as_ieegio_volume(x, vox2ras = vox2ras)
#' overlay <- ieegio::as_ieegio_volume(x > 0, vox2ras = vox2ras)
#'
#' plot_volume_slices(
#'   underlay, overlay = overlay,
#'   depths = seq(-100, 100, length.out = 4),
#'   overlay_col = c("#00000000", "#FF000044", "#FF0000FF")
#' )
#'
#' # Actual NIfTI
#' # Require `install_subject("yael_demo_001")`
#' subject <- ravecore::as_rave_subject("YAEL/yael_demo_001",
#'                                      strict = FALSE)
#'
#'
#' t1 <- file.path(subject$imaging_path, "coregistration",
#'                 "MRI_reference.nii.gz")
#' if (file.exists(t1)) {
#'
#'   ct <- file.path(subject$imaging_path, "coregistration",
#'                   "CT_RAW.nii.gz")
#'   transform <- read.table(
#'     file.path(subject$imaging_path, "coregistration",
#'               "CT_IJK_to_MR_RAS.txt")
#'   )
#'
#'   ct_image_original <- ieegio::read_volume(ct)
#'   ct_image_aligned <- ieegio::as_ieegio_volume(
#'     ct_image_original[], vox2ras = as.matrix(transform)
#'   )
#'
#'   plot_volume_slices(
#'     t1, overlay = ct_image_aligned,
#'     overlay_col = c("#00000000", "#FF000044", "#FF0000FF"),
#'     nc = 6
#'   )
#' }
#'
#'
#'
#' @export
plot_volume_slices <- function(
    x, overlay = NULL, depths = seq(-100, 100, by = 18),
    which = c("coronal", "axial", "sagittal"), nc = NA,
    col = c("black", "white"), overlay_col = col,
    overlay_alpha = NA, interleave = is.na(overlay_alpha),
    interleave_period = 3, pixel_width = 0.5,
    underlay_range = NULL, overlay_range = NULL, ...) {

  # this function requires htmltools
  stopifnot(package_installed("htmltools"))

  image_size <-  512
  which <- match.arg(which)
  ndepths <- length(depths)
  nc <- as.integer(nc)
  if(is.na(nc)) {
    mfr <- grDevices::n2mfrow(ndepths)
    nr <- mfr[[1]]
    nc <- mfr[[2]]
  } else {
    nr <- ceiling(ndepths / nc)
  }

  x <- ieegio::as_ieegio_volume(x)
  if(is.null(underlay_range)) {
    underlay_range <- range(x[], na.rm = TRUE)
  }
  if(!is.null(overlay)) {
    overlay <- ieegio::as_ieegio_volume(overlay)
    if(is.null(overlay_range)) {
      overlay_range <- range(overlay[], na.rm = TRUE)
    }
  }
  plot_args <- list(
    quote(base64_plot_slice),
    x = quote(x),
    overlay = quote(overlay),
    which = which,
    underlay_col = col,
    overlay_col = overlay_col,
    pixel_width = pixel_width,
    underlay_range = underlay_range,
    overlay_range = overlay_range,
    ...
  )

  call <- as.call(plot_args)

  interleave <- isTRUE(as.logical(interleave))
  if(is.na(overlay_alpha)) {
    overlay_alpha <- ifelse(interleave, 1, 0.5)
  }

  svg_content <- htmltools::tagList(lapply(seq_len(nc), function(col) {
    htmltools::tagList(lapply(seq_len(nr), function(row) {
      idx <- (row - 1) * nc + col
      depth <- depths[idx]
      call$depth <- depth
      base64_images <- eval(call)
      htmltools::tagList(
        format(
          base64_images$underlay_base64, type = "html_svg",
          opacity = 1, width = image_size, height = image_size,
          svg_x = (col - 1) * image_size, svg_y = (row - 1) * image_size
        ),
        local({
          if(length(base64_images$overlay_base64)) {
            format(
              base64_images$overlay_base64, type = "html_svg",
              opacity = overlay_alpha, width = image_size, height = image_size,
              svg_x = (col - 1) * image_size, svg_y = (row - 1) * image_size,
              if(interleave) {
                htmltools::tags$animate(
                  attributeName = "opacity", values="0;1;0",
                  dur = sprintf("%.2fs", interleave_period),
                  repeatCount = "indefinite"
                )
              } else {
                NULL
              }
            )
          } else {
            NULL
          }
        })
      )
    }))
  }))

  svg <- htmltools::tags$svg(
    # width = sprintf("%.0f", image_size * nc),
    # height = sprintf("%.0f", image_size * nr),
    width = "100%",
    viewBox = sprintf("0 0 %.0f, %.0f", image_size * nc, image_size * nr),
    preserveAspectRatio="xMidYMid meet",
    xmlns="http://www.w3.org/2000/svg",
    `xmlns:xlink`="http://www.w3.org/1999/xlink",
    htmltools::tags$rect(width="100%", height="100%", fill="black"),
    htmltools::tagList(svg_content)
  )
  structure(
    svg,
    class = unique(c("ravecore_volume_svg", class(svg)))
  )
}

#' @export
print.ravecore_volume_svg <- function(x, ...) {
  call_pkg_fun("htmltools", "html_print", x)
}

#' @export
plot.ravecore_volume_svg <- function(x, ...) {
  call_pkg_fun("htmltools", "html_print", x)
}


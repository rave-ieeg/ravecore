# Generic cubic-bezier evaluator matching CSS semantics.
# Control points are P0=(0,0), P1=(x1,y1), P2=(x2,y2), P3=(1,1).
# Input t in [0,1] is "time progress"; output is eased progress y in [0,1].
cubic_bezier <- function(t, x1, y1, x2, y2,
                             tol = 1e-7, max_iter = 12) {
  # Vectorized over t
  t <- pmin(pmax(t, 0), 1)

  # Precompute polynomial coeffs for x(s) and y(s):
  ax <- 3*x1 - 3*x2 + 1
  bx <- -6*x1 + 3*x2
  cx <- 3*x1

  ay <- 3*y1 - 3*y2 + 1
  by <- -6*y1 + 3*y2
  cy <- 3*y1

  # Helpers: x(s), dx/ds, y(s)
  bezier_x <- function(s) ((ax*s + bx)*s + cx)*s
  bezier_dx <- function(s) (3*ax*s + 2*bx)*s + cx
  bezier_y <- function(s) ((ay*s + by)*s + cy)*s

  # Solve for s given t: bezier_x(s) == t
  solve_s <- function(tt) {
    # Initial guess close to solution
    s <- tt
    # Newton iterations
    for (i in seq_len(max_iter)) {
      x  <- bezier_x(s)
      dx <- bezier_dx(s)
      err <- x - tt
      if (abs(err) < tol) break
      if (abs(dx) < 1e-10) { s <- NA_real_; break }
      s <- s - err / dx
      if (!is.finite(s) || s < 0 || s > 1) { s <- NA_real_; break }
    }
    # Fallback to bisection if needed
    if (is.na(s)) {
      lo <- 0; hi <- 1; s <- (lo + hi)/2
      for (i in 1:30) {
        x <- bezier_x(s)
        if (abs(x - tt) < tol) break
        if (x < tt) lo <- s else hi <- s
        s <- (lo + hi)/2
      }
    }
    s
  }

  s_vals <- vapply(t, solve_s, numeric(1))
  y <- bezier_y(s_vals)
  pmin(pmax(y, 0), 1)
}

# CSS ease-in-out preset: cubic-bezier(0.42, 0.00, 0.58, 1.00)
ease_in_out <- function(t) {
  cubic_bezier(t, 0.42, 0.00, 0.58, 1.00)
}

# # Examples
# # Single value
# ease_in_out(0.5)
#
# # Vector of samples
# ts <- seq(0, 1, length.out = 11)
# ys <- ease_in_out(ts)
# cbind(t = ts, y = ys)

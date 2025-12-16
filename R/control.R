#' Control Parameters for gkwreg2 Fitting
#'
#' Specifies control parameters for the optimization algorithm used in
#' \code{\link{gkwreg2}}.
#'
#' @param method Optimization method. One of \code{"nlminb"} (default),
#'   \code{"BFGS"}, \code{"Nelder-Mead"}, \code{"CG"}, or \code{"L-BFGS-B"}.
#' @param maxit Maximum number of iterations (default 1000).
#' @param reltol Relative convergence tolerance (default 1e-8).
#' @param trace Integer trace level: 0 = silent (default), 1+ = verbose.
#' @param hessian Logical; if \code{TRUE} (default), compute Hessian for
#'   standard error estimation.
#' @param start Optional named list of starting values for coefficients.
#' @param keep_data Logical; if \code{TRUE} (default), keep data in fitted object.
#' @param use_gradient Logical; if \code{TRUE} (default), use analytical gradient.
#'
#' @return A list of control parameters with class \code{"gkw_control"}.
#'
#' @examples
#' # Default control
#' ctrl <- gkw_control()
#'
#' # Use BFGS with more iterations
#' ctrl <- gkw_control(method = "BFGS", maxit = 2000)
#'
#' # Disable Hessian computation for speed
#' ctrl <- gkw_control(hessian = FALSE)
#'
#' @export
gkw_control <- function(
  method = c("nlminb", "BFGS", "Nelder-Mead", "CG", "L-BFGS-B"),
  maxit = 1000L,
  reltol = 1e-8,
  trace = 0L,
  hessian = TRUE,
  start = NULL,
  keep_data = TRUE,
  use_gradient = TRUE
) {
    method <- match.arg(method)

    structure(
        list(
            method = method,
            maxit = as.integer(maxit),
            reltol = reltol,
            trace = as.integer(trace),
            hessian = hessian,
            start = start,
            keep_data = keep_data,
            use_gradient = use_gradient
        ),
        class = "gkw_control"
    )
}

#' Print gkw_control Object
#'
#' @param x A \code{gkw_control} object.
#' @param ... Ignored.
#'
#' @export
print.gkw_control <- function(x, ...) {
    cat("gkwreg2 Control Parameters:\n")
    cat("  Method:", x$method, "\n")
    cat("  Max iterations:", x$maxit, "\n")
    cat("  Relative tolerance:", format(x$reltol, scientific = TRUE), "\n")
    cat("  Compute Hessian:", x$hessian, "\n")
    cat("  Use gradient:", x$use_gradient, "\n")
    invisible(x)
}

#' @useDynLib gkwreg2, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom stats AIC BIC coef vcov logLik nobs fitted residuals resid confint
#' @importFrom stats formula model.frame model.matrix model.response update predict printCoefmat
#' @importFrom stats nlminb optim qnorm pnorm integrate quantile var rbeta na.pass
#' @importFrom stats cor dnorm lowess pchisq ppoints qqline qqnorm setNames symnum
#' @importFrom grDevices devAskNewPage
#' @importFrom graphics abline curve hist legend lines text
#' @importFrom utils head tail
NULL

#' gkwreg2: Generalized Kumaraswamy Regression Models
#'
#' @description
#' The gkwreg2 package provides regression modeling for bounded data in (0,1)
#' using the Generalized Kumaraswamy distribution family. It implements
#' maximum likelihood estimation with analytical gradients via RcppArmadillo.
#'
#' @section Distribution Families:
#' \itemize{
#'   \item \strong{gkw}: Generalized Kumaraswamy (5 parameters: \eqn{\alpha}, \eqn{\beta}, \eqn{\gamma}, \eqn{\delta}, \eqn{\lambda})
#'   \item \strong{bkw}: Beta-Kumaraswamy (4 parameters: \eqn{\alpha}, \eqn{\beta}, \eqn{\gamma}, \eqn{\delta})
#'   \item \strong{kkw}: Kumaraswamy-Kumaraswamy (4 parameters: \eqn{\alpha}, \eqn{\beta}, \eqn{\delta}, \eqn{\lambda})
#'   \item \strong{ekw}: Exponentiated Kumaraswamy (3 parameters: \eqn{\alpha}, \eqn{\beta}, \eqn{\lambda})
#'   \item \strong{mc}: McDonald (3 parameters: \eqn{\gamma}, \eqn{\delta}, \eqn{\lambda})
#'   \item \strong{kw}: Kumaraswamy (2 parameters: \eqn{\alpha}, \eqn{\beta})
#'   \item \strong{beta}: Beta (2 parameters: \eqn{\gamma}, \eqn{\delta})
#' }
#'
#' @section Main Functions:
#' \itemize{
#'   \item \code{\link{gkwreg2}}: Main model fitting function
#'   \item \code{\link{gkw_control}}: Control parameters for optimization
#'   \item \code{\link{get_param_names}}: Get parameter names for a family
#'   \item \code{\link{get_default_links}}: Get default link functions
#' }
#'
#' @author Evandeilton Lopes
#'
#' @docType package
#' @name gkwreg2-package
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # Ensure gkwdist is loaded
  if (!requireNamespace("gkwdist", quietly = TRUE)) {
    warning("Package 'gkwdist' is required but not available")
  }
}

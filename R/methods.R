#' Print gkwreg2 Object
#'
#' Displays a concise summary of a fitted \code{gkwreg2} model.
#'
#' @param x A fitted \code{gkwreg2} object.
#' @param digits Number of significant digits for printing.
#' @param ... Currently ignored.
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.gkwreg2 <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  if (!inherits(x, "gkwreg2")) {
    stop("'x' must be of class 'gkwreg2'", call. = FALSE)
  }

  # Header and Call
  cat("\nCall:  ")
  print(x$call)
  cat("\n")

  # Residual summary
  if (!is.null(x$residuals) && length(x$residuals) > 0) {
    resids <- x$residuals
    if (length(resids) > 5) {
      rq <- structure(quantile(resids, na.rm = TRUE),
        names = c("Min", "1Q", "Median", "3Q", "Max")
      )
      cat("Residuals:\n")
      print(rq, digits = digits)
    } else {
      cat("Residuals:\n")
      print(resids, digits = digits)
    }
    cat("\n")
  }

  # Coefficients
  if (!is.null(x$coefficients)) {
    cat("Coefficients:\n")
    if (is.list(x$coef_list)) {
      for (pname in names(x$coef_list)) {
        cat("  ", pname, ":\n", sep = "")
        print(x$coef_list[[pname]], digits = digits)
      }
    } else {
      print(x$coefficients, digits = digits)
    }
    cat("\n")
  }

  # Link functions
  if (!is.null(x$link)) {
    links <- x$link
    link_text <- paste(names(links), "=", unlist(links), collapse = ", ")
    cat("Link functions:  ", link_text, "\n\n", sep = "")
  }

  # Fit statistics
  cat("Family: ", x$family, "\n", sep = "")

  if (!is.null(x$loglik)) {
    cat(sprintf("Log-Likelihood: %s", format(x$loglik, digits = digits)))
  }

  if (!is.null(x$npar)) {
    aic <- -2 * x$loglik + 2 * x$npar
    bic <- -2 * x$loglik + log(x$n) * x$npar
    cat(sprintf(
      "\nAIC: %s  BIC: %s", format(aic, digits = digits),
      format(bic, digits = digits)
    ))
  }
  cat("\n")

  # Convergence
  if (!x$converged) {
    cat("\nWarning: Algorithm did not converge\n")
    if (!is.null(x$message)) {
      cat("Message: ", x$message, "\n", sep = "")
    }
  } else {
    cat("Number of iterations: ", x$iterations, "\n", sep = "")
  }

  cat("\n")
  invisible(x)
}

#' Summary for gkwreg2 Object
#'
#' Produces a detailed summary of a fitted \code{gkwreg2} model including
#' coefficient tables with standard errors, z-values, and p-values.
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param ... Currently ignored.
#'
#' @return An object of class \code{"summary.gkwreg2"} with components:
#' \describe{
#'   \item{call}{Model call}
#'   \item{family}{Distribution family}
#'   \item{coefficients}{List of coefficient tables per parameter}
#'   \item{residuals}{Summary statistics for residuals}
#'   \item{loglik}{Log-likelihood}
#'   \item{aic, bic}{Information criteria}
#'   \item{n, npar}{Sample size and number of parameters}
#'   \item{converged}{Convergence status}
#' }
#'
#' @export
summary.gkwreg2 <- function(object, ...) {
  if (!inherits(object, "gkwreg2")) {
    stop("'object' must be of class 'gkwreg2'", call. = FALSE)
  }

  # Build coefficient tables for each parameter
  coef_tables <- list()
  param_names <- names(object$coef_list)

  for (pname in param_names) {
    coefs <- object$coef_list[[pname]]
    nc <- length(coefs)

    # Get start index for this parameter's SEs
    idx <- which(param_names == pname)
    start <- object$param_indices$start[idx] + 1

    # Standard errors (if available)
    if (!is.null(object$se)) {
      se <- object$se[start:(start + nc - 1)]
      z_vals <- coefs / se
      p_vals <- 2 * pnorm(-abs(z_vals))

      coef_table <- cbind(
        Estimate = coefs,
        `Std. Error` = se,
        `z value` = z_vals,
        `Pr(>|z|)` = p_vals
      )
    } else {
      coef_table <- cbind(Estimate = coefs)
    }

    rownames(coef_table) <- names(coefs)
    coef_tables[[pname]] <- coef_table
  }

  # Residual summary
  resid_summary <- NULL
  if (!is.null(object$residuals)) {
    resid_summary <- quantile(object$residuals,
      probs = c(0, 0.25, 0.5, 0.75, 1),
      na.rm = TRUE
    )
    names(resid_summary) <- c("Min", "1Q", "Median", "3Q", "Max")
  }

  # Information criteria
  loglik <- object$loglik
  aic <- -2 * loglik + 2 * object$npar
  bic <- -2 * loglik + log(object$n) * object$npar

  # Create summary object
  out <- list(
    call = object$call,
    family = object$family,
    link = object$link,
    coefficients = coef_tables,
    residuals = resid_summary,
    loglik = loglik,
    aic = aic,
    bic = bic,
    n = object$n,
    npar = object$npar,
    df.residual = object$n - object$npar,
    converged = object$converged,
    iterations = object$iterations,
    message = object$message
  )

  class(out) <- "summary.gkwreg2"
  return(out)
}

#' Print Summary of gkwreg2 Object
#'
#' @param x A \code{summary.gkwreg2} object.
#' @param digits Number of significant digits.
#' @param signif.stars Logical: print significance stars?
#' @param ... Currently ignored.
#'
#' @return Invisibly returns \code{x}.
#'
#' @export
print.summary.gkwreg2 <- function(x, digits = max(3, getOption("digits") - 3),
                                  signif.stars = getOption("show.signif.stars"),
                                  ...) {
  cat("\nCall:\n")
  print(x$call)

  cat("\nFamily:", x$family, "\n")

  # Link functions
  link_text <- paste(names(x$link), "=", unlist(x$link), collapse = ", ")
  cat("Link functions:", link_text, "\n")

  # Residuals
  if (!is.null(x$residuals)) {
    cat("\nResiduals:\n")
    print(x$residuals, digits = digits)
  }

  # Coefficient tables
  cat("\nCoefficients:\n")
  for (pname in names(x$coefficients)) {
    cat("\n--- ", toupper(pname), " ---\n", sep = "")
    printCoefmat(x$coefficients[[pname]],
      digits = digits,
      signif.stars = signif.stars, na.print = "NA", ...
    )
  }

  # Fit statistics
  cat("\n---\n")
  cat(sprintf(
    "Log-Likelihood: %s on %d Df\n",
    format(x$loglik, digits = digits), x$df.residual
  ))
  cat(sprintf(
    "AIC: %s   BIC: %s\n",
    format(x$aic, digits = digits),
    format(x$bic, digits = digits)
  ))
  cat(sprintf("Number of observations: %d\n", x$n))
  cat(sprintf("Number of parameters: %d\n", x$npar))

  # Convergence
  if (!x$converged) {
    cat("\nWarning: Algorithm did not converge\n")
    if (!is.null(x$message)) {
      cat("Message: ", x$message, "\n", sep = "")
    }
  } else {
    cat(sprintf("Converged in %d iterations\n", x$iterations))
  }

  cat("\n")
  invisible(x)
}

#' Extract Coefficients from gkwreg2 Object
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param parameter Optional: name of specific parameter to extract.
#' @param flatten Logical: if \code{TRUE}, return a single named vector.
#' @param ... Currently ignored.
#'
#' @return If \code{flatten = FALSE} (default), a named list of coefficient
#'   vectors per parameter. If \code{flatten = TRUE}, a single named vector.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' x <- rnorm(n)
#' y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
#' df <- data.frame(y = y, x = x)
#' fit <- gkwreg2(y ~ x, data = df, family = "kw")
#' coef(fit) # Vector format (default)
#' coef(fit, flatten = FALSE) # List format
#' coef(fit, parameter = "alpha")
#' }
#'
#' @export
coef.gkwreg2 <- function(object, parameter = NULL, flatten = TRUE, ...) {
  # Default: return flat vector (gkwreg compatible)
  if (!is.null(parameter)) {
    if (!parameter %in% names(object$coef_list)) {
      stop(
        "Parameter '", parameter, "' not found. Available: ",
        paste(names(object$coef_list), collapse = ", ")
      )
    }
    return(object$coef_list[[parameter]])
  }

  if (flatten) {
    return(object$coefficients) # Already flat vector with colon naming
  }

  return(object$coef_list) # List format
}

#' Extract Variance-Covariance Matrix from gkwreg2 Object
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param ... Currently ignored.
#'
#' @return The variance-covariance matrix of the estimated coefficients,
#'   or \code{NULL} if not available.
#'
#' @export
vcov.gkwreg2 <- function(object, ...) {
  if (is.null(object$vcov)) {
    warning(
      "Variance-covariance matrix not available. ",
      "Refit with control = gkw_control(hessian = TRUE)"
    )
    return(NULL)
  }
  return(object$vcov) # Already has correct row/col names
}

#' Confidence Intervals for gkwreg2 Coefficients
#'
#' Computes Wald-type confidence intervals for model parameters.
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param parm Parameter specification (currently ignored, all returned).
#' @param level Confidence level (default 0.95).
#' @param ... Currently ignored.
#'
#' @return A matrix with columns for lower and upper bounds.
#'
#' @export
confint.gkwreg2 <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(object$se)) {
    stop(
      "Standard errors not available. ",
      "Refit with control = gkw_control(hessian = TRUE)"
    )
  }

  cf <- object$coefficients # Already flat vector
  se <- object$se

  a <- (1 - level) / 2
  z <- qnorm(1 - a)

  ci <- cbind(cf - z * se, cf + z * se)
  colnames(ci) <- paste0(format(100 * c(a, 1 - a), trim = TRUE), "%")
  rownames(ci) <- names(cf)

  return(ci)
}

#' Extract Log-Likelihood from gkwreg2 Object
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param ... Currently ignored.
#'
#' @return A \code{logLik} object with attributes \code{df} (degrees of freedom)
#'   and \code{nobs} (number of observations).
#'
#' @export
logLik.gkwreg2 <- function(object, ...) {
  ll <- object$loglik
  attr(ll, "df") <- object$npar
  attr(ll, "nobs") <- object$nobs
  class(ll) <- "logLik"
  return(ll)
}

#' Akaike Information Criterion for gkwreg2 Object
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param ... Additional \code{gkwreg2} objects for comparison.
#' @param k Penalty parameter (default 2 for AIC).
#'
#' @return AIC value(s).
#'
#' @export
AIC.gkwreg2 <- function(object, ..., k = 2) {
  -2 * object$loglik + k * object$npar
}

#' Bayesian Information Criterion for gkwreg2 Object
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param ... Additional \code{gkwreg2} objects for comparison.
#'
#' @return BIC value(s).
#'
#' @export
BIC.gkwreg2 <- function(object, ...) {
  -2 * object$loglik + log(object$n) * object$npar
}

#' Number of Observations for gkwreg2 Object
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param ... Currently ignored.
#'
#' @return Number of observations used in fitting.
#'
#' @export
nobs.gkwreg2 <- function(object, ...) {
  object$nobs
}

#' Extract Fitted Values from gkwreg2 Object
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param type Type of fitted values: \code{"response"} (default) for
#'   expected means, or \code{"parameters"} for fitted distribution parameters.
#' @param ... Currently ignored.
#'
#' @return For \code{type = "response"}, a vector of fitted means.
#'   For \code{type = "parameters"}, a list of parameter vectors.
#'
#' @export
fitted.gkwreg2 <- function(object, type = c("response", "parameters"), ...) {
  type <- match.arg(type)

  if (type == "response") {
    return(object$fitted.values)
  } else {
    return(object$fitted_parameters)
  }
}

#' Model Frame for gkwreg2 Object
#'
#' @param formula A fitted \code{gkwreg2} object.
#' @param ... Currently ignored.
#'
#' @return The model frame used for fitting.
#'
#' @export
model.frame.gkwreg2 <- function(formula, ...) {
  if (is.null(formula$data)) {
    stop("Model frame not available. Refit with control = gkw_control(keep_data = TRUE)")
  }
  return(formula$data)
}

#' Extract Model Matrix from gkwreg2 Object
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param parameter Which parameter's design matrix to extract.
#' @param ... Currently ignored.
#'
#' @return A design matrix, or list of matrices if \code{parameter} not specified.
#'
#' @export
model.matrix.gkwreg2 <- function(object, parameter = NULL, ...) {
  if (!is.null(parameter)) {
    if (!parameter %in% names(object$X_list)) {
      stop(
        "Parameter '", parameter, "' not found. Available: ",
        paste(names(object$X_list), collapse = ", ")
      )
    }
    return(object$X_list[[parameter]])
  }
  return(object$X_list)
}

#' Update a gkwreg2 Model
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param formula. Changes to the formula.
#' @param ... Additional arguments to \code{gkwreg2}.
#' @param evaluate If \code{TRUE}, evaluate the updated call.
#'
#' @return Updated \code{gkwreg2} object or call.
#'
#' @export
update.gkwreg2 <- function(object, formula., ..., evaluate = TRUE) {
  call <- object$call

  if (!missing(formula.)) {
    call$formula <- update(formula(object), formula.)
  }

  extras <- match.call(expand.dots = FALSE)$...
  if (length(extras)) {
    existing <- !is.na(match(names(extras), names(call)))
    for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
    if (any(!existing)) {
      call <- c(as.list(call), extras[!existing])
      call <- as.call(call)
    }
  }

  if (evaluate) {
    eval(call, parent.frame())
  } else {
    call
  }
}

#' Extract Formula from gkwreg2 Object
#'
#' @param x A fitted \code{gkwreg2} object.
#' @param ... Currently ignored.
#'
#' @return The model formula.
#'
#' @export
formula.gkwreg2 <- function(x, ...) {
  x$formula
}

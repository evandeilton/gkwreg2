#' Predict Method for gkwreg2 Objects
#'
#' Generates predictions from a fitted \code{gkwreg2} model for new or
#' existing data.
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param newdata Optional data frame for prediction. If omitted, uses
#'   the original training data.
#' @param type Type of prediction:
#'   \describe{
#'     \item{\code{"response"}}{Expected mean E(Y|X) (default)}
#'     \item{\code{"parameters"}}{All distribution parameters}
#'     \item{\code{"link"}}{Linear predictors (\eqn{\eta})}
#'     \item{\code{"quantile"}}{Quantiles at specified probabilities}
#'     \item{\code{"density"}}{Density at specified y values}
#'     \item{\code{"variance"}}{Conditional variance Var(Y|X)}
#'   }
#' @param at For \code{type = "quantile"}: probabilities.
#'   For \code{type = "density"}: y values.
#' @param se.fit Logical: return standard errors? (Not yet implemented)
#' @param ... Currently ignored.
#'
#' @return Depending on \code{type}:
#'   \describe{
#'     \item{\code{"response"}}{Numeric vector of predicted means}
#'     \item{\code{"parameters"}}{Named list of parameter vectors}
#'     \item{\code{"link"}}{Named list of linear predictor vectors}
#'     \item{\code{"quantile"}}{Matrix with rows per obs, cols per quantile}
#'     \item{\code{"density"}}{Vector of density values}
#'     \item{\code{"variance"}}{Vector of variances}
#'   }
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' x <- rnorm(n)
#' y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
#' df <- data.frame(y = y, x = x)
#' fit <- gkwreg2(y ~ x, data = df, family = "kw")
#'
#' # Predicted means
#' pred <- predict(fit, type = "response")
#'
#' # Distribution parameters
#' params <- predict(fit, type = "parameters")
#'
#' # Quantiles
#' q <- predict(fit, type = "quantile", at = c(0.1, 0.5, 0.9))
#' }
#'
#' @export
predict.gkwreg2 <- function(
  object,
  newdata = NULL,
  type = c("response", "parameters", "link", "quantile", "density", "variance"),
  at = NULL,
  se.fit = FALSE,
  ...
) {
  type <- match.arg(type)

  # Use original data if newdata not provided
  if (is.null(newdata)) {
    if (is.null(object$data)) {
      stop("No data available. Refit with keep_data = TRUE or provide newdata.")
    }
    newdata <- object$data
  }

  n <- nrow(newdata)
  param_names <- names(object$X_list)


  # Create model matrices for new data
  X_new_list <- lapply(param_names, function(pname) {
    f <- object$formula_list[[pname]]
    mf <- model.frame(f, data = newdata, na.action = na.pass)
    model.matrix(f, data = mf)
  })
  names(X_new_list) <- param_names

  # Compute linear predictors
  eta_list <- lapply(param_names, function(pname) {
    X_new <- X_new_list[[pname]]
    beta_p <- object$coef_list[[pname]]
    as.vector(X_new %*% beta_p)
  })
  names(eta_list) <- param_names

  # Return linear predictors if requested
  if (type == "link") {
    return(eta_list)
  }

  # Apply inverse links to get parameters
  params <- lapply(param_names, function(pname) {
    apply_invlink_r(eta_list[[pname]], object$link[[pname]])
  })
  names(params) <- param_names

  # Return parameters if requested
  if (type == "parameters") {
    return(params)
  }

  # Compute predicted response (mean)
  if (type == "response") {
    means <- compute_fitted_means(params, object$family)
    return(means)
  }

  # Compute quantiles
  if (type == "quantile") {
    if (is.null(at)) {
      at <- c(0.1, 0.25, 0.5, 0.75, 0.9)
    }

    # Get quantile function
    q_func <- switch(object$family,
      gkw = gkwdist::qgkw,
      bkw = gkwdist::qbkw,
      kkw = gkwdist::qkkw,
      ekw = gkwdist::qekw,
      mc = gkwdist::qmc,
      kw = gkwdist::qkw,
      beta = gkwdist::qbeta_,
      stop("Unknown family")
    )

    quant_mat <- matrix(NA_real_, nrow = n, ncol = length(at))
    colnames(quant_mat) <- paste0("q", at)

    for (i in seq_len(n)) {
      par_i <- lapply(params, function(p) p[i])
      for (j in seq_along(at)) {
        quant_mat[i, j] <- tryCatch(
          {
            args <- c(list(p = at[j]), par_i)
            do.call(q_func, args)
          },
          error = function(e) NA_real_
        )
      }
    }

    return(quant_mat)
  }

  # Compute density
  if (type == "density") {
    if (is.null(at)) {
      stop("'at' must be specified for type = 'density'")
    }

    d_func <- switch(object$family,
      gkw = gkwdist::dgkw,
      bkw = gkwdist::dbkw,
      kkw = gkwdist::dkkw,
      ekw = gkwdist::dekw,
      mc = gkwdist::dmc,
      kw = gkwdist::dkw,
      beta = gkwdist::dbeta_,
      stop("Unknown family")
    )

    if (length(at) == 1) {
      at <- rep(at, n)
    } else if (length(at) != n) {
      stop("Length of 'at' must be 1 or equal to number of observations")
    }

    dens <- numeric(n)
    for (i in seq_len(n)) {
      par_i <- lapply(params, function(p) p[i])
      dens[i] <- tryCatch(
        {
          args <- c(list(x = at[i]), par_i)
          do.call(d_func, args)
        },
        error = function(e) NA_real_
      )
    }

    return(dens)
  }

  # Compute variance
  if (type == "variance") {
    vars <- numeric(n)

    for (i in seq_len(n)) {
      par_i <- sapply(params, function(p) p[i])

      # Compute E(Y) and E(Y^2) via numerical integration
      mean_i <- compute_mean_numerical(par_i, object$family)

      # E(Y^2)
      d_func <- switch(object$family,
        gkw = gkwdist::dgkw,
        bkw = gkwdist::dbkw,
        kkw = gkwdist::dkkw,
        ekw = gkwdist::dekw,
        mc = gkwdist::dmc,
        kw = gkwdist::dkw,
        beta = gkwdist::dbeta_,
        stop("Unknown family")
      )

      integrand <- function(y) {
        args <- c(list(x = y), as.list(par_i))
        y^2 * do.call(d_func, args)
      }

      ey2 <- tryCatch(
        integrate(integrand, lower = 1e-6, upper = 1 - 1e-6)$value,
        error = function(e) NA_real_
      )

      vars[i] <- ey2 - mean_i^2
    }

    return(pmax(vars, 0)) # Ensure non-negative
  }
}

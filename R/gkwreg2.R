#' Fit Generalized Kumaraswamy Regression Model
#'
#' Fits regression models for bounded (0,1) data using the Generalized
#' Kumaraswamy distribution family. Supports extended formula syntax for
#' specifying different covariates for each distribution parameter.
#'
#' @param formula An extended formula with syntax \code{y ~ x1 | x2 | ...}
#'   where each part separated by \code{|} corresponds to a distribution
#'   parameter. Use the \link[Formula]{Formula} package syntax.
#' @param data A data frame containing the variables in the formula.
#' @param family Distribution family. One of \code{"gkw"}, \code{"bkw"},
#'   \code{"kkw"}, \code{"ekw"}, \code{"mc"}, \code{"kw"}, or \code{"beta"}.
#' @param link Optional named list of link functions for each parameter.
#'   Default: \code{"log"} for positive parameters, \code{"logit"} for
#'   probability parameters. See Details.
#' @param start Optional named list of starting values for coefficients.
#' @param control A list of control parameters from \code{\link{gkw_control}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{"gkwreg2"} with components:
#' \describe{
#'   \item{coefficients}{Named list of coefficient vectors per parameter}
#'   \item{vcov}{Variance-covariance matrix (if Hessian computed)}
#'   \item{se}{Standard errors (if Hessian computed)}
#'   \item{loglik}{Maximized log-likelihood}
#'   \item{fitted.values}{Fitted response means}
#'   \item{residuals}{Response residuals (y - fitted)}
#'   \item{converged}{Logical: did optimization converge?}
#'   \item{family}{Distribution family used}
#'   \item{link}{Link functions used}
#'   \item{formula}{Original formula}
#'   \item{data}{Original data (if \code{keep_data = TRUE})}
#'   \item{call}{Matched call}
#'   \item{n}{Number of observations}
#'   \item{npar}{Total number of parameters}
#' }
#'
#' @details
#' ## Formula Syntax
#' The extended formula uses \code{|} to separate specifications for different
#' parameters. The order matches the parameter order for each family:
#' \itemize{
#'   \item \code{kw}: \code{y ~ alpha_formula | beta_formula}
#'   \item \code{beta}: \code{y ~ gamma_formula | delta_formula}
#'   \item \code{ekw}: \code{y ~ alpha | beta | lambda}
#'   \item etc.
#' }
#'
#' Unspecified parts default to intercept-only models.
#'
#' ## Link Functions
#' Available links:
#' \itemize{
#'   \item For positive parameters: \code{"log"} (default), \code{"sqrt"},
#'     \code{"inverse"}, \code{"identity"}
#'   \item For probability parameters: \code{"logit"} (default), \code{"probit"},
#'     \code{"cloglog"}, \code{"cauchy"}
#' }
#'
#' @examples
#' \donttest{
#' # Simulate Kumaraswamy data
#' set.seed(123)
#' n <- 200
#' x <- rnorm(n)
#' alpha <- exp(0.5 + 0.3 * x)
#' beta <- exp(1.0)
#' y <- gkwdist::rkw(n, alpha = alpha, beta = beta)
#' df <- data.frame(y = y, x = x)
#'
#' # Fit Kumaraswamy regression
#' fit <- gkwreg2(y ~ x, data = df, family = "kw")
#' summary(fit)
#'
#' # Different covariates per parameter
#' fit2 <- gkwreg2(y ~ x | 1, data = df, family = "kw")
#'
#' # Beta regression
#' fit_beta <- gkwreg2(y ~ x, data = df, family = "beta")
#' }
#'
#' @seealso
#' \code{\link{gkw_control}} for control parameters,
#' \code{\link{summary.gkwreg2}} for model summaries,
#' \code{\link{predict.gkwreg2}} for predictions,
#' \code{\link{residuals.gkwreg2}} for residuals.
#'
#' @export
gkwreg2 <- function(
  formula,
  data,
  family = c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"),
  link = NULL,
  start = NULL,
  control = gkw_control(),
  ...
) {
    # Capture call
    call <- match.call()

    # Match family

    family <- match.arg(family)

    # Validate formula
    if (!inherits(formula, "formula")) {
        stop("'formula' must be a formula object")
    }

    # Validate data
    if (missing(data) || !is.data.frame(data)) {
        stop("'data' must be a data frame")
    }

    # Validate control
    if (!inherits(control, "gkw_control")) {
        control <- gkw_control()
    }

    # Parse extended formula
    formula_list <- parse_extended_formula(formula, family)

    # Extract response
    y <- extract_response(formula, data)
    n <- length(y)

    # Validate response
    if (any(y <= 0 | y >= 1, na.rm = TRUE)) {
        stop("Response variable must be strictly in (0, 1)")
    }
    if (any(is.na(y))) {
        stop("Response variable contains NA values")
    }

    # Create model matrices
    X_list <- create_model_matrices(formula_list, data)

    # Set default links
    if (is.null(link)) {
        link <- get_default_links(family)
    } else {
        # Merge with defaults
        default_link <- get_default_links(family)
        for (p in names(default_link)) {
            if (is.null(link[[p]])) {
                link[[p]] <- default_link[[p]]
            }
        }
    }

    # Get parameter indices
    indices <- get_param_indices(X_list)
    n_total <- indices$total

    # Get starting values
    if (is.null(start)) {
        if (!is.null(control$start)) {
            start <- control$start
        } else {
            start <- get_starting_values(y, X_list, family, link, control)
        }
    } else if (is.list(start)) {
        start <- unlist(start)
    }

    # Validate starting values length
    if (length(start) != n_total) {
        stop(
            "Length of starting values (", length(start), ") does not match ",
            "total number of coefficients (", n_total, ")"
        )
    }

    # Define objective function
    obj_fn <- function(theta) {
        objective_gkwreg_vec(
            theta_concat = theta,
            y = y,
            X_list = X_list,
            family = family,
            link_list = link,
            param_indices = indices$start,
            n_coefs = indices$n_coefs
        )
    }

    # Define gradient function
    grad_fn <- function(theta) {
        gradient_gkwreg(
            theta_concat = theta,
            y = y,
            X_list = X_list,
            family = family,
            link_list = link,
            param_indices = indices$start,
            n_coefs = indices$n_coefs
        )
    }

    # Optimize
    if (control$method == "nlminb") {
        opt_result <- nlminb(
            start = start,
            objective = obj_fn,
            gradient = if (control$use_gradient) grad_fn else NULL,
            control = list(
                iter.max = control$maxit,
                eval.max = control$maxit * 2,
                rel.tol = control$reltol,
                trace = control$trace
            )
        )
        converged <- opt_result$convergence == 0
        theta_hat <- opt_result$par
        loglik <- -opt_result$objective
        iterations <- opt_result$iterations
    } else {
        # Build control list based on method
        optim_control <- list(
            maxit = control$maxit,
            trace = control$trace
        )

        # L-BFGS-B uses factr and pgtol, not reltol
        if (control$method == "L-BFGS-B") {
            # factr controls relative tolerance: convergence when
            # (f_k - f_(k+1)) / max(|f_k|, |f_(k+1)|, 1) <= factr * .Machine$double.eps
            # Default is 1e7; lower = more precise
            optim_control$factr <- control$reltol / .Machine$double.eps
            optim_control$pgtol <- 0 # tolerance on projected gradient
        } else {
            optim_control$reltol <- control$reltol
        }

        opt_result <- optim(
            par = start,
            fn = obj_fn,
            gr = if (control$use_gradient) grad_fn else NULL,
            method = control$method,
            control = optim_control,
            hessian = control$hessian
        )
        converged <- opt_result$convergence == 0
        theta_hat <- opt_result$par
        loglik <- -opt_result$value
        iterations <- opt_result$counts[1]
    }

    # Compute variance-covariance matrix if requested
    vcov_mat <- NULL
    se <- NULL

    if (control$hessian) {
        # Compute Hessian numerically
        hess <- tryCatch(
            {
                if (requireNamespace("numDeriv", quietly = TRUE)) {
                    numDeriv::hessian(obj_fn, theta_hat)
                } else {
                    # Simple finite difference Hessian
                    eps <- 1e-5
                    n_theta <- length(theta_hat)
                    H <- matrix(0, n_theta, n_theta)
                    f0 <- obj_fn(theta_hat)
                    for (i in 1:n_theta) {
                        for (j in i:n_theta) {
                            theta_pp <- theta_hat
                            theta_pm <- theta_hat
                            theta_mp <- theta_hat
                            theta_mm <- theta_hat

                            theta_pp[i] <- theta_pp[i] + eps
                            theta_pp[j] <- theta_pp[j] + eps
                            theta_pm[i] <- theta_pm[i] + eps
                            theta_pm[j] <- theta_pm[j] - eps
                            theta_mp[i] <- theta_mp[i] - eps
                            theta_mp[j] <- theta_mp[j] + eps
                            theta_mm[i] <- theta_mm[i] - eps
                            theta_mm[j] <- theta_mm[j] - eps

                            H[i, j] <- (obj_fn(theta_pp) - obj_fn(theta_pm) -
                                obj_fn(theta_mp) + obj_fn(theta_mm)) / (4 * eps^2)
                            H[j, i] <- H[i, j]
                        }
                    }
                    H
                }
            },
            error = function(e) NULL
        )

        if (!is.null(hess)) {
            vcov_mat <- tryCatch(
                solve(hess),
                error = function(e) {
                    warning("Hessian is singular. Variance-covariance matrix not available.")
                    NULL
                }
            )

            if (!is.null(vcov_mat)) {
                # Check for negative variances (indicates convergence issues)
                diag_vcov <- diag(vcov_mat)
                if (any(is.na(diag_vcov)) || any(diag_vcov < 0, na.rm = TRUE)) {
                    warning("Some variance estimates are negative or NA. Model may not have converged properly.")
                    se <- rep(NA_real_, length(diag_vcov))
                } else {
                    se <- sqrt(diag_vcov)
                }
            }
        }
    }

    # Organize coefficients with colon naming (gkwreg compatible)
    coef_list <- organize_coefficients(theta_hat, X_list, indices)

    # Create flat coefficients with colon naming (gkwreg style: "alpha:(Intercept)")
    coef_flat <- unlist(coef_list)
    names(coef_flat) <- gsub("\\.", ":", names(coef_flat))

    # Compute fitted parameters and means
    fitted_params <- compute_fitted_params(theta_hat, X_list, family, link, indices)
    fitted_vals <- compute_fitted_means(fitted_params, family)

    # Compute residuals
    resids <- y - fitted_vals

    # =========================================================================
    # GKWREG COMPATIBILITY: Compute additional metrics
    # =========================================================================

    # AIC and BIC
    aic <- -2 * loglik + 2 * n_total
    bic <- -2 * loglik + log(n) * n_total

    # Deviance (for bounded distributions: -2 * loglik)
    deviance <- -2 * loglik

    # Degrees of freedom
    df.residual <- n - n_total

    # RMSE
    rmse <- sqrt(mean(resids^2, na.rm = TRUE))

    # Efron's pseudo R-squared
    ss_res <- sum(resids^2, na.rm = TRUE)
    ss_tot <- sum((y - mean(y))^2, na.rm = TRUE)
    efron_r2 <- 1 - ss_res / ss_tot

    # Mean Absolute Error
    mean_absolute_error <- mean(abs(resids), na.rm = TRUE)

    # Parameter names
    param_names <- get_param_names(family)

    # Model frame
    model <- tryCatch(
        model.frame(formula, data = data),
        error = function(e) NULL
    )

    # Rename se with colon style
    if (!is.null(se)) {
        names(se) <- names(coef_flat)
    }

    # Rename vcov with colon style
    if (!is.null(vcov_mat)) {
        rownames(vcov_mat) <- names(coef_flat)
        colnames(vcov_mat) <- names(coef_flat)
    }

    # Create parameter vectors (for gkwreg compatibility)
    parameter_vectors <- fitted_params

    # Link scale (for gkwreg compatibility)
    link_scale <- lapply(X_list, function(X) {
        matrix(0, nrow = nrow(X), ncol = 1)
    })
    for (i in seq_along(param_names)) {
        pname <- param_names[i]
        X_p <- X_list[[pname]]
        start <- indices$start[i] + 1
        nc <- indices$n_coefs[i]
        beta_p <- theta_hat[start:(start + nc - 1)]
        link_scale[[pname]] <- as.vector(X_p %*% beta_p)
    }

    # Create output object with FULL gkwreg compatibility
    out <- structure(
        list(
            # Core components (same names as gkwreg)
            coefficients = coef_flat,
            fitted.values = fitted_vals,
            residuals = resids,
            fitted_parameters = fitted_params,
            parameter_vectors = parameter_vectors,
            link = link,
            link_scale = link_scale,
            param_names = param_names,
            fixed_params = list(),
            loglik = loglik,
            aic = aic,
            bic = bic,
            deviance = deviance,
            df.residual = df.residual,
            nobs = n,
            npar = n_total,
            vcov = vcov_mat,
            se = se,
            convergence = converged,
            message = if (control$method == "nlminb") opt_result$message else NULL,
            iterations = iterations,
            rmse = rmse,
            efron_r2 = efron_r2,
            mean_absolute_error = mean_absolute_error,
            method = control$method,
            control = control,
            y = y,
            model = model,
            formula = formula,
            family = family,
            call = call,
            # Additional gkwreg2-specific (kept for internal use)
            n = n,
            converged = converged,
            X_list = X_list,
            theta_hat = theta_hat,
            param_indices = indices,
            formula_list = formula_list,
            data = if (control$keep_data) data else NULL,
            coef_list = coef_list,
            fitted.params = fitted_params
        ),
        class = c("gkwreg2", "gkwreg") # Inherit from gkwreg for method compatibility
    )

    return(out)
}

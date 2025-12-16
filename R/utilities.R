#' Get Default Link Functions for Family
#'
#' Returns the default link functions for each parameter of a distribution family.
#' All parameters are positive (> 0), so log link is appropriate.
#'
#' @param family Distribution family name.
#'
#' @return Named list of link function names.
#'
#' @details
#' Parameter spaces for each family:
#' \itemize{
#'   \item kw: alpha > 0, beta > 0
#'   \item beta: gamma > 0, delta > 0 (shape1, shape2)
#'   \item ekw: alpha > 0, beta > 0, lambda > 0
#'   \item mc: gamma > 0, delta > 0, lambda > 0
#'   \item bkw: alpha > 0, beta > 0, gamma > 0, delta > 0
#'   \item kkw: alpha > 0, beta > 0, delta > 0, lambda > 0
#'   \item gkw: alpha > 0, beta > 0, gamma > 0, delta > 0, lambda > 0
#' }
#'
#' All parameters are strictly positive, so log link is the natural choice
#' as it maps R -> (0, inf).
#'
#' @examples
#' get_default_links("kw")
#' # $alpha: "log", $beta: "log"
#'
#' @export
get_default_links <- function(family) {
  param_names <- get_param_names(family)

  # All parameters are positive (> 0), so log link is appropriate
  links <- setNames(rep("log", length(param_names)), param_names)

  return(as.list(links))
}

#' Get Parameter Indices in Concatenated Coefficient Vector
#'
#' Computes the start indices and counts for each parameter's coefficients
#' in the concatenated theta vector.
#'
#' @param X_list Named list of design matrices.
#'
#' @return List with \code{start} (0-indexed start positions) and
#'   \code{n_coefs} (number of coefficients per parameter).
#'
#' @keywords internal
get_param_indices <- function(X_list) {
  n_coefs <- sapply(X_list, ncol)
  start <- c(0L, cumsum(n_coefs[-length(n_coefs)]))

  list(
    start = as.integer(start),
    n_coefs = as.integer(n_coefs),
    total = sum(n_coefs)
  )
}

#' Get Starting Values for Optimization
#'
#' Computes robust starting values for the regression coefficients
#' using moment matching based on the sample mean and variance.
#' This is crucial for optimizers like BFGS that don't handle bounds.
#'
#' @param y Response vector (values in (0,1)).
#' @param X_list Named list of design matrices.
#' @param family Distribution family.
#' @param link Named list of link functions.
#' @param control Control parameters (optional).
#'
#' @return Named numeric vector of starting values.
#'
#' @details
#' The function uses moment matching when possible:
#' \itemize{
#'   \item For Kumaraswamy: approximate based on sample mean
#'   \item For Beta: use method of moments (gamma, delta from mean and variance)
#'   \item For others: use reasonable default values that ensure stability
#' }
#'
#' All starting values are chosen to be well within the valid parameter space
#' to avoid boundary issues during optimization.
#'
#' @keywords internal
get_starting_values <- function(y, X_list, family, link, control = NULL) {
  param_names <- names(X_list)
  start_list <- list()

  # Compute sample moments (clamped to valid range)
  y_mean <- mean(y, na.rm = TRUE)
  y_var <- var(y, na.rm = TRUE)

  # Clamp to safe values
  y_mean <- max(0.05, min(0.95, y_mean))
  y_var <- max(0.001, min(0.2, y_var))

  # Get moment-based starting parameters for common families
  init_params <- get_moment_based_params(y_mean, y_var, family)

  for (pname in param_names) {
    X_p <- X_list[[pname]]
    n_coef <- ncol(X_p)
    link_p <- link[[pname]]

    # Get initial parameter value from moment matching or default
    if (!is.null(init_params[[pname]])) {
      init_val <- init_params[[pname]]
    } else {
      # Default: 1.0 for positive parameters
      init_val <- 1.0
    }

    # Ensure value is safely positive (avoid boundary)
    init_val <- max(0.1, min(10, init_val))

    # Transform to link scale for intercept
    init_eta <- apply_link(init_val, link_p)

    # Set intercept, small random values for other coefficients
    # Small non-zero values help with gradient computation
    if (n_coef == 1) {
      beta_p <- init_eta
    } else {
      beta_p <- c(init_eta, rep(0.01, n_coef - 1))
    }
    names(beta_p) <- colnames(X_p)

    start_list[[pname]] <- beta_p
  }

  # Flatten to vector
  start <- unlist(start_list, use.names = TRUE)

  return(start)
}

#' Get Moment-Based Starting Parameters
#'
#' Computes reasonable starting parameter values based on sample moments.
#'
#' @param y_mean Sample mean of response.
#' @param y_var Sample variance of response.
#' @param family Distribution family.
#'
#' @return Named list of initial parameter values.
#'
#' @keywords internal
get_moment_based_params <- function(y_mean, y_var, family) {
  params <- list()

  if (family == "beta") {
    # Method of moments for Beta(gamma, delta)
    # E[Y] = gamma / (gamma + delta)
    # Var[Y] = gamma * delta / ((gamma + delta)^2 * (gamma + delta + 1))

    # Solve for gamma and delta
    if (y_var < y_mean * (1 - y_mean)) {
      # Use method of moments
      phi <- y_mean * (1 - y_mean) / y_var - 1
      phi <- max(2, min(100, phi)) # Clamp precision

      gamma <- y_mean * phi
      delta <- (1 - y_mean) * phi

      params$gamma <- max(0.5, min(10, gamma))
      params$delta <- max(0.5, min(10, delta))
    } else {
      params$gamma <- 2.0
      params$delta <- 2.0
    }
  } else if (family == "kw") {
    # For Kumaraswamy, use heuristic based on mean
    # E[Y] ≈ beta * B(1 + 1/alpha, beta)
    # Start with alpha ≈ 1, beta from mean

    params$alpha <- 1.5
    # Approximate: if E[Y] = mu, and alpha = 1.5, then beta ≈ ...
    # Use simple heuristic
    if (y_mean > 0.5) {
      params$beta <- 2.0 + (y_mean - 0.5) * 2
    } else {
      params$beta <- 1.5
    }
    params$alpha <- max(0.5, min(5, params$alpha))
    params$beta <- max(0.5, min(5, params$beta))
  } else if (family == "ekw") {
    # Exponentiated Kumaraswamy: base on kw
    params$alpha <- 1.5
    params$beta <- 2.0
    params$lambda <- 1.0
  } else if (family == "mc") {
    # McDonald: use beta-like initialization
    phi <- y_mean * (1 - y_mean) / y_var - 1
    phi <- max(2, min(20, phi))

    params$gamma <- max(0.5, min(5, y_mean * phi))
    params$delta <- max(0.5, min(5, (1 - y_mean) * phi))
    params$lambda <- 1.0
  } else if (family == "bkw") {
    # Beta-Kumaraswamy: combine heuristics
    params$alpha <- 1.5
    params$beta <- 2.0
    params$gamma <- 1.5
    params$delta <- 1.5
  } else if (family == "kkw") {
    # Kumaraswamy-Kumaraswamy
    params$alpha <- 1.5
    params$beta <- 2.0
    params$delta <- 1.5
    params$lambda <- 1.0
  } else if (family == "gkw") {
    # Generalized Kumaraswamy: most complex
    params$alpha <- 1.0
    params$beta <- 1.5
    params$gamma <- 1.0
    params$delta <- 1.0
    params$lambda <- 1.0
  }

  return(params)
}

#' Apply Link Function (forward transform)
#'
#' Transforms parameter value to linear predictor scale.
#'
#' @param val Parameter value (in natural scale).
#' @param link_type Link function name.
#'
#' @return Transformed value (eta).
#'
#' @keywords internal
apply_link <- function(val, link_type) {
  # Ensure val is safe for transformation
  val <- max(1e-6, min(1e6, val))

  switch(link_type,
    log = log(val),
    sqrt = sqrt(val),
    inverse = 1 / val,
    identity = val,
    logit = {
      val <- max(0.001, min(0.999, val))
      log(val / (1 - val))
    },
    probit = {
      val <- max(0.001, min(0.999, val))
      qnorm(val)
    },
    cloglog = {
      val <- max(0.001, min(0.999, val))
      log(-log(1 - val))
    },
    cauchy = {
      val <- max(0.001, min(0.999, val))
      tan(pi * (val - 0.5))
    },
    log(val) # default to log
  )
}

#' Organize Coefficients into Named List
#'
#' Converts the flat coefficient vector back into a named list structure.
#'
#' @param theta_hat Fitted coefficient vector.
#' @param X_list Named list of design matrices.
#' @param indices Parameter indices from \code{get_param_indices}.
#'
#' @return Named list of coefficient vectors.
#'
#' @keywords internal
organize_coefficients <- function(theta_hat, X_list, indices) {
  param_names <- names(X_list)
  coef_list <- list()

  for (i in seq_along(param_names)) {
    pname <- param_names[i]
    start <- indices$start[i] + 1 # Convert to 1-indexed
    nc <- indices$n_coefs[i]

    beta_p <- theta_hat[start:(start + nc - 1)]
    names(beta_p) <- colnames(X_list[[pname]])

    coef_list[[pname]] <- beta_p
  }

  return(coef_list)
}

#' Compute Fitted Parameter Values
#'
#' Computes the fitted distribution parameters for each observation.
#'
#' @param theta_hat Fitted coefficient vector.
#' @param X_list Named list of design matrices.
#' @param family Distribution family.
#' @param link Named list of link functions.
#' @param indices Parameter indices.
#'
#' @return Named list of fitted parameter vectors.
#'
#' @keywords internal
compute_fitted_params <- function(theta_hat, X_list, family, link, indices) {
  param_names <- names(X_list)
  params <- list()

  for (i in seq_along(param_names)) {
    pname <- param_names[i]
    X_p <- X_list[[pname]]

    start <- indices$start[i] + 1
    nc <- indices$n_coefs[i]
    beta_p <- theta_hat[start:(start + nc - 1)]

    # Linear predictor
    eta_p <- as.vector(X_p %*% beta_p)

    # Apply inverse link
    link_p <- link[[pname]]
    params[[pname]] <- apply_invlink_r(eta_p, link_p)
  }

  return(params)
}

#' Apply Inverse Link Function (R version)
#'
#' R wrapper for applying inverse link functions.
#'
#' @param eta Linear predictor vector.
#' @param link_type Link function name.
#'
#' @return Transformed parameter values.
#'
#' @keywords internal
apply_invlink_r <- function(eta, link_type) {
  switch(link_type,
    log = exp(eta),
    sqrt = eta^2,
    inverse = 1 / eta,
    identity = eta,
    logit = 1 / (1 + exp(-eta)),
    probit = pnorm(eta),
    cloglog = 1 - exp(-exp(eta)),
    cauchy = atan(eta) / pi + 0.5,
    stop("Unknown link type: ", link_type)
  )
}

#' Compute Fitted Response Means
#'
#' Computes the expected value E(Y|X) for each observation.
#'
#' @param params Named list of fitted parameter vectors.
#' @param family Distribution family.
#'
#' @return Numeric vector of fitted means.
#'
#' @keywords internal
compute_fitted_means <- function(params, family) {
  n <- length(params[[1]])

  if (family == "kw") {
    # E(X) = beta * B(1 + 1/alpha, beta) for Kumaraswamy
    alpha <- params$alpha
    beta_param <- params$beta
    means <- beta_param * beta(1 + 1 / alpha, beta_param)
  } else if (family == "beta") {
    # E(X) = gamma / (gamma + delta) for Beta
    means <- params$gamma / (params$gamma + params$delta)
  } else {
    # For other distributions, use numerical integration
    means <- numeric(n)
    for (i in seq_len(n)) {
      par_i <- sapply(params, function(p) p[i])
      means[i] <- compute_mean_numerical(par_i, family)
    }
  }

  return(means)
}

#' Compute Mean via Numerical Integration
#'
#' @param par Parameter vector for a single observation.
#' @param family Distribution family.
#'
#' @return Expected value E(Y).
#'
#' @keywords internal
compute_mean_numerical <- function(par, family) {
  # Get density function
  d_func <- switch(family,
    gkw = gkwdist::dgkw,
    bkw = gkwdist::dbkw,
    kkw = gkwdist::dkkw,
    ekw = gkwdist::dekw,
    mc = gkwdist::dmc,
    kw = gkwdist::dkw,
    beta = gkwdist::dbeta_,
    stop("Unknown family")
  )

  # Compute E(Y) = integral of y * f(y) from 0 to 1
  integrand <- function(y) {
    args <- c(list(x = y), as.list(par))
    y * do.call(d_func, args)
  }

  result <- tryCatch(
    integrate(integrand, lower = 1e-6, upper = 1 - 1e-6)$value,
    error = function(e) NA_real_
  )

  return(result)
}

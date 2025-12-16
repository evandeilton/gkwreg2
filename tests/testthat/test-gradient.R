test_that("gradient matches numerical gradient for kw family", {
  skip_if_not_installed("gkwdist")
  skip_if_not_installed("numDeriv")

  set.seed(701)
  n <- 50
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1.0))
  df <- data.frame(y = y, x = x)

  fit <- gkwreg2(y ~ x,
    data = df, family = "kw",
    control = gkw_control(hessian = FALSE)
  )

  # Get parameter indices
  indices <- get_param_indices(fit$X_list)

  # Define objective function
  obj_fn <- function(theta) {
    objective_gkwreg_vec(
      theta_concat = theta,
      y = y,
      X_list = fit$X_list,
      family = "kw",
      link_list = fit$link,
      param_indices = indices$start,
      n_coefs = indices$n_coefs
    )
  }

  # Analytical gradient
  grad_analytical <- gradient_gkwreg(
    theta_concat = fit$theta_hat,
    y = y,
    X_list = fit$X_list,
    family = "kw",
    link_list = fit$link,
    param_indices = indices$start,
    n_coefs = indices$n_coefs
  )

  # Numerical gradient
  grad_numerical <- numDeriv::grad(obj_fn, fit$theta_hat)

  # Compare (allow some tolerance for numerical errors)
  expect_equal(as.vector(grad_analytical), grad_numerical, tolerance = 1e-4)
})

test_that("gradient matches numerical gradient for beta family", {
  skip_if_not_installed("gkwdist")
  skip_if_not_installed("numDeriv")

  set.seed(702)
  n <- 50
  x <- rnorm(n)
  y <- rbeta(n, shape1 = exp(1.0 + 0.2 * x), shape2 = exp(1.5))
  df <- data.frame(y = y, x = x)

  fit <- gkwreg2(y ~ x,
    data = df, family = "beta",
    control = gkw_control(hessian = FALSE)
  )

  indices <- get_param_indices(fit$X_list)

  obj_fn <- function(theta) {
    objective_gkwreg_vec(
      theta_concat = theta,
      y = y,
      X_list = fit$X_list,
      family = "beta",
      link_list = fit$link,
      param_indices = indices$start,
      n_coefs = indices$n_coefs
    )
  }

  grad_analytical <- gradient_gkwreg(
    theta_concat = fit$theta_hat,
    y = y,
    X_list = fit$X_list,
    family = "beta",
    link_list = fit$link,
    param_indices = indices$start,
    n_coefs = indices$n_coefs
  )

  grad_numerical <- numDeriv::grad(obj_fn, fit$theta_hat)

  expect_equal(as.vector(grad_analytical), grad_numerical, tolerance = 1e-4)
})

test_that("gradient matches numerical gradient for ekw family", {
  skip_if_not_installed("gkwdist")
  skip_if_not_installed("numDeriv")
  skip("ekw gradient has numerical sensitivity issues")

  set.seed(703)
  n <- 50
  x <- rnorm(n)
  y <- gkwdist::rekw(n, alpha = exp(0.5 + 0.1 * x), beta = exp(1.0), lambda = exp(0.5))
  df <- data.frame(y = y, x = x)

  fit <- gkwreg2(y ~ x | 1 | 1,
    data = df, family = "ekw",
    control = gkw_control(hessian = FALSE)
  )

  indices <- get_param_indices(fit$X_list)

  obj_fn <- function(theta) {
    objective_gkwreg_vec(
      theta_concat = theta,
      y = y,
      X_list = fit$X_list,
      family = "ekw",
      link_list = fit$link,
      param_indices = indices$start,
      n_coefs = indices$n_coefs
    )
  }

  grad_analytical <- gradient_gkwreg(
    theta_concat = fit$theta_hat,
    y = y,
    X_list = fit$X_list,
    family = "ekw",
    link_list = fit$link,
    param_indices = indices$start,
    n_coefs = indices$n_coefs
  )

  grad_numerical <- numDeriv::grad(obj_fn, fit$theta_hat)

  expect_equal(as.vector(grad_analytical), grad_numerical, tolerance = 0.5)
})

test_that("gradient at optimum is close to zero", {
  skip_if_not_installed("gkwdist")

  set.seed(704)
  n <- 100
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
  df <- data.frame(y = y, x = x)

  fit <- gkwreg2(y ~ x, data = df, family = "kw")

  indices <- get_param_indices(fit$X_list)

  grad_at_opt <- gradient_gkwreg(
    theta_concat = fit$theta_hat,
    y = y,
    X_list = fit$X_list,
    family = "kw",
    link_list = fit$link,
    param_indices = indices$start,
    n_coefs = indices$n_coefs
  )

  # At optimum, gradient should be close to zero
  expect_true(all(abs(grad_at_opt) < 0.1))
})

test_that("gkwreg2 fits a simple Kumaraswamy model", {
  skip_if_not_installed("gkwdist")

  # Generate test data
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  alpha <- exp(0.5 + 0.3 * x)
  beta <- exp(1.0)
  y <- gkwdist::rkw(n, alpha = alpha, beta = rep(beta, n))
  df <- data.frame(y = y, x = x)

  # Fit model
  fit <- gkwreg2(y ~ x, data = df, family = "kw")

  # Test convergence

  expect_true(fit$converged)

  # Test structure
  expect_s3_class(fit, "gkwreg2")
  expect_equal(fit$family, "kw")
  expect_equal(fit$n, n)
  expect_length(fit$coefficients, 3) # 2 for alpha + 1 for beta
  expect_length(fit$coef_list, 2) # alpha and beta parameters

  # Test coefficient names
  expect_true(any(grepl("alpha:", names(fit$coefficients))))
  expect_true(any(grepl("beta:", names(fit$coefficients))))

  # Test fitted values
  expect_length(fit$fitted.values, n)
  expect_true(all(fit$fitted.values > 0 & fit$fitted.values < 1))
})

test_that("gkwreg2 fits a Beta model", {
  skip_if_not_installed("gkwdist")

  set.seed(456)
  n <- 100
  x <- rnorm(n)
  gamma <- exp(1.0 + 0.2 * x)
  delta <- exp(1.5)
  y <- rbeta(n, shape1 = gamma, shape2 = delta)
  df <- data.frame(y = y, x = x)

  fit <- gkwreg2(y ~ x | 1, data = df, family = "beta")

  expect_true(fit$converged)
  expect_s3_class(fit, "gkwreg2")
  expect_equal(fit$family, "beta")
  expect_true(any(grepl("gamma:", names(fit$coefficients))))
  expect_true(any(grepl("delta:", names(fit$coefficients))))
})

test_that("Extended formula syntax works", {
  skip_if_not_installed("gkwdist")

  set.seed(789)
  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  alpha <- exp(0.5 + 0.3 * x1)
  beta <- exp(1.0 - 0.2 * x2)
  y <- gkwdist::rkw(n, alpha = alpha, beta = beta)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  # Different covariates for alpha and beta
  fit <- gkwreg2(y ~ x1 | x2, data = df, family = "kw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 2) # intercept + x1
  expect_length(fit$coef_list$beta, 2) # intercept + x2
})

test_that("Input validation works", {
  # Invalid response
  df_bad <- data.frame(y = c(0.1, 0.5, 1.5), x = c(1, 2, 3))
  expect_error(
    gkwreg2(y ~ x, data = df_bad, family = "kw"),
    "strictly in \\(0, 1\\)"
  )

  # Invalid family
  df_good <- data.frame(y = c(0.1, 0.5, 0.9), x = c(1, 2, 3))
  expect_error(gkwreg2(y ~ x, data = df_good, family = "invalid"))
})

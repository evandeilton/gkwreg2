test_that("residuals type='response' works", {
  skip_if_not_installed("gkwdist")

  set.seed(401)
  n <- 100
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
  df <- data.frame(y = y, x = x)
  fit <- gkwreg2(y ~ x, data = df, family = "kw")

  res <- residuals(fit, type = "response")

  expect_length(res, n)
  expect_equal(res, y - fitted(fit))
  expect_true(abs(mean(res)) < 0.1) # Should be centered near 0
})

test_that("residuals type='quantile' works", {
  skip_if_not_installed("gkwdist")

  set.seed(402)
  n <- 200
  x <- rnorm(n)
  alpha <- exp(0.5 + 0.2 * x)
  y <- gkwdist::rkw(n, alpha = alpha, beta = exp(1.0))
  df <- data.frame(y = y, x = x)
  fit <- gkwreg2(y ~ x, data = df, family = "kw")

  qres <- residuals(fit, type = "quantile")

  expect_length(qres, n)
  expect_false(any(is.na(qres)))

  # Quantile residuals should be approximately normal if model is correct
  # Shapiro-Wilk test (not too strict due to randomness)
  sw_test <- shapiro.test(qres)
  # We don't expect perfect normality, just reasonable
  expect_true(sw_test$p.value > 0.001)

  # Mean should be close to 0, sd close to 1
  expect_true(abs(mean(qres)) < 0.3)
  expect_true(abs(sd(qres) - 1) < 0.3)
})

test_that("residuals type='pearson' works", {
  skip_if_not_installed("gkwdist")

  set.seed(403)
  n <- 100
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
  df <- data.frame(y = y, x = x)
  fit <- gkwreg2(y ~ x, data = df, family = "kw")

  pres <- residuals(fit, type = "pearson")

  expect_length(pres, n)
  expect_false(any(is.na(pres)))
  expect_true(is.finite(sum(pres^2))) # Sum of squares should be finite
})

test_that("residuals type='deviance' works", {
  skip_if_not_installed("gkwdist")

  set.seed(404)
  n <- 100
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
  df <- data.frame(y = y, x = x)
  fit <- gkwreg2(y ~ x, data = df, family = "kw")

  dres <- residuals(fit, type = "deviance")

  expect_length(dres, n)
  expect_false(any(is.na(dres)))
})

test_that("residuals work for all families", {
  skip_if_not_installed("gkwdist")

  set.seed(405)
  n <- 100
  x <- rnorm(n)

  # Test for beta
  y_beta <- rbeta(n, shape1 = exp(1.0), shape2 = exp(1.5))
  df_beta <- data.frame(y = y_beta, x = x)
  fit_beta <- gkwreg2(y ~ x, data = df_beta, family = "beta")
  expect_length(residuals(fit_beta, type = "quantile"), n)

  # Test for ekw
  y_ekw <- gkwdist::rekw(n, alpha = exp(0.5), beta = exp(1.0), lambda = exp(0.5))
  df_ekw <- data.frame(y = y_ekw, x = x)
  fit_ekw <- gkwreg2(y ~ x, data = df_ekw, family = "ekw")
  expect_length(residuals(fit_ekw, type = "quantile"), n)

  # Test for mc
  y_mc <- gkwdist::rmc(n, gamma = exp(1.0), delta = exp(1.0), lambda = exp(0.5))
  df_mc <- data.frame(y = y_mc, x = x)
  fit_mc <- gkwreg2(y ~ x, data = df_mc, family = "mc")
  expect_length(residuals(fit_mc, type = "quantile"), n)
})

test_that("resid alias works", {
  skip_if_not_installed("gkwdist")

  set.seed(406)
  n <- 50
  y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
  df <- data.frame(y = y, x = rnorm(n))
  fit <- gkwreg2(y ~ x, data = df, family = "kw")

  # resid should be alias for residuals
  expect_equal(resid(fit), residuals(fit))
  expect_equal(resid(fit, type = "quantile"), residuals(fit, type = "quantile"))
})

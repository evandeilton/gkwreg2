test_that("Simple formula y ~ 1 (intercept only) works", {
  skip_if_not_installed("gkwdist")

  set.seed(201)
  n <- 100
  y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
  df <- data.frame(y = y)

  fit <- gkwreg2(y ~ 1, data = df, family = "kw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 1)
  expect_length(fit$coef_list$beta, 1)
  expect_equal(fit$npar, 2)
})

test_that("Single covariate formula works", {
  skip_if_not_installed("gkwdist")

  set.seed(202)
  n <- 100
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.3 * x), beta = exp(1.0))
  df <- data.frame(y = y, x = x)

  fit <- gkwreg2(y ~ x, data = df, family = "kw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 2)
  expect_equal(names(fit$coef_list$alpha), c("(Intercept)", "x"))
})

test_that("Multiple covariates work", {
  skip_if_not_installed("gkwdist")

  set.seed(203)
  n <- 150
  x1 <- rnorm(n)
  x2 <- runif(n, -1, 1)
  x3 <- rbinom(n, 1, 0.5)
  alpha <- exp(0.5 + 0.2 * x1 + 0.1 * x2 - 0.15 * x3)
  y <- gkwdist::rkw(n, alpha = alpha, beta = exp(1.0))
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  fit <- gkwreg2(y ~ x1 + x2 + x3, data = df, family = "kw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 4) # intercept + 3 covariates
})

test_that("Interaction terms work", {
  skip_if_not_installed("gkwdist")

  set.seed(204)
  n <- 150
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x1 + 0.1 * x1 * x2), beta = exp(1.0))
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  fit <- gkwreg2(y ~ x1 * x2, data = df, family = "kw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 4) # intercept + x1 + x2 + x1:x2
  expect_true("x1:x2" %in% names(fit$coef_list$alpha))
})

test_that("Polynomial terms work", {
  skip_if_not_installed("gkwdist")

  set.seed(205)
  n <- 150
  x <- runif(n, 0, 3)
  y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.1 * x + 0.05 * x^2), beta = exp(1.0))
  df <- data.frame(y = y, x = x)

  fit <- gkwreg2(y ~ poly(x, 2, raw = TRUE), data = df, family = "kw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 3) # intercept + x + x^2
})

test_that("Extended formula with | separator works", {
  skip_if_not_installed("gkwdist")

  set.seed(206)
  n <- 150
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  alpha <- exp(0.5 + 0.3 * x1)
  beta <- exp(1.0 - 0.2 * x2)
  y <- gkwdist::rkw(n, alpha = alpha, beta = beta)
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  # Different covariates for each parameter
  fit <- gkwreg2(y ~ x1 | x2, data = df, family = "kw")

  expect_true(fit$converged)
  expect_true("x1" %in% names(fit$coef_list$alpha))
  expect_false("x2" %in% names(fit$coef_list$alpha))
  expect_true("x2" %in% names(fit$coef_list$beta))
  expect_false("x1" %in% names(fit$coef_list$beta))
})

test_that("Mixed formula: some with covariates, some intercept only", {
  skip_if_not_installed("gkwdist")

  set.seed(207)
  n <- 150
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.3 * x), beta = exp(1.0))
  df <- data.frame(y = y, x = x)

  # Alpha has covariate, beta is intercept-only
  fit <- gkwreg2(y ~ x | 1, data = df, family = "kw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 2)
  expect_length(fit$coef_list$beta, 1)
})

test_that("Factor variables work", {
  skip_if_not_installed("gkwdist")

  set.seed(208)
  n <- 150
  group <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
  alpha <- exp(0.5 + 0.2 * (group == "B") + 0.4 * (group == "C"))
  y <- gkwdist::rkw(n, alpha = alpha, beta = exp(1.0))
  df <- data.frame(y = y, group = group)

  fit <- gkwreg2(y ~ group, data = df, family = "kw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 3) # intercept + 2 levels
})

test_that("I() function in formula works", {
  skip_if_not_installed("gkwdist")

  set.seed(209)
  n <- 100
  x <- runif(n, 1, 5)
  y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.1 * log(x)), beta = exp(1.0))
  df <- data.frame(y = y, x = x)

  fit <- gkwreg2(y ~ I(log(x)), data = df, family = "kw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 2)
})

test_that("Complex 3-parameter formula works (ekw)", {
  skip_if_not_installed("gkwdist")

  set.seed(210)
  n <- 200
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  alpha <- exp(0.5 + 0.2 * x1)
  beta <- exp(1.0 + 0.15 * x2)
  lambda <- exp(0.3 - 0.1 * x3)
  y <- gkwdist::rekw(n, alpha = alpha, beta = beta, lambda = lambda)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)

  fit <- gkwreg2(y ~ x1 | x2 | x3, data = df, family = "ekw")

  expect_true(fit$converged)
  expect_true("x1" %in% names(fit$coef_list$alpha))
  expect_true("x2" %in% names(fit$coef_list$beta))
  expect_true("x3" %in% names(fit$coef_list$lambda))
})

test_that("Complex 4-parameter formula works (bkw)", {
  skip_if_not_installed("gkwdist")

  set.seed(211)
  n <- 200
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  alpha <- exp(0.5 + 0.1 * x1)
  beta <- exp(1.0)
  gamma <- exp(0.8 + 0.15 * x2)
  delta <- exp(0.5)
  y <- gkwdist::rbkw(n,
    alpha = alpha, beta = rep(beta, n),
    gamma = gamma, delta = rep(delta, n)
  )
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  fit <- gkwreg2(y ~ x1 | 1 | x2 | 1, data = df, family = "bkw")

  expect_true(fit$converged)
  expect_length(fit$coef_list$alpha, 2)
  expect_length(fit$coef_list$beta, 1)
  expect_length(fit$coef_list$gamma, 2)
  expect_length(fit$coef_list$delta, 1)
})

test_that("Complex 5-parameter formula works (gkw)", {
  skip_if_not_installed("gkwdist")

  set.seed(212)
  n <- 250
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  alpha <- exp(0.5 + 0.1 * x1)
  beta <- exp(1.0)
  gamma <- exp(0.8)
  delta <- exp(0.5 + 0.1 * x2)
  lambda <- exp(0.3)
  y <- gkwdist::rgkw(n,
    alpha = alpha, beta = rep(beta, n),
    gamma = rep(gamma, n), delta = delta, lambda = rep(lambda, n)
  )
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  fit <- gkwreg2(y ~ x1 | 1 | 1 | x2 | 1, data = df, family = "gkw")

  # GKW is complex, may not always fully converge but should produce valid output
  expect_true(is.finite(fit$loglik))
  expect_length(fit$coef_list$alpha, 2)
  expect_length(fit$coef_list$delta, 2)
})

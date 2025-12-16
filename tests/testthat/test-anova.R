test_that("anova compares nested models", {
  skip_if_not_installed("gkwdist")

  set.seed(501)
  n <- 150
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.3 * x1 + 0.1 * x2), beta = exp(1.0))
  df <- data.frame(y = y, x1 = x1, x2 = x2)

  # Fit nested models
  fit0 <- gkwreg2(y ~ 1, data = df, family = "kw")
  fit1 <- gkwreg2(y ~ x1, data = df, family = "kw")
  fit2 <- gkwreg2(y ~ x1 + x2, data = df, family = "kw")

  # Compare
  aov_result <- anova(fit0, fit1, fit2)

  expect_s3_class(aov_result, "anova")
  expect_equal(nrow(aov_result), 3)
  expect_true("Chisq" %in% names(aov_result))
  expect_true("Pr(>Chisq)" %in% names(aov_result))

  # Larger model should have higher log-likelihood
  expect_true(aov_result$LogLik[2] >= aov_result$LogLik[1])
  expect_true(aov_result$LogLik[3] >= aov_result$LogLik[2])

  # Degrees of freedom should increase
  expect_true(aov_result$Df[2] > aov_result$Df[1])
  expect_true(aov_result$Df[3] > aov_result$Df[2])
})

test_that("anova works with test='none'", {
  skip_if_not_installed("gkwdist")

  set.seed(502)
  n <- 100
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1.0))
  df <- data.frame(y = y, x = x)

  fit0 <- gkwreg2(y ~ 1, data = df, family = "kw")
  fit1 <- gkwreg2(y ~ x, data = df, family = "kw")

  aov_result <- anova(fit0, fit1, test = "none")

  expect_s3_class(aov_result, "anova")
  expect_true(all(is.na(aov_result$`Pr(>Chisq)`)))
})

test_that("compare_models works", {
  skip_if_not_installed("gkwdist")

  set.seed(503)
  n <- 100
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
  df <- data.frame(y = y, x = x)

  fit_kw <- gkwreg2(y ~ x, data = df, family = "kw")
  fit_beta <- gkwreg2(y ~ x, data = df, family = "beta")

  comp <- compare_models(fit_kw, fit_beta)

  expect_s3_class(comp, "data.frame")
  expect_true("AIC" %in% names(comp))
  expect_true("BIC" %in% names(comp))
  expect_true("LogLik" %in% names(comp))
  expect_equal(nrow(comp), 2)
})

test_that("anova errors with single model", {
  skip_if_not_installed("gkwdist")

  set.seed(504)
  n <- 50
  y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
  df <- data.frame(y = y, x = rnorm(n))
  fit <- gkwreg2(y ~ x, data = df, family = "kw")

  expect_error(anova(fit), "at least two models")
})

test_that("anova warns on different families", {
  skip_if_not_installed("gkwdist")

  set.seed(505)
  n <- 100
  x <- rnorm(n)
  y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
  df <- data.frame(y = y, x = x)

  fit_kw <- gkwreg2(y ~ x, data = df, family = "kw")
  fit_beta <- gkwreg2(y ~ x, data = df, family = "beta")

  expect_warning(anova(fit_kw, fit_beta), "different families")
})

test_that("predict type='response' works for all families", {
    skip_if_not_installed("gkwdist")

    set.seed(301)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    pred <- predict(fit, type = "response")
    expect_length(pred, n)
    expect_true(all(pred > 0 & pred < 1))
    expect_false(any(is.na(pred)))
})

test_that("predict type='parameters' works", {
    skip_if_not_installed("gkwdist")

    set.seed(302)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1.0))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    params <- predict(fit, type = "parameters")

    expect_type(params, "list")
    expect_true("alpha" %in% names(params))
    expect_true("beta" %in% names(params))
    expect_length(params$alpha, n)
    expect_length(params$beta, n)
    expect_true(all(params$alpha > 0))
    expect_true(all(params$beta > 0))
})

test_that("predict type='link' works", {
    skip_if_not_installed("gkwdist")

    set.seed(303)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    eta <- predict(fit, type = "link")

    expect_type(eta, "list")
    expect_true("alpha" %in% names(eta))
    expect_true("beta" %in% names(eta))
    expect_length(eta$alpha, n)
    # Link values can be any real number
})

test_that("predict type='quantile' works", {
    skip_if_not_installed("gkwdist")

    set.seed(304)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    # Single quantile
    q50 <- predict(fit, type = "quantile", at = 0.5)
    expect_true(is.matrix(q50))
    expect_equal(nrow(q50), n)
    expect_equal(ncol(q50), 1)
    expect_true(all(q50 > 0 & q50 < 1))

    # Multiple quantiles
    quants <- predict(fit, type = "quantile", at = c(0.1, 0.25, 0.5, 0.75, 0.9))
    expect_true(is.matrix(quants))
    expect_equal(nrow(quants), n)
    expect_equal(ncol(quants), 5)
    expect_true(all(quants > 0 & quants < 1))

    # Quantiles should be ordered
    for (i in 1:n) {
        expect_true(all(diff(quants[i, ]) >= 0))
    }
})

test_that("predict type='variance' works", {
    skip_if_not_installed("gkwdist")

    set.seed(305)
    n <- 50
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    vars <- predict(fit, type = "variance")

    expect_length(vars, n)
    expect_true(all(vars >= 0)) # Variance must be non-negative
    expect_true(all(vars < 0.25)) # Bounded data variance < 0.25
})

test_that("predict with newdata works", {
    skip_if_not_installed("gkwdist")

    set.seed(306)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.3 * x), beta = exp(1.0))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    # New data
    newdf <- data.frame(x = c(-2, -1, 0, 1, 2))
    pred_new <- predict(fit, newdata = newdf, type = "response")

    expect_length(pred_new, 5)
    expect_true(all(pred_new > 0 & pred_new < 1))

    # Predictions should vary with x
    expect_true(var(pred_new) > 0)
})

test_that("predict for beta family works", {
    skip_if_not_installed("gkwdist")

    set.seed(307)
    n <- 100
    x <- rnorm(n)
    y <- rbeta(n, shape1 = exp(1.0 + 0.2 * x), shape2 = exp(1.5))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "beta")

    pred <- predict(fit, type = "response")
    expect_length(pred, n)
    expect_true(all(pred > 0 & pred < 1))

    params <- predict(fit, type = "parameters")
    expect_true("gamma" %in% names(params))
    expect_true("delta" %in% names(params))
})

test_that("predict for ekw family works", {
    skip_if_not_installed("gkwdist")

    set.seed(308)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rekw(n, alpha = exp(0.5), beta = exp(1.0), lambda = exp(0.5 + 0.1 * x))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ 1 | 1 | x, data = df, family = "ekw")

    params <- predict(fit, type = "parameters")
    expect_true(all(c("alpha", "beta", "lambda") %in% names(params)))
    expect_length(params$lambda, n)
})

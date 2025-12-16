test_that("plot produces 6 diagnostic plots without error", {
    skip_if_not_installed("gkwdist")
    skip_on_cran() # Plotting tests may be slow

    set.seed(601)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1.0))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    # Test each plot individually
    expect_silent(plot(fit, which = 1, ask = FALSE))
    expect_silent(plot(fit, which = 2, ask = FALSE))
    expect_silent(plot(fit, which = 3, ask = FALSE))
    expect_silent(plot(fit, which = 4, ask = FALSE))
    expect_silent(plot(fit, which = 5, ask = FALSE))
    expect_silent(plot(fit, which = 6, ask = FALSE))
})

test_that("plot returns diagnostic data", {
    skip_if_not_installed("gkwdist")
    skip_on_cran()

    set.seed(602)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    diag <- plot(fit, which = 1, ask = FALSE)

    expect_type(diag, "list")
    expect_true("residuals" %in% names(diag))
    expect_true("fitted" %in% names(diag))
    expect_true("observed" %in% names(diag))
})

test_that("plot works with different residual types", {
    skip_if_not_installed("gkwdist")
    skip_on_cran()

    set.seed(603)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = x)
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    expect_silent(plot(fit, which = 1, type = "response", ask = FALSE))
    expect_silent(plot(fit, which = 1, type = "pearson", ask = FALSE))
    expect_silent(plot(fit, which = 1, type = "deviance", ask = FALSE))
    expect_silent(plot(fit, which = 1, type = "quantile", ask = FALSE))
})

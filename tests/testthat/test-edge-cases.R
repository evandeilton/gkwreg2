test_that("input validation: y outside (0,1) throws error", {
    df_bad <- data.frame(y = c(0.1, 0.5, 1.0), x = c(1, 2, 3))
    expect_error(
        gkwreg2(y ~ x, data = df_bad, family = "kw"),
        "strictly in \\(0, 1\\)"
    )

    df_bad2 <- data.frame(y = c(0.0, 0.5, 0.9), x = c(1, 2, 3))
    expect_error(
        gkwreg2(y ~ x, data = df_bad2, family = "kw"),
        "strictly in \\(0, 1\\)"
    )

    df_bad3 <- data.frame(y = c(-0.1, 0.5, 0.9), x = c(1, 2, 3))
    expect_error(
        gkwreg2(y ~ x, data = df_bad3, family = "kw"),
        "strictly in \\(0, 1\\)"
    )
})

test_that("input validation: NA in response throws error", {
    df_na <- data.frame(y = c(0.1, NA, 0.9), x = c(1, 2, 3))
    expect_error(gkwreg2(y ~ x, data = df_na, family = "kw"))
})

test_that("input validation: invalid family throws error", {
    df <- data.frame(y = c(0.1, 0.5, 0.9), x = c(1, 2, 3))
    expect_error(
        gkwreg2(y ~ x, data = df, family = "invalid"),
        "should be one of"
    )
})

test_that("input validation: formula must be formula object", {
    df <- data.frame(y = c(0.1, 0.5, 0.9), x = c(1, 2, 3))
    expect_error(
        gkwreg2("y ~ x", data = df, family = "kw"),
        "must be a formula"
    )
})

test_that("coef with invalid parameter throws error", {
    skip_if_not_installed("gkwdist")

    set.seed(801)
    y <- gkwdist::rkw(50, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = rnorm(50))
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    expect_error(coef(fit, parameter = "gamma"), "not found")
})

test_that("vcov returns NULL with warning when not computed", {
    skip_if_not_installed("gkwdist")

    set.seed(802)
    y <- gkwdist::rkw(50, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = rnorm(50))
    fit <- gkwreg2(y ~ x,
        data = df, family = "kw",
        control = gkw_control(hessian = FALSE)
    )

    expect_warning(vc <- vcov(fit), "not available")
    expect_null(vc)
})

test_that("confint errors when SE not available", {
    skip_if_not_installed("gkwdist")

    set.seed(803)
    y <- gkwdist::rkw(50, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = rnorm(50))
    fit <- gkwreg2(y ~ x,
        data = df, family = "kw",
        control = gkw_control(hessian = FALSE)
    )

    expect_error(confint(fit), "Standard errors not available")
})

test_that("predict errors when data not available", {
    skip_if_not_installed("gkwdist")

    set.seed(804)
    y <- gkwdist::rkw(50, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = rnorm(50))
    fit <- gkwreg2(y ~ x,
        data = df, family = "kw",
        control = gkw_control(keep_data = FALSE)
    )

    expect_error(predict(fit), "No data available")
})

test_that("model.frame errors when data not available", {
    skip_if_not_installed("gkwdist")

    set.seed(805)
    y <- gkwdist::rkw(50, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = rnorm(50))
    fit <- gkwreg2(y ~ x,
        data = df, family = "kw",
        control = gkw_control(keep_data = FALSE)
    )

    expect_error(model.frame(fit), "not available")
})

test_that("model.matrix errors for invalid parameter", {
    skip_if_not_installed("gkwdist")

    set.seed(806)
    y <- gkwdist::rkw(50, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = rnorm(50))
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    expect_error(model.matrix(fit, parameter = "gamma"), "not found")
})

test_that("too many formula parts throws error", {
    skip_if_not_installed("gkwdist")

    set.seed(807)
    y <- gkwdist::rkw(50, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = rnorm(50))

    # kw has 2 parameters, but we provide 3 parts
    expect_error(
        gkwreg2(y ~ x | x | x, data = df, family = "kw"),
        "Too many formula parts"
    )
})

test_that("handles extreme y values near boundaries", {
    skip_if_not_installed("gkwdist")

    set.seed(808)
    n <- 100
    # Generate data with values close to 0 and 1
    y <- c(rep(0.001, 10), gkwdist::rkw(80, alpha = exp(0.5), beta = exp(1.0)), rep(0.999, 10))
    x <- rnorm(n)
    df <- data.frame(y = y, x = x)

    # Should still converge
    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    expect_true(fit$converged)
    expect_true(all(is.finite(fit$theta_hat)))
})

test_that("handles single observation per parameter level", {
    skip_if_not_installed("gkwdist")

    # Small sample
    set.seed(809)
    n <- 10
    y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
    x <- rnorm(n)
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    # Should work even with small sample
    expect_s3_class(fit, "gkwreg2")
})

test_that("S3 methods work correctly", {
    skip_if_not_installed("gkwdist")

    set.seed(111)
    n <- 100
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1.0))
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ x,
        data = df, family = "kw",
        control = gkw_control(hessian = TRUE)
    )

    # coef - default is now flat vector (gkwreg compatible)
    cf <- coef(fit)
    expect_type(cf, "double")
    expect_length(cf, 3) # 2 for alpha, 1 for beta (intercept only)

    cf_list <- coef(fit, flatten = FALSE)
    expect_type(cf_list, "list")
    expect_length(cf_list, 2)

    # vcov
    vc <- vcov(fit)
    expect_true(is.matrix(vc))
    expect_equal(nrow(vc), fit$npar)
    expect_equal(ncol(vc), fit$npar)

    # confint
    ci <- confint(fit)
    expect_true(is.matrix(ci))
    expect_equal(nrow(ci), fit$npar)
    expect_equal(ncol(ci), 2)

    # logLik
    ll <- logLik(fit)
    expect_s3_class(ll, "logLik")
    expect_equal(attr(ll, "df"), fit$npar)

    # AIC/BIC
    aic <- AIC(fit)
    bic <- BIC(fit)
    expect_type(aic, "double")
    expect_type(bic, "double")
    expect_true(bic > aic) # Generally true for n > 7

    # nobs
    expect_equal(nobs(fit), n)

    # fitted
    fv <- fitted(fit)
    expect_length(fv, n)
    expect_true(all(fv > 0 & fv < 1))

    # summary
    s <- summary(fit)
    expect_s3_class(s, "summary.gkwreg2")
    expect_type(s$coefficients, "list")
})

test_that("predict method works", {
    skip_if_not_installed("gkwdist")

    set.seed(222)
    n <- 50
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    # Response predictions
    pred_resp <- predict(fit, type = "response")
    expect_length(pred_resp, n)
    expect_true(all(pred_resp > 0 & pred_resp < 1))

    # Parameter predictions
    pred_params <- predict(fit, type = "parameters")
    expect_type(pred_params, "list")
    expect_true("alpha" %in% names(pred_params))
    expect_true("beta" %in% names(pred_params))

    # Link predictions
    pred_link <- predict(fit, type = "link")
    expect_type(pred_link, "list")

    # Quantile predictions
    pred_q <- predict(fit, type = "quantile", at = c(0.25, 0.5, 0.75))
    expect_true(is.matrix(pred_q))
    expect_equal(nrow(pred_q), n)
    expect_equal(ncol(pred_q), 3)

    # New data prediction
    newdf <- data.frame(x = c(-1, 0, 1))
    pred_new <- predict(fit, newdata = newdf, type = "response")
    expect_length(pred_new, 3)
})

test_that("residuals method works", {
    skip_if_not_installed("gkwdist")

    set.seed(333)
    n <- 50
    x <- rnorm(n)
    y <- gkwdist::rkw(n, alpha = exp(0.5), beta = exp(1.0))
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    # Response residuals
    res_resp <- residuals(fit, type = "response")
    expect_length(res_resp, n)

    # Quantile residuals
    res_q <- residuals(fit, type = "quantile")
    expect_length(res_q, n)
    # Should be approximately normal if model is correct
    expect_true(abs(mean(res_q)) < 1)

    # Pearson residuals
    res_p <- residuals(fit, type = "pearson")
    expect_length(res_p, n)
})

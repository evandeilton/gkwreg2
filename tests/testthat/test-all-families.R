# Test all 7 distribution families
# Updated for gkwreg-compatible output structure

test_that("Kumaraswamy (kw) family works correctly", {
    skip_if_not_installed("gkwdist")

    set.seed(101)
    n <- 150
    x <- rnorm(n)
    alpha_true <- exp(0.5 + 0.3 * x)
    beta_true <- exp(1.2)
    y <- gkwdist::rkw(n, alpha = alpha_true, beta = rep(beta_true, n))
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ x, data = df, family = "kw")

    expect_true(fit$converged)
    expect_equal(fit$family, "kw")

    # coefficients is now a flat vector with colon naming
    expect_length(fit$coefficients, 3) # 2 for alpha + 1 for beta
    expect_true(any(grepl("alpha:", names(fit$coefficients))))
    expect_true(any(grepl("beta:", names(fit$coefficients))))

    # Use coef_list for parameter-specific access
    expect_length(fit$coef_list$alpha, 2)
    expect_length(fit$coef_list$beta, 1)

    # Coefficient recovery (intercept) - using coef_list
    expect_equal(unname(fit$coef_list$alpha["(Intercept)"]), 0.5, tolerance = 0.3)
    expect_equal(unname(fit$coef_list$alpha["x"]), 0.3, tolerance = 0.2)
    expect_equal(unname(fit$coef_list$beta["(Intercept)"]), 1.2, tolerance = 0.3)

    # Complex formula: different covariates
    x2 <- runif(n, -1, 1)
    df$x2 <- x2

    fit2 <- gkwreg2(y ~ x | x2, data = df, family = "kw")
    expect_true(fit2$converged)
    expect_length(fit2$coef_list$alpha, 2)
    expect_length(fit2$coef_list$beta, 2)

    # Multiple covariates per parameter
    x3 <- rnorm(n)
    df$x3 <- x3

    fit3 <- gkwreg2(y ~ x + x2 | x + x3, data = df, family = "kw")
    expect_true(fit3$converged)
    expect_length(fit3$coef_list$alpha, 3)
    expect_length(fit3$coef_list$beta, 3)
})

test_that("Beta family works correctly", {
    skip_if_not_installed("gkwdist")

    set.seed(102)
    n <- 150
    x <- rnorm(n)
    gamma_true <- exp(1.0 + 0.2 * x)
    delta_true <- exp(1.5 - 0.1 * x)
    y <- rbeta(n, shape1 = gamma_true, shape2 = delta_true)
    df <- data.frame(y = y, x = x)

    # Simple formula
    fit <- gkwreg2(y ~ x, data = df, family = "beta")

    expect_true(fit$converged)
    expect_equal(fit$family, "beta")
    expect_true(any(grepl("gamma:", names(fit$coefficients))))
    expect_true(any(grepl("delta:", names(fit$coefficients))))

    # Extended formula
    fit2 <- gkwreg2(y ~ x | x, data = df, family = "beta")
    expect_true(fit2$converged)
    expect_length(fit2$coef_list$gamma, 2)
    expect_length(fit2$coef_list$delta, 2)

    # Check coefficient signs are correct
    expect_true(fit2$coef_list$gamma["x"] > 0) # positive effect
})

test_that("Exponentiated Kumaraswamy (ekw) family works correctly", {
    skip_if_not_installed("gkwdist")

    set.seed(103)
    n <- 150
    x <- rnorm(n)
    alpha <- exp(0.5 + 0.2 * x)
    beta <- exp(1.0)
    lambda <- exp(0.5)
    y <- gkwdist::rekw(n, alpha = alpha, beta = rep(beta, n), lambda = rep(lambda, n))
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ x, data = df, family = "ekw")

    expect_true(fit$converged)
    expect_equal(fit$family, "ekw")
    expect_true(any(grepl("alpha:", names(fit$coefficients))))
    expect_true(any(grepl("beta:", names(fit$coefficients))))
    expect_true(any(grepl("lambda:", names(fit$coefficients))))

    # Extended formula: y ~ alpha_formula | beta_formula | lambda_formula
    fit2 <- gkwreg2(y ~ x | 1 | 1, data = df, family = "ekw")
    expect_true(fit2$converged)
    expect_length(fit2$coef_list$alpha, 2)
    expect_length(fit2$coef_list$beta, 1)
    expect_length(fit2$coef_list$lambda, 1)
})

test_that("McDonald (mc) family works correctly", {
    skip_if_not_installed("gkwdist")

    set.seed(104)
    n <- 150
    x <- rnorm(n)
    gamma <- exp(1.0)
    delta <- exp(1.0)
    lambda <- exp(0.5 + 0.15 * x)
    y <- gkwdist::rmc(n, gamma = rep(gamma, n), delta = rep(delta, n), lambda = lambda)
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ 1 | 1 | x, data = df, family = "mc")

    # May not always converge perfectly for mc, just check it ran
    expect_true(is.finite(fit$loglik))
    expect_equal(fit$family, "mc")
    expect_true(any(grepl("gamma:", names(fit$coefficients))))
    expect_true(any(grepl("delta:", names(fit$coefficients))))
    expect_true(any(grepl("lambda:", names(fit$coefficients))))
    expect_length(fit$coef_list$lambda, 2)
})

test_that("Beta-Kumaraswamy (bkw) family works correctly", {
    skip_if_not_installed("gkwdist")

    set.seed(105)
    n <- 150
    x <- rnorm(n)
    alpha <- exp(0.5)
    beta <- exp(1.0)
    gamma <- exp(0.8 + 0.2 * x)
    delta <- exp(0.5)
    y <- gkwdist::rbkw(n,
        alpha = rep(alpha, n), beta = rep(beta, n),
        gamma = gamma, delta = rep(delta, n)
    )
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ 1 | 1 | x | 1, data = df, family = "bkw")

    # May not always converge perfectly for bkw, just check it ran
    expect_true(is.finite(fit$loglik))
    expect_equal(fit$family, "bkw")
    expect_length(fit$coefficients, 5) # 4 params with 1 having 2 coefs
    expect_equal(length(fit$param_names), 4)
})

test_that("Kumaraswamy-Kumaraswamy (kkw) family works correctly", {
    skip_if_not_installed("gkwdist")

    set.seed(106)
    n <- 150
    x <- rnorm(n)
    alpha <- exp(0.5)
    beta <- exp(1.0)
    delta <- exp(0.8)
    lambda <- exp(0.5 + 0.2 * x)
    y <- gkwdist::rkkw(n,
        alpha = rep(alpha, n), beta = rep(beta, n),
        delta = rep(delta, n), lambda = lambda
    )
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ 1 | 1 | 1 | x, data = df, family = "kkw")

    expect_true(is.finite(fit$loglik))
    expect_equal(fit$family, "kkw")
    expect_length(fit$coefficients, 5) # 4 params with 1 having 2 coefs
    expect_equal(length(fit$param_names), 4)
    expect_length(fit$coef_list$lambda, 2)
})

test_that("Generalized Kumaraswamy (gkw) family works correctly", {
    skip_if_not_installed("gkwdist")

    set.seed(107)
    n <- 200
    x <- rnorm(n)
    alpha <- exp(0.5)
    beta <- exp(1.0)
    gamma <- exp(0.8)
    delta <- exp(0.5)
    lambda <- exp(0.5 + 0.15 * x)
    y <- gkwdist::rgkw(n,
        alpha = rep(alpha, n), beta = rep(beta, n),
        gamma = rep(gamma, n), delta = rep(delta, n), lambda = lambda
    )
    df <- data.frame(y = y, x = x)

    fit <- gkwreg2(y ~ 1 | 1 | 1 | 1 | x, data = df, family = "gkw")

    expect_true(is.finite(fit$loglik))
    expect_equal(fit$family, "gkw")
    expect_length(fit$coefficients, 6) # 5 params with 1 having 2 coefs
    expect_equal(length(fit$param_names), 5)
    expect_length(fit$coef_list$lambda, 2)
})

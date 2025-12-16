test_that("Link functions are correctly computed", {
    # Test log link
    expect_equal(invlink_log(0), 1)
    expect_equal(invlink_log(1), exp(1))
    expect_equal(dinvlink_log(0), 1)
    expect_equal(dinvlink_log(1), exp(1))

    # Test sqrt link
    expect_equal(invlink_sqrt(2), 4)
    expect_equal(dinvlink_sqrt(2), 4)

    # Test identity link
    expect_equal(invlink_identity(5), 5)
    expect_equal(dinvlink_identity(5), 1)

    # Test inverse link
    expect_equal(invlink_inverse(2), 0.5)
    expect_equal(dinvlink_inverse(2), -0.25)
})

test_that("Logit link is correctly computed", {
    # Test logit link
    expect_equal(invlink_logit(0), 0.5)
    expect_equal(dinvlink_logit(0), 0.25) # p(1-p) = 0.5*0.5 = 0.25

    # Test extreme values
    expect_true(invlink_logit(100) > 0.999)
    expect_true(invlink_logit(-100) < 0.001)
})

test_that("Probit link is correctly computed", {
    expect_equal(invlink_probit(0), 0.5)
    expect_equal(dinvlink_probit(0), dnorm(0))
})

test_that("Cloglog link is correctly computed", {
    # At eta = 0: 1 - exp(-1) ≈ 0.632
    expect_equal(invlink_cloglog(0), 1 - exp(-1), tolerance = 1e-10)
})

test_that("Cauchy link is correctly computed", {
    expect_equal(invlink_cauchy(0), 0.5)
    expect_equal(dinvlink_cauchy(0), 1 / pi)
})

test_that("Vectorized link functions work", {
    eta <- c(-1, 0, 1)

    # Test apply_invlink
    result_log <- apply_invlink(eta, "log")
    expect_length(result_log, 3)
    expect_equal(as.vector(result_log), exp(eta))

    result_logit <- apply_invlink(eta, "logit")
    expect_length(result_logit, 3)
    expect_true(all(result_logit > 0 & result_logit < 1))

    # Test derivatives
    result_dinv <- apply_dinvlink(eta, "log")
    expect_equal(as.vector(result_dinv), exp(eta))
})

test_that("Link functions maintain proper bounds", {
    # Log link: output should be positive
    eta <- seq(-5, 5, by = 1)
    theta_log <- apply_invlink(eta, "log")
    expect_true(all(theta_log > 0))

    # Logit link: output should be in (0,1)
    theta_logit <- apply_invlink(eta, "logit")
    expect_true(all(theta_logit > 0 & theta_logit < 1))

    # Probit link: output should be in (0,1)
    theta_probit <- apply_invlink(eta, "probit")
    expect_true(all(theta_probit > 0 & theta_probit < 1))
})

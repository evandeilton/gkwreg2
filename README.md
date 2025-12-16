# gkwreg2 <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/evandeilton/gkwreg2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/evandeilton/gkwreg2/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**gkwreg2** provides regression modeling for bounded (0,1) data using the Generalized Kumaraswamy distribution family. This is a pure RcppArmadillo implementation that replaces the TMB-based gkwreg package, eliminating user-side compilation requirements.

## Features

- **7 Distribution Families**: GKw, BKw, KKw, EKw, McDonald, Kumaraswamy, Beta
- **Extended Formula Syntax**: `y ~ x1 | x2 | ...` for different covariates per parameter
- **8 Link Functions**: log, sqrt, inverse, identity, logit, probit, cloglog, cauchy
- **Analytical Gradients**: Fast optimization using gradients from gkwdist
- **Complete S3 Interface**: print, summary, coef, vcov, predict, residuals, plot, anova

## Installation

```r
# Install from GitHub
devtools::install_github("evandeilton/gkwreg2")
```

## Quick Start

```r
library(gkwreg2)
library(gkwdist)

# Simulate data
set.seed(123)
n <- 200
x <- rnorm(n)
alpha <- exp(0.5 + 0.3 * x)
beta <- exp(1.0)
y <- rkw(n, alpha = alpha, beta = beta)
df <- data.frame(y = y, x = x)

# Fit Kumaraswamy regression
fit <- gkwreg2(y ~ x, data = df, family = "kw")
summary(fit)

# Predictions
predict(fit, type = "response")

# Different covariates per parameter
fit2 <- gkwreg2(y ~ x | 1, data = df, family = "kw")

# Diagnostics
plot(fit, which = 1:4)

# Model comparison
anova(fit, fit2)
```

## Distribution Families

| Family | Parameters | Description |
|--------|------------|-------------|
| `gkw` | α, β, γ, δ, λ | Generalized Kumaraswamy (5 params) |
| `bkw` | α, β, γ, δ | Beta-Kumaraswamy (4 params) |
| `kkw` | α, β, δ, λ | Kumaraswamy-Kumaraswamy (4 params) |
| `ekw` | α, β, λ | Exponentiated Kumaraswamy (3 params) |
| `mc` | γ, δ, λ | McDonald / Beta Power (3 params) |
| `kw` | α, β | Kumaraswamy (2 params) |
| `beta` | γ, δ | Beta (2 params) |

## Extended Formula Syntax

Use `|` to specify different covariates for each parameter:

```r
# Same covariates for all parameters
gkwreg2(y ~ x1 + x2, data = df, family = "kw")

# Different covariates: alpha ~ x1, beta ~ x2
gkwreg2(y ~ x1 | x2, data = df, family = "kw")

# Mixed: alpha ~ x1 + x2, beta ~ intercept only
gkwreg2(y ~ x1 + x2 | 1, data = df, family = "kw")
```

## Dependencies

- **gkwdist**: Distribution functions (d, p, q, r) and log-likelihood/gradient functions
- **Rcpp** / **RcppArmadillo**: C++ interface
- **Formula**: Extended formula parsing

## License

MIT © Evandeilton Lopes

## Citation

To cite gkwreg2 in publications:

```r
citation("gkwreg2")
```

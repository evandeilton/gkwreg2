# gkwreg2: Generalized Kumaraswamy Regression Models for Bounded Data ![gkwreg2 logo](reference/figures/gkwreg2.png)

[![R-CMD-check](https://github.com/evandeilton/gkwreg2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/evandeilton/gkwreg2/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Overview

The **gkwreg2** package provides a comprehensive and computationally
efficient framework for regression modeling of data restricted to the
standard unit interval $(0,1)$, including proportions, rates, fractions,
percentages, and bounded indices.

This is an **alternative implementation** of the
[gkwreg](https://github.com/evandeilton/gkwreg) package, using **pure
RcppArmadillo** with analytical gradients instead of TMB. The result is
a lighter package with fewer dependencies while maintaining full API
compatibility.

### Key Differences from gkwreg

| Feature          | gkwreg2                  | gkwreg                    |
|:-----------------|:-------------------------|:--------------------------|
| **Backend**      | RcppArmadillo            | TMB                       |
| **Gradients**    | Analytical (via gkwdist) | Automatic Differentiation |
| **Dependencies** | Lighter                  | Heavier (TMB ecosystem)   |
| **Compilation**  | Faster                   | Slower                    |
| **API**          | ✅ Compatible            | ✅ Compatible             |

------------------------------------------------------------------------

## Key Features

### Flexible Distribution Hierarchy

Model bounded data using the **5-parameter Generalized Kumaraswamy
(GKw)** distribution and its **seven nested subfamilies**:

| Distribution              | Code   | Parameters Modeled | Fixed Parameters    | \# Params |
|:--------------------------|:-------|:-------------------|:--------------------|:----------|
| Generalized Kumaraswamy   | `gkw`  | α, β, γ, δ, λ      | None                | 5         |
| Beta-Kumaraswamy          | `bkw`  | α, β, γ, δ         | λ = 1               | 4         |
| Kumaraswamy-Kumaraswamy   | `kkw`  | α, β, δ, λ         | γ = 1               | 4         |
| Exponentiated Kumaraswamy | `ekw`  | α, β, λ            | γ = 1, δ = 0        | 3         |
| McDonald (Beta Power)     | `mc`   | γ, δ, λ            | α = 1, β = 1        | 3         |
| Kumaraswamy               | `kw`   | α, β               | γ = 1, δ = 0, λ = 1 | 2         |
| Beta                      | `beta` | γ, δ               | α = 1, β = 1, λ = 1 | 2         |

### Advanced Regression Modeling

- **Extended formula syntax** for parameter-specific linear predictors:

  ``` r
  y ~ alpha_predictors | beta_predictors | gamma_predictors | delta_predictors | lambda_predictors
  ```

  *Example:* `yield ~ batch + temp | temp | 1 | temp | batch`

- **Multiple link functions**:

  - **Positive parameters** (α, β, γ, λ): `log` (default), `sqrt`,
    `inverse`, `identity`
  - **Probability parameters** (δ ∈ (0,1)): `logit` (default), `probit`,
    `cloglog`, `cauchy`

- **Flexible optimization** via
  [`gkw_control()`](https://evandeilton.github.io/gkwreg2/reference/gkw_control.md):

  - Multiple optimizers: `nlminb` (default), `BFGS`, `L-BFGS-B`,
    `Nelder-Mead`, `CG`
  - Custom starting values, convergence tolerances, iteration limits

### Computational Efficiency

- **RcppArmadillo-powered estimation**: Compiled C++ with analytical
  gradients from gkwdist.
- **Robust optimization**: Parameter clamping prevents invalid values
  during optimization.
- **Moment-based starting values**: Intelligent initialization for
  faster convergence.

### Comprehensive S3 Methods

**Standard R Methods** (familiar workflow):

- [`summary()`](https://rdrr.io/r/base/summary.html),
  [`print()`](https://rdrr.io/r/base/print.html),
  [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`vcov()`](https://rdrr.io/r/stats/vcov.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html)
- [`logLik()`](https://rdrr.io/r/stats/logLik.html),
  [`AIC()`](https://rdrr.io/r/stats/AIC.html),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html),
  [`nobs()`](https://rdrr.io/r/stats/nobs.html)
- [`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  [`residuals()`](https://rdrr.io/r/stats/residuals.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html)
- [`anova()`](https://rdrr.io/r/stats/anova.html) for nested model
  comparisons
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  diagnostic plots

------------------------------------------------------------------------

## Installation

``` r
# Install companion distribution package (required):
# install.packages("remotes")
remotes::install_github("evandeilton/gkwdist")

# Install gkwreg2:
remotes::install_github("evandeilton/gkwreg2")
```

------------------------------------------------------------------------

## Quick Start

### Basic Regression

``` r
library(gkwreg2)
library(gkwdist)

# Simulate data
set.seed(123)
n <- 500
x1 <- runif(n, -2, 2)
x2 <- rnorm(n)

# True parameters (log link)
alpha_true <- exp(0.8 + 0.3 * x1)
beta_true <- exp(1.2 - 0.2 * x2)

# Generate response from Kumaraswamy distribution
y <- rkw(n, alpha = alpha_true, beta = beta_true)
y <- pmax(pmin(y, 1 - 1e-7), 1e-7)  # Ensure strict bounds
df <- data.frame(y = y, x1 = x1, x2 = x2)

# Fit Kumaraswamy regression
# Formula: alpha ~ x1, beta ~ x2
fit_kw <- gkwreg2(y ~ x1 | x2, data = df, family = "kw")

# View results
summary(fit_kw)
```

### Model Comparison

``` r
# Fit nested models
fit0 <- gkwreg2(y ~ 1, data = df, family = "kw")       # Null model
fit1 <- gkwreg2(y ~ x1, data = df, family = "kw")      # + x1
fit2 <- gkwreg2(y ~ x1 | x2, data = df, family = "kw") # + x2 on beta

# Information criteria comparison
AIC(fit0, fit1, fit2)

# Likelihood ratio tests
anova(fit0, fit1, fit2, test = "Chisq")
```

### Prediction

``` r
# Create prediction grid
newdata <- data.frame(
  x1 = seq(-2, 2, length.out = 100),
  x2 = 0
)

# Predict different quantities
pred_mean <- predict(fit_kw, newdata, type = "response")    # E(Y|X)
pred_params <- predict(fit_kw, newdata, type = "parameters") # All parameters
pred_quant <- predict(fit_kw, newdata, type = "quantile", 
                      at = c(0.1, 0.5, 0.9))                 # Quantiles
```

### Diagnostic Plots

``` r
# All diagnostic plots
par(mfrow = c(3, 2))
plot(fit_kw, ask = FALSE)

# Select specific plots
plot(fit_kw, which = c(2, 6))  # Q-Q and Obs vs Pred only
```

------------------------------------------------------------------------

## Optimization Control

``` r
# Default control
fit <- gkwreg2(y ~ x1, data = df, family = "kw")

# Increase iterations for difficult problems
fit_robust <- gkwreg2(y ~ x1, data = df, family = "kw",
  control = gkw_control(maxit = 1000, trace = 1)
)

# Use BFGS optimizer
fit_bfgs <- gkwreg2(y ~ x1, data = df, family = "kw",
  control = gkw_control(method = "BFGS")
)

# Fast fitting without standard errors
fit_fast <- gkwreg2(y ~ x1, data = df, family = "kw",
  control = gkw_control(hessian = FALSE)
)

# Custom starting values
fit_custom <- gkwreg2(y ~ x1 + x2 | x3, data = df, family = "kw",
  control = gkw_control(
    start = list(
      alpha = c(0.5, 0.2, -0.1),  # Intercept + 2 slopes
      beta  = c(1.0, 0.3)         # Intercept + 1 slope
    )
  )
)
```

------------------------------------------------------------------------

## Comparison with Other Packages

| Feature                 | gkwreg2             | gkwreg            | betareg         | gamlss         |
|:------------------------|:--------------------|:------------------|:----------------|:---------------|
| **Distribution Family** | GKw hierarchy (7)   | GKw hierarchy (7) | Beta            | 100+           |
| **Estimation**          | MLE (RcppArmadillo) | MLE (TMB/AD)      | MLE             | GAMLSS         |
| **Parameter Modeling**  | All parameters      | All parameters    | Mean, precision | All parameters |
| **Speed**               | **Fast**            | Fast              | Fast            | Moderate       |
| **Dependencies**        | **Light**           | Heavy             | Minimal         | Many           |
| **API Compatibility**   | ✅                  | ✅                | Different       | Different      |

**When to use gkwreg2**:

- Need flexible bounded distributions beyond Beta.
- Want lighter dependencies than TMB.
- Prefer analytical gradients over automatic differentiation.
- Full compatibility with gkwreg workflow.

------------------------------------------------------------------------

## Documentation and Support

- **Reference Manual**:
  [`help(package = "gkwreg2")`](https://rdrr.io/pkg/gkwreg2/man)
- **Function Help**:
  [`?gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/gkwreg2.md),
  [`?predict.gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/predict.gkwreg2.md),
  [`?plot.gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/plot.gkwreg2.md),
  [`?gkw_control`](https://evandeilton.github.io/gkwreg2/reference/gkw_control.md)
- **GitHub Issues**: [Report bugs or request
  features](https://github.com/evandeilton/gkwreg2/issues)

------------------------------------------------------------------------

## Citation

If you use **gkwreg2** in your research, please cite:

``` r
citation("gkwreg2")
```

``` bibtex
@Manual{,
  title = {gkwreg2: Generalized Kumaraswamy Regression Models for Bounded Data},
  author = {José Evandeilton Lopes},
  year = {2025},
  note = {R package version 0.1.0},
  url = {https://github.com/evandeilton/gkwreg2},
}
```

## License

This package is licensed under the **MIT License**. See the
[LICENSE](https://evandeilton.github.io/gkwreg2/LICENSE) file for
details.

------------------------------------------------------------------------

## Author and Maintainer

**José Evandeilton Lopes (Lopes, J. E.)** \| <evandeilton@gmail.com> \|
[GitHub](https://github.com/evandeilton) \|
[ORCID](https://orcid.org/0009-0007-5887-4084)

LEG - Laboratório de Estatística e Geoinformação \| UFPR - Universidade
Federal do Paraná, Brazil

------------------------------------------------------------------------

# Introduction to gkwreg2

## Overview

The **gkwreg2** package provides regression modeling for bounded data in
(0, 1) using the Generalized Kumaraswamy distribution family. This
includes proportions, rates, percentages, and any continuous data
restricted to the unit interval.

## Installation

``` r
# Install from GitHub
remotes::install_github("evandeilton/gkwdist")
remotes::install_github("evandeilton/gkwreg2")
```

## Distribution Families

The package supports 7 distribution families: - **gkw**: Generalized
Kumaraswamy (5 parameters) - **bkw**: Beta-Kumaraswamy (4 parameters) -
**kkw**: Kumaraswamy-Kumaraswamy (4 parameters) - **ekw**: Exponentiated
Kumaraswamy (3 parameters) - **mc**: McDonald/Beta Power (3
parameters) - **kw**: Kumaraswamy (2 parameters) - **beta**: Beta (2
parameters)

## Quick Example

``` r
library(gkwreg2)
library(gkwdist)

# Simulate data
set.seed(123)
n <- 300
x1 <- runif(n, -2, 2)
x2 <- rnorm(n)

# Generate response with known parameters
alpha_true <- exp(0.8 + 0.3 * x1)
beta_true <- exp(1.2 - 0.2 * x2)
y <- rkw(n, alpha = alpha_true, beta = beta_true)
y <- pmax(pmin(y, 1 - 1e-7), 1e-7)

df <- data.frame(y = y, x1 = x1, x2 = x2)

# Fit Kumaraswamy regression
# alpha depends on x1, beta depends on x2
fit <- gkwreg2(y ~ x1 | x2, data = df, family = "kw")

summary(fit)
#> 
#> Call:
#> gkwreg2(formula = y ~ x1 | x2, data = df, family = "kw")
#> 
#> Family: kw 
#> Link functions: alpha = log, beta = log 
#> 
#> Residuals:
#>       Min        1Q    Median        3Q       Max 
#> -0.471126 -0.127675 -0.002251  0.134964  0.446130 
#> 
#> Coefficients:
#> 
#> --- ALPHA ---
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.88527    0.05726   15.46   <2e-16 ***
#> x1           0.29094    0.02903   10.02   <2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> --- BETA ---
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  1.39684    0.10149  13.764  < 2e-16 ***
#> x2          -0.21250    0.05793  -3.668 0.000244 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> ---
#> Log-Likelihood: 104.5 on 296 Df
#> AIC: -201   BIC: -186.2
#> Number of observations: 300
#> Number of parameters: 4
#> Converged in 33 iterations
```

## Extended Formula Syntax

The pipe `|` separator allows specifying different covariates for each
parameter:

``` r
# Intercept-only model
fit0 <- gkwreg2(y ~ 1, data = df, family = "kw")

# x1 for alpha only (beta = intercept)
fit1 <- gkwreg2(y ~ x1, data = df, family = "kw")

# x1 for alpha, x2 for beta
fit2 <- gkwreg2(y ~ x1 | x2, data = df, family = "kw")

# Same covariates for both
fit3 <- gkwreg2(y ~ x1 + x2 | x1 + x2, data = df, family = "kw")
```

## Model Comparison

``` r
# Compare models using AIC
AIC(fit0, fit1, fit2, fit3)
#> [1] -108.9147

# Likelihood ratio tests
anova(fit0, fit1, fit2, test = "Chisq")
#> Analysis of Variance Table (Likelihood Ratio Tests)
#> 
#> Family: kw
#> n = 300
#> 
#>                      Model Df LogLik    AIC    BIC  Chisq Df diff Pr(>Chisq)
#> Model 1: y ~ 1           1  2  56.46 -108.9 -101.5     NA      NA         NA
#> Model 2: y ~ x1          2  3  97.77 -189.5 -178.4 -82.62       1          1
#> Model 3: y ~ x1 | x2     3  4 104.52 -201.0 -186.2 -13.50       1          1
#>                      Signif
#> Model 1: y ~ 1             
#> Model 2: y ~ x1            
#> Model 3: y ~ x1 | x2       
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## Predictions

``` r
# Predicted means
pred_mean <- predict(fit2, type = "response")

# Distribution parameters
pred_params <- predict(fit2, type = "parameters")
head(data.frame(alpha = pred_params$alpha, beta = pred_params$beta))
#>      alpha     beta
#> 1 1.892816 3.419337
#> 2 3.389874 3.432949
#> 3 2.180045 3.766878
#> 4 3.784883 5.008395
#> 5 4.046583 4.146323
#> 6 1.428195 4.290579

# Quantiles
pred_q <- predict(fit2, type = "quantile", at = c(0.1, 0.5, 0.9))
head(pred_q)
#>            q0.1      q0.5      q0.9
#> [1,] 0.15778048 0.4082765 0.6860250
#> [2,] 0.35621552 0.6057702 0.8095796
#> [3,] 0.19262409 0.4412977 0.6984727
#> [4,] 0.35951029 0.5824108 0.7681856
#> [5,] 0.40223816 0.6297678 0.8099323
#> [6,] 0.07397592 0.2638995 0.5404864
```

## Diagnostic Plots

``` r
par(mfrow = c(2, 2))
plot(fit2, which = c(2, 3, 5, 6), ask = FALSE)
```

![](introduction_files/figure-html/diagnostics-1.png)

## Comparing Distribution Families

``` r
# Fit different families
fit_kw <- gkwreg2(y ~ x1 | x2, data = df, family = "kw")
fit_beta <- gkwreg2(y ~ x1 | x2, data = df, family = "beta")

# Compare
data.frame(
  Family = c("Kumaraswamy", "Beta"),
  LogLik = c(logLik(fit_kw), logLik(fit_beta)),
  AIC = c(AIC(fit_kw), AIC(fit_beta)),
  BIC = c(BIC(fit_kw), BIC(fit_beta))
)
#>        Family   LogLik       AIC       BIC
#> 1 Kumaraswamy 104.5208 -201.0417 -186.2265
#> 2        Beta 103.2371 -198.4743 -183.6592
```

## Optimization Control

Fine-tune the optimization with
[`gkw_control()`](https://evandeilton.github.io/gkwreg2/reference/gkw_control.md):

``` r
# Use BFGS optimizer with more iterations
fit <- gkwreg2(y ~ x1,
  data = df, family = "kw",
  control = gkw_control(method = "BFGS", maxit = 500)
)

# Fast fitting without standard errors
fit_fast <- gkwreg2(y ~ x1,
  data = df, family = "kw",
  control = gkw_control(hessian = FALSE)
)
```

## See Also

- [`?gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/gkwreg2.md) -
  Main fitting function
- [`?gkw_control`](https://evandeilton.github.io/gkwreg2/reference/gkw_control.md) -
  Optimization control
- [`?predict.gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/predict.gkwreg2.md) -
  Prediction types
- [`?plot.gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/plot.gkwreg2.md) -
  Diagnostic plots

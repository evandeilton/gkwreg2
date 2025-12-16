# Compare Multiple gkwreg2 Models

A convenience function for model comparison using AIC, BIC, and
log-likelihood.

## Usage

``` r
compare_models(..., criterion = c("AIC", "BIC", "logLik"))
```

## Arguments

- ...:

  `gkwreg2` objects to compare.

- criterion:

  Comparison criterion: `"AIC"` (default), `"BIC"`, or `"logLik"`.

## Value

A data frame with model comparison statistics.

## Examples

``` r
# \donttest{
# Generate test data
set.seed(123)
n <- 150
x <- rnorm(n)
y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
df <- data.frame(y = y, x = x)

fit_kw <- gkwreg2(y ~ x, data = df, family = "kw")
fit_beta <- gkwreg2(y ~ x, data = df, family = "beta")
compare_models(fit_kw, fit_beta)
#>      Model Family Df   LogLik       AIC       BIC Delta_AIC
#> 1 fit_beta   beta  3 31.09726 -56.19451 -47.16261 0.0000000
#> 2   fit_kw     kw  3 30.75295 -55.50590 -46.47400 0.6886098
# }
```

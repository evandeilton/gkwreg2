# Residuals for gkwreg2 Objects

Extracts various types of residuals from a fitted `gkwreg2` model.

## Usage

``` r
# S3 method for class 'gkwreg2'
residuals(object, type = c("response", "pearson", "deviance", "quantile"), ...)

# S3 method for class 'gkwreg2'
resid(object, type = c("response", "pearson", "deviance", "quantile"), ...)
```

## Arguments

- object:

  A fitted `gkwreg2` object.

- type:

  Type of residuals:

  `"response"`

  :   Raw residuals: y - fitted (default)

  `"pearson"`

  :   Pearson residuals: (y - mu) / sqrt(V(mu))

  `"deviance"`

  :   Deviance residuals

  `"quantile"`

  :   Randomized quantile residuals (Dunn & Smyth)

- ...:

  Currently ignored.

## Value

A numeric vector of residuals.

## Details

Randomized quantile residuals (`type = "quantile"`) are useful for model
checking as they should follow a standard normal distribution if the
model is correctly specified. They are computed as: \$\$r_i =
\Phi^{-1}(F(y_i; \hat\theta_i))\$\$ where \\F\\ is the CDF of the fitted
distribution.

## References

Dunn, P.K. and Smyth, G.K. (1996). Randomized quantile residuals.
*Journal of Computational and Graphical Statistics*, 5(3), 236-244.

## Examples

``` r
# \donttest{
set.seed(123)
n <- 100
x <- rnorm(n)
y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
df <- data.frame(y = y, x = x)
fit <- gkwreg2(y ~ x, data = df, family = "kw")
res <- residuals(fit, type = "quantile")
qqnorm(res)
qqline(res)

# }
```

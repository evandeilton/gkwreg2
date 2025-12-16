# Diagnostic Plots for gkwreg2 Objects

Produces diagnostic plots for assessing model fit and detecting
influential observations.

## Usage

``` r
# S3 method for class 'gkwreg2'
plot(
  x,
  which = 1:6,
  type = c("quantile", "pearson", "deviance", "response"),
  ask = interactive() && length(which) > 1,
  id.n = 3,
  labels = NULL,
  ...
)
```

## Arguments

- x:

  A fitted `gkwreg2` object.

- which:

  Integer vector specifying which plots to produce:

  1

  :   Residuals vs Index

  2

  :   Q-Q Plot of residuals

  3

  :   Residuals vs Fitted Values

  4

  :   Cook's Distance (if available)

  5

  :   Histogram of residuals

  6

  :   Observed vs Predicted

- type:

  Residual type for plots 1-3, 5: `"quantile"` (default), `"pearson"`,
  `"deviance"`, or `"response"`.

- ask:

  Logical: prompt before each plot?

- id.n:

  Number of extreme points to label.

- labels:

  Labels for points (default: observation indices).

- ...:

  Additional graphical parameters.

## Value

Invisibly returns the diagnostic data as a list.

## Examples

``` r
# \donttest{
set.seed(123)
n <- 100
x <- rnorm(n)
y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
df <- data.frame(y = y, x = x)
fit <- gkwreg2(y ~ x, data = df, family = "kw")
plot(fit) # All 6 plots






plot(fit, which = c(2, 6)) # Q-Q and Obs vs Pred only


# }
```

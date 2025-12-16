# Predict Method for gkwreg2 Objects

Generates predictions from a fitted `gkwreg2` model for new or existing
data.

## Usage

``` r
# S3 method for class 'gkwreg2'
predict(
  object,
  newdata = NULL,
  type = c("response", "parameters", "link", "quantile", "density", "variance"),
  at = NULL,
  se.fit = FALSE,
  ...
)
```

## Arguments

- object:

  A fitted `gkwreg2` object.

- newdata:

  Optional data frame for prediction. If omitted, uses the original
  training data.

- type:

  Type of prediction:

  `"response"`

  :   Expected mean E(Y\|X) (default)

  `"parameters"`

  :   All distribution parameters

  `"link"`

  :   Linear predictors (\\\eta\\)

  `"quantile"`

  :   Quantiles at specified probabilities

  `"density"`

  :   Density at specified y values

  `"variance"`

  :   Conditional variance Var(Y\|X)

- at:

  For `type = "quantile"`: probabilities. For `type = "density"`: y
  values.

- se.fit:

  Logical: return standard errors? (Not yet implemented)

- ...:

  Currently ignored.

## Value

Depending on `type`:

- `"response"`:

  Numeric vector of predicted means

- `"parameters"`:

  Named list of parameter vectors

- `"link"`:

  Named list of linear predictor vectors

- `"quantile"`:

  Matrix with rows per obs, cols per quantile

- `"density"`:

  Vector of density values

- `"variance"`:

  Vector of variances

## Examples

``` r
# \donttest{
set.seed(123)
n <- 100
x <- rnorm(n)
y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
df <- data.frame(y = y, x = x)
fit <- gkwreg2(y ~ x, data = df, family = "kw")

# Predicted means
pred <- predict(fit, type = "response")

# Distribution parameters
params <- predict(fit, type = "parameters")

# Quantiles
q <- predict(fit, type = "quantile", at = c(0.1, 0.5, 0.9))
# }
```

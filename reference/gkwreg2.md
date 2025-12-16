# Fit Generalized Kumaraswamy Regression Model

Fits regression models for bounded (0,1) data using the Generalized
Kumaraswamy distribution family. Supports extended formula syntax for
specifying different covariates for each distribution parameter.

## Usage

``` r
gkwreg2(
  formula,
  data,
  family = c("gkw", "bkw", "kkw", "ekw", "mc", "kw", "beta"),
  link = NULL,
  start = NULL,
  control = gkw_control(),
  ...
)
```

## Arguments

- formula:

  An extended formula with syntax `y ~ x1 | x2 | ...` where each part
  separated by `|` corresponds to a distribution parameter. Use the
  [Formula](https://rdrr.io/pkg/Formula/man/Formula.html) package
  syntax.

- data:

  A data frame containing the variables in the formula.

- family:

  Distribution family. One of `"gkw"`, `"bkw"`, `"kkw"`, `"ekw"`,
  `"mc"`, `"kw"`, or `"beta"`.

- link:

  Optional named list of link functions for each parameter. Default:
  `"log"` for positive parameters, `"logit"` for probability parameters.
  See Details.

- start:

  Optional named list of starting values for coefficients.

- control:

  A list of control parameters from
  [`gkw_control`](https://evandeilton.github.io/gkwreg2/reference/gkw_control.md).

- ...:

  Additional arguments (currently unused).

## Value

An object of class `"gkwreg2"` with components:

- coefficients:

  Named list of coefficient vectors per parameter

- vcov:

  Variance-covariance matrix (if Hessian computed)

- se:

  Standard errors (if Hessian computed)

- loglik:

  Maximized log-likelihood

- fitted.values:

  Fitted response means

- residuals:

  Response residuals (y - fitted)

- converged:

  Logical: did optimization converge?

- family:

  Distribution family used

- link:

  Link functions used

- formula:

  Original formula

- data:

  Original data (if `keep_data = TRUE`)

- call:

  Matched call

- n:

  Number of observations

- npar:

  Total number of parameters

## Details

### Formula Syntax

The extended formula uses `|` to separate specifications for different
parameters. The order matches the parameter order for each family:

- `kw`: `y ~ alpha_formula | beta_formula`

- `beta`: `y ~ gamma_formula | delta_formula`

- `ekw`: `y ~ alpha | beta | lambda`

- etc.

Unspecified parts default to intercept-only models.

### Link Functions

Available links:

- For positive parameters: `"log"` (default), `"sqrt"`, `"inverse"`,
  `"identity"`

- For probability parameters: `"logit"` (default), `"probit"`,
  `"cloglog"`, `"cauchy"`

## See also

[`gkw_control`](https://evandeilton.github.io/gkwreg2/reference/gkw_control.md)
for control parameters,
[`summary.gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/summary.gkwreg2.md)
for model summaries,
[`predict.gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/predict.gkwreg2.md)
for predictions,
[`residuals.gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/residuals.gkwreg2.md)
for residuals.

## Examples

``` r
# \donttest{
# Simulate Kumaraswamy data
set.seed(123)
n <- 200
x <- rnorm(n)
alpha <- exp(0.5 + 0.3 * x)
beta <- exp(1.0)
y <- gkwdist::rkw(n, alpha = alpha, beta = beta)
df <- data.frame(y = y, x = x)

# Fit Kumaraswamy regression
fit <- gkwreg2(y ~ x, data = df, family = "kw")
summary(fit)
#> 
#> Call:
#> gkwreg2(formula = y ~ x, data = df, family = "kw")
#> 
#> Family: kw 
#> Link functions: alpha = log, beta = log 
#> 
#> Residuals:
#>       Min        1Q    Median        3Q       Max 
#> -0.386021 -0.151330 -0.008634  0.154157  0.520779 
#> 
#> Coefficients:
#> 
#> --- ALPHA ---
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)  0.49668    0.07444   6.672 2.52e-11 ***
#> x            0.32640    0.04902   6.658 2.77e-11 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> --- BETA ---
#>             Estimate Std. Error z value Pr(>|z|)    
#> (Intercept)   0.9832     0.1125    8.74   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> ---
#> Log-Likelihood: 45.53 on 197 Df
#> AIC: -85.06   BIC: -75.17
#> Number of observations: 200
#> Number of parameters: 3
#> Converged in 15 iterations
#> 

# Different covariates per parameter
fit2 <- gkwreg2(y ~ x | 1, data = df, family = "kw")

# Beta regression
fit_beta <- gkwreg2(y ~ x, data = df, family = "beta")
# }
```

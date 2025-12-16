# Summary for gkwreg2 Object

Produces a detailed summary of a fitted `gkwreg2` model including
coefficient tables with standard errors, z-values, and p-values.

## Usage

``` r
# S3 method for class 'gkwreg2'
summary(object, ...)
```

## Arguments

- object:

  A fitted `gkwreg2` object.

- ...:

  Currently ignored.

## Value

An object of class `"summary.gkwreg2"` with components:

- call:

  Model call

- family:

  Distribution family

- coefficients:

  List of coefficient tables per parameter

- residuals:

  Summary statistics for residuals

- loglik:

  Log-likelihood

- aic, bic:

  Information criteria

- n, npar:

  Sample size and number of parameters

- converged:

  Convergence status

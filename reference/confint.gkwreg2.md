# Confidence Intervals for gkwreg2 Coefficients

Computes Wald-type confidence intervals for model parameters.

## Usage

``` r
# S3 method for class 'gkwreg2'
confint(object, parm = NULL, level = 0.95, ...)
```

## Arguments

- object:

  A fitted `gkwreg2` object.

- parm:

  Parameter specification (currently ignored, all returned).

- level:

  Confidence level (default 0.95).

- ...:

  Currently ignored.

## Value

A matrix with columns for lower and upper bounds.

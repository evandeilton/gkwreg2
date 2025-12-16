# Extract Model Matrix from gkwreg2 Object

Extract Model Matrix from gkwreg2 Object

## Usage

``` r
# S3 method for class 'gkwreg2'
model.matrix(object, parameter = NULL, ...)
```

## Arguments

- object:

  A fitted `gkwreg2` object.

- parameter:

  Which parameter's design matrix to extract.

- ...:

  Currently ignored.

## Value

A design matrix, or list of matrices if `parameter` not specified.

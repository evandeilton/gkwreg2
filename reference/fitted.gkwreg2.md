# Extract Fitted Values from gkwreg2 Object

Extract Fitted Values from gkwreg2 Object

## Usage

``` r
# S3 method for class 'gkwreg2'
fitted(object, type = c("response", "parameters"), ...)
```

## Arguments

- object:

  A fitted `gkwreg2` object.

- type:

  Type of fitted values: `"response"` (default) for expected means, or
  `"parameters"` for fitted distribution parameters.

- ...:

  Currently ignored.

## Value

For `type = "response"`, a vector of fitted means. For
`type = "parameters"`, a list of parameter vectors.

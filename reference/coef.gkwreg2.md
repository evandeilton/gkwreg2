# Extract Coefficients from gkwreg2 Object

Extract Coefficients from gkwreg2 Object

## Usage

``` r
# S3 method for class 'gkwreg2'
coef(object, parameter = NULL, flatten = TRUE, ...)
```

## Arguments

- object:

  A fitted `gkwreg2` object.

- parameter:

  Optional: name of specific parameter to extract.

- flatten:

  Logical: if `TRUE`, return a single named vector.

- ...:

  Currently ignored.

## Value

If `flatten = FALSE` (default), a named list of coefficient vectors per
parameter. If `flatten = TRUE`, a single named vector.

## Examples

``` r
# \donttest{
set.seed(123)
n <- 100
x <- rnorm(n)
y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
df <- data.frame(y = y, x = x)
fit <- gkwreg2(y ~ x, data = df, family = "kw")
coef(fit) # Vector format (default)
#> alpha:(Intercept)           alpha:x  beta:(Intercept) 
#>         0.4928363         0.2596803         1.0420609 
coef(fit, flatten = FALSE) # List format
#> $alpha
#> (Intercept)           x 
#>   0.4928363   0.2596803 
#> 
#> $beta
#> (Intercept) 
#>    1.042061 
#> 
coef(fit, parameter = "alpha")
#> (Intercept)           x 
#>   0.4928363   0.2596803 
# }
```

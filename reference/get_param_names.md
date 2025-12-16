# Get Parameter Names for Distribution Family

Returns the parameter names in the correct order for each distribution
family.

## Usage

``` r
get_param_names(family)
```

## Arguments

- family:

  Distribution family: `"gkw"`, `"bkw"`, `"kkw"`, `"ekw"`, `"mc"`,
  `"kw"`, or `"beta"`.

## Value

Character vector of parameter names.

## Examples

``` r
get_param_names("kw")
#> [1] "alpha" "beta" 
# [1] "alpha" "beta"

get_param_names("gkw")
#> [1] "alpha"  "beta"   "gamma"  "delta"  "lambda"
# [1] "alpha" "beta" "gamma" "delta" "lambda"
```

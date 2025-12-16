# Get Default Link Functions for Family

Returns the default link functions for each parameter of a distribution
family. All parameters are positive (\> 0), so log link is appropriate.

## Usage

``` r
get_default_links(family)
```

## Arguments

- family:

  Distribution family name.

## Value

Named list of link function names.

## Details

Parameter spaces for each family:

- kw: alpha \> 0, beta \> 0

- beta: gamma \> 0, delta \> 0 (shape1, shape2)

- ekw: alpha \> 0, beta \> 0, lambda \> 0

- mc: gamma \> 0, delta \> 0, lambda \> 0

- bkw: alpha \> 0, beta \> 0, gamma \> 0, delta \> 0

- kkw: alpha \> 0, beta \> 0, delta \> 0, lambda \> 0

- gkw: alpha \> 0, beta \> 0, gamma \> 0, delta \> 0, lambda \> 0

All parameters are strictly positive, so log link is the natural choice
as it maps R -\> (0, inf).

## Examples

``` r
get_default_links("kw")
#> $alpha
#> [1] "log"
#> 
#> $beta
#> [1] "log"
#> 
# $alpha: "log", $beta: "log"
```

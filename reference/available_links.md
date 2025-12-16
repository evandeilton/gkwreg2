# Available Link Functions

Returns a character vector of available link function names.

## Usage

``` r
available_links(type = c("positive", "probability", "all"))
```

## Arguments

- type:

  Type of parameter: `"positive"` for positive parameters (α, β, γ, λ),
  or `"probability"` for probability parameters (δ ∈ (0,1)).

## Value

Character vector of link function names.

## Examples

``` r
available_links("positive")
#> [1] "log"      "sqrt"     "inverse"  "identity"
available_links("probability")
#> [1] "logit"   "probit"  "cloglog" "cauchy" 
```

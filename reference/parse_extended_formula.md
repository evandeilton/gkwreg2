# Parse Extended Formula with \| Separators

Parses formulas with the extended syntax `y ~ x1 | x2 | ...` where each
part after `|` corresponds to a distribution parameter.

## Usage

``` r
parse_extended_formula(formula, family)
```

## Arguments

- formula:

  A formula object.

- family:

  Distribution family name.

## Value

A named list of formula objects, one per parameter.

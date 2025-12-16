# Get Parameter Indices in Concatenated Coefficient Vector

Computes the start indices and counts for each parameter's coefficients
in the concatenated theta vector.

## Usage

``` r
get_param_indices(X_list)
```

## Arguments

- X_list:

  Named list of design matrices.

## Value

List with `start` (0-indexed start positions) and `n_coefs` (number of
coefficients per parameter).

# Organize Coefficients into Named List

Converts the flat coefficient vector back into a named list structure.

## Usage

``` r
organize_coefficients(theta_hat, X_list, indices)
```

## Arguments

- theta_hat:

  Fitted coefficient vector.

- X_list:

  Named list of design matrices.

- indices:

  Parameter indices from `get_param_indices`.

## Value

Named list of coefficient vectors.

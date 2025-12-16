# Compute numerical gradient via central differences

Compute numerical gradient via central differences

## Usage

``` r
gradient_numerical(
  theta_concat,
  y,
  X_list,
  family,
  link_list,
  param_indices,
  n_coefs,
  eps = 1e-06
)
```

## Arguments

- theta_concat:

  Concatenated regression coefficients

- y:

  Response vector

- X_list:

  List of design matrices

- family:

  Distribution family

- link_list:

  Named list of link functions

- param_indices:

  Start indices

- n_coefs:

  Number of coefficients per parameter

- eps:

  Step size for finite differences

## Value

Numerical gradient vector

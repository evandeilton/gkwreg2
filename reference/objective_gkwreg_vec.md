# Compute negative log-likelihood using vectorized gkwdist calls

Compute negative log-likelihood using vectorized gkwdist calls

## Usage

``` r
objective_gkwreg_vec(
  theta_concat,
  y,
  X_list,
  family,
  link_list,
  param_indices,
  n_coefs
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

## Value

Negative log-likelihood value

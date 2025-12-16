# Compute gradient of negative log-likelihood for gkwreg2

Uses chain rule: ∂ℓ/∂β_p = Xᵀ_p × (∂ℓ/∂θ_p × ∂θ_p/∂η_p)

## Usage

``` r
gradient_gkwreg(
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

  Response vector (values in (0,1))

- X_list:

  List of design matrices (one per parameter)

- family:

  Distribution family name

- link_list:

  Named list of link functions per parameter

- param_indices:

  Integer vector with start indices for each parameter

- n_coefs:

  Integer vector with number of coefficients per parameter

## Value

Gradient vector (same length as theta_concat)

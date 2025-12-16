# Compute Fitted Parameter Values

Computes the fitted distribution parameters for each observation.

## Usage

``` r
compute_fitted_params(theta_hat, X_list, family, link, indices)
```

## Arguments

- theta_hat:

  Fitted coefficient vector.

- X_list:

  Named list of design matrices.

- family:

  Distribution family.

- link:

  Named list of link functions.

- indices:

  Parameter indices.

## Value

Named list of fitted parameter vectors.

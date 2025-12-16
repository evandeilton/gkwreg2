# Get Starting Values for Optimization

Computes robust starting values for the regression coefficients using
moment matching based on the sample mean and variance. This is crucial
for optimizers like BFGS that don't handle bounds.

## Usage

``` r
get_starting_values(y, X_list, family, link, control = NULL)
```

## Arguments

- y:

  Response vector (values in (0,1)).

- X_list:

  Named list of design matrices.

- family:

  Distribution family.

- link:

  Named list of link functions.

- control:

  Control parameters (optional).

## Value

Named numeric vector of starting values.

## Details

The function uses moment matching when possible:

- For Kumaraswamy: approximate based on sample mean

- For Beta: use method of moments (gamma, delta from mean and variance)

- For others: use reasonable default values that ensure stability

All starting values are chosen to be well within the valid parameter
space to avoid boundary issues during optimization.

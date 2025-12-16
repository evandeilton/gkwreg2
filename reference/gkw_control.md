# Control Parameters for gkwreg2 Fitting

Specifies control parameters for the optimization algorithm used in
[`gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/gkwreg2.md).

## Usage

``` r
gkw_control(
  method = c("nlminb", "BFGS", "Nelder-Mead", "CG", "L-BFGS-B"),
  maxit = 1000L,
  reltol = 1e-08,
  trace = 0L,
  hessian = TRUE,
  start = NULL,
  keep_data = TRUE,
  use_gradient = TRUE
)
```

## Arguments

- method:

  Optimization method. One of `"nlminb"` (default), `"BFGS"`,
  `"Nelder-Mead"`, `"CG"`, or `"L-BFGS-B"`.

- maxit:

  Maximum number of iterations (default 1000).

- reltol:

  Relative convergence tolerance (default 1e-8).

- trace:

  Integer trace level: 0 = silent (default), 1+ = verbose.

- hessian:

  Logical; if `TRUE` (default), compute Hessian for standard error
  estimation.

- start:

  Optional named list of starting values for coefficients.

- keep_data:

  Logical; if `TRUE` (default), keep data in fitted object.

- use_gradient:

  Logical; if `TRUE` (default), use analytical gradient.

## Value

A list of control parameters with class `"gkw_control"`.

## Examples

``` r
# Default control
ctrl <- gkw_control()

# Use BFGS with more iterations
ctrl <- gkw_control(method = "BFGS", maxit = 2000)

# Disable Hessian computation for speed
ctrl <- gkw_control(hessian = FALSE)
```

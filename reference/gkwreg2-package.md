# gkwreg2: Generalized Kumaraswamy Regression Models

The gkwreg2 package provides regression modeling for bounded data in
(0,1) using the Generalized Kumaraswamy distribution family. It
implements maximum likelihood estimation with analytical gradients via
RcppArmadillo.

## Distribution Families

- **gkw**: Generalized Kumaraswamy (5 parameters: α, β, γ, δ, λ)

- **bkw**: Beta-Kumaraswamy (4 parameters: α, β, γ, δ)

- **kkw**: Kumaraswamy-Kumaraswamy (4 parameters: α, β, δ, λ)

- **ekw**: Exponentiated Kumaraswamy (3 parameters: α, β, λ)

- **mc**: McDonald (3 parameters: γ, δ, λ)

- **kw**: Kumaraswamy (2 parameters: α, β)

- **beta**: Beta (2 parameters: γ, δ)

## Main Functions

- [`gkwreg2`](https://evandeilton.github.io/gkwreg2/reference/gkwreg2.md):
  Main model fitting function

- [`gkw_control`](https://evandeilton.github.io/gkwreg2/reference/gkw_control.md):
  Control parameters for optimization

- [`get_param_names`](https://evandeilton.github.io/gkwreg2/reference/get_param_names.md):
  Get parameter names for a family

- [`get_default_links`](https://evandeilton.github.io/gkwreg2/reference/get_default_links.md):
  Get default link functions

## See also

Useful links:

- <https://github.com/evandeilton/gkwreg2>

- Report bugs at <https://github.com/evandeilton/gkwreg2/issues>

## Author

Evandeilton Lopes

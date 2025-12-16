# gkwreg2: Generalized Kumaraswamy Regression Models

The gkwreg2 package provides regression modeling for bounded data in
(0,1) using the Generalized Kumaraswamy distribution family. It
implements maximum likelihood estimation with analytical gradients via
RcppArmadillo.

## Distribution Families

- **gkw**: Generalized Kumaraswamy (5 parameters: \\\alpha\\, \\\beta\\,
  \\\gamma\\, \\\delta\\, \\\lambda\\)

- **bkw**: Beta-Kumaraswamy (4 parameters: \\\alpha\\, \\\beta\\,
  \\\gamma\\, \\\delta\\)

- **kkw**: Kumaraswamy-Kumaraswamy (4 parameters: \\\alpha\\, \\\beta\\,
  \\\delta\\, \\\lambda\\)

- **ekw**: Exponentiated Kumaraswamy (3 parameters: \\\alpha\\,
  \\\beta\\, \\\lambda\\)

- **mc**: McDonald (3 parameters: \\\gamma\\, \\\delta\\, \\\lambda\\)

- **kw**: Kumaraswamy (2 parameters: \\\alpha\\, \\\beta\\)

- **beta**: Beta (2 parameters: \\\gamma\\, \\\delta\\)

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

- <https://evandeilton.github.io/gkwreg2/>

- Report bugs at <https://github.com/evandeilton/gkwreg2/issues>

## Author

Evandeilton Lopes

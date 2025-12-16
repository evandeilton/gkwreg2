# gkwreg2 0.1.3

# gkwreg2 0.1.2

## New Features

* Added introductory vignette with comprehensive examples
* Added CITATION file for academic references
* Updated pkgdown site with flatly theme

---

# gkwreg2 0.1.1

## New Features

* Added GitHub Actions R-CMD-check workflow
* Comprehensive README with badges and examples
* Full API compatibility with gkwreg package

## Bug Fixes

* Fixed `predict.gkwreg2()` to correctly access `coef_list` instead of flat `coefficients`
* Fixed NA handling in variance-covariance matrix diagonal check
* Added all required stats imports (AIC, BIC, resid, etc.)
* Corrected `gkw_control()` to use `factr`/`pgtol` for L-BFGS-B optimizer

## Documentation

* All examples now include complete data generation code
* Removed VignetteBuilder without vignettes
* Cleaned up DESCRIPTION metadata

---

# gkwreg2 0.1.0

## Initial Release

* Pure RcppArmadillo implementation (no TMB dependency)
* Support for 7 distribution families: gkw, bkw, kkw, ekw, mc, kw, beta
* Extended formula syntax: `y ~ alpha_formula | beta_formula | ...`
* Analytical gradients via gkwdist package
* Multiple optimizers: nlminb, BFGS, L-BFGS-B, Nelder-Mead, CG
* Parameter clamping for numerical stability
* Moment-based starting values for robust convergence

## S3 Methods

* `coef()`, `vcov()`, `confint()` - coefficient extraction and inference
* `logLik()`, `AIC()`, `BIC()`, `nobs()` - model fit statistics
* `fitted()`, `residuals()` - fitted values and residuals (response, pearson, quantile)
* `predict()` - predictions (response, parameters, link, quantile, density, variance)
* `summary()`, `print()` - model summaries
* `anova()` - likelihood ratio tests for nested models
* `plot()` - 6 diagnostic plot types
* `formula()`, `model.frame()`, `model.matrix()`, `update()` - model utilities

## Compatibility

* Full output structure compatibility with gkwreg
* Same coefficient naming convention (colon-separated)
* Same class inheritance: `c("gkwreg2", "gkwreg")`

## Testing

* 370+ unit tests covering all families and methods
* R CMD check --as-cran: 0 errors, 0 warnings

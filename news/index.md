# Changelog

## gkwreg2 0.1.1

### New Features

- Added GitHub Actions R-CMD-check workflow
- Comprehensive README with badges and examples
- Full API compatibility with gkwreg package

### Bug Fixes

- Fixed
  [`predict.gkwreg2()`](https://evandeilton.github.io/gkwreg2/reference/predict.gkwreg2.md)
  to correctly access `coef_list` instead of flat `coefficients`
- Fixed NA handling in variance-covariance matrix diagonal check
- Added all required stats imports (AIC, BIC, resid, etc.)
- Corrected
  [`gkw_control()`](https://evandeilton.github.io/gkwreg2/reference/gkw_control.md)
  to use `factr`/`pgtol` for L-BFGS-B optimizer

### Documentation

- All examples now include complete data generation code
- Removed VignetteBuilder without vignettes
- Cleaned up DESCRIPTION metadata

------------------------------------------------------------------------

## gkwreg2 0.1.0

### Initial Release

- Pure RcppArmadillo implementation (no TMB dependency)
- Support for 7 distribution families: gkw, bkw, kkw, ekw, mc, kw, beta
- Extended formula syntax: `y ~ alpha_formula | beta_formula | ...`
- Analytical gradients via gkwdist package
- Multiple optimizers: nlminb, BFGS, L-BFGS-B, Nelder-Mead, CG
- Parameter clamping for numerical stability
- Moment-based starting values for robust convergence

### S3 Methods

- [`coef()`](https://rdrr.io/r/stats/coef.html),
  [`vcov()`](https://rdrr.io/r/stats/vcov.html),
  [`confint()`](https://rdrr.io/r/stats/confint.html) - coefficient
  extraction and inference
- [`logLik()`](https://rdrr.io/r/stats/logLik.html),
  [`AIC()`](https://rdrr.io/r/stats/AIC.html),
  [`BIC()`](https://rdrr.io/r/stats/AIC.html),
  [`nobs()`](https://rdrr.io/r/stats/nobs.html) - model fit statistics
- [`fitted()`](https://rdrr.io/r/stats/fitted.values.html),
  [`residuals()`](https://rdrr.io/r/stats/residuals.html) - fitted
  values and residuals (response, pearson, quantile)
- [`predict()`](https://rdrr.io/r/stats/predict.html) - predictions
  (response, parameters, link, quantile, density, variance)
- [`summary()`](https://rdrr.io/r/base/summary.html),
  [`print()`](https://rdrr.io/r/base/print.html) - model summaries
- [`anova()`](https://rdrr.io/r/stats/anova.html) - likelihood ratio
  tests for nested models
- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) - 6
  diagnostic plot types
- [`formula()`](https://rdrr.io/r/stats/formula.html),
  [`model.frame()`](https://rdrr.io/r/stats/model.frame.html),
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html),
  [`update()`](https://rdrr.io/r/stats/update.html) - model utilities

### Compatibility

- Full output structure compatibility with gkwreg
- Same coefficient naming convention (colon-separated)
- Same class inheritance: `c("gkwreg2", "gkwreg")`

### Testing

- 370+ unit tests covering all families and methods
- R CMD check –as-cran: 0 errors, 0 warnings

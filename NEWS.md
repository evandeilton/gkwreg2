# gkwreg2 0.1.0

## Initial Release

* First version of gkwreg2, a pure RcppArmadillo replacement for gkwreg
* Implements maximum likelihood estimation without TMB dependency
* Uses analytical gradients from gkwdist package

### Features

* Support for 7 distribution families: GKw, BKw, KKw, EKw, MC, Kw, Beta
* 8 link functions: log, sqrt, inverse, identity, logit, probit, cloglog, cauchy
* Extended formula syntax with `|` separators for different covariates per parameter
* Complete S3 interface: print, summary, coef, vcov, confint, logLik, AIC, BIC, predict, residuals, plot, anova
* 4 residual types: response, pearson, deviance, quantile (Dunn & Smyth)
* 6 diagnostic plot types
* Model comparison via likelihood ratio tests

### Documentation

* Full roxygen2 documentation for all exported functions
* Package vignettes (coming soon)

# ANOVA for gkwreg2 Objects

Performs likelihood ratio tests to compare nested `gkwreg2` models.

## Usage

``` r
# S3 method for class 'gkwreg2'
anova(object, ..., test = c("Chisq", "LRT", "none"))
```

## Arguments

- object:

  A fitted `gkwreg2` object.

- ...:

  Additional `gkwreg2` objects to compare.

- test:

  Type of test: `"Chisq"` or `"LRT"` (equivalent), or `"none"` to
  suppress p-values.

## Value

A data frame of class `"anova"` with columns:

- Model:

  Model identifier

- Df:

  Degrees of freedom (number of parameters)

- LogLik:

  Log-likelihood

- AIC:

  Akaike Information Criterion

- BIC:

  Bayesian Information Criterion

- Chisq:

  Likelihood ratio test statistic (vs previous model)

- Df diff:

  Difference in degrees of freedom

- Pr(\>Chisq):

  P-value from chi-squared distribution

## Details

Models should be nested for valid likelihood ratio tests. The function
compares each model to the previous one in the list, computing: \$\$LRT
= -2(\ell_0 - \ell_1)\$\$ where \\\ell_0\\ is the log-likelihood of the
simpler model.

## Examples

``` r
# \donttest{
# Generate test data
set.seed(123)
n <- 150
x <- rnorm(n)
y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
df <- data.frame(y = y, x = x)

# Compare nested models
fit0 <- gkwreg2(y ~ 1, data = df, family = "kw")
fit1 <- gkwreg2(y ~ x, data = df, family = "kw")
fit2 <- gkwreg2(y ~ x | x, data = df, family = "kw")

anova(fit0, fit1, fit2)
#> Analysis of Variance Table (Likelihood Ratio Tests)
#> 
#> Family: kw
#> n = 150
#> 
#>                    Model Df LogLik    AIC    BIC    Chisq Df diff Pr(>Chisq)
#> Model 1: y ~ 1         1  2  19.11 -34.21 -28.19       NA      NA         NA
#> Model 2: y ~ x         2  3  30.75 -55.51 -46.47 -23.2931       1          1
#> Model 3: y ~ x | x     3  4  31.04 -54.09 -42.04  -0.5802       1          1
#>                    Signif
#> Model 1: y ~ 1           
#> Model 2: y ~ x           
#> Model 3: y ~ x | x       
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# }
```

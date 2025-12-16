#' ANOVA for gkwreg2 Objects
#'
#' Performs likelihood ratio tests to compare nested \code{gkwreg2} models.
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param ... Additional \code{gkwreg2} objects to compare.
#' @param test Type of test: \code{"Chisq"} or \code{"LRT"} (equivalent),
#'   or \code{"none"} to suppress p-values.
#'
#' @return A data frame of class \code{"anova"} with columns:
#'   \describe{
#'     \item{Model}{Model identifier}
#'     \item{Df}{Degrees of freedom (number of parameters)}
#'     \item{LogLik}{Log-likelihood}
#'     \item{AIC}{Akaike Information Criterion}
#'     \item{BIC}{Bayesian Information Criterion}
#'     \item{Chisq}{Likelihood ratio test statistic (vs previous model)}
#'     \item{Df diff}{Difference in degrees of freedom}
#'     \item{Pr(>Chisq)}{P-value from chi-squared distribution}
#'   }
#'
#' @details
#' Models should be nested for valid likelihood ratio tests. The function
#' compares each model to the previous one in the list, computing:
#' \deqn{LRT = -2(\ell_0 - \ell_1)}
#' where \eqn{\ell_0} is the log-likelihood of the simpler model.
#'
#' @examples
#' \donttest{
#' # Generate test data
#' set.seed(123)
#' n <- 150
#' x <- rnorm(n)
#' y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
#' df <- data.frame(y = y, x = x)
#'
#' # Compare nested models
#' fit0 <- gkwreg2(y ~ 1, data = df, family = "kw")
#' fit1 <- gkwreg2(y ~ x, data = df, family = "kw")
#' fit2 <- gkwreg2(y ~ x | x, data = df, family = "kw")
#'
#' anova(fit0, fit1, fit2)
#' }
#'
#' @export
anova.gkwreg2 <- function(object, ..., test = c("Chisq", "LRT", "none")) {
    test <- match.arg(test)

    # Collect all models
    models <- list(object, ...)
    n_models <- length(models)

    if (n_models < 2) {
        stop("anova requires at least two models for comparison")
    }

    # Validate all are gkwreg2 objects
    for (i in seq_along(models)) {
        if (!inherits(models[[i]], "gkwreg2")) {
            stop("Object ", i, " is not a gkwreg2 object")
        }
    }

    # Check same family
    families <- sapply(models, function(m) m$family)
    if (length(unique(families)) > 1) {
        warning(
            "Models have different families: ",
            paste(unique(families), collapse = ", ")
        )
    }

    # Check same sample size
    n_obs <- sapply(models, function(m) m$n)
    if (length(unique(n_obs)) > 1) {
        stop(
            "Models have different sample sizes: ",
            paste(unique(n_obs), collapse = ", ")
        )
    }

    # Extract statistics
    df_list <- sapply(models, function(m) m$npar)
    loglik_list <- sapply(models, function(m) m$loglik)
    aic_list <- sapply(models, AIC.gkwreg2)
    bic_list <- sapply(models, BIC.gkwreg2)

    # Sort by df for proper comparison
    ord <- order(df_list)
    models <- models[ord]
    df_list <- df_list[ord]
    loglik_list <- loglik_list[ord]
    aic_list <- aic_list[ord]
    bic_list <- bic_list[ord]

    # Compute LRT statistics
    lrt_stat <- c(NA_real_, -2 * diff(loglik_list))
    df_diff <- c(NA_integer_, diff(df_list))

    # P-values
    if (test != "none") {
        p_value <- c(NA_real_, pchisq(lrt_stat[-1], df_diff[-1], lower.tail = FALSE))
    } else {
        p_value <- rep(NA_real_, n_models)
    }

    # Model names/formulas
    model_names <- sapply(seq_along(models), function(i) {
        paste0("Model ", i, ": ", deparse(models[[i]]$call$formula, width.cutoff = 40)[1])
    })

    # Create result table
    result <- data.frame(
        Model = 1:n_models,
        Df = df_list,
        LogLik = loglik_list,
        AIC = aic_list,
        BIC = bic_list,
        Chisq = lrt_stat,
        `Df diff` = df_diff,
        `Pr(>Chisq)` = p_value,
        check.names = FALSE,
        stringsAsFactors = FALSE
    )

    rownames(result) <- model_names

    # Add class and attributes
    class(result) <- c("anova.gkwreg2", "anova", "data.frame")
    attr(result, "heading") <- c(
        "Analysis of Variance Table (Likelihood Ratio Tests)\n",
        paste("Family:", unique(families)),
        paste("n =", unique(n_obs))
    )

    return(result)
}

#' Print ANOVA Table
#'
#' @param x An \code{anova.gkwreg2} object.
#' @param digits Number of significant digits.
#' @param signif.stars Logical: print significance stars?
#' @param ... Currently ignored.
#'
#' @export
print.anova.gkwreg2 <- function(x, digits = max(3, getOption("digits") - 3),
                                signif.stars = getOption("show.signif.stars"),
                                ...) {
    heading <- attr(x, "heading")
    if (!is.null(heading)) {
        cat(heading, sep = "\n")
        cat("\n")
    }

    # Format p-values with stars
    xx <- x
    if (signif.stars && "Pr(>Chisq)" %in% names(xx)) {
        pvals <- xx[["Pr(>Chisq)"]]
        stars <- symnum(pvals,
            cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
            symbols = c("***", "**", "*", ".", " "),
            na = ""
        )
        xx[["Pr(>Chisq)"]] <- format.pval(pvals, digits = digits)
        xx[["Signif"]] <- as.character(stars)
    }

    print.data.frame(xx, digits = digits, row.names = TRUE, ...)

    if (signif.stars) {
        cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    }

    invisible(x)
}

#' Compare Multiple gkwreg2 Models
#'
#' A convenience function for model comparison using AIC, BIC, and log-likelihood.
#'
#' @param ... \code{gkwreg2} objects to compare.
#' @param criterion Comparison criterion: \code{"AIC"} (default), \code{"BIC"},
#'   or \code{"logLik"}.
#'
#' @return A data frame with model comparison statistics.
#'
#' @examples
#' \donttest{
#' # Generate test data
#' set.seed(123)
#' n <- 150
#' x <- rnorm(n)
#' y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
#' df <- data.frame(y = y, x = x)
#'
#' fit_kw <- gkwreg2(y ~ x, data = df, family = "kw")
#' fit_beta <- gkwreg2(y ~ x, data = df, family = "beta")
#' compare_models(fit_kw, fit_beta)
#' }
#'
#' @export
compare_models <- function(..., criterion = c("AIC", "BIC", "logLik")) {
    criterion <- match.arg(criterion)
    models <- list(...)

    if (length(models) < 2) {
        stop("Need at least 2 models to compare")
    }

    # Get model names
    model_names <- as.character(match.call())[-1]
    model_names <- model_names[!model_names %in% c("criterion", names(formals(compare_models)))]
    if (length(model_names) < length(models)) {
        model_names <- paste0("Model", seq_along(models))
    }

    # Extract statistics
    stats <- data.frame(
        Model = model_names,
        Family = sapply(models, function(m) m$family),
        Df = sapply(models, function(m) m$npar),
        LogLik = sapply(models, function(m) m$loglik),
        AIC = sapply(models, AIC.gkwreg2),
        BIC = sapply(models, BIC.gkwreg2),
        stringsAsFactors = FALSE
    )

    # Sort by criterion
    if (criterion == "AIC") {
        stats <- stats[order(stats$AIC), ]
        stats$Delta_AIC <- stats$AIC - min(stats$AIC)
    } else if (criterion == "BIC") {
        stats <- stats[order(stats$BIC), ]
        stats$Delta_BIC <- stats$BIC - min(stats$BIC)
    } else {
        stats <- stats[order(-stats$LogLik), ]
    }

    rownames(stats) <- NULL
    return(stats)
}

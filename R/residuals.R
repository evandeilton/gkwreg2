#' Residuals for gkwreg2 Objects
#'
#' Extracts various types of residuals from a fitted \code{gkwreg2} model.
#'
#' @param object A fitted \code{gkwreg2} object.
#' @param type Type of residuals:
#'   \describe{
#'     \item{\code{"response"}}{Raw residuals: y - fitted (default)}
#'     \item{\code{"pearson"}}{Pearson residuals: (y - mu) / sqrt(V(mu))}
#'     \item{\code{"deviance"}}{Deviance residuals}
#'     \item{\code{"quantile"}}{Randomized quantile residuals (Dunn & Smyth)}
#'   }
#' @param ... Currently ignored.
#'
#' @return A numeric vector of residuals.
#'
#' @details
#' Randomized quantile residuals (\code{type = "quantile"}) are useful for
#' model checking as they should follow a standard normal distribution if
#' the model is correctly specified. They are computed as:
#' \deqn{r_i = \Phi^{-1}(F(y_i; \hat\theta_i))}
#' where \eqn{F} is the CDF of the fitted distribution.
#'
#' @references
#' Dunn, P.K. and Smyth, G.K. (1996). Randomized quantile residuals.
#' \emph{Journal of Computational and Graphical Statistics}, 5(3), 236-244.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' x <- rnorm(n)
#' y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
#' df <- data.frame(y = y, x = x)
#' fit <- gkwreg2(y ~ x, data = df, family = "kw")
#' res <- residuals(fit, type = "quantile")
#' qqnorm(res)
#' qqline(res)
#' }
#' @export
residuals.gkwreg2 <- function(
  object,
  type = c("response", "pearson", "deviance", "quantile"),
  ...
) {
    type <- match.arg(type)

    y <- object$y
    mu <- object$fitted.values
    params <- object$fitted.params
    n <- length(y)

    if (type == "response") {
        return(y - mu)
    }

    if (type == "pearson") {
        # Compute variance for each observation
        vars <- predict(object, type = "variance")
        vars <- pmax(vars, .Machine$double.eps) # Avoid division by zero
        return((y - mu) / sqrt(vars))
    }

    if (type == "deviance") {
        # Deviance residuals: sign(y - mu) * sqrt(2 * (ll_sat - ll_fit))
        # For bounded distributions, saturated model has y_i as parameter

        d_func <- switch(object$family,
            gkw = gkwdist::dgkw,
            bkw = gkwdist::dbkw,
            kkw = gkwdist::dkkw,
            ekw = gkwdist::dekw,
            mc = gkwdist::dmc,
            kw = gkwdist::dkw,
            beta = gkwdist::dbeta_,
            stop("Unknown family")
        )

        dev_res <- numeric(n)
        for (i in seq_len(n)) {
            par_i <- lapply(params, function(p) p[i])

            # Log-density at observed value
            log_dens <- tryCatch(
                {
                    args <- c(list(x = y[i], log = TRUE), par_i)
                    do.call(d_func, args)
                },
                error = function(e) -Inf
            )

            # Saturated log-density (approximation: use very concentrated distribution)
            # For simplicity, use 0 (perfect fit would have infinite density)
            log_dens_sat <- 0

            d_i <- 2 * (log_dens_sat - log_dens)
            dev_res[i] <- sign(y[i] - mu[i]) * sqrt(pmax(d_i, 0))
        }

        return(dev_res)
    }

    if (type == "quantile") {
        # Randomized quantile residuals
        p_func <- switch(object$family,
            gkw = gkwdist::pgkw,
            bkw = gkwdist::pbkw,
            kkw = gkwdist::pkkw,
            ekw = gkwdist::pekw,
            mc = gkwdist::pmc,
            kw = gkwdist::pkw,
            beta = gkwdist::pbeta_,
            stop("Unknown family")
        )

        u <- numeric(n)
        for (i in seq_len(n)) {
            par_i <- lapply(params, function(p) p[i])

            u[i] <- tryCatch(
                {
                    args <- c(list(q = y[i]), par_i)
                    do.call(p_func, args)
                },
                error = function(e) NA_real_
            )
        }

        # Clamp to avoid infinite qnorm values
        u <- pmin(pmax(u, .Machine$double.eps), 1 - .Machine$double.eps)

        # Transform to standard normal
        qres <- qnorm(u)

        return(qres)
    }
}

#' Residuals Alias
#'
#' @rdname residuals.gkwreg2
#' @export
resid.gkwreg2 <- residuals.gkwreg2

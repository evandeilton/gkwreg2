#' Diagnostic Plots for gkwreg2 Objects
#'
#' Produces diagnostic plots for assessing model fit and detecting
#' influential observations.
#'
#' @param x A fitted \code{gkwreg2} object.
#' @param which Integer vector specifying which plots to produce:
#'   \describe{
#'     \item{1}{Residuals vs Index}
#'     \item{2}{Q-Q Plot of residuals}
#'     \item{3}{Residuals vs Fitted Values}
#'     \item{4}{Cook's Distance (if available)}
#'     \item{5}{Histogram of residuals}
#'     \item{6}{Observed vs Predicted}
#'   }
#' @param type Residual type for plots 1-3, 5: \code{"quantile"} (default),
#'   \code{"pearson"}, \code{"deviance"}, or \code{"response"}.
#' @param ask Logical: prompt before each plot?
#' @param id.n Number of extreme points to label.
#' @param labels Labels for points (default: observation indices).
#' @param ... Additional graphical parameters.
#'
#' @return Invisibly returns the diagnostic data as a list.
#'
#' @examples
#' \donttest{
#' set.seed(123)
#' n <- 100
#' x <- rnorm(n)
#' y <- gkwdist::rkw(n, alpha = exp(0.5 + 0.2 * x), beta = exp(1))
#' df <- data.frame(y = y, x = x)
#' fit <- gkwreg2(y ~ x, data = df, family = "kw")
#' plot(fit) # All 6 plots
#' plot(fit, which = c(2, 6)) # Q-Q and Obs vs Pred only
#' }
#'
#' @export
plot.gkwreg2 <- function(
  x,
  which = 1:6,
  type = c("quantile", "pearson", "deviance", "response"),
  ask = interactive() && length(which) > 1,
  id.n = 3,
  labels = NULL,
  ...
) {
    type <- match.arg(type)

    if (!inherits(x, "gkwreg2")) {
        stop("'x' must be of class 'gkwreg2'", call. = FALSE)
    }

    # Compute residuals
    res <- residuals(x, type = type)
    fitted_vals <- x$fitted.values
    y <- x$y
    n <- length(y)

    # Labels for identification
    if (is.null(labels)) {
        labels <- as.character(1:n)
    }

    # Identify extreme points
    extreme_idx <- order(abs(res), decreasing = TRUE)[1:min(id.n, n)]

    # Setup multiple plots
    if (ask) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }

    # Store diagnostic data
    diagnostics <- list(
        residuals = res,
        fitted = fitted_vals,
        observed = y
    )

    # Plot 1: Residuals vs Index
    if (1 %in% which) {
        plot(1:n, res,
            xlab = "Observation Index",
            ylab = paste(tools::toTitleCase(type), "Residuals"),
            main = "Residuals vs Index",
            pch = 16, col = "steelblue", ...
        )
        abline(h = 0, lty = 2, col = "red")
        if (id.n > 0) {
            text(extreme_idx, res[extreme_idx], labels[extreme_idx],
                pos = 4, cex = 0.7, col = "red"
            )
        }
    }

    # Plot 2: Q-Q Plot
    if (2 %in% which) {
        qqnorm(res,
            main = paste("Q-Q Plot:", tools::toTitleCase(type), "Residuals"),
            pch = 16, col = "steelblue", ...
        )
        qqline(res, col = "red", lwd = 2)
        if (id.n > 0) {
            # Identify extreme points on Q-Q plot
            sorted_idx <- order(res)
            n_low <- head(sorted_idx, ceiling(id.n / 2))
            n_high <- tail(sorted_idx, floor(id.n / 2))
            id_points <- c(n_low, n_high)

            theoretical <- qnorm(ppoints(n))[order(order(res))]
            for (idx in id_points) {
                text(theoretical[idx], res[idx], labels[idx],
                    pos = 4, cex = 0.7, col = "red"
                )
            }
        }
    }

    # Plot 3: Residuals vs Fitted
    if (3 %in% which) {
        plot(fitted_vals, res,
            xlab = "Fitted Values",
            ylab = paste(tools::toTitleCase(type), "Residuals"),
            main = "Residuals vs Fitted",
            pch = 16, col = "steelblue", ...
        )
        abline(h = 0, lty = 2, col = "red")
        # Add lowess smoother
        lines(lowess(fitted_vals, res), col = "darkgreen", lwd = 2)
        if (id.n > 0) {
            text(fitted_vals[extreme_idx], res[extreme_idx], labels[extreme_idx],
                pos = 4, cex = 0.7, col = "red"
            )
        }
    }

    # Plot 4: Cook's Distance (approximation)
    if (4 %in% which) {
        # Approximate Cook's distance using leverage approximation
        # For GLM-type models: D_i ≈ r_i^2 * h_ii / (p * (1 - h_ii))
        # Without full leverage, use simplified version

        hat_approx <- 1 / n # Rough approximation
        p <- x$npar
        cooks_d <- res^2 * hat_approx / (p * (1 - hat_approx))

        diagnostics$cooks.d <- cooks_d

        plot(1:n, cooks_d,
            xlab = "Observation Index",
            ylab = "Cook's Distance (approx)",
            main = "Cook's Distance",
            type = "h", col = "steelblue", lwd = 2, ...
        )

        # Threshold line
        abline(h = 4 / n, lty = 2, col = "red")

        # Identify influential points
        influential <- which(cooks_d > 4 / n)
        if (length(influential) > 0) {
            text(influential, cooks_d[influential], labels[influential],
                pos = 3, cex = 0.7, col = "red"
            )
        }
    }

    # Plot 5: Histogram of residuals
    if (5 %in% which) {
        hist(res,
            breaks = "FD",
            main = paste("Histogram of", tools::toTitleCase(type), "Residuals"),
            xlab = "Residuals",
            col = "lightblue", border = "steelblue", freq = FALSE, ...
        )

        # Add normal curve for quantile residuals
        if (type == "quantile") {
            curve(dnorm(x), add = TRUE, col = "red", lwd = 2)
        }
    }

    # Plot 6: Observed vs Predicted
    if (6 %in% which) {
        plot(fitted_vals, y,
            xlab = "Predicted Values",
            ylab = "Observed Values",
            main = "Observed vs Predicted",
            pch = 16, col = "steelblue", ...
        )
        abline(0, 1, col = "red", lwd = 2)

        # Add R-squared
        r2 <- cor(y, fitted_vals)^2
        legend("bottomright",
            legend = bquote(R^2 == .(format(r2, digits = 3))),
            bty = "n", cex = 0.9
        )
    }

    invisible(diagnostics)
}

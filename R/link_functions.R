#' Apply Link Function (R wrapper)
#'
#' R wrappers for applying link functions to parameter values.
#'
#' @param theta Parameter values
#' @param link_type Link function name
#'
#' @return Transformed values (linear predictor scale)
#'
#' @keywords internal
apply_link_r <- function(theta, link_type) {
    switch(link_type,
        log = log(theta),
        sqrt = sqrt(theta),
        inverse = 1 / theta,
        identity = theta,
        logit = log(theta / (1 - theta)),
        probit = qnorm(theta),
        cloglog = log(-log(1 - theta)),
        cauchy = tan(pi * (theta - 0.5)),
        stop("Unknown link type: ", link_type)
    )
}

#' Available Link Functions
#'
#' Returns a character vector of available link function names.
#'
#' @param type Type of parameter: \code{"positive"} for positive parameters
#'   (α, β, γ, λ), or \code{"probability"} for probability parameters (δ ∈ (0,1)).
#'
#' @return Character vector of link function names.
#'
#' @examples
#' available_links("positive")
#' available_links("probability")
#'
#' @export
available_links <- function(type = c("positive", "probability", "all")) {
    type <- match.arg(type)

    positive_links <- c("log", "sqrt", "inverse", "identity")
    prob_links <- c("logit", "probit", "cloglog", "cauchy")

    switch(type,
        positive = positive_links,
        probability = prob_links,
        all = c(positive_links, prob_links)
    )
}

#' Parse Extended Formula with | Separators
#'
#' Parses formulas with the extended syntax \code{y ~ x1 | x2 | ...} where
#' each part after \code{|} corresponds to a distribution parameter.
#'
#' @param formula A formula object.
#' @param family Distribution family name.
#'
#' @return A named list of formula objects, one per parameter.
#'
#' @keywords internal
parse_extended_formula <- function(formula, family) {
  # Get parameter names for this family
  param_names <- get_param_names(family)

  # Convert formula to Formula object for multi-part handling
  if (!inherits(formula, "Formula")) {
    formula <- Formula::Formula(formula)
  }

  # Get number of RHS parts
  n_parts <- length(formula)[2]

  if (n_parts > length(param_names)) {
    stop(
      "Too many formula parts (", n_parts, ") for family '", family,
      "' which has ", length(param_names), " parameters"
    )
  }

  # Extract each part
  formulas <- vector("list", length(param_names))
  names(formulas) <- param_names

  for (i in seq_along(param_names)) {
    if (i <= n_parts) {
      # Extract RHS part i
      formulas[[i]] <- formula(formula, lhs = 0, rhs = i)
    } else {
      # Default to intercept-only for unspecified parameters
      formulas[[i]] <- ~1
    }
  }

  # Store original formula
  attr(formulas, "original") <- formula

  return(formulas)
}

#' Get Parameter Names for Distribution Family
#'
#' Returns the parameter names in the correct order for each distribution family.
#'
#' @param family Distribution family: \code{"gkw"}, \code{"bkw"}, \code{"kkw"},
#'   \code{"ekw"}, \code{"mc"}, \code{"kw"}, or \code{"beta"}.
#'
#' @return Character vector of parameter names.
#'
#' @examples
#' get_param_names("kw")
#' # [1] "alpha" "beta"
#'
#' get_param_names("gkw")
#' # [1] "alpha" "beta" "gamma" "delta" "lambda"
#'
#' @export
get_param_names <- function(family) {
  switch(family,
    gkw = c("alpha", "beta", "gamma", "delta", "lambda"),
    bkw = c("alpha", "beta", "gamma", "delta"),
    kkw = c("alpha", "beta", "delta", "lambda"),
    ekw = c("alpha", "beta", "lambda"),
    mc = c("gamma", "delta", "lambda"),
    kw = c("alpha", "beta"),
    beta = c("gamma", "delta"),
    stop("Unknown family: ", family)
  )
}

#' Create Model Matrices from Formula List
#'
#' Creates design matrices for each parameter from the parsed formula list.
#'
#' @param formula_list Named list of formulas (from \code{parse_extended_formula}).
#' @param data Data frame containing the variables.
#' @param response_name Name of the response variable (to exclude from predictors).
#'
#' @return Named list of model matrices.
#'
#' @keywords internal
create_model_matrices <- function(formula_list, data, response_name = NULL) {
  X_list <- lapply(formula_list, function(f) {
    # Avoid issues with response in the formula
    mf <- model.frame(f, data = data, na.action = na.pass)
    mm <- model.matrix(f, data = mf)
    return(mm)
  })

  return(X_list)
}

#' Extract Response Variable from Formula
#'
#' @param formula A formula object.
#' @param data Data frame.
#'
#' @return Numeric vector of response values.
#'
#' @keywords internal
extract_response <- function(formula, data) {
  # Use Formula to handle extended syntax
  if (!inherits(formula, "Formula")) {
    formula <- Formula::Formula(formula)
  }

  # Get model frame with response
  mf <- model.frame(formula, data = data)
  y <- model.response(mf)

  if (is.null(y)) {
    stop("Could not extract response variable from formula")
  }

  return(as.numeric(y))
}

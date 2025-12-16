// gkwreg2: Objective Function (Negative Log-Likelihood)
// =============================================================================
// Assembles the total negative log-likelihood by calling gkwdist ll* functions.
// Since gkwdist doesn't export C++ headers, we call R functions via Rcpp::Function.
// =============================================================================

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <string>

// Forward declarations for link functions
arma::vec apply_invlink(const arma::vec& eta, const std::string& link_type);

// =============================================================================
// HELPER: Get number of parameters per family
// =============================================================================

int get_npar_family(const std::string& family) {
  if (family == "gkw") return 5;
  if (family == "bkw") return 4;
  if (family == "kkw") return 4;
  if (family == "ekw") return 3;
  if (family == "mc") return 3;
  if (family == "kw") return 2;
  if (family == "beta") return 2;
  Rcpp::stop("Unknown family: " + family);
  return 0;
}

// =============================================================================
// HELPER: Get parameter names per family
// =============================================================================

Rcpp::CharacterVector get_param_names_cpp(const std::string& family) {
  if (family == "gkw") {
    return Rcpp::CharacterVector::create("alpha", "beta", "gamma", "delta", "lambda");
  } else if (family == "bkw") {
    return Rcpp::CharacterVector::create("alpha", "beta", "gamma", "delta");
  } else if (family == "kkw") {
    return Rcpp::CharacterVector::create("alpha", "beta", "delta", "lambda");
  } else if (family == "ekw") {
    return Rcpp::CharacterVector::create("alpha", "beta", "lambda");
  } else if (family == "mc") {
    return Rcpp::CharacterVector::create("gamma", "delta", "lambda");
  } else if (family == "kw") {
    return Rcpp::CharacterVector::create("alpha", "beta");
  } else if (family == "beta") {
    return Rcpp::CharacterVector::create("gamma", "delta");
  }
  Rcpp::stop("Unknown family: " + family);
  return Rcpp::CharacterVector::create();
}

// =============================================================================
// MAIN OBJECTIVE FUNCTION
// =============================================================================

//' Compute negative log-likelihood for gkwreg2
//'
//' @param theta_concat Concatenated regression coefficients
//' @param y Response vector (values in (0,1))
//' @param X_list List of design matrices (one per parameter)
//' @param family Distribution family name
//' @param link_list Named list of link functions per parameter
//' @param param_indices Integer vector with start indices for each parameter
//' @param n_coefs Integer vector with number of coefficients per parameter
//'
//' @return Negative log-likelihood value
//'
//' @keywords internal
// [[Rcpp::export]]
double objective_gkwreg(
    const arma::vec& theta_concat,
    const arma::vec& y,
    const Rcpp::List& X_list,
    const std::string& family,
    const Rcpp::List& link_list,
    const Rcpp::IntegerVector& param_indices,
    const Rcpp::IntegerVector& n_coefs
) {
  int n = y.n_elem;
  int npar = get_npar_family(family);
  Rcpp::CharacterVector param_names = get_param_names_cpp(family);
  
  // Get the appropriate ll* function from gkwdist
  Rcpp::Environment gkwdist = Rcpp::Environment::namespace_env("gkwdist");
  // Note: llbeta (no underscore) but dbeta_ (with underscore) in gkwdist
  std::string ll_func_name = "ll" + family;
  Rcpp::Function ll_func = gkwdist[ll_func_name];
  
  // Compute linear predictors and apply inverse links for each parameter
  std::vector<arma::vec> params(npar);
  
  for (int p = 0; p < npar; p++) {
    std::string pname = Rcpp::as<std::string>(param_names[p]);
    
    // Get design matrix
    arma::mat X_p = Rcpp::as<arma::mat>(X_list[pname]);
    
    // Extract coefficients for this parameter
    int start = param_indices[p];
    int nc = n_coefs[p];
    arma::vec beta_p = theta_concat.subvec(start, start + nc - 1);
    
    // Linear predictor
    arma::vec eta_p = X_p * beta_p;
    
    // Get link function
    std::string link_p = Rcpp::as<std::string>(link_list[pname]);
    
    // Apply inverse link
    params[p] = apply_invlink(eta_p, link_p);
  }
  
  // Compute total negative log-likelihood
  double nll = 0.0;
  
  for (int i = 0; i < n; i++) {
    // Assemble parameter vector for observation i
    Rcpp::NumericVector par_i(npar);
    for (int p = 0; p < npar; p++) {
      par_i[p] = params[p](i);
    }
    
    // Data for observation i
    Rcpp::NumericVector y_i = Rcpp::NumericVector::create(y(i));
    
    // Call ll* function - returns negative log-likelihood for single obs
    double nll_i = Rcpp::as<double>(ll_func(par_i, y_i));
    
    // Check for numerical issues
    if (!R_finite(nll_i)) {
      return R_PosInf;
    }
    
    nll += nll_i;
  }
  
  return nll;
}

// =============================================================================
// VECTORIZED OBJECTIVE (More efficient - processes all observations at once)
// =============================================================================

//' Compute negative log-likelihood using vectorized gkwdist calls
//'
//' @param theta_concat Concatenated regression coefficients
//' @param y Response vector
//' @param X_list List of design matrices
//' @param family Distribution family
//' @param link_list Named list of link functions
//' @param param_indices Start indices
//' @param n_coefs Number of coefficients per parameter
//'
//' @return Negative log-likelihood value
//'
//' @keywords internal
// [[Rcpp::export]]
double objective_gkwreg_vec(
    const arma::vec& theta_concat,
    const arma::vec& y,
    const Rcpp::List& X_list,
    const std::string& family,
    const Rcpp::List& link_list,
    const Rcpp::IntegerVector& param_indices,
    const Rcpp::IntegerVector& n_coefs
) {
  int n = y.n_elem;
  int npar = get_npar_family(family);
  Rcpp::CharacterVector param_names = get_param_names_cpp(family);
  
  // Get the appropriate density function from gkwdist
  Rcpp::Environment gkwdist = Rcpp::Environment::namespace_env("gkwdist");
  // Note: beta family uses "dbeta_" not "dbeta" in gkwdist
  std::string d_func_name = (family == "beta") ? "dbeta_" : ("d" + family);
  Rcpp::Function d_func = gkwdist[d_func_name];
  
  // Compute parameters for all observations
  Rcpp::List params_list(npar);
  
  // Minimum parameter value to avoid numerical issues
  const double MIN_PARAM = 1e-6;
  const double MAX_PARAM = 1e6;
  
  for (int p = 0; p < npar; p++) {
    std::string pname = Rcpp::as<std::string>(param_names[p]);
    
    arma::mat X_p = Rcpp::as<arma::mat>(X_list[pname]);
    int start = param_indices[p];
    int nc = n_coefs[p];
    arma::vec beta_p = theta_concat.subvec(start, start + nc - 1);
    arma::vec eta_p = X_p * beta_p;
    std::string link_p = Rcpp::as<std::string>(link_list[pname]);
    arma::vec param_p = apply_invlink(eta_p, link_p);
    
    // Clamp parameters to valid range (all parameters are positive)
    for (int i = 0; i < (int)param_p.n_elem; i++) {
      if (param_p(i) < MIN_PARAM) param_p(i) = MIN_PARAM;
      if (param_p(i) > MAX_PARAM) param_p(i) = MAX_PARAM;
      if (!R_finite(param_p(i))) param_p(i) = MIN_PARAM;
    }
    
    params_list[p] = Rcpp::wrap(param_p);
  }
  
  // Call density function with vectorized parameters
  Rcpp::NumericVector log_dens;
  
  if (family == "kw") {
    log_dens = d_func(
      Rcpp::Named("x") = Rcpp::wrap(y),
      Rcpp::Named("alpha") = params_list[0],
      Rcpp::Named("beta") = params_list[1],
      Rcpp::Named("log") = true
    );
  } else if (family == "beta") {
    log_dens = d_func(
      Rcpp::Named("x") = Rcpp::wrap(y),
      Rcpp::Named("gamma") = params_list[0],  // shape1
      Rcpp::Named("delta") = params_list[1],  // shape2
      Rcpp::Named("log") = true
    );
  } else if (family == "ekw") {
    log_dens = d_func(
      Rcpp::Named("x") = Rcpp::wrap(y),
      Rcpp::Named("alpha") = params_list[0],
      Rcpp::Named("beta") = params_list[1],
      Rcpp::Named("lambda") = params_list[2],
      Rcpp::Named("log") = true
    );
  } else if (family == "mc") {
    log_dens = d_func(
      Rcpp::Named("x") = Rcpp::wrap(y),
      Rcpp::Named("gamma") = params_list[0],
      Rcpp::Named("delta") = params_list[1],
      Rcpp::Named("lambda") = params_list[2],
      Rcpp::Named("log") = true
    );
  } else if (family == "bkw") {
    log_dens = d_func(
      Rcpp::Named("x") = Rcpp::wrap(y),
      Rcpp::Named("alpha") = params_list[0],
      Rcpp::Named("beta") = params_list[1],
      Rcpp::Named("gamma") = params_list[2],
      Rcpp::Named("delta") = params_list[3],
      Rcpp::Named("log") = true
    );
  } else if (family == "kkw") {
    log_dens = d_func(
      Rcpp::Named("x") = Rcpp::wrap(y),
      Rcpp::Named("alpha") = params_list[0],
      Rcpp::Named("beta") = params_list[1],
      Rcpp::Named("delta") = params_list[2],
      Rcpp::Named("lambda") = params_list[3],
      Rcpp::Named("log") = true
    );
  } else if (family == "gkw") {
    log_dens = d_func(
      Rcpp::Named("x") = Rcpp::wrap(y),
      Rcpp::Named("alpha") = params_list[0],
      Rcpp::Named("beta") = params_list[1],
      Rcpp::Named("gamma") = params_list[2],
      Rcpp::Named("delta") = params_list[3],
      Rcpp::Named("lambda") = params_list[4],
      Rcpp::Named("log") = true
    );
  } else {
    Rcpp::stop("Unknown family: " + family);
  }
  
  // Sum log-densities (return negative for minimization)
  double loglik = 0.0;
  for (int i = 0; i < n; i++) {
    if (!R_finite(log_dens[i])) {
      return R_PosInf;
    }
    loglik += log_dens[i];
  }
  
  return -loglik;
}

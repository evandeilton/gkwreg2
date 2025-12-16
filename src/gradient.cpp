// gkwreg2: Gradient Computation via Chain Rule
// =============================================================================
// Computes analytical gradient using chain rule:
// ∂ℓ/∂β = Xᵀ × (∂ℓ/∂θ × ∂θ/∂η)
// Uses gkwdist gr* functions for score vectors (∂ℓ/∂θ)
// =============================================================================

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <string>

// Forward declarations
arma::vec apply_invlink(const arma::vec& eta, const std::string& link_type);
arma::vec apply_dinvlink(const arma::vec& eta, const std::string& link_type);
int get_npar_family(const std::string& family);
Rcpp::CharacterVector get_param_names_cpp(const std::string& family);

// =============================================================================
// MAIN GRADIENT FUNCTION
// =============================================================================

//' Compute gradient of negative log-likelihood for gkwreg2
//'
//' Uses chain rule: ∂ℓ/∂β_p = Xᵀ_p × (∂ℓ/∂θ_p × ∂θ_p/∂η_p)
//'
//' @param theta_concat Concatenated regression coefficients
//' @param y Response vector (values in (0,1))
//' @param X_list List of design matrices (one per parameter)
//' @param family Distribution family name
//' @param link_list Named list of link functions per parameter
//' @param param_indices Integer vector with start indices for each parameter
//' @param n_coefs Integer vector with number of coefficients per parameter
//'
//' @return Gradient vector (same length as theta_concat)
//'
//' @keywords internal
// [[Rcpp::export]]
arma::vec gradient_gkwreg(
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
  int total_coefs = theta_concat.n_elem;
  Rcpp::CharacterVector param_names = get_param_names_cpp(family);
  
  // Get the appropriate gr* function from gkwdist
  Rcpp::Environment gkwdist = Rcpp::Environment::namespace_env("gkwdist");
  std::string gr_func_name = "gr" + family;
  Rcpp::Function gr_func = gkwdist[gr_func_name];
  
  // Initialize gradient vector
  arma::vec gradient = arma::zeros(total_coefs);
  
  // Store design matrices, linear predictors, and parameters
  std::vector<arma::mat> X_mats(npar);
  std::vector<arma::vec> etas(npar);
  std::vector<arma::vec> params(npar);
  std::vector<arma::vec> dinvlinks(npar);
  std::vector<std::string> links(npar);
  
  // Minimum parameter value to avoid numerical issues
  const double MIN_PARAM = 1e-6;
  const double MAX_PARAM = 1e6;
  
  for (int p = 0; p < npar; p++) {
    std::string pname = Rcpp::as<std::string>(param_names[p]);
    
    // Get design matrix
    X_mats[p] = Rcpp::as<arma::mat>(X_list[pname]);
    
    // Extract coefficients
    int start = param_indices[p];
    int nc = n_coefs[p];
    arma::vec beta_p = theta_concat.subvec(start, start + nc - 1);
    
    // Compute linear predictor
    etas[p] = X_mats[p] * beta_p;
    
    // Get link function
    links[p] = Rcpp::as<std::string>(link_list[pname]);
    
    // Apply inverse link to get parameters
    params[p] = apply_invlink(etas[p], links[p]);
    
    // Clamp parameters to valid range
    for (int i = 0; i < (int)params[p].n_elem; i++) {
      if (params[p](i) < MIN_PARAM) params[p](i) = MIN_PARAM;
      if (params[p](i) > MAX_PARAM) params[p](i) = MAX_PARAM;
      if (!R_finite(params[p](i))) params[p](i) = MIN_PARAM;
    }
    
    // Compute derivative of inverse link
    dinvlinks[p] = apply_dinvlink(etas[p], links[p]);
  }
  
  // Compute scores for each observation and accumulate gradient
  // The gr* functions return NEGATIVE gradient of log-likelihood
  // So we need to negate to get gradient of negative log-likelihood
  
  arma::mat scores(n, npar);  // n observations × npar parameters
  
  for (int i = 0; i < n; i++) {
    // Assemble parameter vector for observation i
    Rcpp::NumericVector par_i(npar);
    for (int p = 0; p < npar; p++) {
      par_i[p] = params[p](i);
    }
    
    // Data for observation i
    Rcpp::NumericVector y_i = Rcpp::NumericVector::create(y(i));
    
    // Call gr* function - returns negative gradient w.r.t. parameters
    Rcpp::NumericVector score_i = gr_func(par_i, y_i);
    
    // Check for numerical issues
    bool valid = true;
    for (int p = 0; p < npar; p++) {
      if (!R_finite(score_i[p])) {
        valid = false;
        break;
      }
    }
    
    if (valid) {
      for (int p = 0; p < npar; p++) {
        // gr* returns -∂logL/∂θ, which is exactly what we need for minimizing -logL
        scores(i, p) = score_i[p];
      }
    } else {
      // Use zeros for invalid observations
      for (int p = 0; p < npar; p++) {
        scores(i, p) = 0.0;
      }
    }
  }
  
  // Apply chain rule: ∂(-logL)/∂β_p = X_p' × (score_p ⊙ dinvlink_p)
  for (int p = 0; p < npar; p++) {
    int start = param_indices[p];
    int nc = n_coefs[p];
    
    // Element-wise product: score × derivative of inverse link
    arma::vec contribution = scores.col(p) % dinvlinks[p];
    
    // Matrix multiplication: X' × contribution
    arma::vec grad_p = X_mats[p].t() * contribution;
    
    // Store in gradient vector
    gradient.subvec(start, start + nc - 1) = grad_p;
  }
  
  return gradient;
}

// =============================================================================
// NUMERICAL GRADIENT (for validation)
// =============================================================================

//' Compute numerical gradient via central differences
//'
//' @param theta_concat Concatenated regression coefficients
//' @param y Response vector
//' @param X_list List of design matrices
//' @param family Distribution family
//' @param link_list Named list of link functions
//' @param param_indices Start indices
//' @param n_coefs Number of coefficients per parameter
//' @param eps Step size for finite differences
//'
//' @return Numerical gradient vector
//'
//' @keywords internal
// [[Rcpp::export]]
arma::vec gradient_numerical(
    const arma::vec& theta_concat,
    const arma::vec& y,
    const Rcpp::List& X_list,
    const std::string& family,
    const Rcpp::List& link_list,
    const Rcpp::IntegerVector& param_indices,
    const Rcpp::IntegerVector& n_coefs,
    double eps = 1e-6
) {
  int n_theta = theta_concat.n_elem;
  arma::vec grad(n_theta);
  
  // Forward declaration of objective function
  // We need to call it via R since it's in another translation unit
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("gkwreg2");
  Rcpp::Function obj_func = pkg["objective_gkwreg_vec"];
  
  for (int j = 0; j < n_theta; j++) {
    arma::vec theta_plus = theta_concat;
    arma::vec theta_minus = theta_concat;
    
    theta_plus(j) += eps;
    theta_minus(j) -= eps;
    
    double f_plus = Rcpp::as<double>(obj_func(
      Rcpp::Named("theta_concat") = Rcpp::wrap(theta_plus),
      Rcpp::Named("y") = Rcpp::wrap(y),
      Rcpp::Named("X_list") = X_list,
      Rcpp::Named("family") = family,
      Rcpp::Named("link_list") = link_list,
      Rcpp::Named("param_indices") = param_indices,
      Rcpp::Named("n_coefs") = n_coefs
    ));
    
    double f_minus = Rcpp::as<double>(obj_func(
      Rcpp::Named("theta_concat") = Rcpp::wrap(theta_minus),
      Rcpp::Named("y") = Rcpp::wrap(y),
      Rcpp::Named("X_list") = X_list,
      Rcpp::Named("family") = family,
      Rcpp::Named("link_list") = link_list,
      Rcpp::Named("param_indices") = param_indices,
      Rcpp::Named("n_coefs") = n_coefs
    ));
    
    grad(j) = (f_plus - f_minus) / (2.0 * eps);
  }
  
  return grad;
}

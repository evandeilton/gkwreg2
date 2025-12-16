// gkwreg2: Link Functions with Derivatives for Regression
// =============================================================================
// Implements link functions for transforming parameters in bounded regression.
// Each link has: link(theta), invlink(eta), dinvlink(eta) [derivative of inverse]
// =============================================================================

// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <cmath>

// Numerical stability constants
const double EPSILON = 1e-10;
const double LOG_EPSILON = -23.0258509299405;  // log(1e-10)
const double MAX_EXP = 700.0;

// =============================================================================
// UTILITY FUNCTIONS FOR NUMERICAL STABILITY
// =============================================================================

inline double safe_exp(double x) {
  if (x > MAX_EXP) return std::exp(MAX_EXP);
  if (x < -MAX_EXP) return 0.0;
  return std::exp(x);
}

inline double safe_log(double x) {
  if (x <= 0.0) return R_NegInf;
  if (x < EPSILON) return LOG_EPSILON;
  return std::log(x);
}

inline double clamp_prob(double p) {
  if (p < EPSILON) return EPSILON;
  if (p > 1.0 - EPSILON) return 1.0 - EPSILON;
  return p;
}

inline double clamp_positive(double x) {
  if (x < EPSILON) return EPSILON;
  return x;
}

// =============================================================================
// LINK FUNCTIONS FOR POSITIVE PARAMETERS (α, β, γ, λ)
// =============================================================================

// ----- Log Link -----
// θ = exp(η), so η = log(θ)

// [[Rcpp::export]]
double link_log(double theta) {
  return safe_log(theta);
}

// [[Rcpp::export]]
double invlink_log(double eta) {
  return safe_exp(eta);
}

// [[Rcpp::export]]
double dinvlink_log(double eta) {
  return safe_exp(eta);  // d/dη [exp(η)] = exp(η)
}

// ----- Sqrt Link -----
// θ = η², so η = sqrt(θ)

// [[Rcpp::export]]
double link_sqrt(double theta) {
  return std::sqrt(clamp_positive(theta));
}

// [[Rcpp::export]]
double invlink_sqrt(double eta) {
  return eta * eta;  // Always positive
}

// [[Rcpp::export]]
double dinvlink_sqrt(double eta) {
  return 2.0 * eta;  // d/dη [η²] = 2η
}

// ----- Inverse Link -----
// θ = 1/η, so η = 1/θ

// [[Rcpp::export]]
double link_inverse(double theta) {
  return 1.0 / clamp_positive(theta);
}

// [[Rcpp::export]]
double invlink_inverse(double eta) {
  if (std::abs(eta) < EPSILON) {
    return (eta > 0) ? 1.0 / EPSILON : -1.0 / EPSILON;
  }
  return 1.0 / eta;
}

// [[Rcpp::export]]
double dinvlink_inverse(double eta) {
  double eta_sq = eta * eta;
  if (eta_sq < EPSILON) eta_sq = EPSILON;
  return -1.0 / eta_sq;  // d/dη [1/η] = -1/η²
}

// ----- Identity Link -----
// θ = η, so η = θ

// [[Rcpp::export]]
double link_identity(double theta) {
  return theta;
}

// [[Rcpp::export]]
double invlink_identity(double eta) {
  return eta;
}

// [[Rcpp::export]]
double dinvlink_identity(double eta) {
  return 1.0;  // d/dη [η] = 1
}

// =============================================================================
// LINK FUNCTIONS FOR PROBABILITY PARAMETERS (δ ∈ (0,1))
// =============================================================================

// ----- Logit Link -----
// θ = 1/(1+exp(-η)), so η = log(θ/(1-θ))

// [[Rcpp::export]]
double link_logit(double theta) {
  double p = clamp_prob(theta);
  return safe_log(p / (1.0 - p));
}

// [[Rcpp::export]]
double invlink_logit(double eta) {
  if (eta > MAX_EXP) return 1.0 - EPSILON;
  if (eta < -MAX_EXP) return EPSILON;
  return 1.0 / (1.0 + std::exp(-eta));
}

// [[Rcpp::export]]
double dinvlink_logit(double eta) {
  double p = invlink_logit(eta);
  return p * (1.0 - p);  // d/dη [logistic(η)] = p(1-p)
}

// ----- Probit Link -----
// θ = Φ(η), so η = Φ⁻¹(θ)

// [[Rcpp::export]]
double link_probit(double theta) {
  double p = clamp_prob(theta);
  return R::qnorm(p, 0.0, 1.0, 1, 0);
}

// [[Rcpp::export]]
double invlink_probit(double eta) {
  return R::pnorm(eta, 0.0, 1.0, 1, 0);
}

// [[Rcpp::export]]
double dinvlink_probit(double eta) {
  return R::dnorm(eta, 0.0, 1.0, 0);  // d/dη [Φ(η)] = φ(η)
}

// ----- Complementary Log-Log Link -----
// θ = 1 - exp(-exp(η)), so η = log(-log(1-θ))

// [[Rcpp::export]]
double link_cloglog(double theta) {
  double p = clamp_prob(theta);
  return safe_log(-safe_log(1.0 - p));
}

// [[Rcpp::export]]
double invlink_cloglog(double eta) {
  if (eta > MAX_EXP) return 1.0 - EPSILON;
  if (eta < -MAX_EXP) return EPSILON;
  return 1.0 - std::exp(-std::exp(eta));
}

// [[Rcpp::export]]
double dinvlink_cloglog(double eta) {
  if (eta > MAX_EXP) return EPSILON;
  if (eta < -MAX_EXP) return EPSILON;
  double exp_eta = std::exp(eta);
  return exp_eta * std::exp(-exp_eta);  // d/dη [1 - exp(-exp(η))]
}

// ----- Cauchy (Cauchit) Link -----
// θ = atan(η)/π + 0.5, so η = tan(π(θ - 0.5))

// [[Rcpp::export]]
double link_cauchy(double theta) {
  double p = clamp_prob(theta);
  return std::tan(M_PI * (p - 0.5));
}

// [[Rcpp::export]]
double invlink_cauchy(double eta) {
  double result = std::atan(eta) / M_PI + 0.5;
  return clamp_prob(result);
}

// [[Rcpp::export]]
double dinvlink_cauchy(double eta) {
  return 1.0 / (M_PI * (1.0 + eta * eta));  // d/dη [atan(η)/π + 0.5]
}

// =============================================================================
// VECTORIZED VERSIONS
// =============================================================================

// [[Rcpp::export]]
arma::vec apply_invlink(const arma::vec& eta, const std::string& link_type) {
  int n = eta.n_elem;
  arma::vec result(n);
  
  for (int i = 0; i < n; i++) {
    if (link_type == "log") {
      result(i) = invlink_log(eta(i));
    } else if (link_type == "sqrt") {
      result(i) = invlink_sqrt(eta(i));
    } else if (link_type == "inverse") {
      result(i) = invlink_inverse(eta(i));
    } else if (link_type == "identity") {
      result(i) = invlink_identity(eta(i));
    } else if (link_type == "logit") {
      result(i) = invlink_logit(eta(i));
    } else if (link_type == "probit") {
      result(i) = invlink_probit(eta(i));
    } else if (link_type == "cloglog") {
      result(i) = invlink_cloglog(eta(i));
    } else if (link_type == "cauchy") {
      result(i) = invlink_cauchy(eta(i));
    } else {
      Rcpp::stop("Unknown link type: " + link_type);
    }
  }
  
  return result;
}

// [[Rcpp::export]]
arma::vec apply_dinvlink(const arma::vec& eta, const std::string& link_type) {
  int n = eta.n_elem;
  arma::vec result(n);
  
  for (int i = 0; i < n; i++) {
    if (link_type == "log") {
      result(i) = dinvlink_log(eta(i));
    } else if (link_type == "sqrt") {
      result(i) = dinvlink_sqrt(eta(i));
    } else if (link_type == "inverse") {
      result(i) = dinvlink_inverse(eta(i));
    } else if (link_type == "identity") {
      result(i) = dinvlink_identity(eta(i));
    } else if (link_type == "logit") {
      result(i) = dinvlink_logit(eta(i));
    } else if (link_type == "probit") {
      result(i) = dinvlink_probit(eta(i));
    } else if (link_type == "cloglog") {
      result(i) = dinvlink_cloglog(eta(i));
    } else if (link_type == "cauchy") {
      result(i) = dinvlink_cauchy(eta(i));
    } else {
      Rcpp::stop("Unknown link type: " + link_type);
    }
  }
  
  return result;
}

// [[Rcpp::export]]
arma::vec apply_link(const arma::vec& theta, const std::string& link_type) {
  int n = theta.n_elem;
  arma::vec result(n);
  
  for (int i = 0; i < n; i++) {
    if (link_type == "log") {
      result(i) = link_log(theta(i));
    } else if (link_type == "sqrt") {
      result(i) = link_sqrt(theta(i));
    } else if (link_type == "inverse") {
      result(i) = link_inverse(theta(i));
    } else if (link_type == "identity") {
      result(i) = link_identity(theta(i));
    } else if (link_type == "logit") {
      result(i) = link_logit(theta(i));
    } else if (link_type == "probit") {
      result(i) = link_probit(theta(i));
    } else if (link_type == "cloglog") {
      result(i) = link_cloglog(theta(i));
    } else if (link_type == "cauchy") {
      result(i) = link_cauchy(theta(i));
    } else {
      Rcpp::stop("Unknown link type: " + link_type);
    }
  }
  
  return result;
}

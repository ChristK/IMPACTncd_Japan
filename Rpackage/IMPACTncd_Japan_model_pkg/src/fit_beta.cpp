/**
 * @file fit_beta.cpp
 * @brief Fast and robust beta distribution parameter estimation from quantiles
 *
 * This file implements optimized C++ versions of fit_beta and fit_beta_vec,
 * providing significant speed improvements over the R implementation while
 * adding safety features for numerical stability.
 *
 * Key Features:
 * - Nelder-Mead simplex optimization (no gradient needed, robust for 2D)
 * - Multiple restart strategy with intelligent initial value selection
 * - Numerical safeguards against NaN/Inf
 * - Early termination when good fit is found
 * - OpenMP parallelization for vectorized version
 */

// [[Rcpp::depends(dqrng, BH, sitmo)]]
// [[Rcpp::plugins(cpp17)]]
// [[Rcpp::plugins(openmp)]]

#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>
#include <vector>
#include <random>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;

// ===============================================================================
// NUMERICAL CONSTANTS AND UTILITIES
// ===============================================================================

constexpr double EPSILON = 1e-15;
constexpr int MAX_ERROR_INDICES_TO_SHOW = 10;  // Max indices to show in error messages
constexpr double LOG_EPSILON = -34.5388; // log(1e-15)
constexpr double MAX_PARAM = 1e8;        // Maximum allowed parameter value
constexpr double MIN_PARAM = 1e-8;       // Minimum allowed parameter value

/**
 * @brief Safe logit transformation with bounds protection
 */
inline double safe_logit(double p) {
    p = std::clamp(p, EPSILON, 1.0 - EPSILON);
    return std::log(p / (1.0 - p));
}

/**
 * @brief Check if quantile values are monotonically increasing with probabilities
 *
 * For a valid distribution, if p[i] < p[j] then the corresponding quantile
 * x[i] < x[j]. This function checks this relationship.
 *
 * @param x Quantile values
 * @param p Corresponding probabilities
 * @return true if quantiles are properly ordered, false otherwise
 */
bool check_quantile_monotonicity(const std::vector<double>& x,
                                  const std::vector<double>& p) {
    int n = static_cast<int>(x.size());
    if (n < 2) return true;

    // Create index array and sort by probability
    std::vector<int> idx(n);
    for (int i = 0; i < n; i++) idx[i] = i;
    std::sort(idx.begin(), idx.end(), [&p](int a, int b) {
        return p[a] < p[b];
    });

    // Check that quantiles are monotonically increasing when sorted by probability
    for (int i = 1; i < n; i++) {
        // Allow small tolerance for nearly equal values
        if (x[idx[i]] < x[idx[i-1]] - EPSILON) {
            return false;
        }
    }
    return true;
}

/**
 * @brief Incomplete beta function using continued fraction expansion
 *
 * This is a numerical approximation of the regularized incomplete beta function.
 * Uses the continued fraction method which is accurate and efficient.
 */
double betacf(double a, double b, double x) {
    constexpr int MAXIT = 200;
    constexpr double EPS = 3.0e-12;
    constexpr double FPMIN = 1.0e-30;

    double qab = a + b;
    double qap = a + 1.0;
    double qam = a - 1.0;
    double c = 1.0;
    double d = 1.0 - qab * x / qap;
    if (std::abs(d) < FPMIN) d = FPMIN;
    d = 1.0 / d;
    double h = d;

    for (int m = 1; m <= MAXIT; m++) {
        int m2 = 2 * m;
        double aa = m * (b - m) * x / ((qam + m2) * (a + m2));
        d = 1.0 + aa * d;
        if (std::abs(d) < FPMIN) d = FPMIN;
        c = 1.0 + aa / c;
        if (std::abs(c) < FPMIN) c = FPMIN;
        d = 1.0 / d;
        h *= d * c;
        aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
        d = 1.0 + aa * d;
        if (std::abs(d) < FPMIN) d = FPMIN;
        c = 1.0 + aa / c;
        if (std::abs(c) < FPMIN) c = FPMIN;
        d = 1.0 / d;
        double del = d * c;
        h *= del;
        if (std::abs(del - 1.0) < EPS) break;
    }
    return h;
}

/**
 * @brief Log of the beta function using lgamma
 */
inline double lbeta(double a, double b) {
    return std::lgamma(a) + std::lgamma(b) - std::lgamma(a + b);
}

/**
 * @brief Regularized incomplete beta function I_x(a,b)
 *
 * This is equivalent to R's pbeta(x, a, b)
 */
double pbeta_cpp(double x, double a, double b) {
    if (x <= 0.0) return 0.0;
    if (x >= 1.0) return 1.0;
    if (!std::isfinite(a) || !std::isfinite(b) || a <= 0 || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }

    double bt;
    if (x == 0.0 || x == 1.0) {
        bt = 0.0;
    } else {
        bt = std::exp(std::lgamma(a + b) - std::lgamma(a) - std::lgamma(b) +
                      a * std::log(x) + b * std::log(1.0 - x));
    }

    // Use symmetry for numerical stability
    if (x < (a + 1.0) / (a + b + 2.0)) {
        return bt * betacf(a, b, x) / a;
    } else {
        return 1.0 - bt * betacf(b, a, 1.0 - x) / b;
    }
}

/**
 * @brief Inverse beta CDF using bisection with Newton refinement
 *
 * This is equivalent to R's qbeta(p, a, b)
 */
double qbeta_cpp(double p, double a, double b) {
    if (p <= 0.0) return 0.0;
    if (p >= 1.0) return 1.0;
    if (!std::isfinite(a) || !std::isfinite(b) || a <= 0 || b <= 0) {
        return std::numeric_limits<double>::quiet_NaN();
    }

    // Initial guess using approximation
    double x;
    if (a >= 1.0 && b >= 1.0) {
        // Use normal approximation for initial guess
        double mean = a / (a + b);
        double var = (a * b) / ((a + b) * (a + b) * (a + b + 1.0));
        // Simple linear interpolation between 0 and 1
        x = mean + (p - 0.5) * std::sqrt(var) * 2.0;
        x = std::clamp(x, 0.01, 0.99);
    } else {
        x = 0.5;
    }

    // Bisection with Newton-Raphson refinement
    double lo = 0.0, hi = 1.0;
    constexpr int MAX_ITER = 100;
    constexpr double TOL = 1e-10;

    for (int iter = 0; iter < MAX_ITER; iter++) {
        double cdf = pbeta_cpp(x, a, b);
        double diff = cdf - p;

        if (std::abs(diff) < TOL) break;

        // Update bounds for bisection
        if (diff > 0) {
            hi = x;
        } else {
            lo = x;
        }

        // Newton step using PDF as derivative of CDF
        // PDF = x^(a-1) * (1-x)^(b-1) / B(a,b)
        double pdf = std::exp((a - 1.0) * std::log(x) + (b - 1.0) * std::log(1.0 - x) - lbeta(a, b));

        if (pdf > EPSILON) {
            double newton_step = diff / pdf;
            double x_new = x - newton_step;

            // Accept Newton step if it's within bounds and reasonable
            if (x_new > lo && x_new < hi && std::abs(newton_step) < 0.5) {
                x = x_new;
            } else {
                // Fall back to bisection
                x = (lo + hi) / 2.0;
            }
        } else {
            x = (lo + hi) / 2.0;
        }
    }

    return x;
}

// ===============================================================================
// NELDER-MEAD SIMPLEX OPTIMIZATION
// ===============================================================================

/**
 * @brief Objective function for beta fitting
 *
 * Computes weighted sum of squared errors between target logit-quantiles
 * and logit of the beta CDF at the given x values.
 *
 * @param log_alpha Log of alpha parameter
 * @param log_beta Log of beta parameter
 * @param x Vector of quantile values
 * @param target_logit_p Vector of logit-transformed target probabilities
 * @param weights Importance weights for each quantile
 * @return Sum of weighted squared errors (lower is better)
 */
double objective(double log_alpha, double log_beta,
                 const std::vector<double>& x,
                 const std::vector<double>& target_logit_p,
                 const std::vector<double>& weights) {
    double alpha = std::exp(log_alpha);
    double beta = std::exp(log_beta);

    // Clamp parameters to reasonable range
    alpha = std::clamp(alpha, MIN_PARAM, MAX_PARAM);
    beta = std::clamp(beta, MIN_PARAM, MAX_PARAM);

    double total_weight = 0.0;
    double sse = 0.0;

    for (size_t i = 0; i < x.size(); i++) {
        double p = pbeta_cpp(x[i], alpha, beta);
        double fit_logit = safe_logit(p);
        double diff = fit_logit - target_logit_p[i];
        sse += weights[i] * diff * diff;
        total_weight += weights[i];
    }

    if (total_weight > 0) {
        sse /= total_weight;
    }

    // Penalize extreme parameters
    if (alpha > 1e6 || beta > 1e6) {
        sse += (alpha + beta) * 1e-10;
    }

    return std::isfinite(sse) ? sse : std::numeric_limits<double>::max();
}

/**
 * @brief 2D Nelder-Mead simplex optimization
 *
 * Optimized for the 2-parameter case (alpha, beta) with intelligent
 * reflection, expansion, and contraction steps.
 *
 * @param x0 Initial log_alpha
 * @param y0 Initial log_beta
 * @param x_vals Quantile values
 * @param target_logit_p Target logit probabilities
 * @param weights Importance weights
 * @param max_iter Maximum iterations
 * @param tol Convergence tolerance
 * @return Pair of (log_alpha, log_beta) at optimum
 */
std::pair<double, double> nelder_mead_2d(
    double x0, double y0,
    const std::vector<double>& x_vals,
    const std::vector<double>& target_logit_p,
    const std::vector<double>& weights,
    int max_iter = 500,
    double tol = 1e-10
) {
    // Simplex vertices: 3 points in 2D
    double p[3][2] = {
        {x0, y0},
        {x0 + 1.0, y0},
        {x0, y0 + 1.0}
    };
    double f[3];

    // Nelder-Mead parameters
    constexpr double ALPHA = 1.0;   // Reflection
    constexpr double GAMMA = 2.0;   // Expansion
    constexpr double RHO = 0.5;     // Contraction
    constexpr double SIGMA = 0.5;   // Shrink

    // Evaluate initial simplex
    for (int i = 0; i < 3; i++) {
        f[i] = objective(p[i][0], p[i][1], x_vals, target_logit_p, weights);
    }

    for (int iter = 0; iter < max_iter; iter++) {
        // Sort vertices by function value (ascending)
        int order[3] = {0, 1, 2};
        if (f[order[0]] > f[order[1]]) std::swap(order[0], order[1]);
        if (f[order[1]] > f[order[2]]) std::swap(order[1], order[2]);
        if (f[order[0]] > f[order[1]]) std::swap(order[0], order[1]);

        int best = order[0];
        int second_worst = order[1];
        int worst = order[2];

        // Check convergence
        double range = f[worst] - f[best];
        if (range < tol && iter > 10) {
            return {p[best][0], p[best][1]};
        }

        // Centroid of the best face (excluding worst point)
        double centroid[2] = {
            (p[best][0] + p[second_worst][0]) / 2.0,
            (p[best][1] + p[second_worst][1]) / 2.0
        };

        // Reflection
        double reflected[2] = {
            centroid[0] + ALPHA * (centroid[0] - p[worst][0]),
            centroid[1] + ALPHA * (centroid[1] - p[worst][1])
        };
        double f_reflected = objective(reflected[0], reflected[1], x_vals, target_logit_p, weights);

        if (f_reflected < f[second_worst] && f_reflected >= f[best]) {
            // Accept reflection
            p[worst][0] = reflected[0];
            p[worst][1] = reflected[1];
            f[worst] = f_reflected;
            continue;
        }

        if (f_reflected < f[best]) {
            // Try expansion
            double expanded[2] = {
                centroid[0] + GAMMA * (reflected[0] - centroid[0]),
                centroid[1] + GAMMA * (reflected[1] - centroid[1])
            };
            double f_expanded = objective(expanded[0], expanded[1], x_vals, target_logit_p, weights);

            if (f_expanded < f_reflected) {
                p[worst][0] = expanded[0];
                p[worst][1] = expanded[1];
                f[worst] = f_expanded;
            } else {
                p[worst][0] = reflected[0];
                p[worst][1] = reflected[1];
                f[worst] = f_reflected;
            }
            continue;
        }

        // Contraction
        double contracted[2];
        double f_contracted;

        if (f_reflected < f[worst]) {
            // Outside contraction
            contracted[0] = centroid[0] + RHO * (reflected[0] - centroid[0]);
            contracted[1] = centroid[1] + RHO * (reflected[1] - centroid[1]);
            f_contracted = objective(contracted[0], contracted[1], x_vals, target_logit_p, weights);

            if (f_contracted <= f_reflected) {
                p[worst][0] = contracted[0];
                p[worst][1] = contracted[1];
                f[worst] = f_contracted;
                continue;
            }
        } else {
            // Inside contraction
            contracted[0] = centroid[0] + RHO * (p[worst][0] - centroid[0]);
            contracted[1] = centroid[1] + RHO * (p[worst][1] - centroid[1]);
            f_contracted = objective(contracted[0], contracted[1], x_vals, target_logit_p, weights);

            if (f_contracted < f[worst]) {
                p[worst][0] = contracted[0];
                p[worst][1] = contracted[1];
                f[worst] = f_contracted;
                continue;
            }
        }

        // Shrink
        for (int i = 0; i < 3; i++) {
            if (i != best) {
                p[i][0] = p[best][0] + SIGMA * (p[i][0] - p[best][0]);
                p[i][1] = p[best][1] + SIGMA * (p[i][1] - p[best][1]);
                f[i] = objective(p[i][0], p[i][1], x_vals, target_logit_p, weights);
            }
        }
    }

    // Return best found
    int best = 0;
    if (f[1] < f[best]) best = 1;
    if (f[2] < f[best]) best = 2;
    return {p[best][0], p[best][1]};
}

// ===============================================================================
// MAIN FIT_BETA FUNCTION
// ===============================================================================

/**
 * @brief Fit beta distribution parameters from quantiles (C++ implementation)
 *
 * This is a faster and safer implementation of the R fit_beta function.
 * Uses Nelder-Mead optimization with multiple restarts.
 *
 * @param x Quantile values (e.g., c(0.01, 0.005, 0.5))
 * @param x_p Corresponding probabilities (e.g., c(0.5, 0.025, 0.975))
 * @param tolerance Relative error tolerance for fit quality
 * @param max_restarts Maximum number of optimization restarts
 * @param verbose Print progress information
 * @return NumericVector with shape1 (alpha) and shape2 (beta), or c(NA, NA) on failure
 *
 * @export
 */
// [[Rcpp::export]]
NumericVector fit_beta_cpp(
    NumericVector x,
    NumericVector x_p,
    double tolerance = 0.01,
    int max_restarts = 1000,
    bool verbose = false
) {
    int n = x.size();

    // Input validation
    if (n != x_p.size()) {
        stop("x and x_p must have the same length");
    }
    if (n < 2) {
        stop("x must have at least length 2");
    }

    // Check for all-same values (early escape)
    bool all_same = true;
    for (int i = 1; i < n; i++) {
        if (std::abs(x[i] - x[0]) > EPSILON) {
            all_same = false;
            break;
        }
    }
    if (all_same) {
        return NumericVector::create(1.0, 1.0);
    }

    // Check for invalid values
    for (int i = 0; i < n; i++) {
        if (!std::isfinite(x[i]) || x[i] <= 0 || x[i] >= 1) {
            if (verbose) Rcpp::Rcout << "Invalid x value at index " << i << ": " << x[i] << std::endl;
            return NumericVector::create(NA_REAL, NA_REAL);
        }
        if (!std::isfinite(x_p[i]) || x_p[i] <= 0 || x_p[i] >= 1) {
            if (verbose) Rcpp::Rcout << "Invalid x_p value at index " << i << ": " << x_p[i] << std::endl;
            return NumericVector::create(NA_REAL, NA_REAL);
        }
    }

    // Check quantile monotonicity: quantiles must increase with probability
    // For a valid distribution, if p[i] < p[j] then x[i] < x[j]
    {
        std::vector<double> x_vec(x.begin(), x.end());
        std::vector<double> p_vec(x_p.begin(), x_p.end());
        if (!check_quantile_monotonicity(x_vec, p_vec)) {
            // Build informative error message
            std::vector<int> idx(n);
            for (int i = 0; i < n; i++) idx[i] = i;
            std::sort(idx.begin(), idx.end(), [&p_vec](int a, int b) {
                return p_vec[a] < p_vec[b];
            });

            std::string err_msg = "Quantile values are not monotonically increasing with probability.\n";
            err_msg += "For a valid beta distribution fit, when probabilities are sorted in increasing order,\n";
            err_msg += "the corresponding quantile values must also be in increasing order.\n\n";
            err_msg += "Sorted by probability:\n";
            for (int i = 0; i < n; i++) {
                err_msg += "  p=" + std::to_string(p_vec[idx[i]]) +
                           " -> x=" + std::to_string(x_vec[idx[i]]);
                if (i > 0 && x_vec[idx[i]] < x_vec[idx[i-1]]) {
                    err_msg += " <- INVERTED (should be > " + std::to_string(x_vec[idx[i-1]]) + ")";
                }
                err_msg += "\n";
            }
            stop(err_msg);
        }
    }

    // Convert to std::vector and compute logit-transformed probabilities
    std::vector<double> x_vals(x.begin(), x.end());
    std::vector<double> target_logit_p(n);
    std::vector<double> weights(n, 1.0);

    for (int i = 0; i < n; i++) {
        target_logit_p[i] = safe_logit(x_p[i]);
    }

    // Random number generator for restarts
    std::mt19937 rng(42); // Fixed seed for reproducibility
    std::uniform_real_distribution<double> log_alpha_dist(-2.0, 6.0);  // exp range: ~0.1 to ~400
    std::uniform_real_distribution<double> log_beta_dist(-2.0, 10.0);  // exp range: ~0.1 to ~22000

    double best_alpha = NA_REAL;
    double best_beta = NA_REAL;
    double best_error = std::numeric_limits<double>::max();

    // Initial guesses based on method of moments
    double mean_x = 0.0;
    double var_x = 0.0;
    for (int i = 0; i < n; i++) {
        mean_x += x[i] * x_p[i];
    }
    mean_x /= n;
    for (int i = 0; i < n; i++) {
        var_x += (x[i] - mean_x) * (x[i] - mean_x);
    }
    var_x /= n;

    // Method of moments initial estimate
    double init_alpha = mean_x * (mean_x * (1 - mean_x) / std::max(var_x, 1e-6) - 1);
    double init_beta = (1 - mean_x) * (mean_x * (1 - mean_x) / std::max(var_x, 1e-6) - 1);
    init_alpha = std::clamp(init_alpha, 0.1, 100.0);
    init_beta = std::clamp(init_beta, 0.1, 1000.0);

    std::vector<std::pair<double, double>> initial_guesses = {
        {std::log(init_alpha), std::log(init_beta)},
        {std::log(1.0), std::log(100.0)},
        {std::log(0.5), std::log(1000.0)},
        {std::log(2.0), std::log(200.0)},
        {std::log(0.1), std::log(10.0)}
    };

    int restart = 0;
    int weight_reduction_stage = 0;

    while (restart < max_restarts) {
        // Choose initial guess
        double log_alpha_init, log_beta_init;
        if (restart < static_cast<int>(initial_guesses.size())) {
            log_alpha_init = initial_guesses[restart].first;
            log_beta_init = initial_guesses[restart].second;
        } else {
            log_alpha_init = log_alpha_dist(rng);
            log_beta_init = log_beta_dist(rng);
        }

        // Run optimization
        auto [opt_log_alpha, opt_log_beta] = nelder_mead_2d(
            log_alpha_init, log_beta_init,
            x_vals, target_logit_p, weights,
            500, 1e-12
        );

        double alpha = std::exp(opt_log_alpha);
        double beta = std::exp(opt_log_beta);

        // Check fit quality using relative error
        bool good_fit = true;
        double max_rel_error = 0.0;

        for (int i = 0; i < n; i++) {
            double fitted_q = qbeta_cpp(x_p[i], alpha, beta);
            if (!std::isfinite(fitted_q) || fitted_q <= 0) {
                good_fit = false;
                break;
            }
            double rel_error = std::abs(x[i] / fitted_q - 1.0);
            max_rel_error = std::max(max_rel_error, rel_error);
            if (rel_error > tolerance * (1.0 + (1.0 - weights[i]))) {
                good_fit = false;
            }
        }

        if (verbose && restart % 100 == 0) {
            Rcpp::Rcout << "Restart " << restart << ": alpha=" << alpha
                       << ", beta=" << beta << ", max_rel_error=" << max_rel_error << std::endl;
        }

        // Track best solution
        if (max_rel_error < best_error) {
            best_error = max_rel_error;
            best_alpha = alpha;
            best_beta = beta;
        }

        if (good_fit) {
            if (verbose) {
                Rcpp::Rcout << "Good fit found at restart " << restart
                           << ": alpha=" << alpha << ", beta=" << beta << std::endl;
            }
            return NumericVector::create(
                Named("shape1") = alpha,
                Named("shape2") = beta
            );
        }

        restart++;

        // Progressively reduce weights on tail quantiles if struggling
        if (n > 2) {
            if (restart == max_restarts / 5 && weight_reduction_stage == 0) {
                weights[n-1] = 0.9;
                weight_reduction_stage = 1;
            } else if (restart == 2 * max_restarts / 5 && weight_reduction_stage == 1) {
                weights[n-1] = 0.8;
                weight_reduction_stage = 2;
            } else if (restart == 3 * max_restarts / 5 && weight_reduction_stage == 2) {
                weights[n-1] = 0.7;
                weight_reduction_stage = 3;
            } else if (restart == 4 * max_restarts / 5 && weight_reduction_stage == 3) {
                // Drop the last quantile entirely
                x_vals.pop_back();
                target_logit_p.pop_back();
                weights.pop_back();
                n--;
                weight_reduction_stage = 4;
                if (verbose) {
                    Rcpp::Rcout << "Dropped last quantile at restart " << restart << std::endl;
                }
            }
        }
    }

    // Return best found solution if it's reasonably good, else NA
    if (best_error < tolerance * 5) {
        if (verbose) {
            Rcpp::Rcout << "Returning best fit with error " << best_error
                       << ": alpha=" << best_alpha << ", beta=" << best_beta << std::endl;
        }
        warning("Beta fit did not meet tolerance but returning best approximation (error=" +
                std::to_string(best_error) + ")");
        return NumericVector::create(
            Named("shape1") = best_alpha,
            Named("shape2") = best_beta
        );
    }

    warning("Beta is not a good fit for these data");
    return NumericVector::create(NA_REAL, NA_REAL);
}

// ===============================================================================
// PURE C++ FIT_BETA (for use inside OpenMP parallel regions)
// ===============================================================================

/**
 * @brief Pure C++ implementation of fit_beta (no Rcpp objects)
 *
 * This function is safe to call from OpenMP parallel regions because it
 * only uses std::vector and primitive types, avoiding R's memory management.
 *
 * @param x_vals Quantile values
 * @param p_vals Corresponding probabilities
 * @param tolerance Relative error tolerance
 * @param max_restarts Maximum restarts
 * @return Pair of (shape1, shape2) - returns (NaN, NaN) on failure
 */
std::pair<double, double> fit_beta_pure_cpp(
    const std::vector<double>& x_vals,
    const std::vector<double>& p_vals,
    double tolerance,
    int max_restarts
) {
    int n = static_cast<int>(x_vals.size());

    // Input validation
    if (n != static_cast<int>(p_vals.size()) || n < 2) {
        return {std::numeric_limits<double>::quiet_NaN(),
                std::numeric_limits<double>::quiet_NaN()};
    }

    // Check for all-same values (early escape)
    bool all_same = true;
    for (int i = 1; i < n; i++) {
        if (std::abs(x_vals[i] - x_vals[0]) > EPSILON) {
            all_same = false;
            break;
        }
    }
    if (all_same) {
        return {1.0, 1.0};
    }

    // Check for invalid values
    for (int i = 0; i < n; i++) {
        if (!std::isfinite(x_vals[i]) || x_vals[i] <= 0 || x_vals[i] >= 1) {
            return {std::numeric_limits<double>::quiet_NaN(),
                    std::numeric_limits<double>::quiet_NaN()};
        }
        if (!std::isfinite(p_vals[i]) || p_vals[i] <= 0 || p_vals[i] >= 1) {
            return {std::numeric_limits<double>::quiet_NaN(),
                    std::numeric_limits<double>::quiet_NaN()};
        }
    }

    // Compute logit-transformed probabilities
    std::vector<double> target_logit_p(n);
    std::vector<double> weights(n, 1.0);
    std::vector<double> x_working = x_vals;  // Modifiable copy
    std::vector<double> p_working = p_vals;

    for (int i = 0; i < n; i++) {
        target_logit_p[i] = safe_logit(p_vals[i]);
    }

    // Random number generator (thread-local seed based on data)
    unsigned int seed = 42;
    for (int i = 0; i < n; i++) {
        seed ^= static_cast<unsigned int>(x_vals[i] * 1e6);
    }
    std::mt19937 rng(seed);
    std::uniform_real_distribution<double> log_alpha_dist(-2.0, 6.0);
    std::uniform_real_distribution<double> log_beta_dist(-2.0, 10.0);

    double best_alpha = std::numeric_limits<double>::quiet_NaN();
    double best_beta = std::numeric_limits<double>::quiet_NaN();
    double best_error = std::numeric_limits<double>::max();

    // Initial guesses based on method of moments
    double mean_x = 0.0;
    for (int i = 0; i < n; i++) {
        mean_x += x_vals[i];
    }
    mean_x /= n;

    double var_x = 0.0;
    for (int i = 0; i < n; i++) {
        var_x += (x_vals[i] - mean_x) * (x_vals[i] - mean_x);
    }
    var_x /= n;

    double init_alpha = mean_x * (mean_x * (1 - mean_x) / std::max(var_x, 1e-6) - 1);
    double init_beta = (1 - mean_x) * (mean_x * (1 - mean_x) / std::max(var_x, 1e-6) - 1);
    init_alpha = std::clamp(init_alpha, 0.1, 100.0);
    init_beta = std::clamp(init_beta, 0.1, 1000.0);

    std::vector<std::pair<double, double>> initial_guesses = {
        {std::log(init_alpha), std::log(init_beta)},
        {std::log(1.0), std::log(100.0)},
        {std::log(0.5), std::log(1000.0)},
        {std::log(2.0), std::log(200.0)},
        {std::log(0.1), std::log(10.0)}
    };

    int restart = 0;
    int weight_reduction_stage = 0;
    int current_n = n;

    while (restart < max_restarts) {
        double log_alpha_init, log_beta_init;
        if (restart < static_cast<int>(initial_guesses.size())) {
            log_alpha_init = initial_guesses[restart].first;
            log_beta_init = initial_guesses[restart].second;
        } else {
            log_alpha_init = log_alpha_dist(rng);
            log_beta_init = log_beta_dist(rng);
        }

        auto [opt_log_alpha, opt_log_beta] = nelder_mead_2d(
            log_alpha_init, log_beta_init,
            x_working, target_logit_p, weights,
            500, 1e-12
        );

        double alpha = std::exp(opt_log_alpha);
        double beta = std::exp(opt_log_beta);

        bool good_fit = true;
        double max_rel_error = 0.0;

        for (int i = 0; i < current_n; i++) {
            double fitted_q = qbeta_cpp(p_working[i], alpha, beta);
            if (!std::isfinite(fitted_q) || fitted_q <= 0) {
                good_fit = false;
                break;
            }
            double rel_error = std::abs(x_working[i] / fitted_q - 1.0);
            max_rel_error = std::max(max_rel_error, rel_error);
            if (rel_error > tolerance * (1.0 + (1.0 - weights[i]))) {
                good_fit = false;
            }
        }

        if (max_rel_error < best_error) {
            best_error = max_rel_error;
            best_alpha = alpha;
            best_beta = beta;
        }

        if (good_fit) {
            return {alpha, beta};
        }

        restart++;

        // Progressive weight reduction
        if (current_n > 2) {
            if (restart == max_restarts / 5 && weight_reduction_stage == 0) {
                weights[current_n-1] = 0.9;
                weight_reduction_stage = 1;
            } else if (restart == 2 * max_restarts / 5 && weight_reduction_stage == 1) {
                weights[current_n-1] = 0.8;
                weight_reduction_stage = 2;
            } else if (restart == 3 * max_restarts / 5 && weight_reduction_stage == 2) {
                weights[current_n-1] = 0.7;
                weight_reduction_stage = 3;
            } else if (restart == 4 * max_restarts / 5 && weight_reduction_stage == 3) {
                x_working.pop_back();
                target_logit_p.pop_back();
                p_working.pop_back();
                weights.pop_back();
                current_n--;
                weight_reduction_stage = 4;
            }
        }
    }

    // Return best found solution if reasonably good
    if (best_error < tolerance * 5) {
        return {best_alpha, best_beta};
    }

    return {std::numeric_limits<double>::quiet_NaN(),
            std::numeric_limits<double>::quiet_NaN()};
}

// ===============================================================================
// VECTORIZED FIT_BETA FUNCTION
// ===============================================================================

/**
 * @brief Vectorized beta distribution fitting (C++ implementation)
 *
 * Fits beta distributions to multiple sets of quantiles in parallel using OpenMP.
 * Pre-extracts all data from R objects before the parallel region for thread safety.
 *
 * @param q List of numeric vectors, each containing quantile values at the same probabilities
 * @param p Numeric vector of probabilities corresponding to the quantiles
 * @param tolerance Relative error tolerance
 * @param max_restarts Maximum restarts per fit
 * @param verbose Print progress
 * @param n_threads Number of OpenMP threads: 0 = auto (half of available cores), 1 = disable parallelization, >1 = use N threads
 * @return List with two numeric vectors: shape1 (alpha) and shape2 (beta)
 *
 * @export
 */
// [[Rcpp::export]]
List fit_beta_vec_cpp(
    List q,
    NumericVector p,
    double tolerance = 0.01,
    int max_restarts = 500,
    bool verbose = false,
    int n_threads = 0
) {
    int n_quantiles = q.size();
    if (n_quantiles == 0) {
        return List::create(
            Named("shape1") = NumericVector(0),
            Named("shape2") = NumericVector(0)
        );
    }

    // Get the length from the first element
    NumericVector first_q = q[0];
    int n_obs = first_q.size();

    // Verify all elements have the same length
    for (int i = 1; i < n_quantiles; i++) {
        NumericVector qi = q[i];
        if (qi.size() != n_obs) {
            stop("All elements in q must have the same length");
        }
    }

    if (static_cast<int>(p.size()) != n_quantiles) {
        stop("Length of p must match number of elements in q");
    }

    // PRE-EXTRACT all data from R objects into std::vectors (before parallel region)
    std::vector<std::vector<double>> q_data(n_quantiles);
    for (int j = 0; j < n_quantiles; j++) {
        NumericVector qj = q[j];
        q_data[j] = std::vector<double>(qj.begin(), qj.end());
    }
    std::vector<double> p_data(p.begin(), p.end());

    // =========================================================================
    // VALIDATE QUANTILE MONOTONICITY
    // For each observation, quantiles must increase with probability.
    // This catches data errors like inverted confidence intervals early.
    // =========================================================================
    std::vector<int> invalid_indices;
    invalid_indices.reserve(100);  // Pre-allocate for efficiency

    for (int i = 0; i < n_obs; i++) {
        // Extract quantile values for this observation
        std::vector<double> x_i(n_quantiles);
        for (int j = 0; j < n_quantiles; j++) {
            x_i[j] = q_data[j][i];
        }

        // Check monotonicity
        if (!check_quantile_monotonicity(x_i, p_data)) {
            invalid_indices.push_back(i);
        }
    }

    if (!invalid_indices.empty()) {
        // Build informative error message
        std::string err_msg = "Quantile values are not monotonically increasing with probability.\n";
        err_msg += "For a valid beta distribution fit, when probabilities are sorted in increasing order,\n";
        err_msg += "the corresponding quantile values must also be in increasing order.\n";
        err_msg += "(e.g., the 2.5th percentile must be less than the 97.5th percentile)\n\n";
        err_msg += "Found " + std::to_string(invalid_indices.size()) + " observation(s) with inverted quantiles.\n";

        // Show first few problematic indices (1-indexed for R users)
        int n_to_show = std::min(static_cast<int>(invalid_indices.size()), MAX_ERROR_INDICES_TO_SHOW);
        err_msg += "First " + std::to_string(n_to_show) + " problematic row indices (1-indexed): ";
        for (int k = 0; k < n_to_show; k++) {
            if (k > 0) err_msg += ", ";
            err_msg += std::to_string(invalid_indices[k] + 1);  // Convert to 1-indexed
        }
        if (static_cast<int>(invalid_indices.size()) > MAX_ERROR_INDICES_TO_SHOW) {
            err_msg += ", ...";
        }
        err_msg += "\n\n";

        // Show details for the first problematic observation
        int first_bad = invalid_indices[0];
        std::vector<double> x_bad(n_quantiles);
        for (int j = 0; j < n_quantiles; j++) {
            x_bad[j] = q_data[j][first_bad];
        }

        // Sort indices by probability for display
        std::vector<int> idx(n_quantiles);
        for (int j = 0; j < n_quantiles; j++) idx[j] = j;
        std::sort(idx.begin(), idx.end(), [&p_data](int a, int b) {
            return p_data[a] < p_data[b];
        });

        err_msg += "Example (row " + std::to_string(first_bad + 1) + "), sorted by probability:\n";
        for (int j = 0; j < n_quantiles; j++) {
            err_msg += "  p=" + std::to_string(p_data[idx[j]]) +
                       " -> quantile=" + std::to_string(x_bad[idx[j]]);
            if (j > 0 && x_bad[idx[j]] < x_bad[idx[j-1]]) {
                err_msg += " <- INVERTED (should be > " + std::to_string(x_bad[idx[j-1]]) + ")";
            }
            err_msg += "\n";
        }
        err_msg += "\nPlease check your input data. Common causes:\n";
        err_msg += "  - Lower/upper confidence bounds are swapped\n";
        err_msg += "  - Column names are mislabeled\n";
        err_msg += "  - Data processing error in upstream pipeline\n";

        stop(err_msg);
    }

    // Prepare output vectors (std::vector for thread safety)
    std::vector<double> shape1_out(n_obs);
    std::vector<double> shape2_out(n_obs);

    // Determine whether to use OpenMP parallelization
    // n_threads: 0 = auto (use half of available cores), 1 = sequential, >1 = use N threads
    bool use_parallel = (n_threads != 1);
    int actual_threads = n_threads;

#ifdef _OPENMP
    if (use_parallel) {
        if (n_threads == 0) {
            // Auto mode: use half of available cores (minimum 1)
            int max_threads = omp_get_max_threads();
            actual_threads = std::max(1, max_threads / 2);
        }
        omp_set_num_threads(actual_threads);
    }
#else
    // OpenMP not available - always run sequentially
    use_parallel = false;
    actual_threads = 1;
#endif

    // Print start message if verbose (before parallel region - safe)
    if (verbose) {
        Rcpp::Rcout << "Fitting " << n_obs << " beta distributions";
#ifdef _OPENMP
        if (use_parallel) {
            Rcpp::Rcout << " (parallel mode, " << actual_threads << " threads)";
        } else {
            Rcpp::Rcout << " (sequential mode)";
        }
#else
        Rcpp::Rcout << " (sequential mode - OpenMP not available)";
#endif
        Rcpp::Rcout << "..." << std::endl;
    }

    // Main processing loop
#ifdef _OPENMP
    if (use_parallel) {
        // Parallel loop - NO printing inside (Rcpp::Rcout is not thread-safe with R's event loop)
        #pragma omp parallel for schedule(dynamic)
        for (int i = 0; i < n_obs; i++) {
            std::vector<double> x_i(n_quantiles);
            for (int j = 0; j < n_quantiles; j++) {
                x_i[j] = q_data[j][i];
            }

            auto [shape1, shape2] = fit_beta_pure_cpp(x_i, p_data, tolerance, max_restarts);

            shape1_out[i] = shape1;
            shape2_out[i] = shape2;
        }
    } else
#endif
    {
        // Sequential loop (when OpenMP disabled or n_threads == 1) - can print safely
        int processed = 0;
        for (int i = 0; i < n_obs; i++) {
            std::vector<double> x_i(n_quantiles);
            for (int j = 0; j < n_quantiles; j++) {
                x_i[j] = q_data[j][i];
            }

            auto [shape1, shape2] = fit_beta_pure_cpp(x_i, p_data, tolerance, max_restarts);

            shape1_out[i] = shape1;
            shape2_out[i] = shape2;

            if (verbose) {
                processed++;
                if (processed % 100 == 0) {
                    Rcpp::Rcout << "Processed " << processed << " / " << n_obs << " observations" << std::endl;
                }
            }
        }
    }

    // Print completion message (after parallel region - safe)
    if (verbose) {
        Rcpp::Rcout << "Completed fitting " << n_obs << " beta distributions." << std::endl;
    }

    // Convert back to R vectors
    return List::create(
        Named("shape1") = NumericVector(shape1_out.begin(), shape1_out.end()),
        Named("shape2") = NumericVector(shape2_out.begin(), shape2_out.end())
    );
}

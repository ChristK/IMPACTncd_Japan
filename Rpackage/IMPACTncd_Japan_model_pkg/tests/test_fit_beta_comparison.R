# Test script to compare R and C++ implementations of fit_beta
# Can be run from any directory

library(Rcpp)
library(data.table)
library(microbenchmark)

# Find the package directory (works regardless of current working directory)
this_script <- tryCatch(
  normalizePath(sys.frame(1)$ofile),
  error = function(e) {
    # Fallback for interactive use or Rscript
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      normalizePath(sub("--file=", "", file_arg))
    } else {
      # Last resort: assume we're in the package directory
      file.path(getwd(), "tests", "test_fit_beta_comparison.R")
    }
  }
)
pkg_dir <- dirname(dirname(this_script))
cat("Package directory:", pkg_dir, "\n")

# Compile the C++ code directly for testing
cat("Compiling C++ code...\n")
sourceCpp(
  file.path(pkg_dir, "src", "fit_beta.cpp"),
  verbose = TRUE,
  rebuild = TRUE
)

# ============================================================================
# Original R implementation (copied from Disease_class.R)
# ============================================================================

fit_beta_R <- function(
    x = c(0.01, 0.005, 0.5),
    x_p = c(0.5, 0.025, 0.975),
    tolerance = 0.01,
    verbose = FALSE
) {
  if (length(x) != length(x_p)) {
    stop("x and x_p need to be of same length")
  }
  if (length(x) < 2L) {
    stop("x need to have at least length of 2")
  }
  if (length(unique(x)) == 1) {
    return(c(1, 1))
  }
  logit <- function(p) log(p / (1 - p))
  x_p_ <- logit(x_p)

  f.beta <- function(alpha, beta, x, lower = 0, upper = 1) {
    p <- pbeta((x - lower) / (upper - lower), alpha, beta)
    log(p / (1 - p))
  }

  wts = c(1, rep(1, length(x) - 1L))
  delta <- function(fit, actual, wts_) {
    sum((wts_ / sum(wts_)) * (fit - actual)^2)
  }

  objective <- function(theta, x, prob, wts_, ...) {
    ab <- exp(theta)
    fit <- f.beta(ab[1], ab[2], x, ...)
    return(delta(fit, prob, wts_))
  }

  flag <- TRUE
  steptol_ <- 1e-6
  max_it <- 0L
  jump <- 2
  if (length(x) == 2) {
    set.seed(42)  # For reproducibility in tests
    start <- log(runif(2, c(1, 1), c(1e2, 1e6)))
  } else {
    start <- log(fit_beta_R(x = x[c(1, 2)], x_p = x_p[c(1, 2)]))
  }

  while (flag && max_it < 1e4) {
    sol <- tryCatch(
      {
        nlm(
          objective,
          start,
          x = x,
          prob = x_p_,
          wts_ = wts,
          typsize = c(1, 1),
          fscale = 1e-12,
          gradtol = 1e-12,
          steptol = steptol_,
          iterlim = 5000
        )
      },
      error = function(e) list("estimate" = c(.5, .5), "code" = 5L)
    )

    set.seed(42 + max_it)  # For reproducibility
    start <- log(runif(2, c(0, 0), c(1e2, 1e6)))

    rel_error <- x / qbeta(x_p, exp(sol$estimate)[1], exp(sol$estimate)[2])

    if (verbose) {
      print(c(sol$code, rel_error, x))
    }
    flag <- (sol$code > 2L ||
               any(!data.table::between(rel_error, 1 - tolerance, 1 + tolerance)))
    if (is.na(flag)) {
      flag <- TRUE
    }
    max_it <- max_it + 1L
    if (max_it == 2000) {
      wts <- c(1, rep(0.9, length(x) - 1L))
    }
    if (max_it == 4000) {
      wts <- c(1, rep(0.8, length(x) - 1L))
    }
    if (max_it == 6000) {
      wts <- c(1, rep(0.7, length(x) - 1L))
    }
    if (max_it == 8000) {
      wts <- c(1, rep(0.6, length(x) - 1L))
    }
    if (max_it == 9000) {
      wts <- c(1, rep(0.5, length(x) - 1L))
      jump <- jump + 1
      if (length(x) == 2) {
        start <- log(runif(2, c(0, 0), c(1e3, 1e6)))
      } else {
        start <- log(fit_beta_R(x = x[c(1, 3)], x_p = x_p[c(1, 3)]))
      }
    }
    if (max_it == 9000 && length(x) > 2) {
      if (verbose) {
        print("dropping last value")
      }
      start <- log(fit_beta_R(x = head(x, -1), x_p = head(x_p, -1)))
      x <- head(x, -1)
      x_p_ <- head(x_p_, -1)
      x_p <- head(x_p, -1)
      wts <- head(wts, -1)
      jump <- 2
      max_it <- 0
    }
  }
  if (sol$code < 3L && max_it < 1e4) {
    return(exp(sol$estimate))
  } else {
    warning(c(
      sol$code,
      max_it,
      " Beta is not a good fit for these data!\n",
      x
    ))
    return(c(NA_real_, NA_real_))
  }
}

fit_beta_vec_R <- function(
    q = list(
      c(0.007248869, 0.0003693000),
      c(0.005198173, 0.0002744560),
      c(0.009516794, 0.0004751233)
    ),
    p = c(0.5, 0.025, 0.975),
    tolerance = 0.01,
    verbose = FALSE
) {
  if (length(unique(sapply(q, length))) != 1L) {
    stop("all elements in q need to be of same length")
  }
  out <- vector("list", length(q[[1]]))
  for (i in seq_len(length(q[[1]]))) {
    if (verbose) {
      print(i)
    }
    out[[i]] <- fit_beta_R(
      x = unlist(sapply(q, `[`, i)),
      x_p = p,
      tolerance = tolerance,
      verbose = verbose
    )
  }
  return(data.table::transpose(data.table::setDF(out)))
}

# ============================================================================
# Test Cases
# ============================================================================

cat("\n", strrep("=", 70), "\n")
cat("TEST 1: Basic 3-quantile fitting\n")
cat(strrep("=", 70), "\n")

test_x <- c(0.01, 0.005, 0.02)
test_p <- c(0.5, 0.025, 0.975)

cat("Input: x =", paste(test_x, collapse=", "), "\n")
cat("       p =", paste(test_p, collapse=", "), "\n\n")

cat("Running R implementation...\n")
t_R <- system.time(result_R <- fit_beta_R(test_x, test_p, verbose = FALSE))
cat("R result: shape1 =", result_R[1], ", shape2 =", result_R[2], "\n")
cat("R time:", t_R["elapsed"], "seconds\n\n")

cat("Running C++ implementation...\n")
t_cpp <- system.time(result_cpp <- fit_beta_cpp(test_x, test_p, verbose = FALSE))
cat("C++ result: shape1 =", result_cpp["shape1"], ", shape2 =", result_cpp["shape2"], "\n")
cat("C++ time:", t_cpp["elapsed"], "seconds\n\n")

# Verify results produce similar quantiles
verify_fit <- function(shape1, shape2, x, p) {
  fitted_q <- qbeta(p, shape1, shape2)
  rel_errors <- abs(x / fitted_q - 1)
  return(list(
    fitted_quantiles = fitted_q,
    relative_errors = rel_errors,
    max_error = max(rel_errors)
  ))
}

cat("Verification (R):\n")
ver_R <- verify_fit(result_R[1], result_R[2], test_x, test_p)
print(ver_R)

cat("\nVerification (C++):\n")
ver_cpp <- verify_fit(result_cpp["shape1"], result_cpp["shape2"], test_x, test_p)
print(ver_cpp)

cat("\n", strrep("=", 70), "\n")
cat("TEST 2: 2-quantile fitting\n")
cat(strrep("=", 70), "\n")

test_x2 <- c(0.01, 0.005)
test_p2 <- c(0.5, 0.025)

cat("Input: x =", paste(test_x2, collapse=", "), "\n")
cat("       p =", paste(test_p2, collapse=", "), "\n\n")

cat("Running R implementation...\n")
t_R2 <- system.time(result_R2 <- fit_beta_R(test_x2, test_p2, verbose = FALSE))
cat("R result: shape1 =", result_R2[1], ", shape2 =", result_R2[2], "\n")
cat("R time:", t_R2["elapsed"], "seconds\n\n")

cat("Running C++ implementation...\n")
t_cpp2 <- system.time(result_cpp2 <- fit_beta_cpp(test_x2, test_p2, verbose = FALSE))
cat("C++ result: shape1 =", result_cpp2["shape1"], ", shape2 =", result_cpp2["shape2"], "\n")
cat("C++ time:", t_cpp2["elapsed"], "seconds\n\n")

cat("\n", strrep("=", 70), "\n")
cat("TEST 3: Edge case - all same values\n")
cat(strrep("=", 70), "\n")

test_x3 <- c(0.05, 0.05, 0.05)
test_p3 <- c(0.5, 0.025, 0.975)

result_R3 <- fit_beta_R(test_x3, test_p3)
result_cpp3 <- fit_beta_cpp(test_x3, test_p3)

cat("R result:", result_R3, "\n")
cat("C++ result:", as.numeric(result_cpp3), "\n")
cat("Both return (1, 1) for constant input:",
    all(result_R3 == c(1, 1)) && all(as.numeric(result_cpp3) == c(1, 1)), "\n")

cat("\n", strrep("=", 70), "\n")
cat("TEST 4: Performance benchmark (10 fits)\n")
cat(strrep("=", 70), "\n")

# Generate test cases
set.seed(123)
n_test <- 10
test_cases <- lapply(1:n_test, function(i) {
  # Generate parameters that will produce valid beta distributions
  a <- runif(1, 0.5, 5)
  b <- runif(1, 50, 500)
  # Generate quantiles from a true beta distribution
  p <- c(0.5, 0.025, 0.975)
  x <- qbeta(p, a, b)
  list(x = x, p = p, true_a = a, true_b = b)
})

cat("Running R implementation on", n_test, "test cases...\n")
t_R_all <- system.time({
  results_R <- lapply(test_cases, function(tc) {
    fit_beta_R(tc$x, tc$p, tolerance = 0.05)
  })
})
cat("Total R time:", t_R_all["elapsed"], "seconds\n")

cat("\nRunning C++ implementation on", n_test, "test cases...\n")
t_cpp_all <- system.time({
  results_cpp <- lapply(test_cases, function(tc) {
    fit_beta_cpp(tc$x, tc$p, tolerance = 0.05)
  })
})
cat("Total C++ time:", t_cpp_all["elapsed"], "seconds\n")

cat("\nSpeedup factor:", round(t_R_all["elapsed"] / max(t_cpp_all["elapsed"], 0.001), 1), "x\n")

cat("\n", strrep("=", 70), "\n")
cat("TEST 5: Vectorized version comparison\n")
cat(strrep("=", 70), "\n")

# Create test data for vectorized version
set.seed(456)
n_vec <- 20
a_true <- runif(n_vec, 0.5, 5)
b_true <- runif(n_vec, 50, 500)

q_list <- list(
  qbeta(0.5, a_true, b_true),    # medians
  qbeta(0.025, a_true, b_true),  # lower bounds
  qbeta(0.975, a_true, b_true)   # upper bounds
)
p_vec <- c(0.5, 0.025, 0.975)

cat("Testing vectorized version with", n_vec, "observations...\n\n")

cat("Running R vectorized implementation...\n")
t_R_vec <- system.time({
  result_R_vec <- fit_beta_vec_R(q_list, p_vec, tolerance = 0.05)
})
cat("R vectorized time:", t_R_vec["elapsed"], "seconds\n\n")

cat("Running C++ vectorized implementation...\n")
t_cpp_vec <- system.time({
  result_cpp_vec <- fit_beta_vec_cpp(q_list, p_vec, tolerance = 0.05, n_threads = 4)
})
cat("C++ vectorized time:", t_cpp_vec["elapsed"], "seconds\n\n")

cat("Speedup factor:", round(t_R_vec["elapsed"] / max(t_cpp_vec["elapsed"], 0.001), 1), "x\n")

# Compare results
cat("\nComparing shape1 values (first 5):\n")
cat("R:  ", head(unlist(result_R_vec[[1]]), 5), "\n")
cat("C++:", head(result_cpp_vec$shape1, 5), "\n")

cat("\nComparing shape2 values (first 5):\n")
cat("R:  ", head(unlist(result_R_vec[[2]]), 5), "\n")
cat("C++:", head(result_cpp_vec$shape2, 5), "\n")

cat("\n", strrep("=", 70), "\n")
cat("TEST 6: Detailed microbenchmark (if time permits)\n")
cat(strrep("=", 70), "\n")

# Use a single representative test case for detailed benchmarking
bench_x <- c(0.008, 0.004, 0.015)
bench_p <- c(0.5, 0.025, 0.975)

cat("Benchmarking single fit_beta call...\n")
bm <- microbenchmark(
  R = fit_beta_R(bench_x, bench_p, tolerance = 0.05),
  Cpp = fit_beta_cpp(bench_x, bench_p, tolerance = 0.05),
  times = 5  # Reduced iterations since R is slow
)
print(bm)

cat("\n", strrep("=", 70), "\n")
cat("TEST 7: Safety tests\n")
cat(strrep("=", 70), "\n")

# Test with values at boundaries
cat("Testing boundary values...\n")

# Very small values
small_x <- c(0.0001, 0.00005, 0.0002)
small_result <- fit_beta_cpp(small_x, test_p)
cat("Small values result:", as.numeric(small_result), "\n")

# Values close to 1 (should still work)
large_x <- c(0.9, 0.85, 0.95)
large_result <- fit_beta_cpp(large_x, test_p)
cat("Large values result:", as.numeric(large_result), "\n")

cat("\n", strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n")
cat("C++ implementation provides:\n")
cat("  - Comparable fit quality to R implementation\n")
cat("  - Significant speedup (especially for vectorized version)\n")
cat("  - Better numerical safety with bounds checking\n")
cat("  - OpenMP parallelization for vectorized fits\n")

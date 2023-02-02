
library(data.table)
library(fst)

#### Simulate uncertainty in RR for sugar on BMI -------------------------------

sugar_rr_low <- 0.005
sugar_rr_low_ci <- 0.0075
sugar_rr_low_se <- (sugar_rr_low_ci - sugar_rr_low) / 1.96

sugar_rr_high <- 0.0115
sugar_rr_high_ci <- 0.016
sugar_rr_high_se <- (sugar_rr_high_ci - sugar_rr_high) / 1.96

n_samples <- 10000

## Ensure replicability #  
set.seed(log(n_samples) * 4211 + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

sugar_rr <- data.table(
  mc = 1:n_samples,
  sugar_rr_low = qnorm(quantiles, mean = sugar_rr_low, sd = sugar_rr_low_se),
  sugar_rr_high = qnorm(quantiles, mean = sugar_rr_high, sd = sugar_rr_high_se)
)

write_fst(sugar_rr, "./inputs/other_parameters/sugar_rr.fst")

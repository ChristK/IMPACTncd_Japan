
library(data.table)
library(fst)

#### Simulate uncertainty in BMI lag -------------------------------

bmi_lag <- 3 # Data from Staudigel EVS paper/tables

n_samples <- 10000

## Ensure replicability #  
set.seed(log(n_samples) * 4 + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

bmi_lag_samples <- data.table(
  mc = 1:n_samples,
  bmi_lag = qunif(quantiles, min = 1, max = 5)
)

write_fst(bmi_lag_samples, "./inputs/other_parameters/bmi_lag.fst")

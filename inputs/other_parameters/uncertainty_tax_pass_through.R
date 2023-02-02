library(data.table)
library(fst)

#### Simulate uncertainty in tax pass-through (based on Andreyeva et al. 2022) -------------------------------

tax_pth <- 0.82  # Data from Staudigel EVS paper/tables
tax_pth_se <- (tax_pth - 0.66)/1.96

n_samples <- 10000

## Ensure replicability #  
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

tax_pth_samples <- data.table(
  mc = 1:n_samples,
  tax_pth = qnorm(quantiles, mean = tax_pth, sd = tax_pth_se)
)

tax_pth_samples[tax_pth > 1, tax_pth := 1]

write_fst(tax_pth_samples, "./inputs/other_parameters/tax_pass_through.fst")


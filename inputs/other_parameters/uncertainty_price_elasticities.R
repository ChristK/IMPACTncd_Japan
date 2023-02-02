
library(data.table)
library(fst)

#### Simulate uncertainty in Marshallian (uncompensated) price elasticities -------------------------------

oPE_ssb <- -0.956137552700242 # Data from Staudigel EVS paper/tables
oPE_ssb_se <- 0.111098287

oPE_juice <- -1.10560924104306
oPE_juice_se <- 0.148790625

cPE_ssb_juice <- 0.051762447
cPE_ssb_juice_se <- 0.096911994
  
n_samples <- 10000

## Ensure replicability #  
set.seed(log(n_samples) + 1337) # Seed is parameter-specific!

## Draw quantiles #
quantiles <- runif(n_samples, min = 0, max = 1) * 0.999

oPE_ssb_samples <- data.table(
  mc = 1:n_samples,
  oPE_ssb = qnorm(quantiles, mean = oPE_ssb, sd = oPE_ssb_se)
)

oPE_juice_samples <- data.table(
  mc = 1:n_samples,
  oPE_juice = qnorm(quantiles, mean = oPE_juice, sd = oPE_juice_se)
)

cPE_ssb_juice_samples <- data.table(
  mc = 1:n_samples,
  cPE_ssb_juice = qnorm(quantiles, mean = cPE_ssb_juice, sd = cPE_ssb_juice_se)
)

write_fst(oPE_ssb_samples, "./inputs/other_parameters/oPE_ssb.fst")
write_fst(oPE_juice_samples, "./inputs/other_parameters/oPE_juice.fst")
write_fst(cPE_ssb_juice_samples, "./inputs/other_parameters/cPE_ssb_juice.fst")



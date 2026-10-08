# Unit test for IMPACTncdJapan:::mrtl_prdv_annual_cost_param()
# (R/cost_mrtl_prdv.R). Expected values are computed by brute force here
# (explicit double loops over single ages), not with the helper's recursion.
# Run from the project root:  Rscript testing/test_mrtl_prdv.R

source("./global.R")
library(data.table)

f <- IMPACTncdJapan:::mrtl_prdv_annual_cost_param

failures <- character(0)
check <- function(cond, msg) {
  ok <- isTRUE(cond)
  message(if (ok) "PASS: " else "FAIL: ", msg)
  if (!ok) failures <<- c(failures, msg)
  invisible(ok)
}
rel_eq <- function(a, b, tol = 1e-12) isTRUE(all(abs(a - b) <= tol * pmax(1, abs(b))))

# Brute force: LV of single age a (sex-specific vector e indexed by age)
lv_brute <- function(e_by_age, a, r) {
  ages <- as.integer(names(e_by_age))
  s <- 0
  for (ap in ages[ages >= a]) s <- s + e_by_age[[as.character(ap)]] * (1 + r)^-(ap - a)
  s
}
# Brute force group LV and calibration denominator
denom_brute <- function(emp, dth, r) {
  tot <- 0
  for (sx in unique(emp$sex)) {
    es <- emp[sex == sx]
    e_age <- numeric(0)
    for (i in seq_len(nrow(es))) {
      lo <- as.integer(sub("-.*", "", es$agegrp[i])); hi <- as.integer(sub(".*-", "", es$agegrp[i]))
      for (a in lo:hi) e_age[as.character(a)] <- es$employees[i]
    }
    for (i in seq_len(nrow(es))) {
      d <- dth[agegrp == es$agegrp[i] & sex == sx]$V1
      if (!length(d) || is.na(d[1])) next
      lo <- as.integer(sub("-.*", "", es$agegrp[i])); hi <- as.integer(sub(".*-", "", es$agegrp[i]))
      lvg <- mean(vapply(lo:hi, function(a) lv_brute(e_age, a, r), 0))
      tot <- tot + d[1] * lvg
    }
  }
  tot
}

# ---- 1. r = 0, tiny profile --------------------------------------------
emp0 <- data.table(agegrp = c("30-34", "35-39", "30-34", "35-39"),
                   sex = c("men", "men", "women", "women"),
                   employees = c(10, 4, 6, 0))
dth0 <- data.table(agegrp = c("30-34", "35-39", "30-34", "35-39"),
                   sex = c("men", "men", "women", "women"),
                   V1 = c(2, 3, 5, 7))
res0 <- f(emp0, dth0, total_cost = 1000, inflation_factor = 1.5, source_discount_rate = 0)
# LV of a single age sums e over that age and every older age of the same sex.
# Men: age 30 -> 5*10+5*4 = 70, 31 -> 60, 32 -> 50, 33 -> 40, 34 -> 30 : mean 50
#      age 35 -> 20, 36 -> 16, 37 -> 12, 38 -> 8, 39 -> 4 : mean 12
# Women: 30-34 -> 5*6 = 30,24,18,12,6 : mean 18 ; 35-39 -> 0
den0 <- 2 * 50 + 3 * 12 + 5 * 18 + 7 * 0
k0 <- 1000 * 1.5 / den0
check(rel_eq(res0[agegrp == "30-34" & sex == "men", cost_param], k0 * 10), "r=0: hand-computed k for men 30-34")
check(rel_eq(res0[agegrp == "35-39" & sex == "men", cost_param], k0 * 4), "r=0: hand-computed k for men 35-39")
check(rel_eq(res0[agegrp == "30-34" & sex == "women", cost_param], k0 * 6), "r=0: women share same k")
check(res0[agegrp == "35-39" & sex == "women", cost_param] == 0, "r=0: zero employees -> zero cost")
check(rel_eq(den0 * k0, 1500), "r=0: k reproduces total*inflation exactly")
check(rel_eq(denom_brute(emp0, dth0, 0), den0), "r=0: brute-force denom matches hand denom")

# ---- 2. r = 3%, tiny 2-group profile -------------------------------------
emp3 <- data.table(agegrp = c("60-64", "65-69"), sex = "men", employees = c(8, 2))
dth3 <- data.table(agegrp = c("60-64", "65-69"), sex = "men", V1 = c(10, 4))
res3 <- f(emp3, dth3, 500, 1.025, 0.03)
den3 <- 0
for (g in 1:2) {
  lo <- c(60, 65)[g]
  lvs <- numeric(5)
  for (j in 0:4) {
    a <- lo + j; s <- 0
    for (ap in a:69) s <- s + c(8, 2)[ifelse(ap <= 64, 1, 2)] * 1.03^-(ap - a)
    lvs[j + 1] <- s
  }
  den3 <- den3 + c(10, 4)[g] * mean(lvs)
}
k3 <- 500 * 1.025 / den3
check(rel_eq(res3$cost_param, k3 * c(8, 2)), "3%: matches explicit double-loop sum")
check(rel_eq(denom_brute(emp3, dth3, 0.03), den3), "3%: two brute-force routes agree")
check(rel_eq(den3 * res3$cost_param[1] / 8, 500 * 1.025), "3%: helper's k * denom = total*inflation")

# ---- 3. Real employee profile, calibration identity -----------------------
ag <- c("30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99")
emp_real <- rbind(
  data.table(agegrp = ag, sex = "men", employees = c(1683780,1829610,2174550,2057710,1702470,1425510,963430,369640,106850,0,0,0,0,0)),
  data.table(agegrp = ag, sex = "women", employees = c(919700,894770,1049490,1037140,854970,685040,376370,132470,44050,0,0,0,0,0)))
# plausible CHD deaths 2016 (rising steeply with age; includes age groups below 30 which have no profile)
d_men <- c(250,420,800,1500,2600,4200,6500,9500,12000,13500,14500,11000,5000,1500)
d_wom <- c(60,100,200,380,700,1200,2000,3800,6500,11000,19000,25000,17000,7000)
dth_real <- rbind(data.table(agegrp = ag, sex = "men", V1 = d_men),
                  data.table(agegrp = ag, sex = "women", V1 = d_wom),
                  data.table(agegrp = c("20-24","25-29"), sex = "men", V1 = c(30, 90)))
for (r in c(0, 0.03)) {
  rr <- f(emp_real, dth_real, 2257e9, 1.025, r)
  dn <- denom_brute(emp_real, dth_real, r)
  check(rel_eq(dn * (rr[agegrp == "30-34" & sex == "men", cost_param] / 1683780), 2257e9 * 1.025),
        sprintf("real profile r=%g: sum D*LV*k == total*inflation (1e-12)", r))
}
res_real <- f(emp_real, dth_real, 2257e9, 1.025, 0.03)

# ---- 4. Output shape ----------------------------------------------------
check(nrow(res_real) == nrow(emp_real) && identical(res_real$agegrp, emp_real$agegrp) && identical(res_real$sex, emp_real$sex),
      "output has same rows/keys/order as employee input")
check(identical(names(res_real), c("agegrp", "sex", "cost_param")), "output columns agegrp, sex, cost_param")
check(all(res_real[emp_real$employees == 0, cost_param] == 0), "zero where employees == 0")
kk <- res_real$cost_param[emp_real$employees > 0] / emp_real$employees[emp_real$employees > 0]
check(rel_eq(kk, rep(kk[1], length(kk))) && kk[1] > 0, "single scalar k across all groups and both sexes")
check(all(is.finite(res_real$cost_param)) && all(res_real$cost_param >= 0), "all finite and non-negative")

# ---- 5. Degenerate ----------------------------------------------------------
z <- copy(dth_real)[, V1 := 0]
rz <- f(emp_real, z, 2257e9, 1.025, 0.03)
check(all(rz$cost_param == 0) && all(is.finite(rz$cost_param)) && nrow(rz) == nrow(emp_real), "all-zero deaths -> all-zero cost_param, no NaN/Inf")
dna <- copy(dth_real); dna[agegrp == "75-79", V1 := NA_real_]
rna <- f(emp_real, dna, 2257e9, 1.025, 0.03)
dna2 <- dth_real[agegrp != "75-79"]
check(rel_eq(rna$cost_param, f(emp_real, dna2, 2257e9, 1.025, 0.03)$cost_param), "NA deaths rows ignored (== rows dropped)")
check(all(is.finite(rna$cost_param)), "NA deaths: no NaN/Inf")
rall <- f(emp_real, dth_real[0], 2257e9, 1.025, 0.03)
check(all(rall$cost_param == 0), "empty deaths table -> zeros")
dnaall <- copy(dth_real)[, V1 := NA_real_]
check(all(f(emp_real, dnaall, 2257e9, 1.025, 0.03)$cost_param == 0), "all-NA deaths -> zeros")

# ---- 6. Factor inputs ---------------------------------------------------------
ef <- copy(emp_real)[, `:=`(agegrp = factor(agegrp, levels = ag), sex = factor(sex))]
df <- copy(dth_real)[, `:=`(agegrp = factor(agegrp), sex = factor(sex))]
rf <- f(ef, df, 2257e9, 1.025, 0.03)
check(is.character(rf$agegrp) && is.character(rf$sex), "factor inputs returned as character")
check(rel_eq(rf$cost_param, res_real$cost_param) && identical(rf$agegrp, res_real$agegrp), "factor-typed inputs give identical result")

# ---- 7. Illustrative table ------------------------------------------------------
message("\n--- Illustration: CHD annual value per death-year (JPY) by attained agegrp/sex ---")
ill <- dcast(res_real[emp_real$employees > 0], agegrp ~ sex, value.var = "cost_param")
print(ill, digits = 12)
k_chd <- res_real[agegrp == "30-34" & sex == "men", cost_param] / 1683780
den_old <- sum(emp_real$employees * c(d_men, d_wom))
message(sprintf("\nk_chd = %.6f JPY per employee-unit; old-method denom sum(employees*D) = %.6e", k_chd, den_old))
em <- emp_real[sex == "men"]
rows <- list()
for (a in c(32, 47, 62)) {
  e_age <- function(x) em$employees[findInterval(x, c(30,35,40,45,50,55,60,65,70,75,80,85,90,95))]
  stream <- k_chd * sum(e_age(a:64))
  old <- 2257e9 * 1.025 * e_age(a) / den_old
  rows[[length(rows) + 1]] <- data.table(age = a, annual_stream_undiscounted = stream, old_oneoff = old, ratio = stream / old)
}
ill2 <- rbindlist(rows)
print(ill2, digits = 12)
message("DONE")
if (length(failures)) stop(length(failures), " check(s) failed:\n", paste(failures, collapse = "\n"))
message("All checks passed.")

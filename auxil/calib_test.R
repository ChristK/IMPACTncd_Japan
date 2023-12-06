source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")
self <- IMPACTncd$.__enclos_env__$self
private <- IMPACTncd$.__enclos_env__$private
age_ = 30
mc <- 1:3
replace <- FALSE

# scenario_fn_primary_prevention   <- function(sp) NULL
# scenario_fn_secondary_prevention <- function(sp) NULL

# IMPACTncd$
#   del_logs()$
#   del_outputs()$
#   run(mc, multicore = TRUE, "sc0")$
#   export_summaries(multicore = TRUE, type = c("incd", "prvl", "dis_mrtl"), single_year_of_age = TRUE)

unclbr <- fread(file.path(self$design$sim_prm$output_dir, "summaries", "incd_scaled_up.csv.gz"), 
                     select = c("year", "age", "sex", "mc", "popsize", "chd_incd", "stroke_incd"))
unclbr <- unclbr[age == age_, .(chd_incd = chd_incd/popsize, stroke_incd = stroke_incd/popsize), keyby = .(age, sex, year, mc)
   ][, .(chd_incd = median(chd_incd), stroke_incd = median(stroke_incd)), keyby = .(age, sex, year)]
     
# for CHD
# fit a log-log linear model to the uncalibrated results and store the coefficients
unclbr[chd_incd > 0, c("intercept_unclbr", "trend_unclbr") := as.list(coef(lm(log(chd_incd)~log(year)))), by = sex]
unclbr[, intercept_unclbr := nafill(intercept_unclbr, "const", max(intercept_unclbr, na.rm = TRUE)), by = sex] # NOTE I use max just to return a value. It doesn't matter what value it is.
unclbr[, trend_unclbr := nafill(trend_unclbr, "const", max(trend_unclbr, na.rm = TRUE)), by = sex] # NOTE I use max just to return a value. It doesn't matter what value it is.


# load benchmark
benchmark <- read_fst(file.path("./inputs/disease_burden", "chd_incd.fst"), columns = c("age", "sex", "year", "mu") , as.data.table = TRUE)[age == age_,]
benchmark[, c("intercept_bnchmrk", "trend_bnchmrk") := as.list(coef(lm(log(mu)~log(year)))), by = sex]

unclbr[benchmark[year == min(year)], chd_incd_clbr_fctr := exp(intercept_bnchmrk + trend_bnchmrk * log(year)) / exp(intercept_unclbr + trend_unclbr * log(year)), on = c("age", "sex")] # Do not join on year!

unclbr[sex == "women", plot(year, chd_incd)]
benchmark[sex == "women", lines(year, mu)]
unclbr[sex == "women" & chd_incd > 0, lines(year, exp(predict(lm(log(chd_incd) ~ log(year)), year = 2000:2050)), col = "red")]
unclbr[sex == "women" & chd_incd > 0, lines(year, exp(predict(lm(log(chd_incd * chd_incd_clbr_fctr) ~ log(year)), year = 2000:2050)), col = "green")]
unclbr[sex == "women" & chd_incd > 0, exp(predict(lm(log(chd_incd * chd_incd_clbr_fctr) ~ log(year)), year = 2000:2050))]

unclbr[, `:=` (chd_prvl_correction = chd_incd * (chd_incd_clbr_fctr - 1))]


# FTLT
prvl <- fread(file.path(self$design$sim_prm$output_dir, "summaries", "prvl_scaled_up.csv.gz"), 
                select = c("year", "age", "sex", "mc", "popsize", "chd_prvl", "stroke_prvl"))
prvl <- prvl[age == age_, .(chd_prvl = chd_prvl/popsize, stroke_prvl = stroke_prvl/popsize), keyby = .(age, sex, year, mc)
  ][, .(chd_prvl = mean(chd_prvl), stroke_prvl = mean(stroke_prvl)), keyby = .(age, sex, year)]
prvl[unclbr, on = c("year", "age", "sex"), `:=` (chd_ftlt_clbr_fctr = 1 / (chd_prvl + i.chd_prvl_correction))]

benchmark <- read_fst(file.path("./inputs/disease_burden", "chd_ftlt.fst"), columns = c("age", "sex", "year", "mu2") , as.data.table = TRUE)[age == age_,]
benchmark[sex == "women", plot(year, mu2)]
prvl[benchmark, on = c("age", "year", "sex")][sex=="women", lines(year, chd_prvl * chd_ftlt_clbr_fctr * mu2, col = "red")]
prvl[benchmark, on = c("age", "year", "sex")][sex=="women", lines(year, chd_prvl  * mu2, col = "red")]

prvl[sex == "women", plot(year, chd_prvl)]
prvl[sex == "women", plot(year, chd_ftlt_clbr_fctr)]

unclbr <- fread(file.path(self$design$sim_prm$output_dir, "summaries", "prvl_scaled_up.csv.gz"), 
                     select = c("year", "agegrp", "sex", "mc", "popsize", "chd_prvl", "stroke_prvl"))

unclbr <- unclbr[agegrp == "95-99", .(chd_prvl = chd_prvl/popsize, stroke_prvl = stroke_prvl/popsize), keyby = .(sex, year, mc)
   ][, .(chd_prvl = mean(chd_prvl), stroke_prvl = mean(stroke_prvl)), keyby = .(sex, year)]

unclbr[sex == "men", plot(year, chd_prvl)]

lc[age > 94, sum(chd_prvl > 0)/.N, keyby = year] # prevalence increases fast
lc[age > 94, sum(chd_prvl == 1)/.N, keyby = year] # incidence plateau
lc[age > 94 & chd_prvl > 0, sum(all_cause_mrtl == 2L)/.N, keyby = year][, plot(year, V1)]
lc[age > 94 & chd_prvl > 0, sum(all_cause_mrtl == 1L)/.N, keyby = year][, plot(year, V1)]
lc[age > 94 & chd_prvl > 0, sum(all_cause_mrtl == 3L)/.N, keyby = year][, plot(year, V1)]
lc[age > 98 & sex == "women", sum(all_cause_mrtl > 0L)/.N, keyby = year][, scatter.smooth(year, V1)]

lc[age == 95 & sex == "women", sum(all_cause_mrtl == 2L)/.N, keyby = year][, scatter.smooth(year, V1)]
benchmark <- read_fst(file.path("./inputs/disease_burden", "chd_ftlt.fst"), columns = c("age", "sex", "year", "mu2") , as.data.table = TRUE)[age == 95L,]
benchmark[sex == "women", lines(year, mu2, col = "red")]

lc[age == 95 & sex == "women", sum(chd_prvl > 0)/.N, keyby = year][, scatter.smooth(year, V1)]
benchmark <- read_fst(file.path("./inputs/disease_burden", "chd_prvl.fst"), columns = c("age", "sex", "year", "mu") , as.data.table = TRUE)[age == 95L,]
benchmark[sex == "women", plot(year, mu, col = "red")]
lc[age == 95 & sex == "women", sum(chd_prvl > 0)/.N, keyby = year][, lines(year, V1)]

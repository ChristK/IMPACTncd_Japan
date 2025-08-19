source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design_clbr.yaml")
self <- IMPACTncd$.__enclos_env__$self
private <- IMPACTncd$.__enclos_env__$private
age_ = 60L
mc <- 1:2
replace <- FALSE


# IMPACTncd$
#           del_logs()$
#           del_outputs()$
#           run(mc, multicore = TRUE, "sc0")$
#           export_summaries(multicore = TRUE, type = c("incd", "prvl", "dis_mrtl"), single_year_of_age = TRUE)

        memedian <- function(x) {
          out <- median(x)
          if (out == 0L) {
            # For rare events, consider alternative estimators
            nonzero_vals <- x[x > 0]
            if (length(nonzero_vals) > 0) {
              # Option 1: Use median of non-zero values scaled by occurrence probability
              prob_occurrence <- length(nonzero_vals) / length(x)
              median_nonzero <- median(nonzero_vals)
              out <- prob_occurrence * median_nonzero

              # Option 2: Alternative - use trimmed mean to reduce impact of outliers
              # trimmed_mean <- mean(x, trim = 0.1)  # Remove top/bottom 10%
              # out <- max(trimmed_mean, prob_occurrence * median_nonzero)
            } else {
              # If all values are 0, keep it as 0
              out <- 0
            }
          }
          out
        }

          # Incidence calibration
          # load the uncalibrated results
          unclbr <- open_dataset(file.path(
            self$design$sim_prm$output_dir,
            "summaries",
            "incd_scaled_up"
          )) %>%
            filter(age == age_) %>%
            select(
              "year",
              "age",
              "sex",
              "mc",
              "popsize",
              "chd_incd",
              "stroke_incd"
            ) %>%
            collect()
          setDT(unclbr)

          unclbr <- unclbr[,
            .(
              age,
              sex,
              year,
              mc,
              chd_incd = chd_incd / popsize,
              stroke_incd = stroke_incd / popsize
            )
          ][,
            .(
              chd_incd = memedian(chd_incd),
              stroke_incd = memedian(stroke_incd)
            ),
            keyby = .(age, sex, year)
          ]

          # for CHD
          # fit a log-log linear model to the uncalibrated results and store the coefficients
          tt <- unclbr[
              chd_incd > 0,
            as.list(coef(lm(
              log(chd_incd) ~ log(year)
            ))),
            by = sex
          ]

          unclbr[
            tt,
            on = "sex",
            c("intercept_unclbr", "trend_unclbr") := .(
              `(Intercept)`,
              `log(year)`
            )
          ]
          rm(tt)
          # load benchmark
          benchmark <- read_fst(
            file.path("./inputs/disease_burden", "chd_incd.fst"),
            columns = c("age", "sex", "year", "mu"),
            as.data.table = TRUE
          )[age == age_, ]
          # fit a log-log linear model to the benchmark incidence and store the coefficients
          benchmark[
            year >= self$design$sim_prm$init_year_long,
            c("intercept_bnchmrk", "trend_bnchmrk") := as.list(coef(lm(
              log(mu) ~ log(year)
            ))),
            by = sex
          ] 
          # calculate the calibration factors that the uncalibrated log-log model
          # need to be multiplied with so it can match the benchmark log-log model
          unclbr[
            benchmark[year == max(year)],
            chd_incd_clbr_fctr := exp(
              intercept_bnchmrk + trend_bnchmrk * log(year)
            ) /
              exp(intercept_unclbr + trend_unclbr * log(year)),
            on = c("age", "sex")
          ] # Do not join on year!
          unclbr[, c("intercept_unclbr", "trend_unclbr") := NULL]
unclbr[sex == "women", plot(year, chd_incd)]
benchmark[sex == "women", lines(year, mu)]
benchmark[sex == "women", lines(year, exp(predict(lm(log(mu) ~ log(year)), year = 2000:2050)), col = "#2200ff", lwd = 6)]
unclbr[sex == "women" & chd_incd > 0, lines(year, exp(predict(lm(log(chd_incd) ~ log(year)), year = 2000:2050)), col = "red")]
unclbr[sex == "women" & chd_incd > 0, lines(year, exp(predict(lm(log(chd_incd * chd_incd_clbr_fctr) ~ log(year)), year = 2000:2050)), col = "green")]

unclbr[sex == "men", plot(year, chd_incd)]
benchmark[sex == "men", lines(year, mu)]
benchmark[sex == "men", lines(year, exp(predict(lm(log(mu) ~ log(year)), year = 2000:2050)), col = "#2200ff", lwd = 6)]
unclbr[sex == "men" & chd_incd > 0, lines(year, exp(predict(lm(log(chd_incd) ~ log(year)), year = 2000:2050)), col = "red")]
unclbr[sex == "men" & chd_incd > 0, lines(year, exp(predict(lm(log(chd_incd * chd_incd_clbr_fctr) ~ log(year)), year = 2000:2050)), col = "green")]
unclbr[sex == "men" & chd_incd > 0, exp(predict(lm(log(chd_incd * chd_incd_clbr_fctr) ~ log(year)), year = 2000:2050))]

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

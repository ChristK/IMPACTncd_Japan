
library(fst)
library(data.table)
library(ggplot2)
library(CKutils)
library(MESS)

synthpops <- grep(".fst", list.files("./inputs/synthpop/"), value = TRUE)

dt <- read_fst(paste0("./inputs/synthpop/", synthpops[50]), as.data.table = TRUE)

tt <-
  read_fst("./inputs/pop_projections/German_pop_combined.fst", as.data.table = TRUE)
tt <- tt[between(age, min(dt$age), max(dt$age)) &
           between(year, min(dt$year), max(dt$year)),
         .(pops = sum(pops)), keyby = .(year, age, sex)]
dt[, wt_immrtl := .N, by = .(year, age, sex)]
absorb_dt(dt, tt)
#dt[, wt_immrtl := pops / (wt_immrtl * design$sim_prm$n_synthpop_aggregation)]
dt[, pops := NULL]

dt[year == 13, lapply(.SD, weighted.mean, w = wt_immrtl),
   .SDcols = c("ssb_curr_xps")]

dt[, adj_fct := ssb_adjust(age)][, ssb_adj := ssb_curr_xps * adj_fct]

dt[year == 13, lapply(.SD, weighted.mean, w = wt_immrtl),
   .SDcols = c("ssb_adj")]

dt_agg_total <- dt[year == 13, lapply(.SD, weighted.mean, w = wt_immrtl),
                   .SDcols = c("ssb_curr_xps", "ssb_adj"),
                   by = c("age", "sex")]

ggplot(dt_agg_total, aes(age, ssb_curr_xps, col = sex)) +
  geom_line() +
  geom_line(aes(age, ssb_adj))

auc(dt_agg_total[sex == "women"]$age, dt_agg_total[sex == "women"]$ssb_curr_xps)


ssb_adjust <- function(age){
  
  adj_fct <- (1 + (age - 20 + 1)^(-0.5))
  
  return(adj_fct)
}
summary(ssb_adjust(age))
plot(age, ssb_adjust(age))

ggplot(dt[sex == "men"], aes(ssb_curr_xps)) +
  facet_wrap(~ age, scale = "free") +
  geom_density()

ggplot(dt[sex == "men"], aes(ssb_adj)) +
  facet_wrap(~ age, scale = "free") +
  geom_density()


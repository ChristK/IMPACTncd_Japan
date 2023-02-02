res2 <- fread("./outputs/summaries/prvl_scaled_up.csv.gz")


ggplot(res2[mc == 2], aes(year, chd_prvl/popsize, linetype = scenario, col = agegrp)) +
  facet_wrap(~ sex) +
  geom_line()


check <- res2[year == 20]


mrtl <- fread("./outputs/summaries/dis_mrtl_scaled_up.csv.gz")

ggplot(mrtl[mc == 1 & scenario == "sc1" & sex == "men"], aes(x = year, y = chd)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_line()



dt <- fread("./outputs/lifecourse/1_lifecourse.csv.gz")

dt <- dt[pid %in% pids]

setkey(dt, pid, year)

dt1 <- dt[scenario == "Baseline"]
dt2 <- dt[scenario == "Main Scenario"]

dt1[, `:=`(scenario = NULL, wt = NULL, wt_esp = NULL)]
dt2[, `:=`(scenario = NULL, wt = NULL, wt_esp = NULL)]

identical(dt1, dt2)

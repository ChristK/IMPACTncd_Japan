
## Manuscript Figures ## ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)
library(viridis)
library(cowplot)

options(scipen = 999)

if(!file.exists(paste0("./outputs/plots/manuscript/"))){
  dir.create(paste0("./outputs/plots/manuscript/"))
}

## Figure 2: Cost-effectiveness ## ----

cea_wo <- fread("./outputs/summaries/without_SSB/cea_results.csv.gz")
cea_wo[, analysis := "only BMI-mediated effects"]

cea_w <- fread("./outputs/summaries/with_SSB/cea_results.csv.gz")
cea_w[, analysis := "incl. direct SSB effects"]

cea <- rbind(cea_w, cea_wo)

cea_agg <- cea[, lapply(.SD, sum), .SDcols = !c("analysis", "scenario", "sex", "agegrp_start", "mc"),
               by = c("scenario", "mc", "analysis")]

#TODO: Eventually extend by error bars!
cea_point <- data.table(x = cea_agg[scenario != "sc0", mean(incr_qaly_scl, na.rm = T), by = .(scenario, analysis)],
                        y = cea_agg[scenario != "sc0", mean(incr_cost_scl, na.rm = T), by = .(scenario, analysis)])

cea_point[, `:=`(scenario = x.scenario, y.scenario = NULL,
                 analysis = x.analysis, y.analysis = NULL,
                 mean_qaly = x.V1, mean_cost = y.V1)]

ggplot(cea_agg[scenario != "sc0"], aes(x = incr_qaly_scl,
                                       y = incr_cost_scl,
                                       col = scenario)) +
  facet_wrap(~ analysis) +
  geom_point(shape = 16, alpha = 0.7, size = 3) +
  geom_point(data = cea_point, aes(x = mean_qaly, y = mean_cost, col = scenario), shape = 4, size = 5, stroke = 1.5) +
  geom_point(data = cea_point, aes(x = mean_qaly, y = mean_cost), shape = 1, size = 2, col = "black") +
  geom_point(data = cea_point, aes(x = mean_qaly, y = mean_cost), shape = 4, size = 4, col = "black") +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  #expand_limits(x = -8e4, y = 2e9) +
  scale_y_continuous(name = "Incremental healthcare costs (in millions)", c(seq(0,80000000000,1000000000)*-1),
                     labels = function(y) format(y/1000000)) +
  scale_x_continuous(name = "Incremental QALYs (in thousands)", c(seq(-1000000,500000,50000)), labels = function(y) format(y/1000)) +
  scale_color_viridis_d(name = "Scenario", option = "viridis",
                       labels = c("Tax on SSBs", "Tax on SSBs + Fruit Juice",
                                  "Reformulation", "Reformulation + Reduced Consumption")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), strip.text.x = element_text(size = 15),
                     axis.line = element_line(colour = "black"),
                     axis.text=element_text(size=13), axis.title=element_text(size=15),
                     legend.position = c(0.8,0.3),
                     legend.title = element_text(size = 15), legend.text = element_text(size = 13))


ggsave("./outputs/plots/manuscript/Figure_1_cost_effectiveness_plane.tiff",
       height = 9, width = 16, dpi = 300)


## Figure 3: Cross-validation IMPACT vs PRIMEtime ## ----

## IMPACT Results

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)

# Epi Results 

impact_epi <- fread("./outputs/tables/without_SSB/cases_prev_post_by_scenario.csv")

#fwrite(impact_epi, "G:/Meine Ablage/PhD/Presentations/2022_EUPHA/impact_epi_results.csv", sep = ";", dec = ".")

impact_epi[, `:=`(model = "IMPACT NCD",
                  outcome = ifelse(disease == "diff_t2dm_prvl", "diabetes_cpp",
                                   ifelse(disease == "diff_chd_prvl", "chd_cpp",
                                          ifelse(disease == "diff_stroke_prvl", "stroke_cpp", NA))))][, `:=`(disease = NULL)]

impact_epi <- na.omit(impact_epi)

# CEA Results

impact_cea <- fread("./outputs/summaries/without_SSB/cea_results.csv.gz")

impact_cea[, model := "IMPACT NCD"]

impact_cea <- impact_cea[, lapply(.SD, sum), .SDcols = !c("model", "scenario", "sex", "agegrp_start", "mc"),
                         by = c("scenario", "mc", "model")]

impact_cea <- impact_cea[, fquantile_byid(incr_qaly_scl, prbl, id = model), keyby = "scenario"]
setnames(impact_cea, c("scenario", "model", percent(prbl, prefix = "prvl_rate_")))

impact_cea <- na.omit(impact_cea)

impact_cea[, outcome := "incr_qaly"]

impact <- rbind(impact_cea, impact_epi)

## PRIMEtime Results

prime <- fread("G:/Meine Ablage/PhD/Publications/2022_Comparative_diet_modeling/PRIMEtime CE/outputs/cea_results.csv")

prime <- melt(prime, id.vars = c("mc", "sex"))

prime <- prime[, CKutils:::fquantile_byid(value, prbl, id = as.character(variable))]
setnames(prime, c("outcome", scales:::percent(prbl, prefix = "prvl_rate_")))

#fwrite(prime, "G:/Meine Ablage/PhD/Presentations/2022_EUPHA/prime_results.csv", sep = ";", dec = ".")

prime[, model := "PRIMEtime CE"][, scenario := "sc1"]


## Combine datasets for plot

dat <- rbind(impact, prime[!outcome %in% c("incr_cost")])

dat[outcome == "incr_qaly", `:=`(`prvl_rate_2.5%` = `prvl_rate_2.5%` * -1,
                                 `prvl_rate_50.0%` = `prvl_rate_50.0%` * -1,
                                 `prvl_rate_97.5%` = `prvl_rate_97.5%` * -1)]

dat[, outcome := ifelse(outcome == "chd_cpp", "Coronary Heart Disease",
                        ifelse(outcome == "stroke_cpp", "Stroke",
                               ifelse(outcome == "incr_qaly", "Incremental QALYs",
                               "Type 2 Diabetes")))]

dat[, scenario := ifelse(scenario == "sc1", "Scenario 1",
                         ifelse(scenario == "sc2", "Scenario 2",
                                ifelse(scenario == "sc3", "Scenario 3", "Scenario 4")))]

## Combined plot for all scenarios

dodge <- position_dodge(width=0.9)

# Panel A: CHD #
sc1 <- ggplot(dat[outcome == "Coronary Heart Disease"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                                   xmin = `prvl_rate_2.5%` * -1,
                                   xmax = `prvl_rate_97.5%` * -1,
                                   by = scenario, fill = model)) +
  #facet_wrap(~ scenario, scales = "fixed", ncol = 1, nrow = 4) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Cases prevented or postponed") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("A - Coronary Heart Disease") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     strip.text.x = element_blank(), strip.background = element_blank(),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

# Panel B: Stroke #
sc2 <- ggplot(dat[outcome == "Stroke"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  #facet_wrap(~ scenario, scales = "free", ncol = 1, nrow = 4) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Cases prevented or postponed") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("B - Stroke") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     strip.text.x = element_blank(), strip.background = element_blank(),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

# Panel C: Type 2 Diabetes #
sc3 <- ggplot(dat[outcome == "Type 2 Diabetes"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  #facet_wrap(~ scenario, scales = "free", ncol = 1, nrow = 4) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Cases prevented or postponed") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("C - Type 2 Diabetes") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     strip.text.x = element_blank(), strip.background = element_blank(),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

# Panel D: QALYs #
sc4 <- ggplot(dat[outcome == "Incremental QALYs"],
              aes(y = scenario, x = `prvl_rate_50.0%` * -1,
                  xmin = `prvl_rate_2.5%` * -1,
                  xmax = `prvl_rate_97.5%` * -1,
                  by = scenario, fill = model)) +
  #facet_wrap(~ scenario, scales = "free", ncol = 1, nrow = 4) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = "dodge") +
  geom_errorbar(aes(xmin = `prvl_rate_2.5%` * -1, xmax = `prvl_rate_97.5%` * -1),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(begin = 0.1, end = 0.7) +
  scale_x_continuous(name = "Quality-adjusted life-years gained") +
  scale_y_discrete(name = NULL) +
  labs(col = "Simulation model") +
  ggtitle("D - Incremental QALYs") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.y = element_text(size = 10), axis.title = element_text(size = 10),
                     axis.ticks.y = element_blank(), plot.title = element_text(size = 16),
                     strip.text.x = element_blank(), strip.background = element_blank(),
                     legend.position = "none",
                     legend.title = element_blank(), legend.text = element_text(size = 12))

plot_grid(sc1, sc2, sc3, sc4, align = "v", ncol = 2)

ggsave("./outputs/plots/manuscript/Figure_2_cross_validation.tiff",
       height = 9, width = 12, dpi = 300)




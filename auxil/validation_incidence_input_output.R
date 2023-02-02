

#### Comparison of DISMOD incidence and model output ## ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)
theme_set(new = theme_economist())
theme_update(axis.text.x = element_text(size = 9), plot.title = element_text(hjust = 0.5))

analysis <- "calibration_test_with_SSB"

## Incidence ## ----

tt <- fread(paste0("./outputs/summaries/", analysis, "/incd_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "agegrp", "sex", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

d <- d[year == 2013 & scenario == "sc0" & disease %in% c("t2dm_prvl", "chd_prvl", "stroke_prvl")]

d[, disease := gsub("_prvl", "", disease)]

# e <- fread("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Preparation/R/output/disease_epidemiology/disease_epi_final.csv", stringsAsFactors = TRUE,
#                             select = c("age", "sex", "disease", "out_incidence_rates",
#                                        "out_prevalence_rates", "out_case_fatality_rates",
#                                        "out_duration_years"))

e <- fst::read_fst("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Preparation/R/output/disease_epidemiology/disease_epi_l.fst", as.data.table = TRUE)
e <- e[, c("age", "sex", "mc", "value_chd_incidence_rates", "value_stroke_incidence_rates", "value_diabetes_incidence_rates")]



to_agegrp(e, max_age = 100)

e <- melt(e[mc != 0], id.vars = c("sex", "age", "agegrp", "mc"))

e <- e[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("sex", "agegrp")]
setnames(e, c("sex", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))

e[, disease := gsub("_incidence_rates", "", disease)]
e[, disease := gsub("value_", "", disease)]

model_ages <- unique(d$agegrp)

e[, sex := ifelse(sex == "male", "Men", "Women")]
d[, sex := ifelse(sex == "men", "Men", "Women")]

for(i in unique(d$disease)){

j <- i

if(i == "t2dm"){j = "diabetes"}
  
ggplot(d[disease == i], aes(x = agegrp, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`,
                                    )) +
  facet_wrap(~ sex) +
  geom_errorbar() +
  geom_line() +
  geom_line(data = e[disease == j & agegrp %in% model_ages],
            aes(x = agegrp, y = `prvl_rate_50.0%`), col = "red", linetype = "dashed") +
  geom_errorbar(data = e[disease == j & agegrp %in% model_ages],
                aes(x = agegrp, ymin = `prvl_rate_2.5%`,
                ymax = `prvl_rate_97.5%`), col = "red", linetype = "dashed") +
  scale_x_discrete(name = "Age") +
  scale_y_continuous(name = "Incidence") +
  ggtitle("Comparison of input and output incidence rates") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank(), legend.position = "none")

cowplot::ggsave2(paste0("validation_i_o_incidence_", i, "_", analysis, ".png"), width = 16, height = 9, units = "cm", 
                 scale = 2, dpi = 300, path = "./outputs/plots/validation")

}

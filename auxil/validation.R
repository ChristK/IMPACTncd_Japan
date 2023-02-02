
#### Validation procedures ## ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)
library(fst)
library(stringr)
library(yardstick)

options(scipen = 999)

setwd("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT NCD Germany")

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)
theme_set(new = theme_economist())
theme_update(axis.text.x = element_text(size = 9), plot.title = element_text(hjust = 0.5))

## Load model data ##

## All-cause Mortality ## ----
#
# tt <- fread("./outputs/summaries/mrtl_scaled_up.csv.gz"
# )[, `:=` (year = year + 2000)]
#
# tt[, agegrp := fifelse(agegrp == "90-94", "90+", agegrp)]
#
# outstrata <- c("mc", "agegrp", "year", "sex")
#
# # Rate #
#
# d <- tt[scenario == "sc0", lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
# ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
# d <- melt(d, id.vars = outstrata)
# dd <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
# setnames(dd, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))
#
# ddd <- d[, lapply(.SD, mean), .SDcols = "value", by = eval(c("variable", setdiff(outstrata, "mc")))]
# setnames(ddd, "value", "mrtl_rate_Mean")
# ddd[, disease := as.character(variable)]
#
# d <- merge(dd[disease != "popsize"], ddd[variable != "popsize"], by = c("disease", "agegrp", "sex", "year"))
#
# d[, variable := NULL]
#
# impact_all_cause <- copy(d)

## Disease-specific Mortality ## ----

# WARNING: For some reason some iteration have trailing comma!
file_lines <- readLines("./outputs/summaries/with_SSB/dis_mrtl_scaled_up.csv.gz")
writeLines(gsub(",+$", "", file_lines), "./outputs/summaries/with_SSB/dis_mrtl_scaled_up.csv.gz")

tt <- fread("./outputs/summaries/with_SSB/dis_mrtl_scaled_up.csv.gz", fill = TRUE,
)[, `:=` (year = year + 2000)]

tt[, agegrp := fifelse(agegrp == "90-94", "90+", agegrp)]

outstrata <- c("mc", "agegrp", "year", "sex")

# Rate #

d <- tt[scenario == "sc0", lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
dd <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(dd, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

ddd <- d[, lapply(.SD, mean), .SDcols = "value", by = eval(c("variable", setdiff(outstrata, "mc")))]
setnames(ddd, "value", "mrtl_rate_Mean")
ddd[, disease := as.character(variable)]

d <- merge(dd[disease != "popsize"], ddd[variable != "popsize"], by = c("disease", "agegrp", "sex", "year"))

d[, variable := NULL]

impact_disease <- copy(d)

## Load original and forecast mortality data ## ----

# CODE COPIED FROM MORTALITY FORECAST PLOT SCRIPT #

age_width <- 5
agg_age <- 90
min_age <- 30
max_age <- 90
hor <- 30
agegroups <- agegrp_name(min_age = min_age, max_age = agg_age, grp_width = age_width) # Define agegroups

setwd("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Preparation/Updated Mortality Forecast/R/")

##########################
#### Read in datasets ####
##########################

dt_pop <- as.data.table(fread("./data/Japan/pop.csv")) # Population estimates 1987-2011 adjusted for census irregularities
dt_pop_proj <- fread("./data/Japan/ger_pop_proj_agg.csv") # Official population projections 2019-2050

#dt_mort_rate_ci_res <- fread("./output/mort_rate_ci.csv")
#setnames(dt_mort_rate_ci_res, c("disease", "upper", "lower"), c("cause", "deaths_hi", "deaths_low"))

dt_mrtl <- as.data.table(read_fst("./output/mortality_projections.fst")) # Mortality projections 2019-2035 and smoothed rates 1998-2018
setnames(dt_mrtl, c("mx_total_1", "mx_total_99"), c("mx_total_low", "mx_total_hi")) # See projection function


##########################################
##### Calculation of mortality rates #####
##########################################

#################################
#### Prepare population data #### <- needed as weights to get correct mortality rate in high ages
#################################

# Manipulate data for computation of death counts
# Prepare population projections 2020-2050

setnames(dt_pop_proj, "reg_grp", "reg")

dt_pop_proj <- dt_pop_proj[year > 2019]

# Prepare population counts 1991-2019
dt_pop[, sex := factor(sex, c("women", "men"), c("women", "men"))]
dt_pop[, reg := factor(
  reg_grp,
  levels = 0:3,
  labels = c("West", "East", "Overall", "Berlin"))][, reg_grp := NULL]

# Prepare combined population counts 1991-2050

dt_pop <- rbind(dt_pop, dt_pop_proj)

dt_pop[, pops := round(pops)] # Round to decimals

dt_pop[age > max_age, age := max_age] # Aggregate ages 96-100
dt_pop <- dt_pop[, lapply(.SD, sum),
                 by = .(year, age, sex, reg),
                 .SDcols = "pops"]

dt_pop <- dt_pop[age >= min_age & year >= 1991 & year <= 2019 + hor]


#### Merge Population counts and mortality rates ####

dt_mrtl_dc <- dt_mrtl[, .(year, age, sex, reg, cause, mx_total)] # Select needed variables
dt_mrtl_dc <- merge(dt_mrtl_dc, dt_pop, by = c("year", "sex", "reg", "age"))


to_agegrp(dt_mrtl_dc, grp_width = age_width, max_age = agg_age, to_factor = F)

dt_mrtl_agg <- dt_mrtl_dc[, lapply(.SD, weighted.mean, w = pops),
                          by = .(year, agegrp, sex, reg, cause),
                          .SDcols = c("mx_total")]


keys <- c("year", "agegrp", "sex", "reg", "cause")

#dt_mrtl_agg <- merge(dt_mrtl_agg, dt_mort_rate_ci_res, all.x = T, by = keys)
setnames(dt_mrtl_agg, c("mx_total"), c("mx_total_mean"))

data_fdm <- dt_mrtl_agg

data_orig <- fread("./data/Japan/ger_all_deaths_grouped.csv") # Load observed raw death counts

#data_orig[, `:=`(deaths_low = 0, deaths_hi = 0)]
setnames(data_orig, "agegroup", "agegrp")
data_orig <- data_orig[!agegrp %in% c(agegrp_name(0, min_age - 1L), "01-14")] # Exclude population below 20 years of age
#
# data_orig[, agegrp := ifelse(agegrp %in% c("30-34", "35-39"), "30-39",
#                              ifelse(agegrp %in% c("40-44", "45-49"), "40-49",
#                                     ifelse(agegrp %in% c("50-54", "55-59"), "50-59",
#                                            ifelse(agegrp %in% c("60-64", "65-69"), "60-69",
#                                                   ifelse(agegrp %in% c("70-74", "75-79"), "70-79", "80+")))))]

#data_orig[, agegrp := ifelse(agegrp %in% c("80-84", "85-89", "90+"), "80+", agegrp)]

data_orig[, deaths_other := deaths_all - deaths_chd - deaths_diab - deaths_stroke]
data_orig[, deaths_other_w_diab := deaths_all - deaths_chd - deaths_stroke]

data_orig <- data_orig[, lapply(.SD, sum),
                       by = .(year, agegrp, sex, reg),
                       .SDcols = c("pops",
                                   "deaths_all", "deaths_diab", "deaths_chd", "deaths_stroke",
                                   "deaths_other", "deaths_other_w_diab")]

data_orig[, `:=`(mx_all = deaths_all/pops,
                 mx_diab = deaths_diab/pops,
                 mx_chd = deaths_chd/pops,
                 mx_stroke = deaths_stroke/pops,
                 mx_other = deaths_other/pops,
                 mx_other_w_diab = deaths_other_w_diab/pops)]

#data_orig[, `:=`(mx_total_hi = as.numeric(NA),
#                 mx_total_low = as.numeric(NA))]

## Prepare data for plots ##

# Functional Demographic Model #

data_fdm <- data_fdm[reg == "Overall" & cause %in% c("all", "stroke", "chd", "other")]
data_fdm[, `:=`(reg = NULL, disease = fifelse(cause == "all", "all_cause_mrtl", cause))][, cause := NULL]

data_fdm_new_all <- data_fdm[disease != "all_cause_mrtl"]

# Save FDM mortality rate as baseline for calibration
write_fst(data_fdm_new_all, "G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT NCD Germany/inputs/mortality/mort_prcjt.fst")

data_fdm_new_all <- data_fdm_new_all[, lapply(.SD, sum), .SDcols = "mx_total_mean", by = .(year, agegrp, sex)]
data_fdm_new_all[, disease := "all_cause_mrtl_recalc"]

data_fdm <- rbind(data_fdm, data_fdm_new_all)

data_orig <- data_orig[reg == "Overall"]
data_orig <- melt(data_orig, measure.vars = c("mx_all", "mx_diab", "mx_chd", "mx_stroke", "mx_other", "mx_other_w_diab"),
                                              id.vars = c("year", "agegrp", "sex"))
data_orig[, `:=`(disease = fifelse(variable == "mx_all", "all_cause_mrtl", gsub("mx_", "", variable)),
                 mx_total_mean = value)][, `:=`(variable = NULL, value = NULL)]





### Plots for  validation ###

setwd("G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Model/IMPACT NCD Germany")

## Non-modelled mortality (without deaths from diabetes included in non-modelled) ##

# Men #

ggplot(data_orig[sex == "men" & disease == "other"],
       aes(x = year, y = mx_total_mean)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_point() +
  geom_line(data = impact_disease[sex == "men" & disease == "nonmodelled"],
            aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
  geom_line(data = data_fdm[sex == "men" & disease == "other"],
            aes(x = year, y = mx_total_mean), col = "red") +
  geom_ribbon(data = impact_disease[sex == "men" & disease == "nonmodelled"],
              aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
              fill = "blue", alpha = 0.3) +
  ggtitle("Non-modelled (men)")

# ggsave("./outputs/plots/validation/validation_nonmodelled_mortality_men.tiff",
#        height = 9, width = 16)
ggsave("./outputs/plots/validation/validation_nonmodelled_mortality_men.jpeg",
       height = 9, width = 16)

# Women #

ggplot(data_orig[sex == "women" & disease == "other"],
       aes(x = year, y = mx_total_mean)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_point() +
  geom_line(data = impact_disease[sex == "women" & disease == "nonmodelled"],
            aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
  geom_line(data = data_fdm[sex == "women" & disease == "other"],
            aes(x = year, y = mx_total_mean), col = "red") +
  geom_ribbon(data = impact_disease[sex == "women" & disease == "nonmodelled"],
              aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
              fill = "blue", alpha = 0.3) +
  ggtitle("Non-modelled (women)")


# ggsave("./outputs/plots/validation/validation_nonmodelled_mortality_women.tiff",
#        height = 9, width = 16)
ggsave("./outputs/plots/validation/validation_nonmodelled_mortality_women.jpeg",
       height = 9, width = 16)

## Non-modelled mortality (with deaths from diabetes included in non-modelled) ##

# Men #

ggplot(data_orig[sex == "men" & disease == "other_w_diab"],
       aes(x = year, y = mx_total_mean)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_point() +
  geom_line(data = impact_disease[sex == "men" & disease == "nonmodelled"],
            aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
  geom_line(data = data_fdm[sex == "men" & disease == "other"],
            aes(x = year, y = mx_total_mean), col = "red") +
  geom_ribbon(data = impact_disease[sex == "men" & disease == "nonmodelled"],
              aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
              fill = "blue", alpha = 0.3) +
  ggtitle("Non-modelled (men)")

# ggsave("./outputs/plots/validation/validation_nonmodelled_w_diab_mortality_men.tiff",
#        height = 9, width = 16)
ggsave("./outputs/plots/validation/validation_nonmodelled_w_diab_mortality_men.jpeg",
       height = 9, width = 16)

# Women #

ggplot(data_orig[sex == "women" & disease == "other_w_diab"],
       aes(x = year, y = mx_total_mean)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_point() +
  geom_line(data = impact_disease[sex == "women" & disease == "nonmodelled"],
            aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
  geom_line(data = data_fdm[sex == "women" & disease == "other"],
            aes(x = year, y = mx_total_mean), col = "red") +
  geom_ribbon(data = impact_disease[sex == "women" & disease == "nonmodelled"],
              aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
              fill = "blue", alpha = 0.3) +
  ggtitle("Non-modelled (women)")


# ggsave("./outputs/plots/validation/validation_nonmodelled_w_diab_mortality_women.tiff",
#        height = 9, width = 16)
ggsave("./outputs/plots/validation/validation_nonmodelled_w_diab_mortality_women.jpeg",
       height = 9, width = 16)

# ## All-cause mortality ##
#
# # Men #
#
# ggplot(data_orig[sex == "men" & disease == "all_cause_mrtl"],
#        aes(x = year, y = mx_total_mean)) +
#   facet_wrap(~ agegrp, scales = "free") +
#   geom_point() +
#   geom_line(data = impact_all_cause[sex == "men" & disease == "all_cause_mrtl"],
#             aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
#   geom_line(data = data_fdm[sex == "men" & disease == "all_cause_mrtl_recalc"],
#             aes(x = year, y = mx_total_mean), col = "red") +
#   geom_ribbon(data = impact_all_cause[sex == "men" & disease == "all_cause_mrtl"],
#               aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
#               fill = "blue", alpha = 0.3) +
#   ggtitle("All-cause mortality (men)")
#
# # ggsave("./outputs/plots/validation/validation_all_cause_mortality_men.tiff",
# #        height = 9, width = 16)
# ggsave("./outputs/plots/validation/validation_all_cause_mortality_men.jpeg",
#        height = 9, width = 16)
#
# # Women #
#
# ggplot(data_orig[sex == "women" & disease == "all_cause_mrtl"],
#        aes(x = year, y = mx_total_mean)) +
#   facet_wrap(~ agegrp, scales = "free") +
#   geom_point() +
#   geom_line(data = impact_all_cause[sex == "women" & disease == "all_cause_mrtl"],
#             aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
#   geom_line(data = data_fdm[sex == "women" & disease == "all_cause_mrtl_recalc"],
#             aes(x = year, y = mx_total_mean), col = "red") +
#   geom_ribbon(data = impact_all_cause[sex == "women" & disease == "all_cause_mrtl"],
#               aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
#               fill = "blue", alpha = 0.3) +
#   ggtitle("All-cause mortality (women)")
#
# # ggsave("./outputs/plots/validation/validation_all_cause_mortality_women.tiff",
# #        height = 9, width = 16)
# ggsave("./outputs/plots/validation/validation_all_cause_mortality_women.jpeg",
#        height = 9, width = 16)

## Disease-specific mortality ##

## CHD ##

# Men #

ggplot(data_orig[sex == "men" & disease == "chd"],
       aes(x = year, y = mx_total_mean)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_point() +
  geom_line(data = impact_disease[sex == "men" & disease == "chd"],
            aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
  geom_line(data = data_fdm[sex == "men" & disease == "chd"],
            aes(x = year, y = mx_total_mean), col = "red") +
  geom_ribbon(data = impact_disease[sex == "men" & disease == "chd"],
              aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
              fill = "blue", alpha = 0.3) +
  ggtitle("CHD mortality (men)")

# ggsave("./outputs/plots/validation/validation_chd_mortality_men.tiff",
#        height = 9, width = 16)
ggsave("./outputs/plots/validation/validation_chd_mortality_men.jpeg",
       height = 9, width = 16)

# Women #

ggplot(data_orig[sex == "women" & disease == "chd"],
       aes(x = year, y = mx_total_mean)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_point() +
  geom_line(data = impact_disease[sex == "women" & disease == "chd"],
            aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
  geom_line(data = data_fdm[sex == "women" & disease == "chd"],
            aes(x = year, y = mx_total_mean), col = "red") +
  geom_ribbon(data = impact_disease[sex == "women" & disease == "chd"],
              aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
              fill = "blue", alpha = 0.3) +
  ggtitle("CHD mortality (women)")

# ggsave("./outputs/plots/validation/validation_chd_mortality_women.tiff",
#        height = 9, width = 16)
ggsave("./outputs/plots/validation/validation_chd_mortality_women.jpeg",
       height = 9, width = 16)


## Stroke ##

# Men #

ggplot(data_orig[sex == "men" & disease == "stroke"],
       aes(x = year, y = mx_total_mean)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_point() +
  geom_line(data = impact_disease[sex == "men" & disease == "stroke"],
            aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
  geom_line(data = data_fdm[sex == "men" & disease == "stroke"],
            aes(x = year, y = mx_total_mean), col = "red") +
  geom_ribbon(data = impact_disease[sex == "men" & disease == "stroke"],
              aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
              fill = "blue", alpha = 0.3) +
  ggtitle("Stroke mortality (men)")

# ggsave("./outputs/plots/validation/validation_stroke_mortality_men.tiff",
#        height = 9, width = 16)
ggsave("./outputs/plots/validation/validation_stroke_mortality_men.jpeg",
       height = 9, width = 16)

# Women #

ggplot(data_orig[sex == "women" & disease == "stroke"],
       aes(x = year, y = mx_total_mean)) +
  facet_wrap(~ agegrp, scales = "free") +
  geom_point() +
  geom_line(data = impact_disease[sex == "women" & disease == "stroke"],
            aes(x = year, y = `mrtl_rate_50.0%`), col = "blue") +
  geom_line(data = data_fdm[sex == "women" & disease == "stroke"],
            aes(x = year, y = mx_total_mean), col = "red") +
  geom_ribbon(data = impact_disease[sex == "women" & disease == "stroke"],
              aes(x = year, y = `mrtl_rate_50.0%`, ymin = `mrtl_rate_2.5%`, ymax= `mrtl_rate_97.5%`),
              fill = "blue", alpha = 0.3) +
  ggtitle("Stroke mortality (women)")

# ggsave("./outputs/plots/validation/validation_stroke_mortality_women.tiff",
#        height = 9, width = 16)
ggsave("./outputs/plots/validation/validation_stroke_mortality_women.jpeg",
       height = 9, width = 16)



#### Calculate ratio between FDM and IMPACT ####

impact <- impact_disease

impact[, mx_total_median_IMPACT := `mrtl_rate_50.0%`]
impact[, mx_total_mean_IMPACT := mrtl_rate_Mean]

data_fdm[, disease := ifelse(disease == "other", "nonmodelled", disease)]

dt <- merge(impact, data_fdm, all.y = TRUE)

dt[, mx_mean_ratio := mx_total_mean_IMPACT/mx_total_mean]
dt[, mx_median_ratio := mx_total_median_IMPACT/mx_total_mean]

dt[is.na(mx_mean_ratio), mx_mean_ratio := 1]
dt[is.na(mx_median_ratio), mx_median_ratio := 1]

for(i in unique(dt$disease)){

  ## Using the mean ##

  ggplot(dt[sex == "women" & disease == i],
         aes(x = year, y = mx_mean_ratio)) +
    facet_wrap(~ agegrp) +
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    ggtitle(paste0("Mortality calibration ", i," (women)"))

  # ggsave(paste0("./outputs/plots/validation/validation_", i, "_mortality_ratio_women_mean.tiff"),
  #        height = 9, width = 16)
  ggsave(paste0("./outputs/plots/validation/validation_", i, "_mortality_ratio_women_mean.jpeg"),
         height = 9, width = 16)

  ggplot(dt[sex == "men" & disease == i],
         aes(x = year, y = mx_mean_ratio)) +
    facet_wrap(~ agegrp) +
    geom_line() +
    geom_hline(yintercept = 1, linetype = "dashed") +
    ggtitle(paste0("Mortality calibration ", i," (men)"))

  # ggsave(paste0("./outputs/plots/validation/validation_", i, "_mortality_ratio_men_mean.tiff"),
  #        height = 9, width = 16)
  ggsave(paste0("./outputs/plots/validation/validation_", i, "_mortality_ratio_men_mean.jpeg"),
         height = 9, width = 16)


  ## Using the median ##

  # ggplot(dt[sex == "women" & disease == i],
  #        aes(x = year, y = mx_median_ratio)) +
  #   facet_wrap(~ agegrp) +
  #   geom_line() +
  #   geom_hline(yintercept = 1, linetype = "dashed") +
  #   ggtitle(paste0("Mortality calibration ", i," (women)"))
  #
  # ggsave(paste0("./outputs/plots/validation_", i, "_mortality_ratio_women_median.tiff"),
  #        height = 9, width = 16)
  # ggsave(paste0("./outputs/plots/validation_", i, "_mortality_ratio_women_median.jpeg"),
  #        height = 9, width = 16)
  #
  # ggplot(dt[sex == "men" & disease == i],
  #        aes(x = year, y = mx_median_ratio)) +
  #   facet_wrap(~ agegrp) +
  #   geom_line() +
  #   geom_hline(yintercept = 1, linetype = "dashed") +
  #   ggtitle(paste0("Mortality calibration ", i," (men)"))
  #
  # ggsave(paste0("./outputs/plots/validation_", i, "_mortality_ratio_men_median.tiff"),
  #        height = 9, width = 16)
  # ggsave(paste0("./outputs/plots/validation_", i, "_mortality_ratio_men_median.jpeg"),
  #        height = 9, width = 16)

}






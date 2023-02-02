
#### Validation of IMPACT NCD results with external epi sources ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)
library(cowplot)

new <- theme_minimal()
theme_set(new)

external_path <- "G:/Meine Ablage/PhD/Publications/2021_Diet_simulation_modeling_Germany/Preparation/data/disease_data/"

if(!file.exists(paste0("./outputs/plots/validation/"))){
  dir.create(paste0("./outputs/plots/validation/"))
}

## Coronary Heart Disease ----

# Prevalence

impact_chd_prev <- fread("./outputs/tables/with_SSB/prevalence_rate_by_year_agegrp_sex.csv")[
  scenario == "sc0" & disease == "chd_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_chd_prev <- fread(paste0(external_path, "chd/ger_chd_data_combined.csv"))[
  study %in% c("GEDA", "KV"),
  c("year", "agegroup", "sex", "prevalence", "comment", "study")
]

ext_chd_prev[, `:=`(`prvl_rate_50.0%` = prevalence/100,
                    `prvl_rate_2.5%` = 0,
                    `prvl_rate_97.5%` = 0,
                    sex = ifelse(sex == "male", "men", "women"),
                    agegrp = agegroup)][, c("prevalence", "agegroup") := NULL]

prev_dat <- rbind(impact_chd_prev, ext_chd_prev)

prev_dat[, sex := ifelse(sex == "women", "Women", "Men")]

# Source: GEDA

# Year 2014

ext1 <- ggplot(prev_dat[study == "GEDA" & year == 2014]) +
               aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
               facet_wrap(~ sex, scales = "fixed") +
               geom_bar(stat = "identity", position = "dodge") + 
               scale_fill_viridis_d(name = "Age group") +
               scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
               ggtitle("CHD prevalence in Germany from GEDA 2014") + 
               ylab("12-month cumulative prevlence") +
               xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2019

ext2 <- ggplot(prev_dat[study == "GEDA" & year == 2019]) +
               aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
               facet_wrap(~ sex, scales = "fixed") +
               geom_bar(stat = "identity", position = "dodge") +
               scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
                   ggtitle("CHD prevalence in Germany from GEDA 2019") + 
               ylab("12-month cumulative prevlence") +
               xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2014

imp1 <- ggplot(prev_dat[study == "IMPACT" & year == 2014]) +
               aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
               facet_wrap(~ sex, scales = "fixed") +
               geom_bar(stat = "identity", position = "dodge") + 
               geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                             position = dodge, width = 0.2) +
               scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
                   ggtitle("CHD prevalence in Germany from IMPACT NCD for 2014") + 
               ylab("12-month prevlence rate") +
               xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2019

imp2 <- ggplot(prev_dat[study == "IMPACT" & year == 2019]) +
               aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
               facet_wrap(~ sex, scales = "fixed") +
               geom_bar(stat = "identity", position = "dodge") +
               geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                             position = dodge, width = 0.2) +
               scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
                   ggtitle("CHD prevalence in Germany from IMPACT NCD for 2019") + 
               ylab("12-month prevlence rate") +
               xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_chd_prev_2014.tiff",
       height = 9, width = 12, dpi = 300)


plot_grid(ext2, imp2, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_chd_prev_2019.tiff",
       height = 9, width = 12, dpi = 300)




## Stroke ----

# Prevalence

impact_stroke_prev <- fread("./outputs/tables/with_SSB/prevalence_rate_by_year_agegrp_sex.csv")[
  scenario == "sc0" & disease == "stroke_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_stroke_prev <- fread(paste0(external_path, "stroke/ger_stroke_data_combined.csv"))[
  study %in% c("GEDA", "AOK"),
  c("year", "agegroup", "sex", "prevalence", "comment", "study")
]

ext_stroke_prev[, `:=`(`prvl_rate_50.0%` = prevalence/100,
                    `prvl_rate_2.5%` = 0,
                    `prvl_rate_97.5%` = 0,
                    sex = ifelse(sex == "male", "men", "women"),
                    agegrp = agegroup)][, c("prevalence", "agegroup") := NULL]

prev_dat <- rbind(impact_stroke_prev, ext_stroke_prev)

prev_dat[, sex := ifelse(sex == "women", "Women", "Men")]

# Source: GEDA

# Year 2014

ext1 <- ggplot(prev_dat[study == "GEDA" & year == 2014]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("Stroke prevalence in Germany from GEDA 2014") + 
  ylab("12-month cumulative prevlence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2019

ext2 <- ggplot(prev_dat[study == "GEDA" & year == 2019]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("Stroke prevalence in Germany from GEDA 2019") + 
  ylab("12-month cumulative prevlence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())


# Source: AOK

# Year 2010

ext3 <- ggplot(prev_dat[study == "AOK" & year == 2010]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("Stroke prevalence in Germany from AOK 2010") + 
  ylab("12-month cumulative prevlence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2011

ext4 <- ggplot(prev_dat[study == "AOK" & year == 2011]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("Stroke prevalence in Germany from AOK 2011") + 
  ylab("12-month cumulative prevlence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())


# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2014

imp1 <- ggplot(prev_dat[study == "IMPACT" & year == 2014]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("Stroke prevalence in Germany from IMPACT NCD for 2014") + 
  ylab("12-month prevlence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2019

imp2 <- ggplot(prev_dat[study == "IMPACT" & year == 2019]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("Stroke prevalence in Germany from IMPACT NCD for 2019") + 
  ylab("12-month prevlence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2013

imp3 <- ggplot(prev_dat[study == "IMPACT" & year == 2013]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05)) +
  expand_limits(y = c(0, 0.3)) +
  ggtitle("Stroke prevalence in Germany from IMPACT NCD for 2013") + 
  ylab("12-month prevlence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_stroke_prev_2014.tiff",
       height = 9, width = 12, dpi = 300)

plot_grid(ext2, imp2, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_stroke_prev_2019.tiff",
       height = 9, width = 12, dpi = 300)

plot_grid(ext3, imp3, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_stroke_prev_2010.tiff",
       height = 9, width = 12, dpi = 300)

plot_grid(ext4, imp3, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_stroke_prev_2011.tiff",
       height = 9, width = 12, dpi = 300)


# Incidence

impact_stroke_incd <- fread("./outputs/tables/with_SSB/incidence_rate_by_year_agegrp_sex.csv")[
  scenario == "sc0" & disease == "stroke_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_stroke_incd <- fread(paste0(external_path, "stroke/ger_stroke_data_combined.csv"))[
  study %in% c("GEDA", "AOK"),
  c("year", "agegroup", "sex", "incidence", "comment", "study")
]

ext_stroke_incd[, `:=`(`prvl_rate_50.0%` = incidence/100,
                       `prvl_rate_2.5%` = 0,
                       `prvl_rate_97.5%` = 0,
                       sex = ifelse(sex == "male", "men", "women"),
                       agegrp = agegroup)][, c("incidence", "agegroup") := NULL]

incd_dat <- rbind(impact_stroke_incd, ext_stroke_incd)

incd_dat[, sex := ifelse(sex == "women", "Women", "Men")]

# Source: AOK

# Year 2010

ext1 <- ggplot(incd_dat[study == "AOK" & year == 2010]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("Stroke incidence in Germany from AOK 2010") + 
  ylab("12-month incidence prevlence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2011

ext2 <- ggplot(incd_dat[study == "AOK" & year == 2011]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("Stroke incidence in Germany from AOK 2011") + 
  ylab("12-month cumulative incidence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())


# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2013

imp1 <- ggplot(incd_dat[study == "IMPACT" & year == 2013]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("Stroke incidence in Germany from IMPACT NCD for 2013") + 
  ylab("12-month incidence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_stroke_incd_2010.tiff",
       height = 9, width = 12, dpi = 300)

plot_grid(ext2, imp1, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_stroke_incd_2011.tiff",
       height = 9, width = 12, dpi = 300)


## Type 2 Diabetes ----

# Prevalence

impact_t2dm_prev <- fread("./outputs/tables/with_SSB/prevalence_rate_by_year_agegrp_sex.csv")[
  scenario == "sc0" & disease == "t2dm_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_t2dm_prev <- fread(paste0(external_path, "diabetes/ger_diabetes_data_combined.csv"))[
  study %in% c("GEDA", "Diab-Surv") & comment == "12-month",
  c("year", "agegroup", "sex", "prevalence", "comment", "study")
]

ext_t2dm_prev[, `:=`(`prvl_rate_50.0%` = prevalence/100,
                       `prvl_rate_2.5%` = 0,
                       `prvl_rate_97.5%` = 0,
                       sex = ifelse(sex == "male", "men", "women"),
                       agegrp = agegroup)][, c("prevalence", "agegroup") := NULL]

prev_dat <- rbind(impact_t2dm_prev, ext_t2dm_prev)

prev_dat[, sex := ifelse(sex == "women", "Women", "Men")]

# Source: GEDA

# Year 2014

ext1 <- ggplot(prev_dat[study == "GEDA" & year == 2014]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("Type 2 diabetes prevalence in Germany from GEDA 2014") + 
  ylab("12-month cumulative prevlence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2019

ext2 <- ggplot(prev_dat[study == "GEDA" & year == 2019]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("Type 2 diabetes prevalence in Germany from GEDA 2019") + 
  ylab("12-month cumulative prevlence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Source: Diabetes Surveillance

# Year 2011

ext3 <- ggplot(prev_dat[study == "Diab-Surv" & year == 2011]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("Type 2 diabetes prevalence in Germany from the national surveillance 2011") + 
  ylab("12-month cumulative prevlence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2014

imp1 <- ggplot(prev_dat[study == "IMPACT" & year == 2014]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("Type 2 diabetes prevalence in Germany from IMPACT NCD for 2014") + 
  ylab("12-month prevlence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2019

imp2 <- ggplot(prev_dat[study == "IMPACT" & year == 2019]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("Type 2 diabetes prevalence in Germany from IMPACT NCD for 2019") + 
  ylab("12-month prevlence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Year 2013

imp3 <- ggplot(prev_dat[study == "IMPACT" & year == 2013]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  expand_limits(y = c(0, 0.5)) +
  ggtitle("Type 2 diabetes prevalence in Germany from IMPACT NCD for 2013") + 
  ylab("12-month prevlence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_t2dm_prev_2014.tiff",
       height = 9, width = 12, dpi = 300)

plot_grid(ext2, imp2, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_t2dm_prev_2019.tiff",
       height = 9, width = 12, dpi = 300)

plot_grid(ext3, imp3, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_t2dm_prev_2013.tiff",
       height = 9, width = 12, dpi = 300)


# Incidence

impact_t2dm_incd <- fread("./outputs/tables/with_SSB/incidence_rate_by_year_agegrp_sex.csv")[
  scenario == "sc0" & disease == "t2dm_prvl" & year <= 2020,
  c("year", "agegrp", "sex", "prvl_rate_50.0%", "prvl_rate_2.5%", "prvl_rate_97.5%")
][, `:=`(study = "IMPACT", comment = "12-month no recurrence")]

ext_t2dm_incd <- fread(paste0(external_path, "diabetes/ger_diabetes_data_combined.csv"))[
  study %in% c("GEDA", "Diab-Surv") & comment == "12-month",
  c("year", "agegroup", "sex", "incidence", "comment", "study")
]

ext_t2dm_incd[, `:=`(`prvl_rate_50.0%` = incidence/100,
                     `prvl_rate_2.5%` = 0,
                     `prvl_rate_97.5%` = 0,
                     sex = ifelse(sex == "male", "men", "women"),
                     agegrp = agegroup)][, c("incidence", "agegroup") := NULL]

incd_dat <- rbind(impact_t2dm_incd, ext_t2dm_incd)

incd_dat[, sex := ifelse(sex == "women", "Women", "Men")]

# Source: Diabetes Surveillance

# Year 2011

ext1 <- ggplot(incd_dat[study == "Diab-Surv" & year == 2011]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("Type 2 diabetes incidence in Germany from the national surveillance 2011") + 
  ylab("12-month cumulative incidence") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

# Source: IMPACT

dodge <- position_dodge(width=0.9)

# Year 2013

imp1 <- ggplot(incd_dat[study == "IMPACT" & year == 2013]) +
  aes(x = agegrp, y = `prvl_rate_50.0%`, fill = agegrp) +
  facet_wrap(~ sex, scales = "fixed") +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = `prvl_rate_2.5%`, ymax = `prvl_rate_97.5%`),
                position = dodge, width = 0.2) +
  scale_fill_viridis_d(name = "Age group") +
  scale_y_continuous(breaks = seq(0, 0.05, 0.025)) +
  expand_limits(y = c(0, 0.05)) +
  ggtitle("Type 2 diabetes incidence in Germany from IMPACT NCD for 2013") + 
  ylab("12-month incidence rate") +
  xlab("Age group") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())


plot_grid(ext1, imp1, align = "h", ncol = 1)

ggsave("./outputs/plots/validation/validation_ext_t2dm_incd_2011.tiff",
       height = 9, width = 12, dpi = 300)


















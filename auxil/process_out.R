

#### Process uncertainty outputs ## ----

library(data.table)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)

## SET ANALYSIS + IN AND OUT PATHS BEFORE USE ##

analysis <- "with_SSB"

if(!file.exists(paste0("./outputs/summaries/", analysis, "/"))){
  dir.create(paste0("./outputs/summaries/", analysis, "/"))
}

# Input path for IMPACT results
in_path <- paste0("./outputs/summaries/", analysis, "/")

# Output path for tables
out_path_tables <- paste0("./outputs/tables/", analysis, "/")

# Output path for plots
out_path_plots <- paste0("./outputs/plots/", analysis, "/")

plot_format <- "tiff"

theme_set(new = theme_economist())
theme_update(axis.text.x = element_text(size = 9), plot.title = element_text(hjust = 0.5))

## SET QUANTILES FOR EXPORT ##

prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)


## Prevalence by age and sex ## ----

tt <- fread(paste0(in_path, "prvl_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "sex", "agegrp", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "prevalence_rate_by_year_agegrp_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`,
                                    colour = scenario, fill = scenario,
                                    linetype = agegrp)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_rate_by_year_agegrp_sex.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex", "agegrp")]
  setnames(dd, c("year", "sex", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "prevalence_rate_diff_by_year_agegrp_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`,
                  colour = scenario, fill = scenario,
                  linetype = agegrp)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_rate_diff_by_year_agegrp_sex.", plot_format),
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "prevalence_numbers_by_year_agegrp_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                    linetype = agegrp)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_numbers_by_year_agegrp_sex.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex", "agegrp")]
  setnames(dd, c("year", "sex", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "prevalence_numbers_diff_by_year_agegrp_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = agegrp)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_numbers_diff_by_year_agegrp_sex.", plot_format),
       height = 9, width = 16)


# Case-years prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex", "agegrp"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "sex", "agegrp", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("sex", "agegrp")]
  setnames(dd, c("sex", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "case_years_prev_post_by_scenario_agegrp_sex.csv"), sep = ";")


## Prevalence by sex ## ----

tt <- fread(paste0(in_path, "prvl_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "sex", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "prevalence_rate_by_year_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`,
                  colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_rate_by_year_sex.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "prevalence_rate_diff_by_year_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`,
                  colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_rate_diff_by_year_sex.", plot_format),
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "prevalence_numbers_by_year_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_numbers_by_year_sex.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "prevalence_numbers_diff_by_year_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_numbers_diff_by_year_sex.", plot_format),
       height = 9, width = 16)


# Case-years prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "sex", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = "sex"]
  setnames(dd, c("sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "case_years_prev_post_by_scenario_sex.csv"), sep = ";")


## Prevalence by age ## ----

tt <- fread(paste0(in_path, "prvl_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "agegrp", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "prevalence_rate_by_year_agegrp.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`,
                                    colour = scenario, fill = scenario,
                                    linetype = agegrp)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_rate_by_year_agegrp.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp")]
  setnames(dd, c("year", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "prevalence_rate_diff_by_year_agegrp.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`,
                  colour = scenario, fill = scenario,
                  linetype = agegrp)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_rate_diff_by_year_agegrp.", plot_format),
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "prevalence_numbers_by_year_agegrp.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                    linetype = agegrp)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_numbers_by_year_agegrp.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp")]
  setnames(dd, c("year", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "prevalence_numbers_diff_by_year_agegrp.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = agegrp)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_numbers_diff_by_year_agegrp.", plot_format),
       height = 9, width = 16)


# Case-years prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "agegrp", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = "agegrp"]
  setnames(dd, c("agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "case_years_prev_post_by_scenario_agegrp.csv"), sep = ";")


## Prevalence total ## ----

tt <- fread(paste0(in_path, "prvl_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "prevalence_rate_by_year.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`,
                                    colour = scenario, fill = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_rate_by_year.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year")]
  setnames(dd, c("year", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "prevalence_rate_diff_by_year.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`,
                  colour = scenario, fill = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_rate_diff_by_year.", plot_format),
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "prevalence_numbers_by_year.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Prevalence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_numbers_by_year.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year")]
  setnames(dd, c("year", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "prevalence_numbers_diff_by_year.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in prevalence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "prevalence_numbers_diff_by_year.", plot_format),
       height = 9, width = 16)


# Case-years prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable))]
  setnames(dd, c("disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "case_years_prev_post_by_scenario.csv"), sep = ";")


## Incidence by age and sex ## ----

tt <- fread(paste0(in_path, "incd_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "sex", "agegrp", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "incidence_rate_by_year_agegrp_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                    linetype = agegrp)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_rate_by_year_agegrp_sex.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex", "agegrp")]
  setnames(dd, c("year", "sex", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "incidence_rate_diff_by_year_agegrp_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = agegrp)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_rate_diff_by_year_agegrp_sex.", plot_format),
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "incidence_numbers_by_year_agegrp_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                    linetype = agegrp)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_numbers_by_year_agegrp_sex.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex", "agegrp")]
  setnames(dd, c("year", "sex", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "incidence_numbers_diff_by_year_agegrp_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = agegrp)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_numbers_diff_by_year_agegrp_sex.", plot_format),
       height = 9, width = 16)

# Cases prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex", "agegrp"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "sex", "agegrp", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("sex", "agegrp")]
  setnames(dd, c("sex", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "cases_prev_post_by_scenario_agegrp_sex.csv"), sep = ";")


## Incidence by sex ## ----

tt <- fread(paste0(in_path, "incd_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "sex", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "incidence_rate_by_year_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                    linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_rate_by_year_sex.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "incidence_rate_diff_by_year_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_rate_diff_by_year_sex.", plot_format),
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "incidence_numbers_by_year_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                    linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_numbers_by_year_sex.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "incidence_numbers_diff_by_year_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = sex)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_numbers_diff_by_year_sex.", plot_format),
       height = 9, width = 16)

# Cases prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "sex", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = "sex"]
  setnames(dd, c("sex", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "cases_prev_post_by_scenario_sex.csv"), sep = ";")


## Incidence by age ## ----

tt <- fread(paste0(in_path, "incd_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "agegrp", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "incidence_rate_by_year_agegrp.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                    linetype = agegrp)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_rate_by_year_agegrp.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp")]
  setnames(dd, c("year", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "incidence_rate_diff_by_year_agegrp.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = agegrp)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_rate_diff_by_year_agegrp.", plot_format),
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "incidence_numbers_by_year_agegrp.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                                    ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                                    linetype = agegrp)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_numbers_by_year_agegrp.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp")]
  setnames(dd, c("year", "agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "incidence_numbers_diff_by_year_agegrp.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario,
                  linetype = agegrp)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_numbers_diff_by_year_agegrp.", plot_format),
       height = 9, width = 16)

# Cases prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "agegrp", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = "agegrp"]
  setnames(dd, c("agegrp", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "cases_prev_post_by_scenario_agegrp.csv"), sep = ";")


## Incidence total ## ----

tt <- fread(paste0(in_path, "incd_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]
tt[, grep("cms", names(tt), value = TRUE) := NULL]

outstrata <- c("mc", "year", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "incidence_rate_by_year.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_rate_by_year.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year")]
  setnames(dd, c("year", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "incidence_rate_diff_by_year.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_rate_diff_by_year.", plot_format),
       height = 9, width = 16)

# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_")))

fwrite(d, paste0(out_path_tables, "incidence_numbers_by_year.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Incidence numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_numbers_by_year.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year")]
  setnames(dd, c("year", "disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "incidence_numbers_diff_by_year.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `prvl_rate_50.0%`, ymin = `prvl_rate_2.5%`,
                  ymax = `prvl_rate_97.5%`, colour = scenario, fill = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Prevalence") +
  ggtitle("Difference in incidence numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "incidence_numbers_diff_by_year.", plot_format),
       height = 9, width = 16)

# Cases prevented or postponed #

d <- tt[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)]

prvls <- grep("_prvl", names(d), value = TRUE)

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-1]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-1])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year"))
  
  dd <- dd[, lapply(.SD, sum), .SDcols = "value", by = c("mc", "variable")]
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable))]
  setnames(dd, c("disease", percent(prbl, prefix = "prvl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "cases_prev_post_by_scenario.csv"), sep = ";")


## Disease-specific Mortality by age and sex ## ----

# WARNING: For some reason some iterations have a trailing comma!
file_lines <- readLines(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))
writeLines(gsub(",+$", "", file_lines), paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))

tt <- fread(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"), fill = TRUE,
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "agegrp", "sex", "year", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "disease_mortality_rate_by_year_age_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_rate_by_year_age_sex.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp", "sex")]
  setnames(dd, c("year", "agegrp", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "disease_mortality_rate_diff_by_year_age_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_rate_diff_by_year_age_sex.", plot_format),
       height = 9, width = 16)


# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "disease_mortality_numbers_by_year_age_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_numbers_by_year_age_sex.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp", "sex")]
  setnames(dd, c("year", "agegrp", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "disease_mortality_numbers_diff_by_year_age_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_numbers_diff_by_year_age_sex.", plot_format),
       height = 9, width = 16)


## Disease-specific Mortality by sex ## ----

# WARNING: For some reason some iterations have a trailing comma!
file_lines <- readLines(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))
writeLines(gsub(",+$", "", file_lines), paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))

tt <- fread(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"), fill = TRUE,
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "sex", "year", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "disease_mortality_rate_by_year_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_rate_by_year_sex.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "disease_mortality_rate_diff_by_year_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_rate_diff_by_year_sex.", plot_format),
       height = 9, width = 16)


# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "disease_mortality_numbers_by_year_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_numbers_by_year_sex.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "sex")]
  setnames(dd, c("year", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "disease_mortality_numbers_diff_by_year_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_numbers_diff_by_year_sex.", plot_format),
       height = 9, width = 16)


## Disease-specific Mortality by age ## ----

# WARNING: For some reason some iterations have a trailing comma!
file_lines <- readLines(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))
writeLines(gsub(",+$", "", file_lines), paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))

tt <- fread(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"), fill = TRUE,
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "agegrp", "year", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "disease_mortality_rate_by_year_age.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_rate_by_year_age.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp")]
  setnames(dd, c("year", "agegrp", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "disease_mortality_rate_diff_by_year_age.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_rate_diff_by_year_age.", plot_format),
       height = 9, width = 16)


# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "disease_mortality_numbers_by_year_age.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_numbers_by_year_age.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp")]
  setnames(dd, c("year", "agegrp", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "disease_mortality_numbers_diff_by_year_age.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_numbers_diff_by_year_age.", plot_format),
       height = 9, width = 16)


## Disease-specific Mortality total ## ----

# WARNING: For some reason some iterations have a trailing comma!
file_lines <- readLines(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))
writeLines(gsub(",+$", "", file_lines), paste0(in_path, "dis_mrtl_scaled_up.csv.gz"))

tt <- fread(paste0(in_path, "dis_mrtl_scaled_up.csv.gz"), fill = TRUE,
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "year", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "disease_mortality_rate_by_year.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    linetype = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality rate of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_rate_by_year.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year")]
  setnames(dd, c("year", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "disease_mortality_rate_diff_by_year.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  linetype = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality rate of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_rate_diff_by_year.", plot_format),
       height = 9, width = 16)


# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "disease_mortality_numbers_by_year.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    linetype = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality numbers of diseases by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_numbers_by_year.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("nonmodelled", "chd", "stroke", "popsize"), keyby = eval(outstrata)]

prvls <- c("nonmodelled", "chd", "stroke")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-4]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-4])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year")]
  setnames(dd, c("year", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "disease_mortality_numbers_diff_by_year.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  linetype = scenario)) +
  facet_wrap(~ disease, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality numbers of diseases compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "disease_mortality_numbers_diff_by_year.", plot_format),
       height = 9, width = 16)


## All-cause Mortality by age and sex ## ----

tt <- fread(paste0(in_path, "mrtl_scaled_up.csv.gz")
)[, `:=` (year = year + 2000)]

outstrata <- c("mc", "agegrp", "sex", "year", "scenario")

# Rate #

d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "mortality_rate_by_year_age_sex.csv"), sep = ";")

ggplot(d[disease != "popsize"], aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality rate by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "mortality_rate_by_year_age_sex.", plot_format),
       height = 9, width = 16)

# Rate difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl", "popsize"), keyby = eval(outstrata)
][, lapply(.SD, function(x) x/popsize), keyby = outstrata]

prvls <- c("all_cause_mrtl")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-2]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-2])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp", "sex")]
  setnames(dd, c("year", "agegrp", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "mortality_rate_diff_by_year_age_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality rate compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "mortality_rate_diff_by_year_age_sex.", plot_format),
       height = 9, width = 16)


# Absolute numbers #

d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "mrtl_rate_")))

fwrite(d, paste0(out_path_tables, "mortality_numbers_by_year_age_sex.csv"), sep = ";")

ggplot(d, aes(x = year, y = `mrtl_rate_50.0%`,
                                    ymin = `mrtl_rate_2.5%`,
                                    ymax = `mrtl_rate_97.5%`,
                                    colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Mortality numbers by scenario over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "mortality_numbers_by_year_age_sex.", plot_format),
       height = 9, width = 16)

# Absolute difference #

d <- tt[, lapply(.SD, sum), .SDcols = c("all_cause_mrtl"), keyby = eval(outstrata)]

prvls <- c("all_cause_mrtl")

d <- melt(d, id.vars = outstrata)
d <- dcast(d, mc + year + agegrp + sex ~ scenario + variable)

diffs0 <- grep("sc0_", names(d), value = TRUE)[-2]

d_out <- data.table(NULL)

for(j in 1:4){  
  
  assign(paste0("diffs", j), grep(paste0("sc", j, "_"), names(d), value = TRUE)[-2])
  
  for(i in 1:(length(get(paste0("diffs", j))))){
    
    d[, paste0("diff_", prvls)[i] := list(get(get(paste0("diffs", j))[i]) - get(diffs0[i]))]
    
  }
  
  dd <- copy(d)
  
  dd[, setdiff(names(d), intersect(c("mc", "year", "agegrp", "sex", grep("diff_", names(d), value = TRUE)), names(d))) := NULL]
  
  dd <- melt(dd, id.vars = c("mc", "year", "agegrp", "sex"))
  
  dd <- dd[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = c("year", "agegrp", "sex")]
  setnames(dd, c("year", "agegrp", "sex", "disease", percent(prbl, prefix = "mrtl_rate_")))
  dd[, scenario := paste0("sc", j)]
  
  d_out <- rbind(d_out, dd)
}

fwrite(d_out, paste0(out_path_tables, "mortality_numbers_diff_by_year_age_sex.csv"), sep = ";")

ggplot(d_out, aes(x = year, y = `mrtl_rate_50.0%`,
                  ymin = `mrtl_rate_2.5%`,
                  ymax = `mrtl_rate_97.5%`,
                  colour = agegrp, fill = agegrp, linetype = scenario)) +
  facet_wrap(~ disease + sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Mortality") +
  ggtitle("Difference in mortality numbers compared to baseline over time") +
  expand_limits(y = 0) +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "mortality_numbers_diff_by_year_age_sex.", plot_format),
       height = 9, width = 16)


## Life expectancy by sex ## ----

tt <- fread(paste0(in_path, "le_scaled_up.csv.gz")
)[, `:=` (year = year + 2000, popsize = NULL)]

outstrata <- c("mc", "sex", "year")

ttt <- dcast(tt, mc + year + sex ~ scenario, value.var = "LE")

ttt[, `:=`(sc1_diff = sc1 - sc0, sc2_diff = sc2 - sc0, sc3_diff = sc3 - sc0, sc4_diff = sc4 - sc0)]

d <- melt(ttt, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "scenario", percent(prbl, prefix = "LE_diff_")))

fwrite(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_expectancy_by_year_sex.csv"), sep = ";")

fwrite(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_expectancy_diff_by_year_sex.csv"), sep = ";")

ggplot(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE_diff_50.0%`,
              ymin = `LE_diff_2.5%`,
              ymax = `LE_diff_97.5%`,
              col = scenario, fill = scenario)) +
  facet_wrap(~ sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Life expectancy (years)") +
  ggtitle("Life expectancy by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_expectancy_by_year_sex.", plot_format),
       height = 9, width = 16)

ggplot(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE_diff_50.0%`,
           ymin = `LE_diff_2.5%`,
           ymax = `LE_diff_97.5%`,
           col = scenario, fill = scenario)) +
  facet_wrap(~ sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Difference in life expectancy (years)") +
  ggtitle("Difference in life expectancy by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_expectancy_diff_by_year_sex.", plot_format),
       height = 9, width = 16)


## Life expectancy total ## ----

tt <- fread(paste0(in_path, "le_scaled_up.csv.gz")
)[, `:=` (year = year + 2000, popsize = NULL)]

outstrata <- c("mc", "year")

ttt <- dcast(tt, mc + year ~ scenario, value.var = "LE", fun.aggregate = mean) # Mean between men and women!

ttt[, `:=`(sc1_diff = sc1 - sc0, sc2_diff = sc2 - sc0, sc3_diff = sc3 - sc0, sc4_diff = sc4 - sc0)]

d <- melt(ttt, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "scenario", percent(prbl, prefix = "LE_diff_")))

fwrite(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_expectancy_by_year.csv"), sep = ";")

fwrite(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_expectancy_diff_by_year.csv"), sep = ";")

ggplot(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE_diff_50.0%`,
           ymin = `LE_diff_2.5%`,
           ymax = `LE_diff_97.5%`,
           col = scenario, fill = scenario)) +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Life expectancy (years)") +
  ggtitle("Life expectancy by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_expectancy_by_year.", plot_format),
       height = 9, width = 16)

ggplot(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE_diff_50.0%`,
           ymin = `LE_diff_2.5%`,
           ymax = `LE_diff_97.5%`,
           col = scenario, fill = scenario)) +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Difference in life expectancy (years)") +
  ggtitle("Difference in life expectancy by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_expectancy_diff_by_year.", plot_format),
       height = 9, width = 16)


## Life years lived by sex ## ----

tt <- fread(paste0(in_path, "ly_scaled_up.csv.gz")
)[, `:=` (year = year + 2000, popsize = NULL)]

outstrata <- c("mc", "sex", "year")

ttt <- dcast(tt, mc + year + sex ~ scenario, value.var = "LY")

ttt[, `:=`(sc1_diff = sc1 - sc0, sc2_diff = sc2 - sc0, sc3_diff = sc3 - sc0, sc4_diff = sc4 - sc0)]

d <- melt(ttt, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "scenario", percent(prbl, prefix = "LY_diff_")))

fwrite(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_years_by_year_sex.csv"), sep = ";")

fwrite(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_years_diff_by_year_sex.csv"), sep = ";")

ggplot(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE_diff_50.0%`,
           ymin = `LE_diff_2.5%`,
           ymax = `LE_diff_97.5%`,
           col = scenario, fill = scenario)) +
  facet_wrap(~ sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Life years lived") +
  ggtitle("Life years lived by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_years_by_year_sex.", plot_format),
       height = 9, width = 16)

ggplot(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE_diff_50.0%`,
           ymin = `LE_diff_2.5%`,
           ymax = `LE_diff_97.5%`,
           col = scenario, fill = scenario)) +
  facet_wrap(~ sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Difference in years lived") +
  ggtitle("Difference in years lived by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_years_diff_by_year_sex.", plot_format),
       height = 9, width = 16)


## Life years lived total ## ----

tt <- fread(paste0(in_path, "ly_scaled_up.csv.gz")
)[, `:=` (year = year + 2000, popsize = NULL)]

outstrata <- c("mc", "year")

ttt <- dcast(tt, mc + year ~ scenario, value.var = "LY", fun.aggregate = sum) # Sum of men and women!

ttt[, `:=`(sc1_diff = sc1 - sc0, sc2_diff = sc2 - sc0, sc3_diff = sc3 - sc0, sc4_diff = sc4 - sc0)]

d <- melt(ttt, id.vars = outstrata)
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "scenario", percent(prbl, prefix = "LY_diff_")))

fwrite(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_years_by_year.csv"), sep = ";")

fwrite(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_years_diff_by_year.csv"), sep = ";")

ggplot(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE_diff_50.0%`,
           ymin = `LE_diff_2.5%`,
           ymax = `LE_diff_97.5%`,
           col = scenario, fill = scenario)) +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Life years lived") +
  ggtitle("Life years lived by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_years_by_year.", plot_format),
       height = 9, width = 16)

ggplot(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE_diff_50.0%`,
           ymin = `LE_diff_2.5%`,
           ymax = `LE_diff_97.5%`,
           col = scenario, fill = scenario)) +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Difference in years lived") +
  ggtitle("Difference in years lived by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_years_diff_by_year.", plot_format),
       height = 9, width = 16)


## Life expectancy at age 60 by sex ## ----

tt <- fread(paste0(in_path, "le60_scaled_up.csv.gz")
)[, `:=` (year = year + 2000, popsize = NULL)]

outstrata <- c("mc", "sex", "year")

ttt <- dcast(tt, mc + year + sex ~ scenario, value.var = "LE60")

ttt[, `:=`(sc1_diff = sc1 - sc0, sc2_diff = sc2 - sc0, sc3_diff = sc3 - sc0, sc4_diff = sc4 - sc0)]

d <- melt(ttt, id.vars = outstrata)
d[!(variable %in% grep("_diff", unique(d$variable), value = TRUE)), value := value - 60]
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "scenario", percent(prbl, prefix = "LE60_diff_")))

fwrite(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_expectancy_at_60_by_year_sex.csv"), sep = ";")

fwrite(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_expectancy_at_60_diff_by_year_sex.csv"), sep = ";")

ggplot(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE60_diff_50.0%`,
           ymin = `LE60_diff_2.5%`,
           ymax = `LE60_diff_97.5%`,
           col = scenario, fill = scenario)) +
  facet_wrap(~ sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Life expectancy at age 60 (years)") +
  ggtitle("Life expectancy at age 60 by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_expectancy_at_60_by_year_sex.", plot_format),
       height = 9, width = 16)

ggplot(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE60_diff_50.0%`,
           ymin = `LE60_diff_2.5%`,
           ymax = `LE60_diff_97.5%`,
           col = scenario, fill = scenario)) +
  facet_wrap(~ sex, scales = "free") +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Difference in life expectancy at age 60 (years)") +
  ggtitle("Difference in life expectancy at age 60 by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_expectancy_at_60_diff_by_year_sex.", plot_format),
       height = 9, width = 16)


## Life expectancy at age 60 total ## ----

tt <- fread(paste0(in_path, "le60_scaled_up.csv.gz")
)[, `:=` (year = year + 2000, popsize = NULL)]

outstrata <- c("mc", "year")

ttt <- dcast(tt, mc + year ~ scenario, value.var = "LE60", fun.aggregate = mean) # Mean between men and women

ttt[, `:=`(sc1_diff = sc1 - sc0, sc2_diff = sc2 - sc0, sc3_diff = sc3 - sc0, sc4_diff = sc4 - sc0)]

d <- melt(ttt, id.vars = outstrata)
d[!(variable %in% grep("_diff", unique(d$variable), value = TRUE)), value := value - 60]
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "scenario", percent(prbl, prefix = "LE60_diff_")))

fwrite(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_expectancy_at_60_by_year.csv"), sep = ";")

fwrite(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       paste0(out_path_tables, "life_expectancy_at_60_diff_by_year.csv"), sep = ";")

ggplot(d[!(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE60_diff_50.0%`,
           ymin = `LE60_diff_2.5%`,
           ymax = `LE60_diff_97.5%`,
           col = scenario, fill = scenario)) +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Life expectancy at age 60 (years)") +
  ggtitle("Life expectancy at age 60 by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_expectancy_at_60_by_year.", plot_format),
       height = 9, width = 16)

ggplot(d[(scenario %in% grep("_diff", unique(d$scenario), value = TRUE))],
       aes(x = year, y = `LE60_diff_50.0%`,
           ymin = `LE60_diff_2.5%`,
           ymax = `LE60_diff_97.5%`,
           col = scenario, fill = scenario)) +
  geom_ribbon(alpha = 0.5/5, colour = NA) +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Difference in life expectancy at age 60 (years)") +
  ggtitle("Difference in life expectancy at age 60 by scenario over time") +
  theme(legend.title = element_blank())

ggsave(paste0(out_path_plots, "life_expectancy_at_60_diff_by_year.", plot_format),
       height = 9, width = 16)


## Costs and QALYs, total, by sex and by agegrp (at model start) ## ----

cea <- fread(paste0(in_path, "cea_results.csv.gz"))
cea[, analysis := analysis]

export_vars <- grep("^incr_.*_scl$", names(cea), value = TRUE)

## Total ##
cea_tot <- cea[, lapply(.SD, sum), .SDcols = !c("analysis", "scenario", "sex", "agegrp_start", "mc"),
               by = c("scenario", "mc", "analysis")]

## Sex ##
cea_sex <- cea[, lapply(.SD, sum), .SDcols = !c("analysis", "scenario", "sex", "agegrp_start", "mc"),
               by = c("scenario", "mc", "analysis", "sex")]

## Age ##
cea_age <- cea[, lapply(.SD, sum), .SDcols = !c("analysis", "scenario", "sex", "agegrp_start", "mc"),
               by = c("scenario", "mc", "analysis", "agegrp_start")]

for(i in export_vars){
  
  # QALYs Scaled #
  dd <- cea_tot[, fquantile_byid(get(i), prbl, id = analysis), keyby = "scenario"]
  setnames(dd, c("scenario", "analysis", percent(prbl, prefix = paste0(i, "_"))))
  
  dd <- na.omit(dd)
  
  fwrite(dd, paste0(out_path_tables, i, "_", analysis, ".csv"), sep = ";")
  
  # QALYs Scaled #
  
  dd <- cea_sex[, fquantile_byid(get(i), prbl, id = analysis), keyby = c("sex", "scenario")]
  setnames(dd, c("sex", "scenario", "analysis", percent(prbl, prefix = paste0(i, "_"))))
  
  dd <- na.omit(dd)
  
  fwrite(dd, paste0(out_path_tables, i, "_", analysis, "_by_sex.csv"), sep = ";")
  
  # QALYs Scaled #
  
  dd <- cea_age[, fquantile_byid(get(i), prbl, id = analysis), keyby = c("agegrp_start", "scenario")]
  setnames(dd, c("agegrp_start", "scenario", "analysis", percent(prbl, prefix = paste0(i, "_"))))
  
  dd <- na.omit(dd)
  
  fwrite(dd, paste0(out_path_tables, i, "_", analysis, "_by_age.csv"), sep = ";")

}





















#### OLD CODE FROM CHRIS ####

# 
# 
# 
# 
# tt_esp <- fread("./outputs/summaries/prvl_scaled_up.csv.gz"
# )[, `:=` (year = year + 2000L)]
# 
# outstrata <- c("mc", "year", "scenario")
# e <- tt_esp[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
# ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
# e <- melt(e, id.vars = outstrata)
# e <- e[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
# setnames(e, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_esp_")))
# fwrite(e, "/mnt/storage_fast/output/hf_real/tables/age-standardised prevalence by year and dimd.csv"), sep = ";"
# 
# e[disease == "cmsmm0_prvl", disease := "CMS score > 0"]
# e[disease == "cmsmm1_prvl", disease := "CMS score > 1"]
# e[disease == "cmsmm1.5_prvl", disease := "CMS score > 1.5"]
# e[disease == "cmsmm2_prvl", disease := "CMS score > 2"]
# e <- e[between(year, 2014, 2040) & disease %in% c("CMS score > 1", "CMS score > 1.5", "CMS score > 2")]
# e[scenario == "sc0", scenario := "Base-case"]
# e[scenario == "sc1", scenario := "10% BMI reduction"]
# 
# ggplot(e[scenario == "Base-case" & disease %in% c("CMS score > 1", "CMS score > 1.5", "CMS score > 2")],
#        aes(x = year, y = `prvl_rate_esp_50.0%`, ymin = `prvl_rate_esp_2.5%`,
#            ymax = `prvl_rate_esp_97.5%`, colour = dimd, fill = dimd)) +
#   # geom_ribbon(alpha = 2/5, colour = NA) +
#   geom_line() +
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Prevalence rate", labels = percent) +
#   ggtitle(paste0("Prevalence of ", "multimorbidity", " by dimd (age-standardised)"))+
#   expand_limits(y = 0) +
#   facet_grid(.~disease)
# ggsave("~/pCloudDrive/pCloud Sync/MM prevalence by dimd.png", scale = 1.5, width = 16/2, height = 9/2)
# 
# e[year %in% c(2020, 2040) & dimd %in% c("1 most deprived", "10 least deprived") & disease == "CMS score > 1.5" & scenario == "Base-case"]
# 
# ggplot(e[disease %in% c("CMS score > 1.5")],
#        aes(x = year, y = `prvl_rate_esp_50.0%`, ymin = `prvl_rate_esp_2.5%`,
#            ymax = `prvl_rate_esp_97.5%`, colour = dimd, fill = dimd)) +
#   # geom_ribbon(alpha = 2/5, colour = NA) +
#   geom_line() +
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Prevalence rate", labels = percent) +
#   ggtitle(paste0("Prevalence of ", "multimorbidity", " by dimd (age-standardised, CMS score > 1.5)"))+
#   expand_limits(y = 0) +
#   facet_grid(.~ scenario)
# ggsave("~/pCloudDrive/pCloud Sync/MM prevalence by dimd scenario.png", scale = 1.5, width = 16/2, height = 9/2)
# 
# e[year %in% c(2020, 2040) & dimd %in% c("1 most deprived", "10 least deprived") & disease == "CMS score > 1.5" & scenario == "10% BMI reduction"]
# 
# 
# fl <- list.files("/mnt/storage_fast/output/hf_real/lifecourse",
#                  "_lifecourse.csv.gz$", full.names = TRUE)
# 
# 
# out <- rbindlist(lapply(fl, fread))[scenario == "sc0", ]
# out[, dimd := factor(dimd, c("1 most deprived", as.character(2:9), "10 least deprived"))]
# setkey(out, mc, pid, year)
# out[, table(cmsmm1.5_prvl)]
# View(out[cmsmm1.5_prvl > 0, .(mc, pid, year, cmsmm1.5_prvl) ])
# t1 <- out[cmsmm1.5_prvl == 1 & year %in% c(20, 40), .(mm_free = weighted.mean(age, wt_esp)), keyby = .(year, sex)] # mean age of multimorbidity
# t2 <- out[all_cause_mrtl > 0 & year %in% c(20, 40), .(le = weighted.mean(age, wt_esp)), keyby = .(year, sex)] # mean life expectancy
# t1 <- t1[t2]
# 
# t1 <- out[cmsmm1.5_prvl == 1 & year %in% c(20, 40), .(mm_free = weighted.mean(age, wt_esp)), keyby = .(year, dimd)] # mean age of multimorbidity
# t2 <- out[all_cause_mrtl > 0 & year %in% c(20, 40), .(le = weighted.mean(age, wt_esp)), keyby = .(year, dimd)] # mean life expectancy
# t1 <- t1[t2]
# View(t1)
# 
# 
# tt_esp <- fread("/mnt/storage_fast/output/hf_real/summaries/incd_esp.csv.gz"
# )[, `:=` (year = year + 2000L,
#           dimd = factor(dimd, c("1 most deprived", as.character(2:9), "10 least deprived")))]
# outstrata <- c("mc", "year", "dimd", "scenario")
# e <- tt_esp[, lapply(.SD, sum), .SDcols = patterns("_prvl$|^popsize$"), keyby = eval(outstrata)
# ][, lapply(.SD, function(x) x/popsize), keyby = outstrata]
# e <- melt(e, id.vars = outstrata)
# e <- e[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
# setnames(e, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "prvl_rate_esp_")))
# # fwrite(e, "/mnt/storage_fast/output/hf_real/tables/age-standardised prevalence by year and dimd.csv"), sep = ";"
# 
# e[disease == "cmsmm0_prvl", disease := "CMS score > 0"]
# e[disease == "cmsmm1_prvl", disease := "CMS score > 1"]
# e[disease == "cmsmm1.5_prvl", disease := "CMS score > 1.5"]
# e[disease == "cmsmm2_prvl", disease := "CMS score > 2"]
# e <- e[between(year, 2014, 2040) & disease %in% c("CMS score > 1", "CMS score > 1.5", "CMS score > 2")]
# e[scenario == "sc0", scenario := "Base-case"]
# e[scenario == "sc1", scenario := "10% BMI reduction"]
# 
# ggplot(e[scenario == "Base-case" & disease %in% c("CMS score > 1", "CMS score > 1.5", "CMS score > 2")],
#        aes(x = year, y = `prvl_rate_esp_50.0%`, ymin = `prvl_rate_esp_2.5%`,
#            ymax = `prvl_rate_esp_97.5%`, colour = dimd, fill = dimd)) +
#   # geom_ribbon(alpha = 2/5, colour = NA) +
#   geom_line() +
#   scale_x_continuous(name = "Year") +
#   scale_y_continuous(name = "Prevalence rate", labels = percent) +
#   ggtitle(paste0("Prevalence of ", "multimorbidity", " by dimd (age-standardised)"))+
#   expand_limits(y = 0) +
#   facet_grid(.~disease)
# ggsave("~/pCloudDrive/pCloud Sync/MM prevalence by dimd.png", scale = 1.5, width = 16/2, height = 9/2)

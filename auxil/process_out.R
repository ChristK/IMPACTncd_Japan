library(data.table)
library(yaml)
library(CKutils)
library(ggplot2)
library(ggthemes)
library(scales)


prbl <- c(0.5, 0.025, 0.975, 0.1, 0.9)
baseline_year_for_change_outputs <- 2001L
theme_set(new = theme_economist())
theme_update(axis.text.x = element_text(size = 9), plot.title = element_text(hjust = 0.5))

what <- "dis_mrtl"
type <- "ons"
strata <- "year"
baseline_year <- baseline_year_for_change_outputs

# Prvl not standardised----
simulationParameters <- read_yaml(base::normalizePath("./inputs/sim_design.yaml", mustWork = TRUE))
sSummariesSubDirPath <- file.path(simulationParameters$output_dir, "summaries")
sTablesSubDirPath <- file.path(simulationParameters$output_dir, "tables")
output_dir <- simulationParameters$output_dir

tbl_smmrs <- function(
    what = c(
            "prvl", "prvl_change", "incd", "incd_change",
            "ftlt", "ftlt_change", "mrtl", "mrtl_change",
            "dis_mrtl", "dis_mrtl_change",
            "cms_score", "cms_score_change", "cms_score_age",
            "cms_score_age_change", "cms_count", "cms_count_change",
            "pop"
    ),
    type = c("ons", "esp"),
    strata,
    output_dir = output_dir,
    prbl = c(0.5, 0.025, 0.975, 0.1, 0.9),
    baseline_year = 2019L # only used for prvl_change etc.
    ) {
        strata <- lapply(strata, function(x) {
                c("mc", "scenario", x)
        })

        # construct file path to read from summaries
        str0 <- c(
                "prvl" = "prvl",
                "prvl_change" = "prvl",
                "incd" = "incd",
                "incd_change" = "incd",
                "ftlt" = "/dis_mrtl",
                "ftlt_change" = "/dis_mrtl",
                "mrtl" = "mrtl",
                "mrtl_change" = "mrtl",
                "dis_mrtl" = "dis_mrtl",
                "dis_mrtl_change" = "dis_mrtl",
                "cms_score" = "cms_score",
                "cms_score_change" = "cms_score",
                "cms_score_age" = "cms_score_by_age",
                "cms_score_age_change" = "cms_score_by_age",
                "cms_count" = "cms_count",
                "cms_count_change" = "cms_count",
                "pop" = "prvl"
        )
        str1 <- c("ons" = "_scaled_up.csv.gz", "esp" = "_esp.csv.gz")
        fpth <- file.path(output_dir, "summaries", paste0(str0[[what]], str1[[type]]))
        if (!file.exists(fpth)) {
                message(fpth, " doesn't exist")
                return(NULL)
        }
        # other useful strings
        str2 <- c(
                "prvl" = "_prvl$|^popsize$",
                "prvl_change" = "_prvl$|^popsize$",
                "incd" = "_incd$|^popsize$",
                "incd_change" = "_incd$|^popsize$",
                "ftlt" = "_deaths$|_prvl$",
                "ftlt_change" = "_deaths$|_prvl$",
                "mrtl" = "_mrtl$|^popsize$",
                "mrtl_change" = "_mrtl$|^popsize$",
                "dis_mrtl" = "^nonmodelled_deaths$|^chd_deaths$|^stroke_deaths$|^popsize$",
                "dis_mrtl_change" = "^nonmodelled_deaths$|^chd_deaths$|^stroke_deaths$|^popsize$",
                "cms_score" = "cms_score",
                "cms_score_change" = "cms_score",
                "cms_score_age" = "cms_score",
                "cms_score_age_change" = "cms_score",
                "cms_count" = "cms_count",
                "cms_count_change" = "cms_count",
                "pop" = "^popsize$"
        ) # used in grep
        str3 <- c(
                "prvl" = "prvl_rate_",
                "prvl_change" = "prct_change_",
                "incd" = "incd_rate_",
                "incd_change" = "prct_change_",
                "ftlt" = "ftlt_rate_",
                "ftlt_change" = "ftlt_rate_",
                "mrtl" = "mrtl_rate_",
                "mrtl_change" = "mrtl_change_",
                "dis_mrtl" = "disease_mrtl_rate_",
                "dis_mrtl_change" = "disease_mrtl_change_",                
                "cms_score" = "mean_cms_score_",
                "cms_score_change" = "mean_cms_score_",
                "cms_score_age" = "mean_cms_score_",
                "cms_score_age_change" = "mean_cms_score_",
                "cms_count" = "mean_cms_count_",
                "cms_count_change" = "mean_cms_count_",
                "pop" = "pop_size_"
        ) # used to col name output
        str4 <- c(
                "prvl" = "prevalence by ",
                "prvl_change" = "prevalence change by ",
                "incd" = "incidence by ",
                "incd_change" = "incidence change by ",
                "ftlt" = "fatality by ",
                "ftlt_change" = "fatality change by ",
                "mrtl" = "mortality by ",
                "mrtl_change" = "mortality change by ",
                "dis_mrtl" = "disease mortality by ",
                "dis_mrtl_change" = "disease mortality change by ",                
                "cms_score" = "mean CMS score by ",
                "cms_score_change" = "mean CMS score change by ",
                "cms_score_age" = "mean CMS score by ",
                "cms_score_age_change" = "mean CMS score change by ",
                "cms_count" = "mean CMS count by ",
                "cms_count_change" = "mean CMS count change by ",
                "pop" = "pop size by "
        ) # used in output filenames



        tt <- fread(fpth) # numerator data

        # For ftlt I need prvl for the denominator
        if (grepl("^ftlt", what)) {
                fpth <- file.path(output_dir, "summaries", paste0(str0[["prvl"]], str1[[type]]))
                if (!file.exists(fpth)) stop(fpth, " doesn't exist")

                t1 <- fread(fpth)
                setnames(t1, "popsize", "nonmodelled_prvl")
                absorb_dt(tt, t1)
                tt <- tt[nonmodelled_prvl > 0] # This is the denom. Cannot be 0 and it is meaningless anyway
        }

        lapply(strata, function(x) {
                if (grepl("^cms_", what)) {
                        d <- tt[, .("value" = weighted.mean(get(str2[[what]]), popsize)),
                                keyby = eval(x)
                        ]

                        if (grepl("_change$", what)) { # when calculating change
                                d19 <- d[year == baseline_year][, year := NULL]
                                d[d19, on = c(setdiff(x, "year")), value := value / i.value]
                        }
                        d <- d[, as.list(fquantile(value, prbl)), keyby = eval(setdiff(x, "mc"))]
                        setnames(d, c(setdiff(x, "mc"), percent(prbl, prefix = str3[[what]])))
                } else { # if not cms...
                        d <- tt[, lapply(.SD, sum),
                                .SDcols = patterns(str2[[what]]),
                                keyby = x
                        ]


                        if (grepl("^ftlt", what)) {
                                nm <- names(d)
                                nm <- grep("_deaths$", nm, value = TRUE)
                                nm <- gsub("_deaths$", "", nm)
                                nm <- setdiff(nm, "alive")
                                for (i in nm) {
                                        set(
                                                d, NULL, paste0(i, "_ftlt"),
                                                d[[paste0(i, "_deaths")]] / d[[paste0(i, "_prvl")]]
                                        )
                                }

                                nm <- names(d)
                                nm <- grep("_deaths$|_prvl$", nm, value = TRUE)
                                d[, (nm) := NULL]
                                setnafill(d, "const", 0, cols = grep("_ftlt$", names(d), value = TRUE))
                        } else if (what != "pop") { # if not ftlt related and not pop
                                d <- d[, lapply(.SD, function(y) {
                                        y / popsize
                                }), keyby = x]
                        }

                        d <- melt(d, id.vars = x)

                        if (grepl("_change$", what)) { # when calculating change
                                d19 <- d[year == baseline_year][, year := NULL]
                                d[d19, on = c(setdiff(x, "year"), "variable"), value := value / i.value]
                        }

                        setkey(d, "variable")
                        d <-
                                d[, fquantile_byid(value, prbl, id = as.character(variable), rounding = what == "pop"),
                                        keyby = eval(setdiff(x, "mc"))
                                ]
                        setnames(d, c(
                                setdiff(x, "mc"),
                                "disease",
                                percent(prbl, prefix = str3[[what]])
                        ))
                        if (what == "pop") {
                                d[, disease := NULL]
                        } else {
                                d <- d[disease != "popsize"]
                        }
                }
                setkeyv(d, setdiff(x, "mc"))
                setcolorder(d, setdiff(x, "mc"))
                str5 <- c(
                        "ons" = " (not standardised).csv",
                        "esp" = paste0(" (", paste(setdiff(c("mc", "scenario", "year", "age", "sex"), x),
                                collapse = "-"
                        ), " standardised).csv")
                )

                str6 <- paste0(
                        str4[[what]],
                        paste(setdiff(x, c("mc", "scenario")), collapse = "-"),
                        str5[[type]]
                ) # used for output file name/path
                fwrite(d, file.path(
                        output_dir, "tables", str6
                ))
        })
}


outperm <- expand.grid(
        what = c(
                "prvl", "prvl_change", "incd", "incd_change",
                "ftlt", "ftlt_change", "mrtl", "mrtl_change",
                "dis_mrtl", "dis_mrtl_change",
                # "cms_score", "cms_score_change", "cms_score_age",
                # "cms_score_age_change", "cms_count", "cms_count_change",
                "pop"
        ),
        type = c("ons", "esp")
)

for (i in seq_len(nrow(outperm))) {
        what <- as.character(outperm$what[[i]])
        type <- as.character(outperm$type[[i]])
        if (type == "ons") {
                strata <- list(
                        "year",
                        c("year", "sex"),
                        c("year", "agegrp"),
                        c("year", "agegrp", "sex")
                )
        } else if (type == "esp") {
                strata <- list(
                        "year",
                        c("year", "sex")
                )
        } else {
                stop()
        }

        if (grepl("_age", what)) {
                strata <- lapply(strata, function(st) {
                        st[st == "agegrp"] <- "age"
                        st
                })
        }

        if ((what == "pop" && type == "esp") || (grepl("_age", what) && type == "esp")) next()

        print(paste0(what, "-", type))
        tbl_smmrs(what, type, strata, output_dir, prbl = prbl, baseline_year = baseline_year_for_change_outputs)
}

tbl_smmrs("pop", "ons", list(
        "year",
        c("year", "sex"),
        c("year", "agegrp"),
        c("year", "agegrp", "sex")
), output_dir, prbl = prbl, baseline_year = baseline_year_for_change_outputs)


# All-cause mortality by disease not standardised----
tt <- fread(file.path(sSummariesSubDirPath, "all_cause_mrtl_by_dis_scaled_up.csv.gz"))

outstrata <- c("mc", "year", "scenario")
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = c(outstrata, "variable"), value := value / i.value]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year (not standardised).csv"))

outstrata <- c("mc", "year", "sex", "scenario")
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = c(outstrata, "variable"), value := value / i.value]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year-sex (not standardised).csv"))

outstrata <- c("mc", "year", "agegrp", "sex", "scenario")
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = c(outstrata, "variable"), value := value / i.value]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year-agegroup-sex (not standardised).csv"))

outstrata <- c("mc", "year", "agegrp", "sex", "scenario")
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = c(outstrata, "variable"), value := value / i.value]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year-agegroup-sex (not standardised).csv"))

# All-cause mortality by disease not standardised pop denominator----
tt <- fread(file.path(sSummariesSubDirPath, "all_cause_mrtl_by_dis_scaled_up.csv.gz"))
pp <- fread(file.path(sSummariesSubDirPath, "prvl_scaled_up.csv.gz"))

outstrata <- c("mc", "year", "scenario")
cases <- pp[, lapply(.SD, sum), .SDcols = patterns("^popsize$"), keyby = eval(outstrata)]
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = outstrata, value := value / popsize]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year popdenom (not standardised).csv"))

outstrata <- c("mc", "year", "sex", "scenario")
cases <- pp[, lapply(.SD, sum), .SDcols = patterns("^popsize$"), keyby = eval(outstrata)]
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = outstrata, value := value / popsize]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year-sex popdenom (not standardised).csv"))

outstrata <- c("mc", "year", "agegrp", "sex", "scenario")
cases <- pp[, lapply(.SD, sum), .SDcols = patterns("^popsize$"), keyby = eval(outstrata)]
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = outstrata, value := value / popsize]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year-agegroup-sex popdenom (not standardised).csv"))

outstrata <- c("mc", "year", "agegrp", "sex", "scenario")
cases <- pp[, lapply(.SD, sum), .SDcols = patterns("^popsize$"), keyby = eval(outstrata)]
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = outstrata, value := value / popsize]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year-agegroup-sex popdenom (not standardised).csv"))

rm(pp)

# All-cause mortality by disease standardised----
tt <- fread(file.path(sSummariesSubDirPath, "all_cause_mrtl_by_dis_esp.csv.gz"))
outstrata <- c("mc", "year", "scenario")
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = c(outstrata, "variable"), value := value / i.value]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year (age-sex standardised).csv"))

outstrata <- c("mc", "year", "sex", "scenario")
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = c(outstrata, "variable"), value := value / i.value]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year-sex (age standardised).csv"))

outstrata <- c("mc", "year", "scenario")
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = c(outstrata, "variable"), value := value / i.value]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year (age-sex standardised).csv"))

outstrata <- c("mc", "year", "sex", "scenario")
d <- tt[, lapply(.SD, sum),
        .SDcols = patterns("^deaths_|^cases_"),
        keyby = eval(outstrata)
]
d <- melt(d, id.vars = outstrata)
cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
d[cases, on = c(outstrata, "variable"), value := value / i.value]
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "disease", percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "all-cause mrtl by disease-year-sex (age standardised).csv"))
rm(cases)


# Disease characteristics non standardised ----
tt <- fread(file.path(sSummariesSubDirPath, "dis_characteristics_scaled_up.csv.gz"))[, `:=`(
        mean_cms_count_cms1st_cont = as.numeric(mean_cms_count_cms1st_cont)
)]
d1 <- tt[, .SD, .SDcols = patterns("mc|scenario|year|sex|^cases_")]
d1 <- melt(d1, id.vars = c("mc", "year", "scenario", "sex"))
d1 <- unique(d1, by = c("mc", "year", "scenario", "sex", "variable"))
d1[, `:=`(disease = gsub("^cases_", "", variable), variable = NULL)]
tt <- tt[, .SD, .SDcols = patterns("mc|scenario|year|sex|^mean_duration_|^mean_age_incd_|^mean_age_1st_onset_|^mean_age_prvl_|^mean_cms_score_|^mean_cms_count_")]
tt[, mean_cms_count_cmsmm1 := as.double(mean_cms_count_cmsmm1)]
tt <- melt(tt, id.vars = c("mc", "year", "scenario", "sex"))
tt[, disease := gsub("^mean_duration_|^mean_age_incd_|^mean_age_1st_onset_|^mean_age_prvl_|^mean_cms_score_|^mean_cms_count_", "", variable)]
tt[d1, on = c("mc", "year", "scenario", "sex", "disease"), cases := i.value]
rm(d1) # NOTE mean_age_incd contains NAs

outstrata <- c("mc", "year", "scenario")
d <- tt[, weighted.mean(value, cases, na.rm = TRUE), keyby = c(outstrata, "variable")] # na.rm = TRUE for mean_age_incd
setkey(d, "variable")
d <- d[, fquantile_byid(V1, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "variable", percent(prbl, prefix = "value_")))
d[, disease := gsub("^mean_duration_|^mean_age_incd_|^mean_age_1st_onset_|^mean_age_prvl_|^mean_cms_score_|^mean_cms_count_", "", variable)]
d[grep("^mean_duration_", variable), type := "mean_duration"]
d[grep("^mean_age_incd_", variable), type := "mean_age_incd"]
d[grep("^mean_age_1st_onset_", variable), type := "mean_age_1st_onset"]
d[grep("^mean_age_prvl_", variable), type := "mean_age_prvl"]
d[grep("^mean_cms_score_", variable), type := "mean_cms_score"]
d[grep("^mean_cms_count_", variable), type := "mean_cms_count"]
d[, variable := NULL]
setkeyv(d, c(setdiff(outstrata, "mc"), "disease", "type"))
setcolorder(d)
fwrite(d, file.path(sTablesSubDirPath, "disease characteristics by year (not standardised).csv"))

outstrata <- c("mc", "year", "sex", "scenario")
d <- tt[, weighted.mean(value, cases, na.rm = TRUE), keyby = c(outstrata, "variable")]
setkey(d, "variable")
d <- d[, fquantile_byid(V1, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "variable", percent(prbl, prefix = "value_")))
d[, disease := gsub("^mean_duration_|^mean_age_incd_|^mean_age_1st_onset_|^mean_age_prvl_|^mean_cms_score_|^mean_cms_count_", "", variable)]
d[grep("^mean_duration_", variable), type := "mean_duration"]
d[grep("^mean_age_incd_", variable), type := "mean_age_incd"]
d[grep("^mean_age_1st_onset_", variable), type := "mean_age_1st_onset"]
d[grep("^mean_age_prvl_", variable), type := "mean_age_prvl"]
d[grep("^mean_cms_score_", variable), type := "mean_cms_score"]
d[grep("^mean_cms_count_", variable), type := "mean_cms_count"]
d[, variable := NULL]
setkeyv(d, c(setdiff(outstrata, "mc"), "disease", "type"))
setcolorder(d)
fwrite(d, file.path(sTablesSubDirPath, "disease characteristics by year-sex (not standardised).csv"))

outstrata <- c("mc", "year", "scenario")
d <- tt[, weighted.mean(value, cases, na.rm = TRUE), keyby = c(outstrata, "variable")]
setkey(d, "variable")
d <- d[, fquantile_byid(V1, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "variable", percent(prbl, prefix = "value_")))
d[, disease := gsub("^mean_duration_|^mean_age_incd_|^mean_age_1st_onset_|^mean_age_prvl_|^mean_cms_score_|^mean_cms_count_", "", variable)]
d[grep("^mean_duration_", variable), type := "mean_duration"]
d[grep("^mean_age_incd_", variable), type := "mean_age_incd"]
d[grep("^mean_age_1st_onset_", variable), type := "mean_age_1st_onset"]
d[grep("^mean_age_prvl_", variable), type := "mean_age_prvl"]
d[grep("^mean_cms_score_", variable), type := "mean_cms_score"]
d[grep("^mean_cms_count_", variable), type := "mean_cms_count"]
d[, variable := NULL]
setkeyv(d, c(setdiff(outstrata, "mc"), "disease", "type"))
setcolorder(d)
fwrite(d, file.path(sTablesSubDirPath, "disease characteristics by year (not standardised).csv"))

outstrata <- c("mc", "year", "sex", "scenario")
d <- tt[, weighted.mean(value, cases, na.rm = TRUE), keyby = c(outstrata, "variable")]
setkey(d, "variable")
d <- d[, fquantile_byid(V1, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "variable", percent(prbl, prefix = "value_")))
d[, disease := gsub("^mean_duration_|^mean_age_incd_|^mean_age_1st_onset_|^mean_age_prvl_|^mean_cms_score_|^mean_cms_count_", "", variable)]
d[grep("^mean_duration_", variable), type := "mean_duration"]
d[grep("^mean_age_incd_", variable), type := "mean_age_incd"]
d[grep("^mean_age_1st_onset_", variable), type := "mean_age_1st_onset"]
d[grep("^mean_age_prvl_", variable), type := "mean_age_prvl"]
d[grep("^mean_cms_score_", variable), type := "mean_cms_score"]
d[grep("^mean_cms_count_", variable), type := "mean_cms_count"]
d[, variable := NULL]
setkeyv(d, c(setdiff(outstrata, "mc"), "disease", "type"))
setcolorder(d)
fwrite(d, file.path(sTablesSubDirPath, "disease characteristics by year-sex (not standardised).csv"))

rm(d, tt)

# XPS ----
xps_tab <- fread(file.path(simulationParameters$output_dir, "xps/xps20.csv.gz"))

xps_names <- grep("_curr_xps$", names(xps_tab), value = TRUE)

outstrata <- c("mc", "year", "agegrp20", "sex", "scenario")
d <- xps_tab[sex != "All" & agegrp20 != "All"] # This should depend on outstrata

d <- d[, lapply(.SD, mean), .SDcols = patterns("_curr_xps$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "exposure", percent(prbl, prefix = "xps_mean_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "exposures by year-agegroup-sex (not standardised).csv"))

outstrata <- c("mc", "year", "scenario")
d <- xps_tab[sex == "All" & agegrp20 == "All"] # This should depend on outstrata

d <- d[, lapply(.SD, mean), .SDcols = patterns("_curr_xps$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "exposure", percent(prbl, prefix = "xps_mean_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "exposures by year (not standardised).csv"))

outstrata <- c("mc", "year", "agegrp20", "scenario")
d <- xps_tab[sex == "All" & agegrp20 != "All"] # This should depend on outstrata
d <- d[, lapply(.SD, mean), .SDcols = patterns("_curr_xps$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "exposure", percent(prbl, prefix = "xps_mean_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "exposures by year-agegroup (not standardised).csv"))

outstrata <- c("mc", "year", "agegrp20", "sex", "scenario")
d <- xps_tab[sex != "All" & agegrp20 != "All"] # This should depend on outstrata
d <- d[, lapply(.SD, mean), .SDcols = patterns("_curr_xps$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "exposure", percent(prbl, prefix = "xps_mean_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "exposures by year-agegroup-sex (not standardised).csv"))

outstrata <- c("mc", "year", "scenario")
d <- xps_tab[sex == "All" & agegrp20 == "All"] # This should depend on outstrata
d <- d[, lapply(.SD, mean), .SDcols = patterns("_curr_xps$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "exposure", percent(prbl, prefix = "xps_mean_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "exposures by year (not standardised).csv"))

# XPS standardised ----
xps_tab <- fread(file.path(simulationParameters$output_dir, "xps/xps_esp.csv.gz"))
xps_names <- grep("_curr_xps$", names(xps_tab), value = TRUE)

outstrata <- c("mc", "year", "sex", "scenario")
d <- xps_tab[sex != "All"] # This should depend on outstrata

d <- d[, lapply(.SD, mean), .SDcols = patterns("_curr_xps$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "exposure", percent(prbl, prefix = "xps_mean_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "exposures by year-sex (age standardised).csv"))

outstrata <- c("mc", "year", "scenario")
d <- xps_tab[sex == "All"] # This should depend on outstrata

d <- d[, lapply(.SD, mean), .SDcols = patterns("_curr_xps$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "exposure", percent(prbl, prefix = "xps_mean_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "exposures by year (age-sex standardised).csv"))

outstrata <- c("mc", "year", "sex", "scenario")
d <- xps_tab[sex != "All"] # This should depend on outstrata
d <- d[, lapply(.SD, mean), .SDcols = patterns("_curr_xps$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "exposure", percent(prbl, prefix = "xps_mean_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "exposures by year-sex (age standardised).csv"))

outstrata <- c("mc", "year", "scenario")
d <- xps_tab[sex == "All"] # This should depend on outstrata
d <- d[, lapply(.SD, mean), .SDcols = patterns("_curr_xps$"), keyby = eval(outstrata)]
d <- melt(d, id.vars = outstrata)
setkey(d, "variable")
d <- d[, fquantile_byid(value, prbl, id = as.character(variable)), keyby = eval(setdiff(outstrata, "mc"))]
setnames(d, c(setdiff(outstrata, "mc"), "exposure", percent(prbl, prefix = "xps_mean_")))
setkeyv(d, setdiff(outstrata, "mc"))
fwrite(d, file.path(sTablesSubDirPath, "exposures by year (age-sex standardised).csv"))

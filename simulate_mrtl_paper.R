mcnum <- 20L # Number of iterations
source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

IMPACTncd$
  del_logs()$
  del_outputs()$
  run(1:mcnum, multicore = TRUE, "sc0")

# F&V
IMPACTncd$update_primary_prevention_scn(
    function(synthpop) {
        setyear <- 2001L
        tbl <-
            read_fst("./inputs/exposure_distributions/Table_Fruit_vege.fst",
                as.data.table = TRUE
            )[Year == setyear & between(Age, self$design$sim_prm$ageL, self$design$sim_prm$ageH)]
        tbl[, Year := NULL]
        setnames(tbl, tolower(names(tbl)))
        tbl[, sex := factor(sex, 0:1, c("men", "women"))]

        col_nam <-
            setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
        absorb_dt(synthpop$pop, tbl)

        synthpop$pop[year > setyear, Fruit_vege_curr_xps := qZINBI(rank_Fruit_vege, mu, sigma, nu)]
        synthpop$pop[, c(col_nam) := NULL]
        NULL
    }
)

IMPACTncd$
  run(1:mcnum, multicore = TRUE, "FV")

# Smoking
IMPACTncd$update_primary_prevention_scn(
    function(synthpop) {
        setyear <- 2001L
        rs <- .Random.seed
        on.exit(set.seed(rs))
        set.seed(2121870L + synthpop$mc) # Not mc_aggr # same as in Synthpop class

        synthpop$pop[, tax_tabaco := fcase(
            year < 2006L,                 0L,
            year >= 2006L & year < 2010L, 1L,
            year >= 2010L & year < 2018L, 2L,
            year >= 2018L,                3L
        )]
        synthpop$pop[, tax_tabaco := factor(tax_tabaco, 0:3, 0:3)]
        synthpop$pop[, Smoking_curr_xps := as.integer(Smoking_curr_xps) - 1L]

        tbl <-
            read_fst("./inputs/exposure_distributions/Table_Smoking_NevEx_vs_current.fst",
                as.data.table = TRUE
            )[Year == setyear & between(Age, self$design$sim_prm$ageL, self$design$sim_prm$ageH)]
        tbl[, Year := NULL]
        setnames(tbl, tolower(names(tbl)))
        tbl[, sex := factor(sex, 0:1, c("men", "women"))]

        col_nam <-
            setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
        absorb_dt(synthpop$pop, tbl)
        synthpop$pop[year > setyear, Smoking_curr_xps := as.integer(rankstat_Smoking_act < mu) * 2L] # 0 = never smoker or ex, 2 = current
        synthpop$pop[, c(col_nam) := NULL]


        tbl <-
            read_fst("./inputs/exposure_distributions/Table_Smoking_never_vs_ex.fst",
                as.data.table = TRUE
            )[Year == setyear & between(Age, self$design$sim_prm$ageL, self$design$sim_prm$ageH)]
        tbl[, Year := NULL]
        setnames(tbl, tolower(names(tbl)))
        tbl[, sex := factor(sex, 0:1, c("men", "women")), ]
        col_nam <-
            setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
        absorb_dt(synthpop$pop, tbl)
        synthpop$pop[year > setyear & Smoking_curr_xps == 0L, Smoking_curr_xps := as.integer(rankstat_Smoking_ex < mu), by = .(year)] # 0 = never smoker, 1=ex, 2=current

        synthpop$pop[, Smoking_curr_xps := factor(Smoking_curr_xps + 1L)]
        synthpop$pop[, c(col_nam, "tax_tabaco") := NULL]


        tbl <-
            read_fst("./inputs/exposure_distributions/Table_Smoking_number.fst",
                as.data.table = TRUE
            )[Year == setyear & between(Age, self$design$sim_prm$ageL, self$design$sim_prm$ageH)]
        tbl[, Year := NULL]
        setnames(tbl, tolower(names(tbl)))
        tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

        col_nam <-
            setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
        absorb_dt(synthpop$pop, tbl)

        synthpop$pop[
            year > setyear & Smoking_curr_xps == 3,
            Smoking_number_grp := (rankstat_Smoking_number > pa0) +
                (rankstat_Smoking_number > pa1) +
                (rankstat_Smoking_number > pa2) +
                (rankstat_Smoking_number > pa3) +
                (rankstat_Smoking_number > pa4) +
                (rankstat_Smoking_number > pa5) +
                (rankstat_Smoking_number > pa6) +
                (rankstat_Smoking_number > pa7)
        ]

        synthpop$pop[Smoking_number_grp == 0L, Smoking_number_curr_xps := 5L]
        synthpop$pop[Smoking_number_grp == 1L, Smoking_number_curr_xps := 10L]
        synthpop$pop[Smoking_number_grp == 2L, Smoking_number_curr_xps := 15L]
        synthpop$pop[Smoking_number_grp == 3L, Smoking_number_curr_xps := 20L]
        synthpop$pop[Smoking_number_grp == 4L, Smoking_number_curr_xps := 25L]
        synthpop$pop[Smoking_number_grp == 5L, Smoking_number_curr_xps := 30L]
        synthpop$pop[Smoking_number_grp == 6L, Smoking_number_curr_xps := 35L]
        synthpop$pop[Smoking_number_grp == 7L, Smoking_number_curr_xps := 40L]
        # I do not explicitly set.seed because I do so at the beginning of the scenario
        synthpop$pop[Smoking_number_grp == 8L, Smoking_number := sample(c(50L, 60L, 80L), .N, TRUE, prob = c(0.4, 0.45, 0.15))]

        synthpop$pop[, c(col_nam, "Smoking_number_grp") := NULL]
        NULL
    }
)

IMPACTncd$
  run(1:mcnum, multicore = TRUE, "smoking")

# biological rf meds+ bmi+hba1c+pa+ldlc+sbp
IMPACTncd$update_primary_prevention_scn(
    function(synthpop) {
        setyear <- 2001L
        tbl <-
            read_fst("./inputs/exposure_distributions/Table_Med_HT.fst",
                as.data.table = TRUE
            )[Year == setyear & between(Age, self$design$sim_prm$ageL, self$design$sim_prm$ageH)]
        tbl[, Year := NULL]
        setnames(tbl, tolower(names(tbl)))
        tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

        col_nam <-
            setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
        absorb_dt(synthpop$pop, tbl)
        synthpop$pop[year > setyear, Med_HT_curr_xps := qbinom(rankstat_Med_HT, 1L, mu)] # , n_cpu = design_$sim_prm$n_cpu)]
        synthpop$pop[, c(col_nam) := NULL]

        tbl <-
            read_fst("./inputs/exposure_distributions/Table_Med_HL.fst",
                as.data.table = TRUE
            )[Year == setyear & between(Age, self$design$sim_prm$ageL, self$design$sim_prm$ageH)]
        tbl[, Year := NULL]
        setnames(tbl, tolower(names(tbl)))
        tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

        col_nam <-
            setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
        absorb_dt(synthpop$pop, tbl)

        synthpop$pop[year > setyear, Med_HL_curr_xps := qbinom(rankstat_Med_HL, 1L, mu)] # , n_cpu = design_$sim_prm$n_cpu)]
        synthpop$pop[, c(col_nam) := NULL]

        if (design_$sim_prm$logs) message("Generate Med_DM")

        tbl <-
            read_fst("./inputs/exposure_distributions/Table_Med_DM.fst",
                as.data.table = TRUE
            )[Year == setyear & between(Age, self$design$sim_prm$ageL, self$design$sim_prm$ageH)]
        tbl[, Year := NULL]
        setnames(tbl, tolower(names(tbl)))
        tbl[, sex := factor(sex, 0:1, c("men", "women")), ]
        col_nam <-
            setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
        absorb_dt(synthpop$pop, tbl)

        synthpop$pop[year > setyear, Med_DM_curr_xps := qbinom(rankstat_Med_DM, 1L, mu)] # , n_cpu = design_$sim_prm$n_cpu)]
        synthpop$pop[, c(col_nam) := NULL]

        # TODO PA and below

        NULL
    }
)

IMPACTncd$
  run(1:mcnum, multicore = TRUE, "biol")

# TODO scenario all RF

IMPACTncd$export_summaries(
  multicore = TRUE,
  type = c(
    "le", "hle", "dis_char", "prvl",
    "incd", "dis_mrtl", "mrtl",
    "allcause_mrtl_by_dis", "cms"
  )
)

source("./auxil/process_out.R")


# sp$pop colnames 
#  [1] "pid"                     "year"                    "age"                     "sex"                     "type"                   
#  [6] "rank_Fruit_vege"         "rankstat_Smoking_act"    "rankstat_Smoking_ex"     "rankstat_Med_HT"         "rankstat_Med_HL"        
# [11] "rankstat_Med_DM"         "rank_PA_days"            "rank_BMI"                "rank_HbA1c"              "rank_LDLc"              
# [16] "rank_SBP"                "rankstat_Smoking_number" "Fruit_vege_curr_xps"     "Smoking_curr_xps"        "Smoking_number_curr_xps"
# [21] "Med_HT_curr_xps"         "Med_HL_curr_xps"         "Med_DM_curr_xps"         "PA_days_curr_xps"        "BMI_curr_xps"           
# [26] "HbA1c_curr_xps"          "LDLc_curr_xps"           "SBP_curr_xps"            "pid_mrk"                 "wt_immrtl"              
# [31] "all_cause_mrtl"          "cms_score"               "cms_count" 


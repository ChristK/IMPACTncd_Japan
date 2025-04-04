# https://stackoverflow.com/questions/53622354/how-to-debug-line-by-line-rcpp-generated-code-in-windows
# R -d gdb -e "source('debug.r')"
# break simcpp
# run
source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

# IMPACTncd$del_parfs()
# IMPACTncd$del_synthpops()

# g <- IMPACTncd$get_causal_structure(print_plot = TRUE)
# g <- IMPACTncd$get_causal_structure(processed = FALSE, print_plot = TRUE, focus = "chd")

# plot(igraph::make_ego_graph(g, order = 1, c("chd"), "in")[[1]])

mcnum <- 50

IMPACTncd$
  del_logs()$
  del_outputs()$
  run(1:mcnum, multicore = TRUE, "sc0")



# HTN treatment for the untreated scenario: 10%
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    # setting prop of ht treat initiation in the untreated
    init_pcnt <- 10
    
    # years when ht treatment is modified
    years <- 2019:2039 # from 2020 through 2040
    
    # for() loop for ht treatment modification
    for(i in 1:length(years)){
      id0 <- synthpop$pop[year==years[i] & Med_HT_curr_xps==0 & SBP_curr_xps>=140, pid]
      id1 <- synthpop$pop[year==years[i]+1 & Med_HT_curr_xps==0 & SBP_curr_xps>=140 & pid %in% id0, pid]
      n_id1 <- length(id1)
      if (n_id1 > 1L) {
        id_init <- sample(id1, round(n_id1 * init_pcnt / 100))
      } else {
        id_init <- id1
      }
      synthpop$pop[year>years[i] & pid %in% id_init, Med_HT_curr_xps := 1L]
    }
    
    # then do the SBP
    tbl <- read_fst("./inputs/exposure_distributions/Table_SBP.fst",
                    as.data.table = TRUE
    )[between(Age, self$design$sim_prm$ageL - self$design$sim_prm$maxlag, self$design$sim_prm$ageH)]
    setnames(tbl, 
             old = c("Year","Age","Sex","BMI","Smoking","Med_HT"),
             new = c("year","age","sex","BMI_round","smoking_tmp","Med_HT_curr_xps"))
    tbl[, sex := factor(sex, 0:1, c("men", "women"))]
    tbl[, BMI_round := as.integer(10 * BMI_round)]
    tbl[, smoking_tmp := as.integer(smoking_tmp)]
    synthpop$pop[, BMI_round := as.integer(round(10 * BMI_curr_xps, 0))]
    synthpop$pop[, smoking_tmp := as.integer(Smoking_curr_xps==3L)]
    col_nam <-
      setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
    absorb_dt(synthpop$pop, tbl)
    if (anyNA(synthpop$pop[, ..col_nam])) stop("NA in the exposure distribution")
    synthpop$pop[year>years[1], SBP_curr_xps := qBCPE(rank_SBP, mu, sigma, nu, tau)]

    # synthpop$pop[year > years[1], hist(SBP_curr_xps2 - SBP_curr_xps)]
    # synthpop$pop[(SBP_curr_xps2 - SBP_curr_xps) > 1, .(year, pid, SBP_curr_xps2, SBP_curr_xps, Med_HT_curr_xps)]


    synthpop$pop[, c(col_nam, "BMI_round", "smoking_tmp") := NULL]
    NULL
  }
)

IMPACTncd$
  run(1:mcnum, multicore = TRUE, "httrt_10pcnt")



# HTN treatment for the untreated scenario: 20%
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    # setting prop of ht treat initiation in the untreated
    init_pcnt <- 20
    
    # years when ht treatment is modified
    years <- 2019:2039 # from 2020 through 2040
    
    # for() loop for ht treatment modification
    for(i in 1:length(years)){
      id0 <- synthpop$pop[year==years[i] & Med_HT_curr_xps==0 & SBP_curr_xps>=140, pid]
      id1 <- synthpop$pop[year==years[i]+1 & Med_HT_curr_xps==0 & SBP_curr_xps>=140 & pid %in% id0, pid]
      n_id1 <- length(id1)
      id_init <- sample(id1, round(n_id1 * init_pcnt/100))
      synthpop$pop[year>years[i] & pid %in% id_init, Med_HT_curr_xps := 1]
    }
    
    # then do the SBP
    tbl <- read_fst("./inputs/exposure_distributions/Table_SBP.fst",
                    as.data.table = TRUE
                    )[between(Age, self$design$sim_prm$ageL - self$design$sim_prm$maxlag, self$design$sim_prm$ageH)]
    setnames(tbl, 
             old = c("Year","Age","Sex","BMI","Smoking","Med_HT"),
             new = c("year","age","sex","BMI_round","smoking_tmp","Med_HT_curr_xps"))
    tbl[, sex := factor(sex, 0:1, c("men", "women"))]
    tbl[, BMI_round := as.integer(10 * BMI_round)]
    tbl[, smoking_tmp := as.integer(smoking_tmp)]
    synthpop$pop[, BMI_round := as.integer(round(10 * BMI_curr_xps, 0))]
    synthpop$pop[, smoking_tmp := as.integer(Smoking_curr_xps==3)]
    col_nam <-
      setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
    absorb_dt(synthpop$pop, tbl)
    if (anyNA(synthpop$pop[, ..col_nam])) stop("NA in the exposure distribution")
    synthpop$pop[year>years[1], SBP_curr_xps := qBCPE(rank_SBP, mu, sigma, nu, tau)]
    synthpop$pop[, c(col_nam, "BMI_round", "smoking_tmp") := NULL]
    NULL
  }
)

IMPACTncd$
  run(1:mcnum, multicore = TRUE, "httrt_20pcnt")



# HTN treatment for the untreated scenario: 30%
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    # setting prop of ht treat initiation in the untreated
    init_pcnt <- 30
    
    # years when ht treatment is modified
    years <- 2019:2039 # from 2020 through 2040
    
    # for() loop for ht treatment modification
    for(i in 1:length(years)){
      id0 <- synthpop$pop[year==years[i] & Med_HT_curr_xps==0 & SBP_curr_xps>=140, pid]
      id1 <- synthpop$pop[year==years[i]+1 & Med_HT_curr_xps==0 & SBP_curr_xps>=140 & pid %in% id0, pid]
      n_id1 <- length(id1)
      id_init <- sample(id1, round(n_id1 * init_pcnt/100))
      synthpop$pop[year>years[i] & pid %in% id_init, Med_HT_curr_xps := 1]
    }
    
    # then do the SBP
    tbl <- read_fst("./inputs/exposure_distributions/Table_SBP.fst",
                    as.data.table = TRUE
    )[between(Age, self$design$sim_prm$ageL - self$design$sim_prm$maxlag, self$design$sim_prm$ageH)]
    setnames(tbl, 
             old = c("Year","Age","Sex","BMI","Smoking","Med_HT"),
             new = c("year","age","sex","BMI_round","smoking_tmp","Med_HT_curr_xps"))
    tbl[, sex := factor(sex, 0:1, c("men", "women"))]
    tbl[, BMI_round := as.integer(10 * BMI_round)]
    tbl[, smoking_tmp := as.integer(smoking_tmp)]
    synthpop$pop[, BMI_round := as.integer(round(10 * BMI_curr_xps, 0))]
    synthpop$pop[, smoking_tmp := as.integer(Smoking_curr_xps==3)]
    col_nam <-
      setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
    absorb_dt(synthpop$pop, tbl)
    if (anyNA(synthpop$pop[, ..col_nam])) stop("NA in the exposure distribution")
    synthpop$pop[year>years[1], SBP_curr_xps := qBCPE(rank_SBP, mu, sigma, nu, tau)]
    synthpop$pop[, c(col_nam, "BMI_round", "smoking_tmp") := NULL]
    NULL
  }
)

IMPACTncd$
  run(1:mcnum, multicore = TRUE, "httrt_30pcnt")


# HTN treatment for the untreated scenario: 40%
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    # setting prop of ht treat initiation in the untreated
    init_pcnt <- 40
    
    # years when ht treatment is modified
    years <- 2019:2039 # from 2020 through 2040
    
    # for() loop for ht treatment modification
    for(i in 1:length(years)){
      id0 <- synthpop$pop[year==years[i] & Med_HT_curr_xps==0 & SBP_curr_xps>=140, pid]
      id1 <- synthpop$pop[year==years[i]+1 & Med_HT_curr_xps==0 & SBP_curr_xps>=140 & pid %in% id0, pid]
      n_id1 <- length(id1)
      id_init <- sample(id1, round(n_id1 * init_pcnt/100))
      synthpop$pop[year>years[i] & pid %in% id_init, Med_HT_curr_xps := 1]
    }
    
    # then do the SBP
    tbl <- read_fst("./inputs/exposure_distributions/Table_SBP.fst",
                    as.data.table = TRUE
    )[between(Age, self$design$sim_prm$ageL - self$design$sim_prm$maxlag, self$design$sim_prm$ageH)]
    setnames(tbl, 
             old = c("Year","Age","Sex","BMI","Smoking","Med_HT"),
             new = c("year","age","sex","BMI_round","smoking_tmp","Med_HT_curr_xps"))
    tbl[, sex := factor(sex, 0:1, c("men", "women"))]
    tbl[, BMI_round := as.integer(10 * BMI_round)]
    tbl[, smoking_tmp := as.integer(smoking_tmp)]
    synthpop$pop[, BMI_round := as.integer(round(10 * BMI_curr_xps, 0))]
    synthpop$pop[, smoking_tmp := as.integer(Smoking_curr_xps==3)]
    col_nam <-
      setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
    absorb_dt(synthpop$pop, tbl)
    if (anyNA(synthpop$pop[, ..col_nam])) stop("NA in the exposure distribution")
    synthpop$pop[year>years[1], SBP_curr_xps := qBCPE(rank_SBP, mu, sigma, nu, tau)]
    synthpop$pop[, c(col_nam, "BMI_round", "smoking_tmp") := NULL]
    NULL
  }
)

IMPACTncd$
  run(1:mcnum, multicore = TRUE, "httrt_40pcnt")


# HTN treatment for the untreated scenario: 50%
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    # setting prop of ht treat initiation in the untreated
    init_pcnt <- 50
    
    # years when ht treatment is modified
    years <- 2019:2039 # from 2020 through 2040
    
    # for() loop for ht treatment modification
    for(i in 1:length(years)){
      id0 <- synthpop$pop[year==years[i] & Med_HT_curr_xps==0 & SBP_curr_xps>=140, pid]
      id1 <- synthpop$pop[year==years[i]+1 & Med_HT_curr_xps==0 & SBP_curr_xps>=140 & pid %in% id0, pid]
      n_id1 <- length(id1)
      id_init <- sample(id1, round(n_id1 * init_pcnt/100))
      synthpop$pop[year>years[i] & pid %in% id_init, Med_HT_curr_xps := 1]
    }
    
    # then do the SBP
    tbl <- read_fst("./inputs/exposure_distributions/Table_SBP.fst",
                    as.data.table = TRUE
    )[between(Age, self$design$sim_prm$ageL - self$design$sim_prm$maxlag, self$design$sim_prm$ageH)]
    setnames(tbl, 
             old = c("Year","Age","Sex","BMI","Smoking","Med_HT"),
             new = c("year","age","sex","BMI_round","smoking_tmp","Med_HT_curr_xps"))
    tbl[, sex := factor(sex, 0:1, c("men", "women"))]
    tbl[, BMI_round := as.integer(10 * BMI_round)]
    tbl[, smoking_tmp := as.integer(smoking_tmp)]
    synthpop$pop[, BMI_round := as.integer(round(10 * BMI_curr_xps, 0))]
    synthpop$pop[, smoking_tmp := as.integer(Smoking_curr_xps==3)]
    col_nam <-
      setdiff(names(tbl), intersect(names(synthpop$pop), names(tbl)))
    absorb_dt(synthpop$pop, tbl)
    if (anyNA(synthpop$pop[, ..col_nam])) stop("NA in the exposure distribution")
    synthpop$pop[year>years[1], SBP_curr_xps := qBCPE(rank_SBP, mu, sigma, nu, tau)]
    synthpop$pop[, c(col_nam, "BMI_round", "smoking_tmp") := NULL]
    NULL
  }
)

IMPACTncd$
  run(1:mcnum, multicore = TRUE, "httrt_50pcnt")


IMPACTncd$export_summaries(
  multicore = TRUE,
  type = c(
    "le", "hle", "dis_char", "prvl",
    "incd", "dis_mrtl", "mrtl",
    "allcause_mrtl_by_dis", "cms", 
    "qalys", "costs"
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
    absorb_dt(synthpop$pop, tbl)

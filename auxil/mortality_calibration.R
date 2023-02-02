
new_runs <- FALSE
new_calibration <- FALSE # SET CALIBRATION IN DESIGN FILE TO "NO" BEFORE ATTEMPT
check_runs <- TRUE

if(new_runs || check_runs){
  
  source("./global.R")
  
} else {
  
  library(fst)
  library(data.table)
  
}

# if(file.exists("./inputs/mortality/mrtl_clb.fst")){
#   
#   e <- read_fst("./inputs/mortality/mrtl_clb.fst", as.data.table = TRUE)
#   e[, mrtl_clbr := 1] # Cancel previous calibrarion
#   write_fst(e, "./inputs/mortality/mrtl_clb.fst")
# 
# }

if(new_runs){
  
  runif(1)
  
  IMPACTncd <- Simulation$new("./inputs/sim_design_mrtl_clbr.yaml")
  
  IMPACTncd$del_outputs()$del_logs()
  
  # Original iterations #
  batch_size <- 4
  iterations <- 100
  batches <- split(seq(1, iterations),
                   f = findInterval(seq(1, iterations),
                                    vec = seq(1, iterations, batch_size)))
  for(i in batches){
    
    scenario_fn <- function(sp) NULL
    
    IMPACTncd$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)
    
  }
  IMPACTncd$export_summaries(multicore = TRUE, type = "dis_mrtl")
  
}

if(new_calibration){
  
  lifetable_all <- read_fst("./inputs/mortality/mort_prcjt.fst", as.data.table = TRUE) # Load FDM mortality forecast
  lifetable_all[, `:=` (year = year - 2000L)]
  
  ## Calibration factors for nonmodelled mortality ##
  
  lifetable_nonmodelled <- lifetable_all[disease == "other"][, disease := NULL]
  
  # WARNING: For some reason some iteration have trailing comma!
  file_lines <- readLines("./outputs/summaries/dis_mrtl_scaled_up.csv.gz")
  writeLines(gsub(",+$", "", file_lines), "./outputs/summaries/dis_mrtl_scaled_up.csv.gz")
  
  tt <- fread("./outputs/summaries/dis_mrtl_scaled_up.csv.gz"
  )[scenario == "sc0"]
  e <- tt[, .(mrtl_rate = sum(nonmodelled)/sum(popsize)), keyby = .(year, agegrp, sex)]
  e[, agegrp := ifelse(agegrp == "90-94", "90+", agegrp)]
  
  e[lifetable_nonmodelled, on = c("year", "agegrp", "sex"), mrtl_clbr := mx_total_mean/mrtl_rate]
  e[, summary(mrtl_clbr)]
  e[is.infinite(mrtl_clbr), ]
  e[is.infinite(mrtl_clbr), mrtl_clbr := 1]
  
  tt <- data.table(age = 30:90, agegrp = CKutils::agegrp_name(30, 90, match_input = TRUE, match_input_max_age = 90))
  e <- tt[e, on = .NATURAL, allow.cartesian = TRUE]
  e[, c("mrtl_rate", "agegrp") := NULL]
  e[, sex := factor(sex, c("men", "women"))]
  
  setkeyv(e, c("year", "age", "sex"))
  
  if(exists("IMPACTncd")){
    IMPACTncdEngl::is_valid_lookup_tbl(e, c("year", "age", "sex"))
  }
  write_fst(e, "./inputs/mortality/mrtl_clbr_nonmodelled.fst")
  
  ## Calibration factors for CHD mortality ##
  
  lifetable_chd <- lifetable_all[disease == "chd"][, disease := NULL]
  
  tt <- fread("./outputs/summaries/dis_mrtl_scaled_up.csv.gz"
  )[scenario == "sc0"]
  e <- tt[, .(mrtl_rate = sum(chd)/sum(popsize)), keyby = .(year, agegrp, sex)]
  e[, agegrp := ifelse(agegrp == "90-94", "90+", agegrp)]
  
  e[lifetable_chd, on = c("year", "agegrp", "sex"), mrtl_clbr := mx_total_mean/mrtl_rate]
  e[, summary(mrtl_clbr)]
  e[is.infinite(mrtl_clbr), ]
  e[is.infinite(mrtl_clbr), mrtl_clbr := 1]
  
  tt <- data.table(age = 30:90, agegrp = CKutils::agegrp_name(30, 90, match_input = TRUE, match_input_max_age = 90))
  e <- tt[e, on = .NATURAL, allow.cartesian = TRUE]
  e[, c("mrtl_rate", "agegrp") := NULL]
  e[, sex := factor(sex, c("men", "women"))]
  
  setkeyv(e, c("year", "age", "sex"))
  
  if(exists("IMPACTncd")){
    IMPACTncdEngl::is_valid_lookup_tbl(e, c("year", "age", "sex"))
  }
  write_fst(e, "./inputs/mortality/mrtl_clbr_chd.fst")
  
  ## Calibration factors for CHD mortality ##
  
  lifetable_stroke <- lifetable_all[disease == "stroke"][, disease := NULL]
  
  tt <- fread("./outputs/summaries/dis_mrtl_scaled_up.csv.gz"
  )[scenario == "sc0"]
  e <- tt[, .(mrtl_rate = sum(stroke)/sum(popsize)), keyby = .(year, agegrp, sex)]
  e[, agegrp := ifelse(agegrp == "90-94", "90+", agegrp)]
  
  e[lifetable_stroke, on = c("year", "agegrp", "sex"), mrtl_clbr := mx_total_mean/mrtl_rate]
  e[, summary(mrtl_clbr)]
  e[is.infinite(mrtl_clbr), ]
  e[is.infinite(mrtl_clbr), mrtl_clbr := 1]
  
  tt <- data.table(age = 30:90, agegrp = CKutils::agegrp_name(30, 90, match_input = TRUE, match_input_max_age = 90))
  e <- tt[e, on = .NATURAL, allow.cartesian = TRUE]
  e[, c("mrtl_rate", "agegrp") := NULL]
  e[, sex := factor(sex, c("men", "women"))]
  
  setkeyv(e, c("year", "age", "sex"))
  
  if(exists("IMPACTncd")){
    IMPACTncdEngl::is_valid_lookup_tbl(e, c("year", "age", "sex"))
  }
  write_fst(e, "./inputs/mortality/mrtl_clbr_stroke.fst")
  
}

if(check_runs){
  
  runif(1)
  
  IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")
  
  IMPACTncd$del_outputs()$del_logs()
  
  # Original iterations #
  batch_size <- 4
  iterations <- 50
  batches <- split(seq(1, iterations),
                   f = findInterval(seq(1, iterations),
                                    vec = seq(1, iterations, batch_size)))
  for(i in batches){
    
    scenario_fn <- function(sp) NULL
    
    IMPACTncd$
      run(i, multicore = TRUE, "sc0", m_zero_trend = -0.03, p_zero_trend = 0)
    
  }
  IMPACTncd$export_summaries(multicore = TRUE, type = c("dis_mrtl"))
  
}

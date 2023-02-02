
## Health Economic Analyses ##

library(data.table)
library(CKutils)
library(doParallel)
library(fst)


if (Sys.info()["sysname"] == "Windows") {
  cl <-
    makeCluster(detectCores()/8) # used for clustering. Windows compatible
  registerDoParallel(cl)
} else {
  registerDoParallel(detectCores()/8) # used for forking. Only Linux/OSX compatible
}

## Computation of QALYs and costs per MC iteration ##

iterations <- list.files("./outputs/lifecourse/")

out <- foreach(i = iterations,
               .packages = c("data.table",
                             "CKutils",
                             "fst",
                             "MESS")) %dopar% {
  
  # Load lifecourse file #
  
  lc <- fread(paste0("./outputs/lifecourse/", i))
  
  # Set MC iteration #
  
  mc_ <- as.integer(unique(lc$mc))
  
  lc[, agegrp_start := first(agegrp), by = "pid"]
  
  cea_strata <- c("scenario", "sex", "agegrp_start")
  
  # Load health utility data #
  
  util_indx <- read_fst("./inputs/other_parameters/health_utility_indx.fst", as.data.table = TRUE)
  
  ro <- util_indx[
    mc %in% mc_, 
    .("from" = min(from), "to" = max(to))
  ]
  
  util <- read_fst(
    "./inputs/other_parameters/health_utility.fst",
    as.data.table = TRUE,
    from = ro$from,
    to = ro$to
  )
  
  # Load healthcare cost data #
  
  cost_indx <- read_fst("./inputs/other_parameters/healthcare_costs_indx.fst", as.data.table = TRUE)
  
  ro <- cost_indx[
    mc %in% mc_, 
    .("from" = min(from), "to" = max(to))
  ]
  
  cost <- read_fst(
    "./inputs/other_parameters/healthcare_costs.fst",
    as.data.table = TRUE,
    from = ro$from,
    to = ro$to
  )
  
  # Setup lifecourse #
  
  lc[, `:=`(t2dm_stat = fifelse(t2dm_prvl > 1, 1, 0),
            chd_stat = fifelse(chd_prvl == 1, 1,
                               fifelse(chd_prvl > 1, 2,
                                       fifelse(chd_prvl > 0 & all_cause_mrtl == 2, 3, 0))), # Mortality from CHD
            stroke_stat = fifelse(stroke_prvl == 1, 1,
                                  fifelse(stroke_prvl > 1, 2,
                                          fifelse(stroke_prvl > 0 & all_cause_mrtl == 3, 3, 0))))] # Mortality from stroke
  
  to_agegrp(lc, grp_width = 10, min_age = 50, max_age = 80, agegrp_colname = "age_cost")
  lc[is.na(age_cost), age_cost := fifelse(age < 50, "<50", "80+")]
  
  absorb_dt(lc, util)
  absorb_dt(lc, cost)
  
  # Calculate health utility #
  
  lc[, health_util := util_incpt + age * util_age + util_sex + bmi_curr_xps * util_bmi + util_disease]
  
  # Setup results object #
  
  cea <- CJ(scenario = unique(lc$scenario),
            pid = unique(lc$pid))
  
  # qalys_out <- data.table(NULL)
  #
  # Calculate QALYs #
  # 
  # for(j in pid_batches){
  #   
  #   qalys_out_x <- lc[pid %in% j, lapply(.SD, function(x){
  #     
  #     if(length(x) > 1) {
  #       
  #       q <- integrate(approxfun(c(1:length(x)), x),
  #                      lower = range(c(1:length(x)))[1],
  #                      upper = range(c(1:length(x)))[2], abs.tol = 0.01)$value
  #       
  #     } else {
  #       
  #       q <- x # For individuals with only one year per scenario
  #       
  #     }
  #     
  #     return(q)
  #     
  #   }), .SDcols = "health_util", by = .(pid, scenario)]
  #   
  #   qalys_out <- rbind(qalys_out, qalys_out_x)
  #   
  # }
  # setnames(qalys_out, "health_util", "qalys_out")
  # 
  # absorb_dt(cea, qalys_out)
  
  setkeyv(lc, c("pid", cea_strata))
  
  qalys_scaled <- lc[, lapply(.SD, function(x){
    
    if(length(x) > 1) {
      
      q <- MESS::auc(c(1:length(x)), x * wt)
      
    } else {
      
      q <- x * wt # For individuals with only one year per scenario
      
    }
    
    return(q) 
    
  }), .SDcols = "health_util", keyby = c("pid", cea_strata)]
  
  setnames(qalys_scaled, "health_util", "qalys_scl")
  
  absorb_dt(cea, qalys_scaled)
  
  qalys_esp <- lc[, lapply(.SD, function(x){
    
    if(length(x) > 1) {
      
      q <- MESS::auc(c(1:length(x)), x * wt_esp)
      
    } else {
      
      q <- x * wt_esp # For individuals with only one year per scenario
      
    }
    
    return(q)
    
  }), .SDcols = "health_util", keyby = c("pid", cea_strata)]
  
  setnames(qalys_esp, "health_util", "qalys_esp")
  
  absorb_dt(cea, qalys_esp)
  
  
  # Calculate Healthcare Costs #
  
  costs_scl <- lc[, lapply(.SD, function(x){
    
    x <- sum(x * wt)
    
    return(x)
    
  }), .SDcols = c("cost", "cost_t2dm", "cost_chd", "cost_stroke"),
  keyby = c("pid", cea_strata)]
  
  setnames(costs_scl, c("cost", "cost_t2dm", "cost_chd", "cost_stroke"),
           c("cost_scl", "cost_t2dm_scl", "cost_chd_scl", "cost_stroke_scl"))
  
  absorb_dt(cea, costs_scl)
  
  
  costs_esp <- lc[, lapply(.SD, function(x){
    
    x <- sum(x * wt_esp)
    
    return(x)
    
  }), .SDcols = c("cost", "cost_t2dm", "cost_chd", "cost_stroke"),
  keyby = c("pid", cea_strata)]
  
  setnames(costs_esp, c("cost", "cost_t2dm", "cost_chd", "cost_stroke"),
           c("cost_esp", "cost_t2dm_esp", "cost_chd_esp", "cost_stroke_esp"))
  
  absorb_dt(cea, costs_esp)
  
  
  cea[, `:=`(
    disease_costs_scl = cost_t2dm_scl + cost_chd_scl + cost_stroke_scl,
    disease_costs_esp = cost_t2dm_esp + cost_chd_esp + cost_stroke_esp,
    tot_costs_scl = cost_scl + cost_t2dm_scl + cost_chd_scl + cost_stroke_scl,
    tot_costs_esp = cost_esp + cost_t2dm_esp + cost_chd_esp + cost_stroke_esp
    )]
  
  cea_agg <- cea[, lapply(.SD, sum),
                 .SDcols = c("tot_costs_esp", "tot_costs_scl",
                             "disease_costs_scl", "disease_costs_esp",
                             "cost_t2dm_scl", "cost_t2dm_esp",
                             "cost_chd_scl", "cost_chd_esp",
                             "cost_stroke_scl", "cost_stroke_esp",
                             "qalys_esp", "qalys_scl"),
                 keyby = cea_strata]
  
  scenarios <- unique(cea_agg$scenario)[unique(cea_agg$scenario) != "sc0"] # Exclude baseline scenario
  
  cea_diff <- data.table(NULL)
  
  for(i in scenarios){
    
    xx <- cea_agg[scenario %in% c("sc0", i)]
    
    setkeyv(xx, cea_strata[cea_strata != "scenario"])
    
    xx[, `:=`(
      incr_qaly_scl = qalys_scl - shift(qalys_scl),
      incr_cost_scl = tot_costs_scl - shift(tot_costs_scl),
      incr_dis_cost_scl = disease_costs_scl - shift(disease_costs_scl),
      incr_t2dm_cost_scl = cost_t2dm_scl - shift(cost_t2dm_scl),
      incr_chd_cost_scl = cost_chd_scl - shift(cost_chd_scl),
      incr_stroke_cost_scl = cost_stroke_scl - shift(cost_stroke_scl),
      incr_qaly_esp = qalys_esp - shift(qalys_esp),
      incr_cost_esp = tot_costs_esp - shift(tot_costs_esp),
      incr_dis_cost_esp = disease_costs_esp - shift(disease_costs_esp),
      incr_t2dm_cost_esp = cost_t2dm_esp - shift(cost_t2dm_esp),
      incr_chd_cost_esp = cost_chd_esp - shift(cost_chd_esp),
      incr_stroke_cost_esp = cost_stroke_esp - shift(cost_stroke_esp)
      ), keyby = .(agegrp_start, sex)]
    
    cea_diff <- rbind(cea_diff, xx)
    
  }
  
  cea_diff <- rbind(cea_diff[scenario != "sc0"],
                    cea_diff[scenario == "sc0", lapply(.SD, mean),
                             .SDcols = !cea_strata,
                             by = cea_strata])
  
  cea_diff[, mc := mc_]
  
  fwrite_safe(cea_diff,
              private$output_dir(paste0("summaries/", "cea_results.csv.gz")))


}






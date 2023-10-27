source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

scenario_fn_primary_prevention   <- function(sp) NULL
scenario_fn_secondary_prevention <- function(sp) NULL

IMPACTncd$
  del_logs()$
  del_outputs()$
  calibrate_incd_ftlt(1:10, replace = FALSE)$
  del_logs()$
  del_outputs()$
  run(1:10, multicore = TRUE, "sc0")$
  export_summaries(multicore = TRUE,
  type = c(
    "prvl", "incd", "dis_mrtl", "mrtl",
    "allcause_mrtl_by_dis"))$
  validate()

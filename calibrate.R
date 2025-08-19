source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design_clbr.yaml")

IMPACTncd$
  del_logs()$
  del_outputs()$
  del_parfs()$
  calibrate_incd_ftlt(1:10, replace = FALSE)$
  del_logs()$
  del_outputs()
  
# Run validation if TRUE.
if (TRUE) {
  IMPACTncd$
    run(1:10, multicore = TRUE, "sc0")$
    export_summaries(
    multicore = TRUE,
    type = c(
      "prvl", "incd", "dis_mrtl", "mrtl",
      "all_cause_mrtl_by_dis"
    )
  )$
    validate()
}
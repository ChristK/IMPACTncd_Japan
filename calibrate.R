source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

IMPACTncd$
  del_logs()$
  del_outputs()$
  calibrate_incd_ftlt(1:100, replace = FALSE)$
  del_logs()$
  del_outputs()
  
# Run validation if TRUE. 
if (FALSE) {
  IMPACTncd$
    run(1:100, multicore = TRUE, "sc0")$
    export_summaries(
    multicore = TRUE,
    type = c(
      "prvl", "incd", "dis_mrtl", "mrtl",
      "allcause_mrtl_by_dis"
    )
  )$
    validate()
}
# https://stackoverflow.com/questions/53622354/how-to-debug-line-by-line-rcpp-generated-code-in-windows
# R -d gdb -e "source('debug.r')"
# break simcpp
# run
source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

# g <- IMPACTncd$get_causal_structure(print_plot = TRUE)
# g <- IMPACTncd$get_causal_structure(processed = FALSE, print_plot = TRUE, focus = "chd")

# plot(igraph::make_ego_graph(g, order = 1, c("chd"), "in")[[1]])

scenario_fn_primary_prevention   <- function(sp) NULL
scenario_fn_secondary_prevention <- function(sp) NULL

IMPACTncd$
  del_logs()$
  del_outputs()$
  run(1:2, multicore = TRUE, "sc0")

IMPACTncd$export_summaries(
  multicore = TRUE,
  type = c(
    "le", "hle", "dis_char", "prvl",
    "incd", "dis_mrtl", "mrtl",
    "allcause_mrtl_by_dis", "cms"
  )
)

source("./auxil/process_out.R")
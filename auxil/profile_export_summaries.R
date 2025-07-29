source("./global.R")
library(htmlwidgets)
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

# system.time(IMPACTncd$export_summaries(
#   multicore = TRUE,
#   type = c(
#     "prvl"
#     # "le", "hle", "dis_char", "prvl",
#     # "incd", "dis_mrtl", "mrtl",
#     # "all_cause_mrtl_by_dis",
#     #  "cms", "qalys" , "costs"
#   )
# ))
# 1060 sec with 4 cores and not disabling implicit parallelism

# lc <-  as.data.table(open_dataset("/mnt/storage_fast4/jpn/outputs/lifecourse/mc=1")) # "10.2 Gb"
self <- IMPACTncd$.__enclos_env__$self
private <- IMPACTncd$.__enclos_env__$private
mcaggr <- 5L
strata <- c("mc", self$design$sim_prm$strata_for_output)
strata_noagegrp <- c(
  "mc",
  setdiff(self$design$sim_prm$strata_for_output, c("agegrp"))
)
strata_age <- c(strata_noagegrp, "age")
ext <- "parquet"
arrow::set_cpu_count(1L)
data.table::setDTthreads(threads = 1L, restore_after_fork = NULL)

p <- profvis::profvis({
  lc <- open_dataset(private$output_dir(file.path(
    "lifecourse",
    paste0("mc=", mcaggr)
  )))
  duckdb_con <- dbConnect(duckdb::duckdb(), ":memory:", read_only = FALSE) # not read only to allow creation of VIEWS etc
  dbExecute(duckdb_con, "PRAGMA threads=1")
  duckdb::duckdb_register_arrow(duckdb_con, "lc_table_raw", lc)
  dbExecute(
    duckdb_con,
    sprintf(
      "CREATE VIEW lc_table AS SELECT *, %d::INTEGER AS mc FROM lc_table_raw",
      mcaggr
    )
  )

  private$export_costs_summaries(duckdb_con, mcaggr, strata, ext) 
  dbDisconnect(duckdb_con, shutdown = TRUE)
})

saveWidget(p, file = "./auxil/profvis_export_summaries.html", selfcontained = TRUE)
# browseURL("./auxil/profvis_export_summaries.html")
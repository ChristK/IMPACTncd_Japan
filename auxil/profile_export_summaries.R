source("./global.R")
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

# p <- profvis::profvis({
  lc <- open_dataset(private$output_dir("lifecourse"))
  duckdb_con <- dbConnect(duckdb::duckdb(), ":memory:", read_only = FALSE) # not read only to allow creation of VIEWS etc
  dbExecute(duckdb_con, "PRAGMA threads=1")
  duckdb::duckdb_register_arrow(duckdb_con, "lc_table_raw", lc)
  dbExecute(
    duckdb_con,
    sprintf(
      "CREATE VIEW lc_table AS SELECT * FROM lc_table_raw WHERE mc = %d",
      mcaggr
    )
  )

  private$export_dis_char_summaries(duckdb_con, mcaggr, strata, ext) 
  dbDisconnect(duckdb_con, shutdown = TRUE)
# })

# htmlwidgets::saveWidget(p, file = "./auxil/profvis_export_summaries.html", selfcontained = TRUE)
# browseURL("./auxil/profvis_export_summaries.html")

# costs                 4gb
# dis_char              1.2gb
# prvl                  2.7gb
# incd                  2.7gb
# dis_mrtl              1.8gb
# mrtl                  1.8gb
# all_cause_mrtl_by_dis 2.8gb
# cms                   0.5gb
# qalys                 2.5gb
# le                    0.9gb
# hle                   0.9gb


# ./auxil/mem_profile.sh ./auxil/profile_export_summaries.R ./auxil/memlog.txt
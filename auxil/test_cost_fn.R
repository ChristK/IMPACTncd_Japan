source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")
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


input_table_name <- "lc_table"
# Define the name for the temporary view that calc_costs will create
output_view_name <- "lc_with_costs"

# Call calc_costs to create/replace the temporary view with cost columns.
# This calculates cost parameters using sc0 baseline scenario data,
# but applies them to all scenarios for the current mcaggr.
private$calc_costs(
  duckdb_con = duckdb_con,
  mcaggr = mcaggr,
  input_table_name,
  output_view_name
)
dbGetQuery(
  duckdb_con,
  "
  SELECT table_name
    FROM information_schema.views
   WHERE table_schema = 'main';
"
)
# dbGetQuery(duckdb_con, "SELECT * FROM lc_with_costs_All_view LIMIT 10;")
dbGetQuery(duckdb_con, "SELECT COUNT(*) FROM lc_with_costs_All_view;")

# ./auxil/mem_profile.sh ./auxil/test_cost_fn.R ./auxil/memlog.txt

source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")
self <- IMPACTncd$.__enclos_env__$self
private <- IMPACTncd$.__enclos_env__$private

lc <- open_dataset(private$output_dir("lifecourse"))

# Connect DuckDB and register the Arrow dataset as a DuckDB view
duckdb_con <- dbConnect(duckdb::duckdb(), ":memory:", read_only = TRUE)
duckdb::duckdb_register_arrow(duckdb_con, "lc_table", lc)

lcdt <- setDT(lc |> filter(mc == 1L) |> collect())


single_year_of_age <- FALSE
mcaggr <- 1L

strata <- c("mc", self$design$sim_prm$strata_for_output)
strata_noagegrp <- c(
  "mc",
  setdiff(self$design$sim_prm$strata_for_output, c("agegrp"))
)
strata_age <- c(strata_noagegrp, "age")

if (single_year_of_age) strata <- strata_age # used for calibrate_incd_ftlt

ext <- "parquet"


private$calc_costs(lcdt)
tt <- lcdt[, c(
              # "popsize" = sum(wt),
              lapply(.SD, function(x, wt) sum(x * wt), wt)
            ),
            .SDcols = patterns("_costs$"), keyby = strata
            ]
# ====================== DuckDB version ======================#
lc <- open_dataset(private$output_dir("lifecourse"))

# Connect DuckDB and register the Arrow dataset as a DuckDB view
duckdb_con <- dbConnect(duckdb::duckdb(), ":memory:", read_only = TRUE)
duckdb::duckdb_register_arrow(duckdb_con, "lc_table", lc)

# Get disease prevalence columns from DuckDB schema
lc_table_name <- "lc_table"
# calc_costs_duckdb ----
# Refactored version of calc_costs to use DuckDB SQL.
# Creates a temporary view named 'output_view_name' in DuckDB,
# which is the 'input_table_name' (filtered by mcaggr) augmented with calculated cost columns.
calc_costs_duckdb <- function(duckdb_con, mcaggr, input_table_name, output_view_name) {
  # Helper function to execute SQL, with error reporting
  execute_sql <- function(sql, context = "") {
    # For debugging: message(paste("Executing SQL for", context, ":\n", sql))
    tryCatch(
      {
        dbExecute(duckdb_con, sql)
      },
      error = function(e) {
        stop(paste("Error executing SQL for", context, ":", e$message, "\nSQL:\n", sql))
      }
    )
  }

  # Helper to register a data.frame/data.table as a DuckDB temp table
  register_df_as_table <- function(df, table_name, con = duckdb_con) {
    duckdb::dbWriteTable(con, table_name, as.data.frame(df), overwrite = TRUE)
  }

  # --- Inflation Factors ---
  prod_informal_inflation_factor <- 1.025
  direct_costs_inflation_factor <- 99.6 / 99.7

  # --- Step 1: Initial Aggregations from lc_table ---
  # These views are filtered by year, scenario 'sc0', and mcaggr.
  base_agg_sql <- "
          CREATE OR REPLACE TEMP VIEW %s AS
          SELECT agegrp, sex, ROUND(SUM(CASE WHEN %s THEN wt ELSE 0 END)) AS V1
          FROM %s WHERE year = %d AND scenario = 'sc0' AND mc = %d GROUP BY agegrp, sex;
        "
  execute_sql(sprintf(base_agg_sql, "chd_prvl_2016_agg_view", "chd_dgns > 0", input_table_name, 2016, mcaggr), "chd_prvl_2016_agg_view")
  execute_sql(sprintf(base_agg_sql, "chd_prvl_2019_agg_view", "chd_dgns > 0", input_table_name, 2019, mcaggr), "chd_prvl_2019_agg_view")
  execute_sql(sprintf(base_agg_sql, "stroke_prvl_2016_agg_view", "stroke_dgns > 0", input_table_name, 2016, mcaggr), "stroke_prvl_2016_agg_view")
  execute_sql(sprintf(base_agg_sql, "stroke_prvl_2019_agg_view", "stroke_dgns > 0", input_table_name, 2019, mcaggr), "stroke_prvl_2019_agg_view")
  execute_sql(sprintf(base_agg_sql, "chd_mrtl_2016_initial_view", "all_cause_mrtl = 2", input_table_name, 2016, mcaggr), "chd_mrtl_2016_initial_view")
  execute_sql(sprintf(base_agg_sql, "stroke_mrtl_2016_initial_view", "all_cause_mrtl = 3", input_table_name, 2016, mcaggr), "stroke_mrtl_2016_initial_view")

  # --- Step 2: Load external data and update mortality views ---
  # (Handling to_agegrp by pre-aggregation in R before loading into DuckDB)
  obs_pop_df_2016 <- read_fst("inputs/pop_estimates/observed_population_japan.fst", as.data.table = TRUE)[year == 2016]

  # CHD Mortality Update
  chd_ftlt_df_2016 <- read_fst("inputs/disease_burden/chd_ftlt.fst", as.data.table = TRUE)[year == 2016]
  chd_ftlt_joined_2016 <- chd_ftlt_df_2016[obs_pop_df_2016, on = c("age", "year", "sex"), nomatch = 0L][, deaths_calc := mu2 * pops]
  to_agegrp(chd_ftlt_joined_2016, 5, 99L) # Assumes to_agegrp is in scope
  chd_ftlt_agg_ext_2016 <- chd_ftlt_joined_2016[, .(calculated_deaths = round(sum(deaths_calc))), keyby = .(agegrp, sex)]
  register_df_as_table(chd_ftlt_agg_ext_2016, "chd_ftlt_ext_2016_table")

  execute_sql("
          CREATE OR REPLACE TEMP VIEW chd_mrtl_2016_agg_view AS
          SELECT i.agegrp, i.sex, CASE WHEN i.V1 = 0 THEN COALESCE(f.calculated_deaths, i.V1) ELSE i.V1 END AS V1
          FROM chd_mrtl_2016_initial_view i LEFT JOIN chd_ftlt_ext_2016_table f ON i.agegrp = f.agegrp AND i.sex = f.sex;
        ", "chd_mrtl_2016_agg_view (updated)")

  # Stroke Mortality Update
  stroke_ftlt_df_2016 <- read_fst("inputs/disease_burden/stroke_ftlt.fst", as.data.table = TRUE)[year == 2016]
  stroke_ftlt_joined_2016 <- stroke_ftlt_df_2016[obs_pop_df_2016, on = c("age", "year", "sex"), nomatch = 0L][, deaths_calc := mu2 * pops]
  to_agegrp(stroke_ftlt_joined_2016, 5, 99L)
  stroke_ftlt_agg_ext_2016 <- stroke_ftlt_joined_2016[, .(calculated_deaths = round(sum(deaths_calc))), keyby = .(agegrp, sex)]
  register_df_as_table(stroke_ftlt_agg_ext_2016, "stroke_ftlt_ext_2016_table")

  execute_sql("
          CREATE OR REPLACE TEMP VIEW stroke_mrtl_2016_agg_view AS
          SELECT i.agegrp, i.sex, CASE WHEN i.V1 = 0 THEN COALESCE(f.calculated_deaths, i.V1) ELSE i.V1 END AS V1
          FROM stroke_mrtl_2016_initial_view i LEFT JOIN stroke_ftlt_ext_2016_table f ON i.agegrp = f.agegrp AND i.sex = f.sex;
        ", "stroke_mrtl_2016_agg_view (updated)")

  # --- Step 3: Define Cost Parameter Tables & Views ---
  # Helper for productivity/informal cost parameter calculation
  calc_cost_param_sql <- "
          CREATE OR REPLACE TEMP VIEW %s AS
          WITH joined_data AS (
            SELECT p.agegrp, p.sex, p.%s AS factor_col, agg.V1 FROM %s p JOIN %s agg ON p.agegrp = agg.agegrp AND p.sex = agg.sex
          ), weighted_data AS (
            SELECT *, (factor_col * V1) AS weighted_factor FROM joined_data
          ), total_weighted_sum AS (
            SELECT SUM(weighted_factor) AS total_wt_sum FROM weighted_data
          )
          SELECT wd.agegrp, wd.sex, (%f * wd.weighted_factor / tws.total_wt_sum) * %f / NULLIF(wd.V1, 0) AS cost_param
          FROM weighted_data wd, total_weighted_sum tws;
        "
  # Productivity Prevalence Costs
  employee_params_df <- data.table(agegrp = rep(c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), 2), sex = rep(c("men", "women"), each = 14), employees = c(1683780, 1829610, 2174550, 2057710, 1702470, 1425510, 963430, 369640, 106850, 0, 0, 0, 0, 0, 919700, 894770, 1049490, 1037140, 854970, 685040, 376370, 132470, 44050, 0, 0, 0, 0, 0))
  register_df_as_table(employee_params_df, "employee_params_table")
  execute_sql(sprintf(calc_cost_param_sql, "chd_prvl_prdv_cost_param_view", "employees", "employee_params_table", "chd_prvl_2016_agg_view", 141000000000.00, prod_informal_inflation_factor), "chd_prvl_prdv_cost_param_view")
  execute_sql(sprintf(calc_cost_param_sql, "stroke_prvl_prdv_cost_param_view", "employees", "employee_params_table", "stroke_prvl_2016_agg_view", 322000000000.00, prod_informal_inflation_factor), "stroke_prvl_prdv_cost_param_view")

  # Productivity Mortality Costs
  execute_sql(sprintf(calc_cost_param_sql, "chd_mrtl_prdv_cost_param_view", "employees", "employee_params_table", "chd_mrtl_2016_agg_view", 2257000000000.00, prod_informal_inflation_factor), "chd_mrtl_prdv_cost_param_view")
  execute_sql(sprintf(calc_cost_param_sql, "stroke_mrtl_prdv_cost_param_view", "employees", "employee_params_table", "stroke_mrtl_2016_agg_view", 1352000000000.00, prod_informal_inflation_factor), "stroke_mrtl_prdv_cost_param_view")

  # Informal Costs
  chd_infm_care_df <- data.table(agegrp = rep(c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), 2), sex = rep(c("men", "women"), each = 14), infm_care_hrs = c(0.030, 0.030, 0.030, 0.030, 0.030, 0.030, 0.030, 0.200, 0.200, 0.200, 0, 0, 0, 0, 0.030, 0.030, 0.030, 0.030, 0.030, 0.030, 0.030, 0.200, 0.200, 0.200, 0, 0, 0, 0))
  register_df_as_table(chd_infm_care_df, "chd_infm_care_table")
  execute_sql(sprintf(calc_cost_param_sql, "chd_informal_cost_param_view", "infm_care_hrs", "chd_infm_care_table", "chd_prvl_2016_agg_view", 291000000000.00, prod_informal_inflation_factor), "chd_informal_cost_param_view")

  stroke_infm_care_df <- data.table(agegrp = rep(c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), 2), sex = rep(c("men", "women"), each = 14), infm_care_hrs = c(5.20, 5.20, 5.20, 5.20, 5.20, 5.20, 5.20, 5.03, 5.03, 5.03, 9.23, 9.23, 9.23, 9.23, 5.20, 5.20, 5.20, 5.20, 5.20, 5.20, 5.20, 5.03, 5.03, 5.03, 9.23, 9.23, 9.23, 9.23))
  register_df_as_table(stroke_infm_care_df, "stroke_infm_care_table")
  execute_sql(sprintf(calc_cost_param_sql, "stroke_informal_cost_param_view", "infm_care_hrs", "stroke_infm_care_table", "stroke_prvl_2016_agg_view", 1651000000000.00, prod_informal_inflation_factor), "stroke_informal_cost_param_view")

  # Direct Costs
  direct_cost_param_sql <- "
          CREATE OR REPLACE TEMP VIEW %s AS
          WITH lc_with_agegrp2 AS (
            SELECT *, CASE agegrp
              WHEN '30-34' THEN '30-44' WHEN '35-39' THEN '30-44' WHEN '40-44' THEN '30-44'
              WHEN '45-49' THEN '45-64' WHEN '50-54' THEN '45-64' WHEN '55-59' THEN '45-64' WHEN '60-64' THEN '45-64'
              WHEN '65-69' THEN '65-69' WHEN '70-74' THEN '70-74' ELSE '75-99' END AS agegrp2
            FROM %s
          ), agg_by_agegrp2 AS (
            SELECT agegrp2, sex, SUM(V1) AS V1_sum FROM lc_with_agegrp2 GROUP BY agegrp2, sex
          ), joined_tcost AS (
            SELECT agg.agegrp2, agg.sex, agg.V1_sum, tc.tcost_val FROM agg_by_agegrp2 agg JOIN %s tc ON agg.agegrp2 = tc.agegrp2 AND agg.sex = tc.sex
          )
          SELECT orig.agegrp, orig.sex, (jt.tcost_val * %f / NULLIF(jt.V1_sum, 0)) AS cost_param
          FROM %s orig
          JOIN lc_with_agegrp2 lwa ON orig.agegrp = lwa.agegrp AND orig.sex = lwa.sex
          JOIN joined_tcost jt ON lwa.agegrp2 = jt.agegrp2 AND lwa.sex = jt.sex
          GROUP BY orig.agegrp, orig.sex, jt.tcost_val, jt.V1_sum;
        "
  chd_direct_tcost_df <- data.table(agegrp2 = rep(c("30-44", "45-64", "65-69", "70-74", "75-99"), 2), sex = rep(c("men", "women"), each = 5), tcost_val = c(10500 - 200, 121000, 72300, 90100, 197000, 2600 - 100, 22500, 18900, 30300, 132800) * 1e6)
  register_df_as_table(chd_direct_tcost_df, "chd_direct_tcost_table")
  execute_sql(sprintf(direct_cost_param_sql, "chd_direct_cost_param_view", "chd_prvl_2019_agg_view", "chd_direct_tcost_table", direct_costs_inflation_factor, "chd_prvl_2019_agg_view"), "chd_direct_cost_param_view")

  stroke_direct_tcost_df <- data.table(agegrp2 = rep(c("30-44", "45-64", "65-69", "70-74", "75-99"), 2), sex = rep(c("men", "women"), each = 5), tcost_val = c(26600 - 1700, 186400, 109000, 144100, 465600, 19700 - 1700, 106800, 60900, 95700, 606800) * 1e6)
  register_df_as_table(stroke_direct_tcost_df, "stroke_direct_tcost_table")
  execute_sql(sprintf(direct_cost_param_sql, "stroke_direct_cost_param_view", "stroke_prvl_2019_agg_view", "stroke_direct_tcost_table", direct_costs_inflation_factor, "stroke_prvl_2019_agg_view"), "stroke_direct_cost_param_view")

  # --- Step 4: Create Final Output View with All Cost Columns ---
  final_view_creation_sql <- sprintf("
          CREATE OR REPLACE TEMP VIEW %s AS
          WITH base_filtered AS (SELECT * FROM %s WHERE mc = %d),
          chd_prod_prvl_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM chd_prvl_prdv_cost_param_view),
          chd_prod_mrtl_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM chd_mrtl_prdv_cost_param_view),
          stroke_prod_prvl_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM stroke_prvl_prdv_cost_param_view),
          stroke_prod_mrtl_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM stroke_mrtl_prdv_cost_param_view),
          chd_inf_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM chd_informal_cost_param_view),
          stroke_inf_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM stroke_informal_cost_param_view),
          chd_dir_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM chd_direct_cost_param_view),
          stroke_dir_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM stroke_direct_cost_param_view)
          SELECT
            m.*,
            CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END AS chd_prvl_prdv_costs,
            CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END AS chd_mrtl_prdv_costs,
            (CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) AS chd_productivity_costs,

            CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END AS stroke_prvl_prdv_costs,
            CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END AS stroke_mrtl_prdv_costs,
            (CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) AS stroke_productivity_costs,

            CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END AS chd_informal_costs,
            CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END AS stroke_informal_costs,

            CASE WHEN m.chd_dgns > 0 THEN cdc.val ELSE 0 END AS chd_direct_costs,
            CASE WHEN m.stroke_dgns > 0 THEN sdc.val ELSE 0 END AS stroke_direct_costs,

            -- Totals and Indirect
            ((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) + CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END + CASE WHEN m.chd_dgns > 0 THEN cdc.val ELSE 0 END) AS chd_total_costs,
            ((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) + CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END) AS chd_indirect_costs,

            ((CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END + CASE WHEN m.stroke_dgns > 0 THEN sdc.val ELSE 0 END) AS stroke_total_costs,
            ((CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END) AS stroke_indirect_costs,

            -- CVD Costs (Sum of CHD and Stroke costs)
            (((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) + CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END + CASE WHEN m.chd_dgns > 0 THEN cdc.val ELSE 0 END) +
             ((CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END + CASE WHEN m.stroke_dgns > 0 THEN sdc.val ELSE 0 END)
            ) AS cvd_total_costs,
            (((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) + CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END) +
             ((CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END)
            ) AS cvd_indirect_costs,
            (CASE WHEN m.chd_dgns > 0 THEN cdc.val ELSE 0 END + CASE WHEN m.stroke_dgns > 0 THEN sdc.val ELSE 0 END) AS cvd_direct_costs,
            ((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) +
             (CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END)
            ) AS cvd_productivity_costs,
            (CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END) AS cvd_informal_costs
          FROM base_filtered m
          LEFT JOIN chd_prod_prvl_cost cppc ON m.agegrp = cppc.agegrp AND m.sex = cppc.sex
          LEFT JOIN chd_prod_mrtl_cost cpmc ON m.agegrp = cpmc.agegrp AND m.sex = cpmc.sex
          LEFT JOIN stroke_prod_prvl_cost sppc ON m.agegrp = sppc.agegrp AND m.sex = sppc.sex
          LEFT JOIN stroke_prod_mrtl_cost spmc ON m.agegrp = spmc.agegrp AND m.sex = spmc.sex
          LEFT JOIN chd_inf_cost cic ON m.agegrp = cic.agegrp AND m.sex = cic.sex
          LEFT JOIN stroke_inf_cost sic ON m.agegrp = sic.agegrp AND m.sex = sic.sex
          LEFT JOIN chd_dir_cost cdc ON m.agegrp = cdc.agegrp AND m.sex = cdc.sex
          LEFT JOIN stroke_dir_cost sdc ON m.agegrp = sdc.agegrp AND m.sex = sdc.sex;
        ", output_view_name, input_table_name, mcaggr)
  execute_sql(final_view_creation_sql, paste("Final cost view:", output_view_name))

  # Clean up intermediate tables/views (optional, as they are TEMP)
  # Example: dbExecute(duckdb_con, "DROP VIEW IF EXISTS chd_prvl_2016_agg_view;")

  return(invisible(NULL))
} # end calc_costs_duckdb

# export_costs_summaries ----
export_costs_summaries = function(duckdb_con, mcaggr, strata, ext) {
  # Create output directories for scaled-up and ESP-weighted summaries
  lapply(paste0(rep(c("costs"), each = 2), "_", c("scaled_up", "esp")), function(subdir_suffix) {
    private$create_new_folder(private$output_dir(paste0("summaries/", subdir_suffix)))
  })

  lc_table_name <- "lc_table"

  # Define the name for the temporary view that calc_costs_duckdb will create
  costs_view_name <- "lc_with_costs_view"

  # Call calc_costs_duckdb to create/replace the temporary view with cost columns.
  # This view will be based on lc_table and filtered for the current mcaggr.
  calc_costs_duckdb(
    duckdb_con = duckdb_con,
    mcaggr = mcaggr,
    input_table_name = lc_table_name,
    output_view_name = costs_view_name
  )

  # Prepare strata columns for SQL query (quoted)
  quoted_strata_cols_sql <- paste(sprintf('"%s"', strata), collapse = ", ")

  # Define cost metrics for SELECT statement
  cost_metrics_select_wt <- paste(
    'SUM("chd_productivity_costs" * wt) AS "chd_productivity_costs"',
    'SUM("stroke_productivity_costs" * wt) AS "stroke_productivity_costs"',
    'SUM("chd_informal_costs" * wt) AS "chd_informal_costs"',
    'SUM("stroke_informal_costs" * wt) AS "stroke_informal_costs"',
    'SUM("chd_direct_costs" * wt) AS "chd_direct_costs"',
    'SUM("stroke_direct_costs" * wt) AS "stroke_direct_costs"',
    'SUM("chd_total_costs" * wt) AS "chd_total_costs"',
    'SUM("stroke_total_costs" * wt) AS "stroke_total_costs"',
    'SUM("cvd_productivity_costs" * wt) AS "cvd_productivity_costs"',
    'SUM("cvd_informal_costs" * wt) AS "cvd_informal_costs"',
    'SUM("cvd_direct_costs" * wt) AS "cvd_direct_costs"',
    'SUM("cvd_total_costs" * wt) AS "cvd_total_costs"',
    sep = ", "
  )

  cost_metrics_select_wt_esp <- gsub("wt", "wt_esp", cost_metrics_select_wt)

  # --- Scaled-up Costs ---
  query_scaled_up <- sprintf(
    "SELECT %s, SUM(wt) AS popsize, %s
       FROM %s
       GROUP BY %s
       ORDER BY %s",
    quoted_strata_cols_sql,
    cost_metrics_select_wt,
    costs_view_name,
    quoted_strata_cols_sql,
    quoted_strata_cols_sql
  )
  output_path_scaled_up <- private$output_dir(
    paste0("summaries/costs_scaled_up/", mcaggr, "_costs_scaled_up.", ext)
  )
  dbExecute(duckdb_con, sprintf(
    "COPY (%s) TO '%s' (FORMAT PARQUET);",
    query_scaled_up, output_path_scaled_up
  ))

  # --- ESP Costs ---
  query_esp <- sprintf(
    "SELECT %s, SUM(wt_esp) AS popsize, %s
       FROM %s
       GROUP BY %s
       ORDER BY %s",
    quoted_strata_cols_sql,
    cost_metrics_select_wt_esp,
    costs_view_name,
    quoted_strata_cols_sql,
    quoted_strata_cols_sql
  )
  output_path_esp <- private$output_dir(
    paste0("summaries/costs_esp/", mcaggr, "_costs_esp.", ext)
  )
  dbExecute(duckdb_con, sprintf(
    "COPY (%s) TO '%s' (FORMAT PARQUET);",
    query_esp, output_path_esp
  ))

  # Drop the temporary view if it's no longer needed for this mcaggr
  dbExecute(duckdb_con, sprintf("DROP VIEW IF EXISTS %s;", costs_view_name))

  NULL
}


setkey(tt, NULL)
setkey(tt, pid, year, scenario)
setDT(tt_duckdb)
# Ensure column types match for comparison (factors, numeric etc.)
for (col in names(tt)) {
  if (is.factor(tt[[col]])) {
    tt_duckdb[, (col) := factor(get(col), levels = levels(tt[[col]]))]
  } else if (is.integer(tt[[col]]) && !is.integer(tt_duckdb[[col]])) {
    tt_duckdb[, (col) := as.integer(get(col))]
  } else if (is.numeric(tt[[col]]) && !is.numeric(tt_duckdb[[col]])) {
    tt_duckdb[, (col) := as.numeric(get(col))]
  }
}

setcolorder(tt_duckdb, names(tt))
tt_duckdb[, sex := factor(sex, levels = levels(tt$sex))]
tt_duckdb[, agegrp := factor(agegrp, levels = levels(tt$agegrp))]
setkeyv(tt_duckdb, key(tt))
all.equal(tt, tt_duckdb[, .SD, .SDcols = names(tt)])






open_dataset(file.path(self$design$sim_prm$output_dir, "summaries", "incd_scaled_up.csv.gz"))

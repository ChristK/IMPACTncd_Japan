SELECT 
  mc, 
  scenario, 
  year, 
  agegrp, 
  sex, 
  SUM(wt) AS popsize,
  
  SUM(chd_productivity_costs * wt) AS chd_productivity_costs,
  SUM(stroke_productivity_costs * wt) AS stroke_productivity_costs,
  SUM(chd_informal_costs * wt) AS chd_informal_costs,
  SUM(stroke_informal_costs * wt) AS stroke_informal_costs,
  SUM(chd_direct_costs * wt) AS chd_direct_costs,
  SUM(stroke_direct_costs * wt) AS stroke_direct_costs,
  SUM(chd_indirect_costs * wt) AS chd_indirect_costs,
  SUM(stroke_indirect_costs * wt) AS stroke_indirect_costs,
  SUM(chd_total_costs * wt) AS chd_total_costs,
  SUM(stroke_total_costs * wt) AS stroke_total_costs,

  SUM(cvd_productivity_costs * wt) AS cvd_productivity_costs,
  SUM(cvd_informal_costs * wt) AS cvd_informal_costs,
  SUM(cvd_direct_costs * wt) AS cvd_direct_costs,
  SUM(cvd_indirect_costs * wt) AS cvd_indirect_costs,
  SUM(cvd_total_costs * wt) AS cvd_total_costs

FROM lc_with_costs
GROUP BY mc, scenario, year, agegrp, sex
ORDER BY mc, scenario, year, agegrp, sex;

test <- '
SELECT * FROM (
SELECT mc, scenario, year, agegrp, sex, SUM(wt) AS popsize, 
  SUM(chd_total_costs * wt) AS chd_total_costs, 
  SUM(stroke_total_costs * wt) AS stroke_total_costs 
FROM lc_with_costs_All_view 
GROUP BY mc, scenario, year, agegrp, sex
UNION ALL
SELECT mc, scenario, year, agegrp, sex, SUM(wt) AS popsize, 
  SUM(chd_total_costs * wt) AS chd_total_costs, 
  SUM(stroke_total_costs * wt) AS stroke_total_costs 
FROM lc_with_costs_BMI_view 
GROUP BY mc, scenario, year, agegrp, sex
UNION ALL
SELECT mc, scenario, year, agegrp, sex, SUM(wt) AS popsize, 
  SUM(chd_total_costs * wt) AS chd_total_costs, 
  SUM(stroke_total_costs * wt) AS stroke_total_costs 
FROM lc_with_costs_sc0_view 
GROUP BY mc, scenario, year, agegrp, sex
)
ORDER BY mc, scenario, year, agegrp, sex
'
        
# List all views
dbGetQuery(duckdb_con, "
  SELECT table_name
    FROM information_schema.views
   WHERE table_schema = 'main';
")

# Or list everything (tables and views) with their types
dbGetQuery(duckdb_con, "
  SELECT table_name, table_type
    FROM information_schema.tables
   WHERE table_schema = 'main';
")

dbGetQuery(duckdb_con, "
  SELECT table_name, table_type
    FROM information_schema.tables
   WHERE table_schema = 'main'
     AND table_name = 'stroke_ftlt_ext_2016_table';
")

dbGetQuery(duckdb_con, "
SELECT table_name
FROM information_schema.views
WHERE table_name IN (
    'lc_table',
    'chd_prvl_prdv_cost_param_view',
    'chd_mrtl_prdv_cost_param_view',
    'stroke_prvl_prdv_cost_param_view',
    'stroke_mrtl_prdv_cost_param_view',
    'chd_informal_cost_param_view',
    'stroke_informal_cost_param_view',
    'chd_direct_cost_param_view',
    'stroke_direct_cost_param_view'
);
")


dbGetQuery(duckdb_con, "PRAGMA table_info('employee_params_view');")

dbGetQuery(duckdb_con, "SELECT * FROM lc_with_costs_view LIMIT 10;")


dbGetQuery(duckdb_con, "
SELECT m.*, cppc.val
FROM (SELECT * FROM lc_table WHERE mc = 5) m
LEFT JOIN chd_prvl_prdv_cost_param_view cppc
ON m.agegrp = cppc.agegrp AND m.sex = cppc.sex
LIMIT 10;
")

dbExecute(duckdb_con, query_scaled_up)

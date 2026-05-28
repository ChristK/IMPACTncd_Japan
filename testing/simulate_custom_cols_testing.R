# Test that user-created columns in scenarios flow into the summaries via the
# suffix convention documented in
# Rpackage/IMPACTncd_Japan_model_pkg/vignettes/custom-scenario-columns.Rmd
#
#   *_prvl  -> prevalence (SUM wt where col > 0) AND incidence (SUM wt where col = 1)
#   *_contd -> population-weighted mean
#   *_costs -> SUM(col * wt)
#
# Run from the project root, e.g.:
#   Rscript testing/simulate_custom_cols_testing.R
#
# The script runs a tiny two-scenario simulation, exports the relevant
# summaries, then asserts that the custom columns are present and behave as
# expected. It prints PASS/FALL for each check and stops with an error if any
# check fails, so it is suitable for CI.

source("./global.R")
library(arrow)
library(data.table)

INTERVENTION_YEAR <- 2025L
CUSTOM_COLS <- c(
  "sbp_intervention_prvl", # duration counter -> prvl + incd summaries
  "sbp_excess_contd", # continuous -> weighted mean
  "sbp_intervention_costs" # monetary -> weighted sum
)

IMPACTncd <- Simulation$new("testing/sim_design_testing.yaml")

# List the custom columns explicitly in cols_for_output. They are also kept
# automatically by their suffix, but listing them exercises the documented
# user-facing path and keeps the test independent of the auto-keep regex.
IMPACTncd$design$sim_prm$cols_for_output <- union(
  IMPACTncd$design$sim_prm$cols_for_output,
  CUSTOM_COLS
)

# Baseline scenario (sc0): create the custom columns at baseline values, with
# the intervention switched OFF. Doing this in EVERY scenario keeps the
# lifecourse schema consistent across scenarios (see the vignette's "gotchas").
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    synthpop$pop[, sbp_intervention_prvl := 0L]
    synthpop$pop[, sbp_excess_contd := pmax(0, SBP_curr_xps - 130)]
    synthpop$pop[, sbp_intervention_costs := 0]
    NULL
  }
)

IMPACTncd$
  del_logs()$
  del_outputs()$
  run(1:2, multicore = TRUE, "sc0")

# Intervention scenario (sc1): cut SBP by 10% from INTERVENTION_YEAR onwards and
# record the three custom columns accordingly.
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    synthpop$pop[
      year >= INTERVENTION_YEAR,
      SBP_curr_xps := SBP_curr_xps * 0.9
    ]

    # _prvl: 1 in the first intervention year, then 2, 3, ... thereafter.
    synthpop$pop[, sbp_intervention_prvl := carry_forward_incr(
      as.integer(year >= INTERVENTION_YEAR),
      pid_mrk,
      recur = FALSE,
      y = 1L,
      byref = TRUE
    )]

    # _contd: mmHg of SBP above 130 (lower than baseline once SBP is cut).
    synthpop$pop[, sbp_excess_contd := pmax(0, SBP_curr_xps - 130)]

    # _costs: 500 per person-year while the intervention is active.
    synthpop$pop[, sbp_intervention_costs := fifelse(
      year >= INTERVENTION_YEAR, 500, 0
    )]
    NULL
  }
)

IMPACTncd$run(1:2, multicore = TRUE, "sc1")

IMPACTncd$export_summaries(
  multicore = TRUE,
  type = c("prvl", "incd", "costs", "contd")
)

# ---------------------------------------------------------------------------
# Assertions
# ---------------------------------------------------------------------------
smr <- file.path(IMPACTncd$design$sim_prm$output_dir, "summaries")
failures <- character(0)

check <- function(cond, msg) {
  ok <- isTRUE(cond)
  message(if (ok) "PASS: " else "FAIL: ", msg)
  if (!ok) failures <<- c(failures, msg)
  invisible(ok)
}

read_summary <- function(subdir) {
  pth <- file.path(smr, subdir)
  if (!dir.exists(pth) || length(list.files(pth)) == 0L) {
    return(NULL)
  }
  as.data.table(open_dataset(pth))
}

# --- 1. _prvl -> prevalence ------------------------------------------------
prvl <- read_summary("prvl_scaled_up")
check(!is.null(prvl), "prvl_scaled_up summary was written")
if (!is.null(prvl)) {
  check(
    "sbp_intervention_prvl" %in% names(prvl),
    "_prvl column present in prvl summary"
  )
  sc1_prev <- prvl[
    scenario == "sc1" & year >= INTERVENTION_YEAR,
    sum(sbp_intervention_prvl, na.rm = TRUE)
  ]
  sc0_prev <- prvl[
    scenario == "sc0",
    sum(sbp_intervention_prvl, na.rm = TRUE)
  ]
  check(
    sc1_prev > 0,
    "sc1 intervention prevalence is > 0 in/after the intervention year"
  )
  check(
    isTRUE(all.equal(sc0_prev, 0)),
    "sc0 intervention prevalence is 0 (intervention off at baseline)"
  )
}

# --- 2. the SAME _prvl column -> incidence ---------------------------------
incd <- read_summary("incd_scaled_up")
check(!is.null(incd), "incd_scaled_up summary was written")
if (!is.null(incd)) {
  check(
    "sbp_intervention_incd" %in% names(incd),
    "_prvl column produced a renamed _incd column in incd summary"
  )
  sc1_inc_year <- incd[
    scenario == "sc1" & year == INTERVENTION_YEAR,
    sum(sbp_intervention_incd, na.rm = TRUE)
  ]
  check(
    sc1_inc_year > 0,
    "sc1 intervention incidence is > 0 in the first intervention year"
  )
}

# --- 3. _contd -> population-weighted mean ---------------------------------
contd <- read_summary("contd_scaled_up")
contd_esp <- read_summary("contd_esp")
check(!is.null(contd), "contd_scaled_up summary was written (type='contd')")
check(!is.null(contd_esp), "contd_esp summary was written")
if (!is.null(contd)) {
  check(
    "sbp_excess_contd" %in% names(contd),
    "_contd column present in contd summary"
  )
  vals <- contd[["sbp_excess_contd"]]
  check(
    any(!is.na(vals)) && all(vals[!is.na(vals)] >= 0),
    "contd weighted means are non-negative and not all NA"
  )
  # Behavioural check (soft): cutting SBP should lower the mean excess in sc1
  # versus sc0 in/after the intervention year. n is small, so report only.
  m <- contd[
    year >= INTERVENTION_YEAR,
    .(mean_excess = weighted.mean(sbp_excess_contd, popsize, na.rm = TRUE)),
    by = scenario
  ]
  if (all(c("sc0", "sc1") %in% m$scenario)) {
    sc0m <- m[scenario == "sc0", mean_excess]
    sc1m <- m[scenario == "sc1", mean_excess]
    message(sprintf(
      "INFO: mean SBP excess (year >= %d): sc0 = %.3f, sc1 = %.3f%s",
      INTERVENTION_YEAR, sc0m, sc1m,
      if (sc1m <= sc0m) " (sc1 lower, as expected)" else ""
    ))
  }
}

# --- 4. _costs -> weighted sum ---------------------------------------------
costs <- read_summary("costs_scaled_up")
check(!is.null(costs), "costs_scaled_up summary was written")
if (!is.null(costs)) {
  check(
    "sbp_intervention_costs" %in% names(costs),
    "_costs column present alongside built-in cost columns"
  )
  sc1_cost <- costs[
    scenario == "sc1" & year >= INTERVENTION_YEAR,
    sum(sbp_intervention_costs, na.rm = TRUE)
  ]
  sc0_cost <- costs[
    scenario == "sc0",
    sum(sbp_intervention_costs, na.rm = TRUE)
  ]
  check(
    sc1_cost > 0,
    "sc1 intervention cost is > 0 in/after the intervention year"
  )
  check(
    isTRUE(all.equal(sc0_cost, 0)),
    "sc0 intervention cost is 0 (intervention off at baseline)"
  )
}

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
if (length(failures) > 0L) {
  stop(
    "Custom-column export test FAILED (", length(failures), " check(s)):\n",
    paste0("  - ", failures, collapse = "\n")
  )
}

# Smoke-test the table export end-to-end using the new class method (which
# uses this simulation's own design, unlike auxil/process_out.R which reads
# ./inputs/sim_design.yaml). Summaries for absent sources are skipped.
IMPACTncd$export_tables(baseline_year_for_change_outputs = 2001L)

message("\nAll custom-column export checks PASSED.")
print("Custom column export test has finished!")

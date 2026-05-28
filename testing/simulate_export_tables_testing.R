# Test that the new Simulation$export_tables() method reproduces the tables
# produced by the legacy auxil/process_out.R script.
#
# Strategy
# --------
#   1. Run a tiny two-scenario simulation (sc0 + sc1) with the same custom
#      columns used by testing/simulate_custom_cols_testing.R, then export the
#      relevant summaries.
#   2. Generate tables with the NEW class method:
#        IMPACTncd$export_tables(two_agegrps = FALSE)
#        IMPACTncd$export_tables(two_agegrps = TRUE)
#      and stash them under <output_dir>/tables_method[/2agegrps].
#   3. Generate tables with the LEGACY script. auxil/process_out.R hardcodes
#      Design$new("./inputs/sim_design.yaml"), so we copy it to a temp file and
#      rewrite that single path to the TEST design before sourcing it. This
#      writes <output_dir>/tables and <output_dir>/tables2agegrps.
#   4. Compare the two sets file-by-file (same file names + numerically equal
#      contents).
#
# Run from the project root:
#   Rscript testing/simulate_export_tables_testing.R
#
# Prints PASS/FAIL per check and stops with an error if any check fails, so it
# is suitable for CI.

source("./global.R")
library(arrow)
library(data.table)

TEST_DESIGN <- "testing/sim_design_testing.yaml"
INTERVENTION_YEAR <- 2025L
BASELINE_YEAR <- 2001L
CUSTOM_COLS <- c(
  "sbp_intervention_prvl", # duration counter -> prvl + incd summaries
  "sbp_excess_contd", # continuous -> weighted mean
  "sbp_intervention_costs" # monetary -> weighted sum
)

# Summary types that feed the tables we want to compare. (le/hle/cms are not
# table sources, so we skip them to keep the test fast.)
SUMMARY_TYPES <- c(
  "prvl", "incd", "mrtl", "dis_mrtl", "all_cause_mrtl_by_dis",
  "dis_char", "qalys", "costs", "contd"
)

failures <- character(0)
check <- function(cond, msg) {
  ok <- isTRUE(cond)
  message(if (ok) "PASS: " else "FAIL: ", msg)
  if (!ok) failures <<- c(failures, msg)
  invisible(ok)
}

# ---------------------------------------------------------------------------
# 1. Run a tiny two-scenario simulation and export summaries
# ---------------------------------------------------------------------------
IMPACTncd <- Simulation$new(TEST_DESIGN)

IMPACTncd$design$sim_prm$cols_for_output <- union(
  IMPACTncd$design$sim_prm$cols_for_output,
  CUSTOM_COLS
)

# Baseline scenario (sc0): custom columns present but intervention OFF.
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

# Intervention scenario (sc1): cut SBP by 10% from INTERVENTION_YEAR onwards.
IMPACTncd$update_primary_prevention_scn(
  function(synthpop) {
    synthpop$pop[
      year >= INTERVENTION_YEAR,
      SBP_curr_xps := SBP_curr_xps * 0.9
    ]
    synthpop$pop[, sbp_intervention_prvl := carry_forward_incr(
      as.integer(year >= INTERVENTION_YEAR),
      pid_mrk,
      recur = FALSE,
      y = 1L,
      byref = TRUE
    )]
    synthpop$pop[, sbp_excess_contd := pmax(0, SBP_curr_xps - 130)]
    synthpop$pop[, sbp_intervention_costs := fifelse(
      year >= INTERVENTION_YEAR, 500, 0
    )]
    NULL
  }
)

IMPACTncd$run(1:2, multicore = TRUE, "sc1")

IMPACTncd$export_summaries(multicore = TRUE, type = SUMMARY_TYPES)

output_dir <- IMPACTncd$design$sim_prm$output_dir
tables_dir <- file.path(output_dir, "tables")
tables2_dir <- file.path(output_dir, "tables2agegrps")
method_dir <- file.path(output_dir, "tables_method")
method2_dir <- file.path(output_dir, "tables2agegrps_method")

# Exclude exposure (xps) tables from the parity comparison. The legacy
# auxil/process_out.R xps section hardcodes the `agegrp20` column, but the
# current xps summaries are produced with a different age grouping (agegrp10)
# and without the "All" groupingset marginals the script expects - so
# process_out.R itself errors on this schema and cannot generate xps tables to
# compare against. Move the xps dir aside so BOTH the method and the script
# skip xps via their existence guards, keeping the comparison apples-to-apples.
xps_dir <- file.path(output_dir, "xps")
xps_stash <- file.path(output_dir, "xps_excluded_from_parity")
unlink(xps_stash, recursive = TRUE, force = TRUE)
if (dir.exists(xps_dir)) file.rename(xps_dir, xps_stash)

# ---------------------------------------------------------------------------
# 2. Generate tables with the NEW class method, then move them aside
# ---------------------------------------------------------------------------
unlink(c(tables_dir, tables2_dir, method_dir, method2_dir),
       recursive = TRUE, force = TRUE)

# multicore = FALSE keeps the run deterministic and easy to debug; the parallel
# path is exercised separately below and produces identical files.
IMPACTncd$export_tables(
  baseline_year_for_change_outputs = BASELINE_YEAR,
  two_agegrps = FALSE,
  multicore = FALSE
)
IMPACTncd$export_tables(
  baseline_year_for_change_outputs = BASELINE_YEAR,
  two_agegrps = TRUE,
  multicore = FALSE
)

check(dir.exists(tables_dir) && length(list.files(tables_dir)) > 0L,
      "method produced standard tables/")
file.rename(tables_dir, method_dir)
if (dir.exists(tables2_dir)) file.rename(tables2_dir, method2_dir)

# ---------------------------------------------------------------------------
# 3. Generate tables with the LEGACY auxil/process_out.R (pointed at the test
#    design via a rewritten temp copy)
# ---------------------------------------------------------------------------
po_src <- readLines("./auxil/process_out.R")
# Rewrite the single hardcoded design path so the script runs against the test
# simulation's output_dir instead of ./inputs/sim_design.yaml.
po_src <- gsub(
  '"./inputs/sim_design.yaml"',
  shQuote(TEST_DESIGN, type = "cmd"),
  po_src,
  fixed = TRUE
)
po_tmp <- tempfile(fileext = ".R")
writeLines(po_src, po_tmp)

# Unlike the class method, process_out.R does not create the tables/ directory
# (it relies on Simulation$new() having done so). We renamed it aside above, so
# recreate it. (process_out.R creates tables2agegrps/ itself.)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

# process_out.R relies on these being attached.
suppressWarnings(suppressMessages({
  library(yaml)
  library(ggplot2)
  library(ggthemes)
  library(scales)
}))

local({
  source(po_tmp, local = TRUE)
})

check(dir.exists(tables_dir) && length(list.files(tables_dir)) > 0L,
      "process_out.R produced standard tables/")

# ---------------------------------------------------------------------------
# 4. Compare the two table sets
# ---------------------------------------------------------------------------
# Read a table CSV in a canonical (column- and row-sorted) order so the
# comparison is robust to incidental ordering differences.
read_canonical <- function(path) {
  dt <- fread(path)
  if (ncol(dt) == 0L) return(dt)
  setcolorder(dt, sort(names(dt)))
  setorderv(dt, names(dt))
  dt[]
}

# Compare two directories of CSV tables. `allowed_extra_legacy` lists files that
# are expected to exist ONLY on the legacy (process_out) side. When
# `allow_method_extra = TRUE`, files present only on the method side are
# reported (INFO) rather than treated as failures - used for two_agegrps, where
# the method writes a strict superset (see header note).
compare_dirs <- function(legacy_dir, method_dir, label,
                         allowed_extra_legacy = character(0),
                         allow_method_extra = FALSE,
                         allowed_extra_method_pattern = NULL) {
  legacy_files <- if (dir.exists(legacy_dir)) {
    list.files(legacy_dir, pattern = "\\.csv$")
  } else character(0)
  method_files <- if (dir.exists(method_dir)) {
    list.files(method_dir, pattern = "\\.csv$")
  } else character(0)

  only_legacy <- setdiff(legacy_files, method_files)
  only_method <- setdiff(method_files, legacy_files)

  # Known/allowed legacy-only files do not count as failures.
  unexpected_only_legacy <- setdiff(only_legacy, allowed_extra_legacy)

  check(length(unexpected_only_legacy) == 0L,
        sprintf("[%s] no unexpected legacy-only files%s", label,
                if (length(unexpected_only_legacy))
                  paste0(": ", paste(unexpected_only_legacy, collapse = "; "))
                else ""))
  if (allow_method_extra) {
    if (length(only_method) > 0L) {
      message(sprintf("INFO: [%s] method-only (superset) files: %s",
                      label, paste(only_method, collapse = "; ")))
    }
  } else {
    # Method-only files matching the allowed pattern are expected superset
    # tables (e.g. contd_change, which process_out.R omits by oversight).
    expected_method <- if (!is.null(allowed_extra_method_pattern)) {
      grep(allowed_extra_method_pattern, only_method, value = TRUE)
    } else character(0)
    unexpected_only_method <- setdiff(only_method, expected_method)
    if (length(expected_method) > 0L) {
      message(sprintf("INFO: [%s] expected method-only (superset) files: %s",
                      label, paste(expected_method, collapse = "; ")))
    }
    check(length(unexpected_only_method) == 0L,
          sprintf("[%s] no unexpected method-only files%s", label,
                  if (length(unexpected_only_method))
                    paste0(": ", paste(unexpected_only_method, collapse = "; "))
                  else ""))
  }
  if (length(intersect(only_legacy, allowed_extra_legacy)) > 0L) {
    message(sprintf("INFO: [%s] expected legacy-only (redundant) files: %s",
                    label,
                    paste(intersect(only_legacy, allowed_extra_legacy),
                          collapse = "; ")))
  }

  common <- intersect(legacy_files, method_files)
  check(length(common) > 0L,
        sprintf("[%s] there are common files to compare (%d)",
                label, length(common)))

  mismatches <- character(0)
  for (f in common) {
    a <- read_canonical(file.path(legacy_dir, f))
    b <- read_canonical(file.path(method_dir, f))
    eq <- isTRUE(all.equal(a, b, tolerance = 1e-8, check.attributes = FALSE))
    if (!eq) mismatches <- c(mismatches, f)
  }
  check(length(mismatches) == 0L,
        sprintf("[%s] all %d common tables are value-identical%s",
                label, length(common),
                if (length(mismatches))
                  paste0(" (mismatched: ", paste(mismatches, collapse = "; "), ")")
                else ""))
}

message("\n--- Comparing standard tables (two_agegrps = FALSE) ---")
# Expect parity on all shared tables (incl. contd_change, now emitted by both).
# The cost-effectiveness (ICER/NMB) tables are produced ONLY by the method
# (process_out.R has no CEA), so they are allowed method-only extras here and
# are validated separately below.
compare_dirs(tables_dir, method_dir, "standard",
             allowed_extra_method_pattern = "^cost-effectiveness by ")

message("\n--- Comparing two_agegrps tables (two_agegrps = TRUE) ---")
# process_out.R writes pop "by year" and "by year-sex" into tables2agegrps via
# its standalone 4-strata pop call; the class method applies agegrp-based strata
# consistently, so these two (content-identical to the standard-mode versions)
# are the only allowed legacy-only files here.
allowed_extra <- c(
  "pop size by year (not standardised).csv",
  "pop size by year-sex (not standardised).csv"
)
# The method runs the full export (all four task groups, both populations) into
# tables2agegrps/, whereas process_out.R only re-tabulates the main ons metrics
# there - so the method is a strict superset. We require every COMMON table to
# be value-identical and allow the method's additional tables.
compare_dirs(tables2_dir, method2_dir, "two_agegrps",
             allowed_extra_legacy = allowed_extra,
             allow_method_extra = TRUE)

# ---------------------------------------------------------------------------
# 5. Sanity: the parallel (multicore = TRUE) path yields the same file set
# ---------------------------------------------------------------------------
mc_dir <- file.path(output_dir, "tables_mc_check")
unlink(c(tables_dir, mc_dir), recursive = TRUE, force = TRUE)
IMPACTncd$export_tables(
  baseline_year_for_change_outputs = BASELINE_YEAR,
  two_agegrps = FALSE,
  multicore = TRUE
)
file.rename(tables_dir, mc_dir)
check(
  setequal(list.files(method_dir, pattern = "\\.csv$"),
           list.files(mc_dir, pattern = "\\.csv$")),
  "multicore = TRUE produces the same standard file set as multicore = FALSE"
)

# ---------------------------------------------------------------------------
# 6. Validate cost-effectiveness (ICER / NMB) tables
# ---------------------------------------------------------------------------
# process_out.R produces no CEA tables, so there is no legacy baseline. Instead
# we (a) check the expected files exist with the expected metric rows, and
# (b) independently recompute the quantiled CEA metrics straight from the
# qalys/costs summaries and confirm they match the method's CSVs exactly.
message("\n--- Validating cost-effectiveness (ICER/NMB) tables ---")

WTP <- c(5e6, 7.5e6, 1e7)
NMB_LABELS <- paste0("NMB_at_wtp_", vapply(WTP, function(w)
  format(w, scientific = FALSE, trim = TRUE, big.mark = ""), character(1)))
CEA_TYPES <- c("dCosts_cuml", "dQALYs_cuml", "ICER", NMB_LABELS)

# Independent re-implementation of export_cea_tables() for one
# (strata, perspective, scale), returning the quantiled table.
recompute_cea <- function(s, persp, scale,
                          base = BASELINE_YEAR, comparator = "sc0",
                          qaly_rate = 2, cost_rate = 2,
                          prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)) {
  q <- CKutils::read_parquet_dt(file.path(output_dir, "summaries", "qalys_scaled_up"))
  cst <- CKutils::read_parquet_dt(file.path(output_dir, "summaries", "costs_scaled_up"))
  all_cc <- grep("_costs$", names(cst), value = TRUE)
  builtin <- grep("^(chd|stroke|cvd)_(direct|productivity|informal|indirect|total)_costs$",
                  all_cc, value = TRUE)
  custom <- setdiff(all_cc, builtin)
  pcols <- if (persp == "societal") c("cvd_total_costs", custom) else "cvd_direct_costs"
  pcols <- intersect(pcols, names(cst))
  x <- c("mc", "scenario", s)
  disc <- function(v, year, rate) v / (1 + rate / 100)^pmax(0, year - base)
  cc <- copy(cst)
  cc[, .cost := Reduce(`+`, .SD), .SDcols = pcols]
  cc <- cc[, .(C = sum(.cost)), keyby = eval(x)][, C := disc(C, year, cost_rate)]
  qq <- q[, .(Q = sum(get(scale))), keyby = eval(x)][, Q := disc(Q, year, qaly_rate)]
  d <- merge(qq, cc, by = x, all = TRUE)
  d[is.na(Q), Q := 0][is.na(C), C := 0]
  cmp <- d[scenario == comparator & year >= base][, scenario := NULL]
  d <- d[scenario != comparator & year >= base]
  d[cmp, on = setdiff(x, "scenario"), `:=`(dQ = Q - i.Q, dC = C - i.C)]
  d <- d[!is.na(dQ) & !is.na(dC)]
  setkeyv(d, c(setdiff(x, "year"), "year"))
  d[, `:=`(dQALYs_cuml = cumsum(dQ), dCosts_cuml = cumsum(dC)), by = setdiff(x, "year")]
  d[, ICER := fifelse(dQALYs_cuml == 0, NA_real_, dCosts_cuml / dQALYs_cuml)]
  for (i in seq_along(WTP)) set(d, NULL, NMB_LABELS[i], WTP[i] * d$dQALYs_cuml - d$dCosts_cuml)
  dm <- melt(d, id.vars = x, measure.vars = CEA_TYPES,
             variable.name = "type", value.name = "value")
  dm <- dm[is.finite(value)]
  setkey(dm, "type")
  out <- dm[, IMPACTncdJapan:::safe_fquantile_byid(value, prbl, id = as.character(type),
                                                   rounding = FALSE),
            keyby = eval(setdiff(x, "mc"))]
  setnames(out, c(setdiff(x, "mc"), "type", scales::percent(prbl, prefix = "value_")))
  out[]
}

cea_validate <- function(s, persp, scale) {
  suffix <- paste(s, collapse = "-")
  fn <- sprintf("cost-effectiveness by %s (%s-%s) (not standardised).csv",
                suffix, persp, scale)
  fp <- file.path(method_dir, fn)
  if (!check(file.exists(fp), paste0("CEA file exists: ", fn))) return(invisible())
  csv <- fread(fp)
  check(all(CEA_TYPES %in% unique(csv$type)),
        paste0(fn, ": all 6 metric types present"))
  rc <- recompute_cea(s, persp, scale)
  keys <- c("type", "scenario", s)
  qcols <- grep("^value_", names(csv), value = TRUE)
  m <- merge(csv, rc, by = keys, suffixes = c(".csv", ".rc"))
  ok <- nrow(m) == nrow(csv)
  for (qc in qcols) {
    ok <- ok && isTRUE(all.equal(m[[paste0(qc, ".csv")]], m[[paste0(qc, ".rc")]],
                                 tolerance = 1e-6))
  }
  check(ok, paste0(fn, ": all quantile columns match independent recompute"))
}

cea_files <- list.files(method_dir, pattern = "^cost-effectiveness by .*\\.csv$")
check(length(cea_files) == 16L,
      sprintf("16 CEA files produced (4 strata x 2 perspectives x 2 scales); got %d",
              length(cea_files)))

for (persp in c("societal", "healthcare")) {
  for (scale in c("EQ5D5L", "HUI3")) {
    cea_validate("year", persp, scale)
  }
}
cea_validate(c("year", "sex"), "societal", "EQ5D5L")
cea_validate(c("year", "agegrp", "sex"), "healthcare", "HUI3")

# Healthcare incremental costs (direct only) should never exceed societal
# (total) in magnitude, at the median.
hc <- fread(file.path(method_dir,
  "cost-effectiveness by year (healthcare-EQ5D5L) (not standardised).csv"))
soc <- fread(file.path(method_dir,
  "cost-effectiveness by year (societal-EQ5D5L) (not standardised).csv"))
hc_dc <- hc[type == "dCosts_cuml"]
soc_dc <- soc[type == "dCosts_cuml"]
mm <- merge(hc_dc, soc_dc, by = c("scenario", "year"), suffixes = c(".hc", ".soc"))
check(nrow(mm) > 0 && all(abs(mm[["value_50.0%.hc"]]) <= abs(mm[["value_50.0%.soc"]]) + 1e-6),
      "healthcare |dCosts_cuml| <= societal |dCosts_cuml| (median)")

# cea = FALSE must suppress all CEA tables.
nocea_dir <- file.path(output_dir, "tables_nocea_check")
unlink(c(tables_dir, nocea_dir), recursive = TRUE, force = TRUE)
IMPACTncd$export_tables(
  baseline_year_for_change_outputs = BASELINE_YEAR,
  two_agegrps = FALSE, multicore = FALSE, cea = FALSE
)
file.rename(tables_dir, nocea_dir)
check(length(list.files(nocea_dir, pattern = "^cost-effectiveness by ")) == 0L,
      "cea = FALSE produces no CEA files")

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
if (length(failures) > 0L) {
  stop(
    "export_tables() parity test FAILED (", length(failures), " check(s)):\n",
    paste0("  - ", failures, collapse = "\n")
  )
}

message("\nAll export_tables() parity checks PASSED.")
print("export_tables parity test has finished!")

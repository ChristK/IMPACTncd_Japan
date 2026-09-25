# Test the multi-level discounting added to Simulation$export_tables().
#
# What changed
# ------------
# Discounting used to be applied only inside the CEA tables, with scalar
# `qaly_discount_rate` / `cost_discount_rate` (default 2 each). It now accepts
# *vectors* (default c(0, 2)) that are paired element-wise into discount
# levels, and every level appears in the qalys, net_qalys, costs, net_costs AND
# cost-effectiveness tables, tagged in a `discount` column.
#
# Strategy
# --------
#   A. Unit-test the discounting helpers (level construction, recycling,
#      validation, present-value factor).
#   B. Re-export tables from the summaries already sitting in the test
#      output_dir (no re-simulation), then check:
#        - which tables gained a `discount` column and which did not;
#        - the levels present, for equal and for differential rates;
#        - that the values at each level match an independent recompute
#          straight from the parquet summaries (qalys, costs and CEA);
#        - that the "0%" level reproduces the undiscounted figures;
#        - that discount levels and `wtp` thresholds are CROSSED in the CEA
#          tables (every threshold at every level), and that ICER - which does
#          not depend on wtp - appears once per level. This pins the layout
#          documented in ?export_tables and the custom-scenario-columns
#          vignette.
#
# Run from the project root, AFTER testing/simulate_export_tables_testing.R has
# populated <output_dir>/summaries (this script does not simulate):
#   Rscript testing/simulate_discount_levels_testing.R
#
# Prints PASS/FAIL per check and stops with an error if any check fails.

source("./global.R")
library(arrow)
library(data.table)

TEST_DESIGN <- "testing/sim_design_testing.yaml"
BASELINE_YEAR <- 2001L
PRBL <- c(0.5, 0.025, 0.975, 0.1, 0.9)
WTP <- c(5e6, 7.5e6, 1e7)

failures <- character(0)
check <- function(cond, msg) {
  ok <- isTRUE(cond)
  message(if (ok) "PASS: " else "FAIL: ", msg)
  if (!ok) failures <<- c(failures, msg)
  invisible(ok)
}

# Package internals under test.
build_discount_levels <- IMPACTncdJapan:::build_discount_levels
discount_factor <- IMPACTncdJapan:::discount_factor
expand_discount_levels <- IMPACTncdJapan:::expand_discount_levels
safe_fquantile_byid <- IMPACTncdJapan:::safe_fquantile_byid

# ---------------------------------------------------------------------------
# A. Discounting helpers
# ---------------------------------------------------------------------------
message("\n--- Discounting helpers ---")

lv <- build_discount_levels(c(0, 2), c(0, 2))
check(nrow(lv) == 2L, "default c(0, 2) gives two discount levels")
check(identical(lv$label, c("0%", "2%")),
      'equal rates label as "0%" / "2%"')
check(identical(lv$qaly_label, lv$cost_label),
      "equal rates give identical QALY and cost labels")

lv1 <- build_discount_levels(2.5, 2.5)
check(nrow(lv1) == 1L && lv1$label == "2.5%",
      "a single scalar rate gives one level")

# A length-1 rate is recycled against the longer vector.
lvr <- build_discount_levels(c(0, 1.5), 4)
check(nrow(lvr) == 2L && identical(lvr$cost, c(4, 4)),
      "a length-1 cost rate is recycled across QALY rates")
check(identical(lvr$label,
                c("QALYs 0%/costs 4%", "QALYs 1.5%/costs 4%")),
      "differential rates spell both rates out in the label")
check(identical(lvr$qaly_label, c("0%", "1.5%")),
      "the QALY-only label carries just the QALY rate")

# Levels that share a QALY rate collapse in a QALYs-only table.
lvd <- build_discount_levels(c(2, 2), c(0, 4))
dq <- expand_discount_levels(
  data.table(year = 2001:2003, v = 1),
  "v", lvd, 2001L, "qaly"
)
check(uniqueN(dq$discount) == 1L && nrow(dq) == 3L,
      "levels sharing a QALY rate collapse to one block in a QALYs table")
dc <- expand_discount_levels(
  data.table(year = 2001:2003, v = 1),
  "v", lvd, 2001L, "cost"
)
check(uniqueN(dc$discount) == 2L && nrow(dc) == 6L,
      "the same levels stay distinct in a costs table")

check(nrow(build_discount_levels(c(0, 0, 2.5), c(0, 0, 2.5))) == 2L,
      "duplicate levels are de-duplicated")

check(inherits(try(build_discount_levels(c(0, 2, 4), c(0, 2)), silent = TRUE),
               "try-error"),
      "incompatible rate vector lengths are an error")

# The contract is EQUAL lengths or one length-1 rate -- NOT "any divisor length".
# The guard used to be `n %% length(q) != 0L`, which accepted 4-vs-2 and 6-vs-3
# and let rep_len() invent pairings nobody asked for: c(0,1,2,3) against c(0,2)
# recycled the costs to c(0,2,0,2) and published "QALYs 2%/costs 0%" and
# "QALYs 3%/costs 2%" -- discounting health but not money. It also made rejection
# non-monotonic: 4-vs-2 passed while 3-vs-2 failed.
check(inherits(try(build_discount_levels(c(0, 1, 2, 3), c(0, 2)), silent = TRUE),
               "try-error"),
      "a divisor length is NOT recycling: 4-vs-2 is an error")
check(inherits(try(build_discount_levels(c(0, 2), c(0, 1, 2, 3)), silent = TRUE),
               "try-error"),
      "the same holds with the longer vector on the cost side")
check(nrow(build_discount_levels(c(0, 1, 2, 3), 2)) == 4L,
      "a genuine length-1 rate still recycles against any length")
check(inherits(try(build_discount_levels(-100, 0), silent = TRUE), "try-error"),
      "a rate of -100% or below is an error")
check(inherits(try(build_discount_levels(NA_real_, 0), silent = TRUE),
               "try-error"),
      "a non-finite rate is an error")

check(isTRUE(all.equal(discount_factor(2005L, 2.5, 2001L), 1 / 1.025^4)),
      "discount_factor() compounds from the base year")
check(discount_factor(1999L, 2.5, 2001L) == 1,
      "years before the base year are undiscounted")
check(all(discount_factor(2001:2050, 0, 2001L) == 1),
      "a 0% rate leaves every year unchanged")

check(inherits(
  try(expand_discount_levels(data.table(v = 1), "v", lv, 2001L, "qaly"),
      silent = TRUE), "try-error"),
  "discounting without a `year` column is a clear error")

# ---------------------------------------------------------------------------
# B. End-to-end table export (reuses existing summaries; no simulation)
# ---------------------------------------------------------------------------
IMPACTncd <- Simulation$new(TEST_DESIGN)
output_dir <- IMPACTncd$design$sim_prm$output_dir
summaries_dir <- file.path(output_dir, "summaries")
tables_dir <- file.path(output_dir, "tables")
disc_dir <- file.path(output_dir, "tables_discount_check")
diff_dir <- file.path(output_dir, "tables_discount_diffrates_check")

if (!dir.exists(file.path(summaries_dir, "qalys_scaled_up")) ||
    !dir.exists(file.path(summaries_dir, "costs_scaled_up"))) {
  stop("No qalys/costs summaries in ", summaries_dir,
       ".\nRun testing/simulate_export_tables_testing.R first.")
}

message("\n--- Exporting tables with the default discount levels ---")
unlink(c(tables_dir, disc_dir, diff_dir), recursive = TRUE, force = TRUE)
IMPACTncd$export_tables(
  baseline_year_for_change_outputs = BASELINE_YEAR,
  two_agegrps = FALSE,
  multicore = FALSE
)
file.rename(tables_dir, disc_dir)

# ---- B1. Which tables carry a `discount` column ---------------------------
message("\n--- `discount` column placement ---")

discounted_prefixes <- c("QALYs by ", "net QALYs by ", "costs by ",
                         "net costs by ", "cost-effectiveness by ")
has_discount <- function(f) "discount" %in% names(fread(file = f, nrows = 0L))

all_csv <- list.files(disc_dir, pattern = "\\.csv$", full.names = TRUE)
is_money <- grepl(paste0("^(", paste(discounted_prefixes, collapse = "|"), ")"),
                  basename(all_csv))
check(sum(is_money) > 0L, "money/QALY tables were produced")
check(all(vapply(all_csv[is_money], has_discount, logical(1))),
      "every qalys/costs/net_*/CEA table has a `discount` column")
check(!any(vapply(all_csv[!is_money], has_discount, logical(1))),
      "no other table gained a `discount` column")

levels_in <- function(f) sort(unique(fread(file = f)$discount))
check(all(vapply(all_csv[is_money],
                 function(f) identical(levels_in(f), c("0%", "2%")),
                 logical(1))),
      'every discounted table carries both levels ("0%", "2%")')

# ---- B2. Independent recompute: qalys and costs ---------------------------
message("\n--- qalys / costs tables vs independent recompute ---")

# Mirrors the qalys branch of tbl_smmrs_core() for the "by year" stratum.
recompute_qalys <- function(rate, from_year = BASELINE_YEAR) {
  q <- CKutils::read_parquet_dt(file.path(summaries_dir, "qalys_scaled_up"))
  x <- c("mc", "scenario", "year")
  d <- q[, .(EQ5D5L = sum(EQ5D5L), HUI3 = sum(HUI3)), keyby = eval(x)]
  d <- melt(d, id.vars = x, variable.name = "scale", value.name = "QALYs")
  d[, QALYs := QALYs / (1 + rate / 100)^pmax(0, year - from_year)]
  setkeyv(d, c("mc", "scenario", "scale", "year"))
  d[, cumulative := cumsum(QALYs), keyby = c("mc", "scenario", "scale")]
  d <- melt(d, id.vars = c(x, "scale"), variable.name = "type")
  d[, type := fifelse(type == "cumulative", "QALYs_cuml", "QALYs")]
  setkey(d, "type", "scale")
  out <- d[, safe_fquantile_byid(value, PRBL, id = as.character(type),
                                 rounding = FALSE),
           keyby = c("scenario", "year", "scale")]
  setnames(out, c("scenario", "year", "scale", "type",
                  scales::percent(PRBL, prefix = "qalys_")))
  out[]
}

# Mirrors the costs branch of tbl_smmrs_core() for the "by year" stratum.
recompute_costs <- function(rate, from_year = BASELINE_YEAR) {
  cst <- CKutils::read_parquet_dt(file.path(summaries_dir, "costs_scaled_up"))
  x <- c("mc", "scenario", "year")
  d <- cst[, lapply(.SD, sum), .SDcols = patterns("_costs$"), keyby = eval(x)]
  d <- melt(d, id.vars = x, variable.name = "costs_type", value.name = "costs")
  d[, costs := costs / (1 + rate / 100)^pmax(0, year - from_year)]
  setkeyv(d, c("mc", "scenario", "costs_type", "year"))
  d[, cumulative := cumsum(costs), keyby = c("mc", "scenario", "costs_type")]
  d <- melt(d, id.vars = c(x, "costs_type"), variable.name = "type")
  d[type == "cumulative", type := "costs_cuml"]
  setkey(d, "type", "costs_type")
  out <- d[, safe_fquantile_byid(value, PRBL, id = as.character(type),
                                 rounding = FALSE),
           keyby = c("scenario", "year", "costs_type")]
  setnames(out, c("scenario", "year", "costs_type", "type",
                  scales::percent(PRBL, prefix = "costs_")))
  out[]
}

# Compare one discount level of a written table against a recomputed reference.
compare_level <- function(file, level, reference, keys, label) {
  fp <- file.path(disc_dir, file)
  if (!check(file.exists(fp), paste0("file exists: ", file))) return(invisible())
  csv <- fread(file = fp)[discount == level]
  csv[, discount := NULL]
  vcols <- grep("^(qalys|costs)_", names(csv), value = TRUE)
  m <- merge(csv, reference, by = keys, suffixes = c(".csv", ".rc"))
  ok <- nrow(m) == nrow(csv) && nrow(m) > 0L
  for (vc in vcols) {
    ok <- ok && isTRUE(all.equal(m[[paste0(vc, ".csv")]],
                                 m[[paste0(vc, ".rc")]], tolerance = 1e-9))
  }
  check(ok, paste0(label, " (", level, ") matches independent recompute"))
}

for (lvl in c("0%", "2%")) {
  rate <- if (lvl == "0%") 0 else 2
  compare_level("QALYs by year (not standardised).csv", lvl,
                recompute_qalys(rate), c("type", "scenario", "year", "scale"),
                "qalys by year")
  compare_level("costs by year (not standardised).csv", lvl,
                recompute_costs(rate),
                c("type", "scenario", "year", "costs_type"), "costs by year")
}

# Discounting must bite: the 2% level is strictly smaller in later years.
qcsv <- fread(file = file.path(disc_dir, "QALYs by year (not standardised).csv"))
qw <- dcast(qcsv[type == "QALYs_cuml" & scale == "EQ5D5L"],
            scenario + year ~ discount, value.var = "qalys_50.0%")
late <- qw[year > BASELINE_YEAR]
check(nrow(late) > 0L && all(late[["2%"]] < late[["0%"]]),
      "discounted cumulative QALYs are below the undiscounted ones after the base year")
base <- qw[year == BASELINE_YEAR]
check(nrow(base) == 0L ||
        isTRUE(all.equal(base[["2%"]], base[["0%"]], tolerance = 1e-9)),
      "the base year itself is undiscounted at every level")

# ---- B3. Independent recompute: CEA ---------------------------------------
message("\n--- CEA tables vs independent recompute ---")

NMB_LABELS <- paste0("NMB_at_wtp_", vapply(WTP, function(w)
  format(w, scientific = FALSE, trim = TRUE, big.mark = ""), character(1)))
CEA_TYPES <- c("dCosts_cuml", "dQALYs_cuml", "ICER", NMB_LABELS)

recompute_cea <- function(persp, scale, qaly_rate, cost_rate,
                          base = BASELINE_YEAR, comparator = "sc0") {
  q <- CKutils::read_parquet_dt(file.path(summaries_dir, "qalys_scaled_up"))
  cst <- CKutils::read_parquet_dt(file.path(summaries_dir, "costs_scaled_up"))
  all_cc <- grep("_costs$", names(cst), value = TRUE)
  builtin <- grep("^(chd|stroke|cvd)_(direct|productivity|informal|indirect|total)_costs$",
                  all_cc, value = TRUE)
  custom <- setdiff(all_cc, builtin)
  pcols <- if (persp == "societal") c("cvd_total_costs", custom) else "cvd_direct_costs"
  pcols <- intersect(pcols, names(cst))
  x <- c("mc", "scenario", "year")
  cc <- copy(cst)
  cc[, .cost := Reduce(`+`, .SD), .SDcols = pcols]
  cc <- cc[, .(C = sum(.cost)), keyby = eval(x)]
  qq <- q[, .(Q = sum(get(scale))), keyby = eval(x)]
  d <- merge(qq, cc, by = x, all = TRUE)
  d[is.na(Q), Q := 0][is.na(C), C := 0]
  # Discount each arm at its own rate BEFORE differencing - the method
  # differences first, so agreeing here also proves the two orders coincide.
  d[, `:=`(Q = Q / (1 + qaly_rate / 100)^pmax(0, year - base),
           C = C / (1 + cost_rate / 100)^pmax(0, year - base))]
  cmp <- d[scenario == comparator & year >= base][, scenario := NULL]
  d <- d[scenario != comparator & year >= base]
  d[cmp, on = setdiff(x, "scenario"), `:=`(dQ = Q - i.Q, dC = C - i.C)]
  d <- d[!is.na(dQ) & !is.na(dC)]
  setkeyv(d, c("mc", "scenario", "year"))
  d[, `:=`(dQALYs_cuml = cumsum(dQ), dCosts_cuml = cumsum(dC)),
    by = c("mc", "scenario")]
  d[, ICER := fifelse(dQALYs_cuml == 0, NA_real_, dCosts_cuml / dQALYs_cuml)]
  for (i in seq_along(WTP)) {
    set(d, NULL, NMB_LABELS[i], WTP[i] * d$dQALYs_cuml - d$dCosts_cuml)
  }
  dm <- melt(d, id.vars = x, measure.vars = CEA_TYPES,
             variable.name = "type", value.name = "value")
  dm <- dm[is.finite(value)]
  setkey(dm, "type")
  out <- dm[, safe_fquantile_byid(value, PRBL, id = as.character(type),
                                  rounding = FALSE),
            keyby = c("scenario", "year")]
  setnames(out, c("scenario", "year", "type",
                  scales::percent(PRBL, prefix = "value_")))
  out[]
}

cea_validate <- function(dir, persp, scale, level, qaly_rate, cost_rate) {
  fn <- sprintf("cost-effectiveness by year (%s-%s) (not standardised).csv",
                persp, scale)
  fp <- file.path(dir, fn)
  if (!check(file.exists(fp), paste0("CEA file exists: ", fn))) return(invisible())
  csv <- fread(file = fp)[discount == level]
  check(all(CEA_TYPES %in% unique(csv$type)),
        sprintf("%s (%s): all %d metric types present", fn, level,
                length(CEA_TYPES)))
  rc <- recompute_cea(persp, scale, qaly_rate, cost_rate)
  qcols <- grep("^value_", names(csv), value = TRUE)
  m <- merge(csv, rc, by = c("type", "scenario", "year"),
             suffixes = c(".csv", ".rc"))
  ok <- nrow(m) == nrow(csv) && nrow(m) > 0L
  for (qc in qcols) {
    ok <- ok && isTRUE(all.equal(m[[paste0(qc, ".csv")]], m[[paste0(qc, ".rc")]],
                                 tolerance = 1e-6))
  }
  check(ok, sprintf("%s (%s): quantiles match independent recompute", fn, level))
}

for (persp in c("societal", "healthcare")) {
  cea_validate(disc_dir, persp, "EQ5D5L", "0%", 0, 0)
  cea_validate(disc_dir, persp, "EQ5D5L", "2%", 2, 2)
}
cea_validate(disc_dir, "societal", "HUI3", "2%", 2, 2)

# ---- B4. The documented discount x wtp cross product ----------------------
# The discount levels and the wtp thresholds are CROSSED (unlike the two
# discount-rate vectors, which are paired with each other). These checks pin
# the layout documented in ?export_tables and the vignette.
message("\n--- discount x wtp grid ---")

cea <- fread(file = file.path(
  disc_dir, "cost-effectiveness by year (societal-EQ5D5L) (not standardised).csv"))
cea[, type := as.character(type)]
disc_lv <- unique(cea$discount)

# Every threshold is evaluated at every discount level.
want <- CJ(discount = disc_lv, type = NMB_LABELS, unique = TRUE)
got <- unique(cea[type %in% NMB_LABELS, .(discount, type)])
check(nrow(got) == nrow(want) &&
        nrow(merge(got, want, by = c("discount", "type"))) == nrow(want),
      sprintf("all %d x %d (discount, wtp) combinations are written",
              length(disc_lv), length(NMB_LABELS)))
check(uniqueN(cea[type %in% NMB_LABELS, .N, by = .(discount, type)]$N) == 1L,
      "every (discount, wtp) cell spans the same set of rows")

# ICER does not depend on wtp: one row per (discount, scenario, year).
icer <- cea[type == "ICER"]
check(nrow(icer) > 0L &&
        uniqueN(icer[, .(discount, scenario, year)]) == nrow(icer),
      "ICER appears once per (discount, scenario, year), not once per wtp")

# NMB increases with wtp within a level. Quantile-safe: wtp * Q - C is
# increasing in wtp for every Monte-Carlo draw when Q > 0, hence for every
# quantile of it.
w_order <- NMB_LABELS[order(WTP)]
wide <- dcast(cea, discount + scenario + year ~ type, value.var = "value_50.0%")
check(all(vapply(seq_len(length(w_order) - 1L), function(i)
  all(wide[[w_order[i]]] <= wide[[w_order[i + 1L]]] + 1e-6, na.rm = TRUE),
  logical(1))),
  "NMB increases with wtp within each discount level")

# Discounting reaches NMB, not just the raw QALY/cost columns.
top <- dcast(cea[type == w_order[length(w_order)]], scenario + year ~ discount,
             value.var = "value_50.0%")
late <- top[year > BASELINE_YEAR]
check(nrow(late) > 0L && !isTRUE(all.equal(late[["0%"]], late[["2%"]])),
      "NMB at a given wtp differs between discount levels")

# ---------------------------------------------------------------------------
# C. Differential rates for costs and QALYs
# ---------------------------------------------------------------------------
message("\n--- Differential QALY / cost discount rates ---")

IMPACTncd$export_tables(
  baseline_year_for_change_outputs = BASELINE_YEAR,
  two_agegrps = FALSE,
  multicore = FALSE,
  qaly_discount_rate = c(0, 1.5),
  cost_discount_rate = c(0, 4)
)
file.rename(tables_dir, diff_dir)

cea_lv <- levels_in(file.path(
  diff_dir, "cost-effectiveness by year (societal-EQ5D5L) (not standardised).csv"))
# The pair is spelled out only when the two rates actually differ, so the
# (0, 0) level stays "0%" while the (1.5, 4) level names both.
check(setequal(cea_lv, c("0%", "QALYs 1.5%/costs 4%")),
      "CEA labels spell out both rates only when they differ")
check(setequal(levels_in(file.path(diff_dir, "QALYs by year (not standardised).csv")),
               c("0%", "1.5%")),
      "the qalys table is labelled with the QALY rate only")
check(setequal(levels_in(file.path(diff_dir, "costs by year (not standardised).csv")),
               c("0%", "4%")),
      "the costs table is labelled with the cost rate only")

cea_validate(diff_dir, "societal", "EQ5D5L", "QALYs 1.5%/costs 4%", 1.5, 4)

# ---------------------------------------------------------------------------
# D. The multicore path produces the same tables
# ---------------------------------------------------------------------------
message("\n--- multicore parity ---")
mc_dir <- file.path(output_dir, "tables_discount_mc_check")
unlink(c(tables_dir, mc_dir), recursive = TRUE, force = TRUE)
IMPACTncd$export_tables(
  baseline_year_for_change_outputs = BASELINE_YEAR,
  two_agegrps = FALSE,
  multicore = TRUE
)
file.rename(tables_dir, mc_dir)

read_canonical <- function(path) {
  dt <- fread(file = path)
  if (ncol(dt) == 0L) return(dt)
  setcolorder(dt, sort(names(dt)))
  setorderv(dt, names(dt))
  dt[]
}
mc_files <- list.files(mc_dir, pattern = "\\.csv$")
check(setequal(mc_files, list.files(disc_dir, pattern = "\\.csv$")),
      "multicore = TRUE produces the same file set")
mc_mismatch <- Filter(function(f) {
  !isTRUE(all.equal(read_canonical(file.path(disc_dir, f)),
                    read_canonical(file.path(mc_dir, f)),
                    tolerance = 1e-8, check.attributes = FALSE))
}, intersect(mc_files, list.files(disc_dir, pattern = "\\.csv$")))
check(length(mc_mismatch) == 0L,
      paste0("multicore tables are value-identical",
             if (length(mc_mismatch))
               paste0(" (mismatched: ", paste(mc_mismatch, collapse = "; "), ")")
             else ""))

# ---------------------------------------------------------------------------
# Report
# ---------------------------------------------------------------------------
if (length(failures) > 0L) {
  stop(
    "discount-levels test FAILED (", length(failures), " check(s)):\n",
    paste0("  - ", failures, collapse = "\n")
  )
}

message("\nAll discount-level checks PASSED.")
print("discount levels test has finished!")

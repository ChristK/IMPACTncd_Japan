# Unit test for the built-in cost column registry (R/cost_columns.R).
#
# The registry is the single source of truth shared by export_costs_summaries()
# (which GENERATES the SQL that creates these columns) and export_cea_tables()
# (which uses it to tell built-in from user-defined). If those two ever stated
# the set differently, a built-in column would be classified user-defined and
# added to the SOCIETAL perspective on top of cvd_total_costs, which already
# contains it - a silent double count.
#
# Note the two end-to-end harnesses (simulate_export_tables_testing.R:~356 and
# simulate_discount_levels_testing.R:~280) deliberately RE-STATE the built-in
# regex rather than calling the helper. That independence is what makes them a
# parity check, so do not "fix" them to use the helper - this file covers the
# helper instead.
#
# Run from the project root:
#   Rscript testing/test_cost_columns.R
#
# Prints PASS/FAIL per check and stops with an error if any fails.

source("./global.R")

bcc <- IMPACTncdJapan:::builtin_cost_cols
cd <- IMPACTncdJapan:::costed_diseases
comps <- IMPACTncdJapan:::cost_disease_components
rgx <- IMPACTncdJapan:::builtin_cost_col_regex
shape <- IMPACTncdJapan:::cost_view_builtin_shape
assrt <- IMPACTncdJapan:::assert_cost_view_matches_registry
registry <- IMPACTncdJapan:::.cost_registry

failures <- character(0)
check <- function(cond, msg) {
  ok <- isTRUE(cond)
  message(if (ok) "PASS: " else "FAIL: ", msg)
  if (!ok) failures <<- c(failures, msg)
  invisible(ok)
}

# The 15 columns exactly as the hand-written SQL emitted them, in file order.
# Component-major, disease-minor. This order is the on-disk column order of the
# costs summaries, so it must not move.
LEGACY <- c(
  "chd_direct_costs", "stroke_direct_costs", "cvd_direct_costs",
  "chd_productivity_costs", "stroke_productivity_costs", "cvd_productivity_costs",
  "chd_informal_costs", "stroke_informal_costs", "cvd_informal_costs",
  "chd_indirect_costs", "stroke_indirect_costs", "cvd_indirect_costs",
  "chd_total_costs", "stroke_total_costs", "cvd_total_costs"
)
LEGACY_RGX <-
  "^(chd|stroke|cvd)_(direct|productivity|informal|indirect|total)_costs$"

declared_of <- function(f) {
  vapply(yaml::read_yaml(f)$diseases, `[[`, character(1), "name")
}

# --- 1. Parity with the code being replaced --------------------------------
check(identical(bcc(cd(declared_of("inputs/sim_design.yaml"))), LEGACY),
      "helper reproduces the 15 legacy cost columns exactly, in order")
check(identical(rgx(), LEGACY_RGX),
      "builtin_cost_col_regex() is string-identical to the legacy regex")

# --- 2. The auto-population rule -------------------------------------------
check(identical(cd(declared_of("inputs/sim_design.yaml")), c("chd", "stroke", "cvd")),
      "the production design auto-populates to chd, stroke, cvd")
check(identical(cd(declared_of("testing/sim_design_testing.yaml")), c("chd", "stroke", "cvd")),
      "the testing design auto-populates to the same set")
# An extra declared disease with no cost machinery must contribute nothing -
# otherwise the writer would emit SQL for a column calc_costs never built.
check(identical(bcc(cd(declared_of("scenarios/sim_design_JPN21.yaml"))), LEGACY),
      "an 8th declared disease with no cost components adds no columns")
check(identical(cd(c("t2dm", "LDLcOver160")), character(0)),
      "a run with no costed disease yields no built-in cost columns")
# cvd is an aggregate: available when its constituents are, not when declared.
check(identical(cd(c("chd", "stroke")), c("chd", "stroke", "cvd")),
      "cvd is included from its constituents even when not declared")
check(identical(cd(c("chd", "cvd")), "chd"),
      "cvd is dropped when only one constituent is present")
# Design$new() topologically re-sorts the disease list, so declared order must
# not leak into the column order.
check(identical(bcc(cd(rev(declared_of("inputs/sim_design.yaml")))), LEGACY),
      "column order is registry-ordered, independent of declared order")

# --- 3. Availability is computed from primitives, not listed ----------------
check(identical(comps("cvd"),
                c("direct", "productivity", "informal", "indirect", "total")),
      "cvd has every component (intersection of chd and stroke)")
check(identical(bcc("cvd", "total"), "cvd_total_costs"),
      "components can be narrowed to address a single column")
shrunk <- registry
shrunk$chd$primitives <- "direct" # pretend chd only had direct costs
gen <- bcc(cd(c("chd", "stroke", "cvd"), registry = shrunk), registry = shrunk)
check(!("chd_total_costs" %in% gen) && "chd_direct_costs" %in% gen,
      "shrinking a disease's primitives drops its derived components")
check(identical(comps("cvd", registry = shrunk), "direct"),
      "the cvd aggregate collapses to the intersection of its constituents")

# --- 4. Loud, not silent ----------------------------------------------------
check(inherits(try(bcc("t2dm"), silent = TRUE), "try-error"),
      "an unregistered disease is an error, not a silent empty result")
check(inherits(try(bcc("chd", "bogus"), silent = TRUE), "try-error"),
      "an unknown component is an error")
check(inherits(try(assrt(setdiff(LEGACY, "cvd_total_costs"), LEGACY, "t"),
                   silent = TRUE), "try-error"),
      "assertion fires when the cost view lacks a claimed column")
check(inherits(try(assrt(c(LEGACY, "dementia_direct_costs"), LEGACY, "t"),
                   silent = TRUE), "try-error"),
      "assertion fires on a built-in-shaped column the registry does not claim")
check(isTRUE(assrt(LEGACY, LEGACY, "t")),
      "assertion passes when the view and the registry agree")

# --- 5. The shape regex must not swallow user columns ----------------------
# It is only ever applied to the cost VIEW, where these intermediates live.
check(!any(grepl(shape(), c("chd_prvl_prdv_costs", "chd_mrtl_prdv_costs",
                            "stroke_prvl_prdv_costs", "stroke_mrtl_prdv_costs"))),
      "the never-exported productivity intermediates are not built-in-shaped")
check(!grepl(rgx(), "statin_direct_costs") &&
        !grepl(rgx(), "sbp_intervention_costs"),
      "user-defined cost columns do not match the built-in regex")

if (length(failures) > 0L) {
  stop("Cost column registry test FAILED (", length(failures), " check(s)):\n",
       paste0("  - ", failures, collapse = "\n"))
}
message("\nAll cost column registry checks PASSED.")

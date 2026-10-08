# Unit test for the Design guard that the simulation includes year 2016.
#
# calc_costs() calibrates every disease cost against the simulated year 2016;
# a simulation that skips 2016 used to export all-zero costs silently. Design
# now stops on such a design instead.
#
# Run from the project root:
#   Rscript testing/test_design_init_year.R
#
# Prints PASS/FAIL per check and stops with an error if any fails.

source("./global.R")

failures <- character(0)
check <- function(cond, msg) {
  ok <- isTRUE(cond)
  message(if (ok) "PASS: " else "FAIL: ", msg)
  if (!ok) failures <<- c(failures, msg)
  invisible(ok)
}

base <- yaml::read_yaml("testing/sim_design_testing.yaml")
design_with <- function(init, horizon = base$sim_horizon_max) {
  prm <- base
  prm$init_year_long <- init
  prm$sim_horizon_max <- horizon
  tryCatch(Design$new(prm), error = function(e) e)
}

check(base$init_year_long <= 2016L,
      "testing/sim_design_testing.yaml itself starts in or before 2016")

d <- design_with(2016L)
check(inherits(d, "Design"), "init_year_long 2016 is accepted")
check(inherits(d, "Design") && d$sim_prm$init_year == 16L &&
        d$sim_prm$sim_horizon_max == base$sim_horizon_max - 2016L,
      "accepted design still converts init_year / sim_horizon_max as before")

check(inherits(design_with(2001L), "Design"), "an earlier start (2001) is accepted")

e <- design_with(2017L)
check(inherits(e, "error") && grepl("must include year 2016", conditionMessage(e)),
      "init_year_long 2017 stops with the year-2016 message")

e <- design_with(2019L)
check(inherits(e, "error") && grepl("got 2019-", conditionMessage(e)),
      "init_year_long 2019 stops and reports the offending years")

e <- design_with(2010L, horizon = 2015L)
check(inherits(e, "error") && grepl("must include year 2016", conditionMessage(e)),
      "a horizon ending before 2016 also stops")

if (length(failures)) {
  stop(length(failures), " check(s) failed:\n", paste(failures, collapse = "\n"))
}
message("All checks passed.")

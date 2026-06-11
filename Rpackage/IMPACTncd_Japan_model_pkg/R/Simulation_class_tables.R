## IMPACTncdJapan is an implementation of the IMPACTncd framework, developed by Chris
## Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
## Guzman-Castillo, Amandine Robert, and Piotr Bandosz.
##
## Copyright (C) 2018-2026 University of Liverpool, Chris Kypridemos
##
## IMPACTncdJapan is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version. This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details. You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/> or write
## to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301 USA.

# -----------------------------------------------------------------------------
# Simulation class table export methods
# This file adds table export methods to the Simulation class using $set().
# The logic is ported from auxil/process_out.R and mirrors the structure of the
# England model's Simulation_class_tables.R, adapted for the Japan model:
#   - no dimd/qimd (deprivation) stratification,
#   - CMS metrics (cms_score, cms_count) and user-defined *_contd columns,
#   - two QALY scales (EQ5D5L and HUI3),
#   - summary years are already stored in full 4-digit format (no +2000L),
#   - exposures stratified by agegrp20.
# Requires lock_class = FALSE on the Simulation R6 generator.
# -----------------------------------------------------------------------------


# safe_fquantile_byid ----
# Safe wrapper for CKutils::fquantile_byid that handles empty vectors.
# The underlying C++ function crashes with a segfault when passed empty vectors.
# This wrapper returns an empty data.table with the expected structure instead.
safe_fquantile_byid <- function(x, q, id, rounding = FALSE) {
  # Handle empty vector case - return empty data.table with expected structure
  if (length(x) == 0L) {
    result <- data.table::data.table(id = character(0))
    for (qi in q) {
      result[, (as.character(qi)) := numeric(0)]
    }
    return(result)
  }

  # Call the original function for non-empty vectors
  fquantile_byid(x, q, id, rounding)
}


# export_tables ----
#' @description
#' Export summary tables for policy analysis.
#'
#' Builds main, all-cause mortality, disease-characteristics, and exposure
#' tables from the per-summary parquet datasets produced by
#' `$export_summaries()`. Outputs are written to `output_dir/tables/`
#' (or `output_dir/tables2agegrps/` when `two_agegrps = TRUE`).
#'
#' This method replaces the standalone `auxil/process_out.R` script.
#'
#' @param baseline_year_for_change_outputs Integer. Reference year used for
#'   computing change-from-baseline columns. Default `2001L`. Two-digit
#'   values (e.g. `1`) are auto-promoted by adding 2000.
#' @param prbl Numeric vector of probability levels for output quantiles
#'   (median plus uncertainty bounds). Default
#'   `c(0.5, 0.025, 0.975, 0.1, 0.9)`.
#' @param comparator_scenario Character. Name of the scenario used as the
#'   comparator when computing differences between scenarios (cypp, cpp, dpp,
#'   net_qalys, net_costs). Default `"sc0"`.
#' @param two_agegrps Logical. If `TRUE`, collapses age into two groups
#'   (30-64 and 65-99) and writes to `tables2agegrps/`; otherwise uses the
#'   standard 5-year age groups and writes to `tables/`.
#' @param strata Named list giving the stratification configuration. Recognised
#'   names: `ons`, `esp`, `mrtl_ons`, `mrtl_esp`, `disease_char`, `xps_ons`,
#'   `xps_esp`. Each is a list of character vectors (e.g.
#'   `list("year", c("year", "sex"))`). Valid stratification variables: `year`
#'   (always required), `sex`, `agegrp`, `agegrp20` (xps only). Defaults to the
#'   full standard configuration shown in the signature, so the defaults are
#'   visible at the call site. A partial list overrides only the named entries
#'   and keeps the defaults for the rest (e.g. `list(ons = list("year"))`).
#'   When `two_agegrps = TRUE`, the default `ons` and `mrtl_ons` strata are
#'   restricted to their age-group combinations; explicitly supplied strata are
#'   used verbatim.
#' @param multicore Logical. If `TRUE`, runs the table-building task
#'   groups in parallel with single-threaded workers; otherwise runs
#'   sequentially with implicit (within-task) parallelism.
#' @param cea Logical. If `TRUE` (default), additionally export
#'   cost-effectiveness tables (incremental cost-effectiveness ratio, ICER, and
#'   net monetary benefit, NMB) from the societal and healthcare perspectives.
#'   These are built from the `qalys` and `costs` summaries for the actual
#'   (`scaled_up`) population only.
#' @param wtp Numeric vector of willingness-to-pay thresholds (monetary units
#'   per QALY, i.e. the same currency as the cost columns) at which NMB is
#'   computed. Default `c(5e6, 7.5e6, 1e7)`.
#' @param qaly_discount_rate Numeric. Annual discount rate (percent) applied to
#'   QALYs in the CEA tables. Default `2`.
#' @param cost_discount_rate Numeric. Annual discount rate (percent) applied to
#'   costs in the CEA tables. Default `2`.
#' @param discount_from_year Integer or `NULL`. First year from which present
#'   values are discounted (`PV = FV / (1 + rate/100)^max(0, year - base)`).
#'   When `NULL` (default) it is set to `baseline_year_for_change_outputs`.
#' @param custom_costs_in_healthcare Character vector of user-defined `*_costs`
#'   column names to additionally include in the healthcare perspective.
#'   User-defined `*_costs` columns are *always* included in the societal
#'   perspective; by default (`NULL`) none of them are added to the healthcare
#'   perspective (which captures direct treatment costs only). Pass the subset
#'   of custom cost columns that represent healthcare costs, e.g.
#'   `c("screening_costs", "drug_costs")`. Names not matching a user-defined
#'   cost column are ignored (with a message when `logs` is on). For backward
#'   compatibility a logical is also accepted: `FALSE`/`NULL` means none and
#'   `TRUE` means all user-defined cost columns. Default `NULL`.
#' @return The `Simulation` object, invisibly.
#' @examples
#' \dontrun{
#' # Use default strata
#' IMPACTncd$export_tables()
#'
#' # Custom strata - minimal outputs
#' IMPACTncd$export_tables(strata = list(
#'   ons = list("year", c("year", "sex")),
#'   esp = list("year")
#' ))
#' }
Simulation$set("public", "export_tables", function(
    baseline_year_for_change_outputs = 2001L,
    prbl = c(0.5, 0.025, 0.975, 0.1, 0.9),
    comparator_scenario = "sc0",
    two_agegrps = FALSE,
    strata = list(
      ons          = list("year", c("year", "sex"),
                          c("year", "agegrp"), c("year", "agegrp", "sex")),
      esp          = list("year", c("year", "sex")),
      mrtl_ons     = list("year", c("year", "sex"), c("year", "agegrp", "sex")),
      mrtl_esp     = list("year", c("year", "sex")),
      disease_char = list("year", c("year", "sex")),
      xps_ons      = list("year", c("year", "agegrp20"),
                          c("year", "sex"), c("year", "agegrp20", "sex")),
      xps_esp      = list("year", c("year", "sex"))
    ),
    multicore = TRUE,
    cea = TRUE,
    wtp = c(5e6, 7.5e6, 1e7),
    qaly_discount_rate = 2,
    cost_discount_rate = 2,
    discount_from_year = NULL,
    custom_costs_in_healthcare = NULL
) {
  # Promote a two-digit baseline year to full format (Japan years are 4-digit)
  if (baseline_year_for_change_outputs <= 100) {
    baseline_year_for_change_outputs <- baseline_year_for_change_outputs + 2000L
  }

  # CEA present values are discounted from the baseline year unless overridden
  if (is.null(discount_from_year)) {
    discount_from_year <- baseline_year_for_change_outputs
  } else if (discount_from_year <= 100) {
    discount_from_year <- discount_from_year + 2000L
  }

  # Thread control for parallel execution
  if (multicore) {
    arrow::set_cpu_count(1L)
    data.table::setDTthreads(threads = 1L, restore_after_fork = NULL)
    fst::threads_fst(nr_of_threads = 1L, reset_after_fork = NULL)
  } else {
    arrow::set_cpu_count(self$design$sim_prm$clusternumber_export)
    data.table::setDTthreads(
      threads = self$design$sim_prm$clusternumber_export,
      restore_after_fork = NULL
    )
    fst::threads_fst(
      nr_of_threads = self$design$sim_prm$clusternumber_export,
      reset_after_fork = NULL
    )
  }

  # Build strata configuration (merge user-provided with defaults)
  strata_cfg <- private$build_strata_config(strata, two_agegrps)

  tables_subdir <- if (two_agegrps) "tables2agegrps" else "tables"
  tables_dir <- private$output_dir(tables_subdir)
  private$create_new_folder(tables_dir)

  # Build task list (one group per source family) for parallel execution
  tasks <- list(
    list(
      id = 1L,
      type = "main",
      prbl = prbl,
      baseline_year = baseline_year_for_change_outputs,
      output_dir = private$output_dir(),
      tables_dir = tables_dir,
      comparator_scenario = comparator_scenario,
      two_agegrps = two_agegrps,
      strata_ons = strata_cfg$ons,
      strata_esp = strata_cfg$esp
    ),
    list(
      id = 2L,
      type = "all_cause_mrtl",
      prbl = prbl,
      summaries_dir = private$output_dir("summaries"),
      tables_dir = tables_dir,
      strata_ons = strata_cfg$mrtl_ons,
      strata_esp = strata_cfg$mrtl_esp
    ),
    list(
      id = 3L,
      type = "disease_char",
      prbl = prbl,
      summaries_dir = private$output_dir("summaries"),
      tables_dir = tables_dir,
      strata = strata_cfg$disease_char
    ),
    list(
      id = 4L,
      type = "xps",
      prbl = prbl,
      output_dir = private$output_dir(),
      tables_dir = tables_dir,
      strata_ons = strata_cfg$xps_ons,
      strata_esp = strata_cfg$xps_esp
    )
  )

  # Cost-effectiveness (ICER / NMB) tables, built from qalys + costs summaries.
  if (cea) {
    tasks[[length(tasks) + 1L]] <- list(
      id = 5L,
      type = "cea",
      prbl = prbl,
      summaries_dir = private$output_dir("summaries"),
      tables_dir = tables_dir,
      comparator_scenario = comparator_scenario,
      baseline_year = baseline_year_for_change_outputs,
      wtp = wtp,
      qaly_discount_rate = qaly_discount_rate,
      cost_discount_rate = cost_discount_rate,
      discount_from_year = discount_from_year,
      custom_costs_in_healthcare = custom_costs_in_healthcare,
      strata = strata_cfg$ons
    )
  }

  if (multicore) {
    if (self$design$sim_prm$logs) {
      private$time_mark("Start exporting tables (parallel)")
    }

    n_cores <- min(length(tasks), self$design$sim_prm$clusternumber_export)

    if (.Platform$OS.type == "windows") {
      cl <- parallelly::makeClusterPSOCK(
        n_cores,
        dryrun = FALSE,
        quiet = !self$design$sim_prm$logs,
        rscript_startup = quote(local({
          library(CKutils)
          library(IMPACTncdJapan)
          library(R6)
          library(data.table)
          library(scales)
        })),
        rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"),
        setup_strategy = "parallel"
      )
      on.exit(parallel::stopCluster(cl), add = TRUE)

      parallel::parLapplyLB(
        cl = cl,
        X = tasks,
        fun = function(task) {
          private$export_tables_hlpr(task, implicit_parallelism = FALSE)
          NULL
        }
      )
    } else {
      # Linux/macOS: forking
      doParallel::registerDoParallel(n_cores)
      foreach::foreach(
        task = tasks,
        .inorder = FALSE,
        .packages = c("R6", "CKutils", "IMPACTncdJapan", "data.table", "scales"),
        .verbose = self$design$sim_prm$logs
      ) %dopar% {
        private$export_tables_hlpr(task, implicit_parallelism = FALSE)
        NULL
      }
    }

    if (self$design$sim_prm$logs) {
      private$time_mark("End exporting tables (parallel)")
    }
  } else {
    # Sequential execution
    lapply(tasks, function(task) {
      private$export_tables_hlpr(task, implicit_parallelism = TRUE)
    })
  }

  invisible(self)
})


# export_tables_hlpr ----
# Helper for parallel table export. Dispatches to the appropriate export
# function based on task type.
Simulation$set("private", "export_tables_hlpr", function(task, implicit_parallelism) {
  # Thread control
  if (implicit_parallelism) {
    arrow::set_cpu_count(self$design$sim_prm$clusternumber_export)
    data.table::setDTthreads(
      threads = self$design$sim_prm$clusternumber_export,
      restore_after_fork = NULL
    )
    fst::threads_fst(
      nr_of_threads = self$design$sim_prm$clusternumber_export,
      reset_after_fork = NULL
    )
  } else {
    arrow::set_cpu_count(1L)
    data.table::setDTthreads(threads = 1L, restore_after_fork = NULL)
    fst::threads_fst(nr_of_threads = 1L, reset_after_fork = NULL)
  }

  # Dispatch to appropriate export function
  switch(task$type,
    "main" = private$export_main_tables(
      prbl = task$prbl,
      baseline_year = task$baseline_year,
      output_dir = task$output_dir,
      tables_dir = task$tables_dir,
      comparator_scenario = task$comparator_scenario,
      two_agegrps = task$two_agegrps,
      strata_ons = task$strata_ons,
      strata_esp = task$strata_esp
    ),
    "all_cause_mrtl" = private$export_all_cause_mrtl_tables(
      prbl = task$prbl,
      summaries_dir = task$summaries_dir,
      tables_dir = task$tables_dir,
      strata_ons = task$strata_ons,
      strata_esp = task$strata_esp
    ),
    "disease_char" = private$export_disease_characteristics_tables(
      prbl = task$prbl,
      summaries_dir = task$summaries_dir,
      tables_dir = task$tables_dir,
      strata = task$strata
    ),
    "xps" = private$export_xps_tables(
      prbl = task$prbl,
      output_dir = task$output_dir,
      tables_dir = task$tables_dir,
      strata_ons = task$strata_ons,
      strata_esp = task$strata_esp
    ),
    "cea" = private$export_cea_tables(
      prbl = task$prbl,
      summaries_dir = task$summaries_dir,
      tables_dir = task$tables_dir,
      comparator_scenario = task$comparator_scenario,
      baseline_year = task$baseline_year,
      wtp = task$wtp,
      qaly_discount_rate = task$qaly_discount_rate,
      cost_discount_rate = task$cost_discount_rate,
      discount_from_year = task$discount_from_year,
      custom_costs_in_healthcare = task$custom_costs_in_healthcare,
      strata = task$strata
    )
  )

  gc(verbose = FALSE)
  invisible(NULL)
})


# read_summary_dataset ----
# Reads a per-summary parquet dataset (e.g. "prvl_scaled_up") into a
# data.table. Returns NULL (with an optional message) when the dataset is
# absent, so callers can skip metrics whose source summary was not produced.
Simulation$set("private", "read_summary_dataset", function(summary_type, standardization = "scaled_up") {
  fpth <- private$output_dir(
    paste0("summaries/", summary_type, "_", standardization)
  )
  if (!dir.exists(fpth)) {
    if (self$design$sim_prm$logs) {
      message(fpth, " doesn't exist, skipping...")
    }
    return(NULL)
  }
  tt <- CKutils::read_parquet_dt(fpth)
  return(tt)
})


# build_strata_config ----
# Builds the strata configuration by merging user-provided strata with
# defaults. Defaults match the stratification from auxil/process_out.R.
# Japan has no dimd/qimd (deprivation) dimension.
#
# The standard (5-year age group) defaults are declared once, as the visible
# default of the `strata` argument in export_tables(), and read back here via
# formals() so there is a single source of truth. The coarse two_agegrps set is
# derived from them: it is identical except that the actual-population ons /
# mrtl_ons tables keep only their age-group strata.
Simulation$set("private", "build_strata_config", function(user_strata, two_agegrps = FALSE) {
  # Single source of truth: the export_tables() signature default.
  standard <- eval(formals(self$export_tables)[["strata"]])

  # An unchanged default (or an explicit NULL) means "use the defaults".
  if (is.null(user_strata)) user_strata <- standard
  customized <- !identical(user_strata, standard)

  result <- standard

  # In two_agegrps mode the actual-population main (ons) and all-cause-mortality
  # (mrtl_ons) tables are broken down by age group only, so drop their
  # non-age-group default strata. The esp / xps / disease_char defaults are
  # unaffected. User-supplied strata (merged below) are applied verbatim and so
  # override this coarsening.
  if (two_agegrps) {
    for (nm in c("ons", "mrtl_ons")) {
      result[[nm]] <- Filter(function(s) "agegrp" %in% s, result[[nm]])
    }
  }

  # Merge user-provided strata over the defaults (recognised names only).
  if (customized) {
    for (name in names(user_strata)) {
      if (name %in% names(result)) {
        result[[name]] <- user_strata[[name]]
      }
    }
  }

  return(result)
})


# tbl_smmrs_core ----
# Core table summary logic adapted from auxil/process_out.R tbl_smmrs().
# Handles aggregation, rate calculation and quantile computation for a single
# already-loaded `tt` data.table, writing one CSV per stratum to tables_dir.
Simulation$set("private", "tbl_smmrs_core", function(
    tt,                    # data.table with summary data
    what,                  # metric type
    population,            # "ons" or "esp"
    strata,                # strata list (e.g., list("year", c("year", "sex")))
    prbl,                  # quantile probabilities
    baseline_year,         # for _change calculations
    comparator_scenario,   # for comparison metrics
    comparison_starting_year,
    tables_dir             # output directory
) {
  # Source datasets (used only for the *_pp variable-name stripping below)
  str0 <- c(
    "prvl" = "prvl", "prvl_change" = "prvl",
    "incd" = "incd", "incd_change" = "incd",
    "ftlt" = "dis_mrtl", "ftlt_change" = "dis_mrtl",
    "mrtl" = "mrtl", "mrtl_change" = "mrtl",
    "dis_mrtl" = "dis_mrtl", "dis_mrtl_change" = "dis_mrtl",
    "cms_score" = "cms_score", "cms_score_change" = "cms_score",
    "cms_score_age" = "cms_score_by_age", "cms_score_age_change" = "cms_score_by_age",
    "cms_count" = "cms_count", "cms_count_change" = "cms_count",
    "contd" = "contd", "contd_change" = "contd",
    "qalys" = "qalys", "net_qalys" = "qalys",
    "costs" = "costs", "net_costs" = "costs",
    "cypp" = "prvl", "cpp" = "incd", "dpp" = "mrtl",
    "pop" = "prvl"
  )

  # Column patterns for grep / .SDcols
  str2 <- c(
    "prvl" = "_prvl$|^popsize$", "prvl_change" = "_prvl$|^popsize$",
    "incd" = "_incd$|^popsize$", "incd_change" = "_incd$|^popsize$",
    "ftlt" = "_deaths$|_prvl$", "ftlt_change" = "_deaths$|_prvl$",
    "mrtl" = "_mrtl$|^popsize$", "mrtl_change" = "_mrtl$|^popsize$",
    "dis_mrtl" = "^nonmodelled_deaths$|^chd_deaths$|^stroke_deaths$|^popsize$",
    "dis_mrtl_change" = "^nonmodelled_deaths$|^chd_deaths$|^stroke_deaths$|^popsize$",
    "cms_score" = "cms_score", "cms_score_change" = "cms_score",
    "cms_score_age" = "cms_score", "cms_score_age_change" = "cms_score",
    "cms_count" = "cms_count", "cms_count_change" = "cms_count",
    "contd" = "_contd$", "contd_change" = "_contd$",
    "qalys" = "^EQ5D5L$|^HUI3$", "net_qalys" = "^EQ5D5L$|^HUI3$",
    "costs" = "_costs$", "net_costs" = "_costs$",
    "cypp" = "_prvl$", "cpp" = "_incd$", "dpp" = "_mrtl$",
    "pop" = "^popsize$"
  )

  # Output column-name prefixes
  str3 <- c(
    "prvl" = "prvl_rate_", "prvl_change" = "prct_change_",
    "incd" = "incd_rate_", "incd_change" = "prct_change_",
    "ftlt" = "ftlt_rate_", "ftlt_change" = "ftlt_rate_",
    "mrtl" = "mrtl_rate_", "mrtl_change" = "mrtl_change_",
    "dis_mrtl" = "disease_mrtl_rate_", "dis_mrtl_change" = "disease_mrtl_change_",
    "cms_score" = "mean_cms_score_", "cms_score_change" = "mean_cms_score_",
    "cms_score_age" = "mean_cms_score_", "cms_score_age_change" = "mean_cms_score_",
    "cms_count" = "mean_cms_count_", "cms_count_change" = "mean_cms_count_",
    "contd" = "mean_", "contd_change" = "mean_change_",
    "qalys" = "qalys_", "net_qalys" = "net_qalys_",
    "costs" = "costs_", "net_costs" = "net_costs_",
    "cypp" = "cypp_", "cpp" = "cpp_", "dpp" = "dpp_",
    "pop" = "pop_size_"
  )

  # Output file descriptions
  str4 <- c(
    "prvl" = "prevalence by ", "prvl_change" = "prevalence change by ",
    "incd" = "incidence by ", "incd_change" = "incidence change by ",
    "ftlt" = "case fatality by ", "ftlt_change" = "case fatality change by ",
    "mrtl" = "all-cause mortality by ", "mrtl_change" = "all-cause mortality change by ",
    "dis_mrtl" = "disease-specific mortality by ",
    "dis_mrtl_change" = "disease-specific mortality change by ",
    "cms_score" = "mean CMS score by ", "cms_score_change" = "mean CMS score change by ",
    "cms_score_age" = "mean CMS score by ", "cms_score_age_change" = "mean CMS score change by ",
    "cms_count" = "mean CMS count by ", "cms_count_change" = "mean CMS count change by ",
    "contd" = "mean of continuous outcomes by ",
    "contd_change" = "mean of continuous outcomes change by ",
    "qalys" = "QALYs by ", "net_qalys" = "net QALYs by ",
    "costs" = "costs by ", "net_costs" = "net costs by ",
    "cypp" = "case-years prevented or postponed by ",
    "cpp" = "cases prevented or postponed by ",
    "dpp" = "deaths prevented or postponed by ",
    "pop" = "pop size by "
  )

  # Add mc and scenario to each strata combination
  strata <- lapply(strata, function(x) c("mc", "scenario", x))

  # Process each strata combination
  lapply(strata, function(x) {
    if (grepl("^cms_", what)) {
      d <- tt[, .("value" = weighted.mean(get(str2[[what]]), popsize)),
              keyby = eval(x)]

      if (grepl("_change$", what)) { # when calculating change
        d19 <- d[year == baseline_year][, year := NULL]
        d[d19, on = c(setdiff(x, "year")), value := value / i.value]
      }
      d <- d[, as.list(fquantile(value, prbl)), keyby = eval(setdiff(x, "mc"))]
      setnames(d, c(setdiff(x, "mc"), scales::percent(prbl, prefix = str3[[what]])))
      setkeyv(d, setdiff(x, "mc"))
      setcolorder(d, setdiff(x, "mc"))

    } else if (grepl("^qalys$", what)) {
      d <- tt[, .("EQ5D5L" = sum(EQ5D5L),
                  "HUI3" = sum(HUI3)),
              keyby = eval(x)]
      d <- melt(d, id.vars = x, variable.name = "scale", value.name = "QALYs")
      setkeyv(d, c(x[x != "year"], "scale", "year"))
      d[, cumulative := cumsum(QALYs), keyby = c(setdiff(x, "year"), "scale")]
      d <- melt(d, id.vars = c(x, "scale"), variable.name = "type")
      d[, type := fifelse(type == "cumulative", "QALYs_cuml", "QALYs")]

      setkey(d, "type", "scale")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(type), rounding = FALSE),
             keyby = eval(setdiff(c(x, "scale"), "mc"))]
      setnames(d, c(setdiff(c(x, "scale"), "mc"), "type", scales::percent(prbl, prefix = str3[[what]])))
      setkeyv(d, c("type", setdiff(c(x, "scale"), "mc")))
      setcolorder(d, setdiff(c(x, "scale"), "mc"))

    } else if (grepl("^net_qalys$", what)) {
      d <- tt[, .("EQ5D5L" = sum(EQ5D5L),
                  "HUI3" = sum(HUI3)),
              keyby = eval(x)]
      d <- melt(d, id.vars = x, variable.name = "scale", value.name = "QALYs")
      d_sc0 <- d[scenario == comparator_scenario & year >= comparison_starting_year][, scenario := NULL]
      d <- d[scenario != comparator_scenario & year >= comparison_starting_year][
        d_sc0, on = c(setdiff(x, "scenario"), "scale"), net_QALYs := QALYs - i.QALYs] # positive numbers for prevention
      d[, QALYs := NULL]
      setkeyv(d, c(x[x != "year"], "scale", "year"))
      d[, cumulative := cumsum(net_QALYs), keyby = c(setdiff(x, "year"), "scale")]
      d <- melt(d, id.vars = c(x, "scale"), variable.name = "type")
      d[type == "cumulative", type := "net_QALYs_cuml"]
      setkey(d, "type", "scale")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(type), rounding = FALSE),
             keyby = eval(setdiff(c(x, "scale"), "mc"))]
      x <- c(x, "scale")
      setnames(d, c(setdiff(x, "mc"), "type", scales::percent(prbl, prefix = str3[[what]])))
      setkeyv(d, c("type", setdiff(x, "mc")))
      setcolorder(d, setdiff(x, "mc"))

    } else if (grepl("^costs", what)) {
      d <- tt[, lapply(.SD, sum), .SDcols = patterns("_costs$"), keyby = eval(x)]
      d <- melt(d, id.vars = x, variable.name = "costs_type", value.name = "costs")
      d[, cumulative := cumsum(costs), keyby = c(setdiff(x, "year"), "costs_type")]
      d <- melt(d, id.vars = c(x, "costs_type"), variable.name = "type")
      d[type == "cumulative", type := "costs_cuml"]
      setkey(d, "type", "costs_type")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(type), rounding = FALSE),
             keyby = eval(setdiff(c(x, "costs_type"), "mc"))]
      setnames(d, c(setdiff(c(x, "costs_type"), "mc"), "type", scales::percent(prbl, prefix = str3[[what]])))
      setkeyv(d, c("type", setdiff(c(x, "costs_type"), "mc")))
      setcolorder(d, setdiff(c(x, "costs_type"), "mc"))

    } else if (grepl("^net_costs", what)) {
      d <- tt[, lapply(.SD, sum), .SDcols = patterns("_costs$"), keyby = eval(x)]
      d <- melt(d, id.vars = x, variable.name = "costs_type", value.name = "value")
      d_sc0 <- d[scenario == comparator_scenario & year >= comparison_starting_year][, scenario := NULL]
      d <- d[scenario != comparator_scenario & year >= comparison_starting_year][
        d_sc0, on = c(setdiff(x, "scenario"), "costs_type"), net_costs := value - i.value] # negative numbers for prevention
      d[, value := NULL]
      setkeyv(d, c(x[x != "year"], "costs_type", "year"))
      d[, cumulative := cumsum(net_costs), keyby = c(setdiff(x, "year"), "costs_type")]
      d <- melt(d, id.vars = c(x, "costs_type"), variable.name = "type")
      d[type == "cumulative", type := "net_costs_cuml"]
      setkey(d, "type", "costs_type")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(type), rounding = FALSE),
             keyby = eval(setdiff(c(x, "costs_type"), "mc"))]
      x <- c(x, "costs_type")
      setnames(d, c(setdiff(x, "mc"), "type", scales::percent(prbl, prefix = str3[[what]])))
      setkeyv(d, c("type", setdiff(x, "mc")))
      setcolorder(d, setdiff(x, "mc"))

    } else if (grepl("^contd", what)) {
      # _contd summaries already hold a population-weighted mean per stratum,
      # so collapsing to coarser strata re-weights by popsize (NOT a sum, and
      # no division by popsize).
      contd_cols <- grep("_contd$", names(tt), value = TRUE)
      d <- tt[, lapply(.SD, function(v) weighted.mean(v, popsize, na.rm = TRUE)),
              .SDcols = contd_cols, keyby = eval(x)]
      d <- melt(d, id.vars = x)

      if (grepl("_change$", what)) { # when calculating change
        d19 <- d[year == baseline_year][, year := NULL]
        d[d19, on = c(setdiff(x, "year"), "variable"), value := value / i.value]
      }

      setkey(d, "variable")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(variable), rounding = FALSE),
             keyby = eval(setdiff(x, "mc"))]
      setnames(d, c(setdiff(x, "mc"), "disease", scales::percent(prbl, prefix = str3[[what]])))
      setkeyv(d, setdiff(x, "mc"))
      setcolorder(d, setdiff(x, "mc"))

    } else { # if not cms or qalys or costs or contd...
      d <- tt[, lapply(.SD, sum), .SDcols = patterns(str2[[what]]), keyby = x]

      # convert int cols to numeric (avoids warning with melt())
      is_int <- sapply(d[, .SD, .SDcols = -x], is.integer)
      is_int <- names(is_int[is_int])
      if (length(is_int) > 0) {
        d[, (is_int) := lapply(.SD, as.numeric), .SDcols = is_int]
      }

      if (grepl("^ftlt", what)) {
        nm <- names(d)
        nm <- grep("_deaths$", nm, value = TRUE)
        nm <- gsub("_deaths$", "", nm)
        nm <- setdiff(nm, "alive")
        for (i in nm) {
          set(d, NULL, paste0(i, "_ftlt"),
              d[[paste0(i, "_deaths")]] / d[[paste0(i, "_prvl")]])
        }
        nm <- names(d)
        nm <- grep("_deaths$|_prvl$", nm, value = TRUE)
        d[, (nm) := NULL]
        setnafill(d, "const", 0, cols = grep("_ftlt$", names(d), value = TRUE))
      } else if (!what %in% c("pop", "cypp", "cpp", "dpp")) {
        # avoid calculating rates for pop, cypp, cpp, dpp
        d <- d[, lapply(.SD, function(y) y / popsize), keyby = x]
      }

      d <- melt(d, id.vars = x)

      if (grepl("_change$", what)) { # when calculating change
        d19 <- d[year == baseline_year][, year := NULL]
        d[d19, on = c(setdiff(x, "year"), "variable"), value := value / i.value]
      }

      if (grepl("^cypp$|^cpp$|^dpp$", what)) {
        d_sc0 <- d[scenario == comparator_scenario & year >= comparison_starting_year][, scenario := NULL]
        d <- d[scenario != comparator_scenario & year >= comparison_starting_year][
          d_sc0, on = c(setdiff(x, "scenario"), "variable"), value := i.value - value] # positive numbers for prevention
        d[, variable := gsub(paste0("_", str0[[what]]), "", variable)]
        setkeyv(d, c(x[x != "year"], "variable", "year"))
        d[, cumulative := cumsum(value), keyby = c(setdiff(x, "year"), "variable")]
        d <- melt(d, id.vars = c(x, "variable"), variable.name = "type")
        d[, type := fifelse(type == "cumulative", paste0(what, "_cuml"), what)]
        setkey(d, "type", "variable")
        d <- d[, safe_fquantile_byid(value, prbl, id = as.character(variable),
                                     rounding = (what %in% c("pop", "cypp", "cpp", "dpp"))),
               keyby = eval(setdiff(c(x, "type"), "mc"))]
        x <- c(x, "type")
        setnames(d, c(setdiff(x, "mc"), "disease", scales::percent(prbl, prefix = str3[[what]])))
      } else {
        setkey(d, "variable")
        d <- d[, safe_fquantile_byid(value, prbl, id = as.character(variable),
                                     rounding = what == "pop"),
               keyby = eval(setdiff(x, "mc"))]
        setnames(d, c(setdiff(x, "mc"), "disease", scales::percent(prbl, prefix = str3[[what]])))
      }

      if (what == "pop") {
        d[, disease := NULL]
      } else {
        if ("popsize" %in% d$disease) d <- d[disease != "popsize"]
      }
      setkeyv(d, setdiff(x, "mc"))
      setcolorder(d, setdiff(x, "mc"))
    }

    # Build output filename
    str5 <- c(
      "ons" = " (not standardised).csv",
      "esp" = paste0(" (", paste(setdiff(c("mc", "scenario", "year", "age", "sex"), x),
                                 collapse = "-"), " standardised).csv")
    )
    str6 <- paste0(
      str4[[what]],
      paste(setdiff(x, c("mc", "scenario", "type", "scale", "costs_type")), collapse = "-"),
      str5[[population]]
    )

    fwrite(d, file.path(tables_dir, str6))
  })

  invisible(NULL)
})


# export_main_tables ----
# Generate main summary tables (prevalence, incidence, mortality, QALYs,
# costs, continuous outcomes, etc.). Memory-optimised: datasets are read once
# per source/population and reused across the metrics derived from them.
Simulation$set("private", "export_main_tables", function(
    prbl,
    baseline_year,
    output_dir,
    tables_dir,
    comparator_scenario = "sc0",
    two_agegrps = FALSE,
    strata_ons = NULL,
    strata_esp = NULL
) {
  if (self$design$sim_prm$logs) {
    message("Generating main summary tables...")
  }

  str1 <- c("ons" = "scaled_up", "esp" = "esp")

  # Group metrics by source dataset for efficient memory usage.
  # NOTE: CMS metrics (cms_score, cms_count, cms_score_age) are intentionally
  # omitted here - matching the commented-out state in auxil/process_out.R.
  # The cms branch in tbl_smmrs_core() remains available; add the relevant
  # sources here to re-enable them.
  source_to_metrics <- list(
    prvl = c("prvl", "prvl_change", "cypp", "pop"),
    incd = c("incd", "incd_change", "cpp"),
    dis_mrtl = c("ftlt", "ftlt_change", "dis_mrtl", "dis_mrtl_change"),
    mrtl = c("mrtl", "mrtl_change", "dpp"),
    qalys = c("qalys", "net_qalys"),
    costs = c("costs", "net_costs"),
    # contd_change is included here even though auxil/process_out.R omits it from
    # its metric list: that omission is an oversight (the script still defines
    # all the contd_change string mappings), and every other metric has a
    # _change variant. tbl_smmrs_core computes it as the relative change of the
    # population-weighted mean from the baseline year.
    contd = c("contd", "contd_change")
  )

  agegrp_young <- c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64")
  agegrp_old <- c("65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99")

  comparison_metrics <- c("cypp", "cpp", "dpp", "net_qalys", "net_costs")

  # Process each source dataset group
  for (source_name in names(source_to_metrics)) {
    metrics_for_source <- source_to_metrics[[source_name]]

    # Process both populations for this source
    for (pop_name in c("ons", "esp")) {
      pop_key <- str1[[pop_name]]

      # Load dataset once for this source/population combination
      tt_base <- private$read_summary_dataset(source_name, pop_key)
      if (is.null(tt_base)) next

      # Also load prvl for ftlt metrics (case fatality denominator)
      prvl_for_ftlt <- NULL
      if (source_name == "dis_mrtl") {
        prvl_for_ftlt <- private$read_summary_dataset("prvl", pop_key)
      }

      # Process each metric that uses this source
      for (what in metrics_for_source) {
        # Skip pop and *_age for the standardised population
        if (what == "pop" && pop_name == "esp") next
        if (grepl("_age", what) && pop_name == "esp") next

        # Use configurable strata (already filtered by two_agegrps in
        # build_strata_config). Swap agegrp -> age for *_age metrics.
        strata <- if (pop_name == "ons") strata_ons else strata_esp
        if (grepl("_age", what)) {
          strata <- lapply(strata, function(st) {
            st[st == "agegrp"] <- "age"
            st
          })
        }

        if (self$design$sim_prm$logs) {
          message(paste0("  ", what, "-", pop_name))
        }

        # Get a copy of the base dataset
        tt <- copy(tt_base)

        # Check that comparison metrics have at least one intervention scenario
        if (what %in% comparison_metrics) {
          available_scenarios <- unique(tt$scenario)
          non_comparator_scenarios <- setdiff(available_scenarios, comparator_scenario)
          if (length(non_comparator_scenarios) == 0) {
            if (self$design$sim_prm$logs) {
              message("    Skipping ", what, " - no intervention scenarios (only '",
                      comparator_scenario, "' found)")
            }
            rm(tt)
            next
          }
        }

        # Handle two_agegrps transformation
        if (two_agegrps && "agegrp" %in% names(tt)) {
          tt[agegrp %in% agegrp_young, agegrp := "30-64"]
          tt[agegrp %in% agegrp_old, agegrp := "65-99"]
        }

        # For case fatality, add prevalence denominator
        if (grepl("^ftlt", what) && !is.null(prvl_for_ftlt)) {
          t1 <- copy(prvl_for_ftlt)
          setnames(t1, "popsize", "nonmodelled_prvl")
          if (two_agegrps && "agegrp" %in% names(t1)) {
            t1[agegrp %in% agegrp_young, agegrp := "30-64"]
            t1[agegrp %in% agegrp_old, agegrp := "65-99"]
          }
          absorb_dt(tt, t1)
          tt <- tt[nonmodelled_prvl > 0] # denominator cannot be 0
          rm(t1)
        }

        # Generate tables
        private$tbl_smmrs_core(
          tt = tt,
          what = what,
          population = pop_name,
          strata = strata,
          prbl = prbl,
          baseline_year = baseline_year,
          comparator_scenario = comparator_scenario,
          comparison_starting_year = baseline_year,
          tables_dir = tables_dir
        )
        rm(tt)
      }

      # Cleanup after processing this source/population
      rm(tt_base)
      if (!is.null(prvl_for_ftlt)) rm(prvl_for_ftlt)
    }

    # Garbage collect after each source group
    gc(verbose = FALSE)
  }

  invisible(NULL)
})


# export_all_cause_mrtl_tables ----
# Generate all-cause mortality by disease tables (disease-denominator and
# population-denominator versions, non-standardised and ESP-standardised).
Simulation$set("private", "export_all_cause_mrtl_tables", function(
    prbl,
    summaries_dir,
    tables_dir,
    strata_ons = NULL,
    strata_esp = NULL
) {
  if (self$design$sim_prm$logs) {
    message("Generating all-cause mortality by disease tables...")
  }

  # Convert user strata to internal format with mc and scenario.
  make_strata_configs <- function(strata_list, standardised = FALSE) {
    lapply(strata_list, function(s) {
      outstrata <- c("mc", s, "scenario")
      suffix <- paste(s, collapse = "-")
      suffix <- gsub("agegrp", "agegroup", suffix)
      if (standardised) {
        possible_vars <- c("age", "sex")
        standardised_vars <- setdiff(possible_vars, s)
        std_suffix <- paste(standardised_vars, collapse = "-")
        list(strata = outstrata, suffix = suffix, std = std_suffix)
      } else {
        list(strata = outstrata, suffix = suffix)
      }
    })
  }

  # Load datasets once
  tt_scaled <- private$read_summary_dataset("all_cause_mrtl_by_dis", "scaled_up")
  pp_scaled <- private$read_summary_dataset("prvl", "scaled_up")
  tt_esp <- private$read_summary_dataset("all_cause_mrtl_by_dis", "esp")

  # ---- Non-standardised with disease denominator ----
  if (!is.null(tt_scaled)) {
    strata_configs <- make_strata_configs(strata_ons, standardised = FALSE)

    for (cfg in strata_configs) {
      outstrata <- cfg$strata
      d <- tt_scaled[, lapply(.SD, sum), .SDcols = patterns("^deaths_|^cases_"), keyby = eval(outstrata)]
      d <- melt(d, id.vars = outstrata)
      cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
      d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
      d[cases, on = c(outstrata, "variable"), value := value / i.value]
      rm(cases)
      setkey(d, "variable")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(variable)),
             keyby = eval(setdiff(outstrata, "mc"))]
      setnames(d, c(setdiff(outstrata, "mc"), "disease",
                    scales::percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
      setkeyv(d, setdiff(outstrata, "mc"))
      fwrite(d, file.path(tables_dir,
                          paste0("all-cause mortality given disease-", cfg$suffix, " (not standardised).csv")))
      rm(d)
    }
  }

  # ---- Non-standardised with population denominator ----
  if (!is.null(tt_scaled) && !is.null(pp_scaled)) {
    strata_configs <- make_strata_configs(strata_ons, standardised = FALSE)

    for (cfg in strata_configs) {
      outstrata <- cfg$strata
      cases <- pp_scaled[, lapply(.SD, sum), .SDcols = patterns("^popsize$"), keyby = eval(outstrata)]
      d <- tt_scaled[, lapply(.SD, sum), .SDcols = patterns("^deaths_|^cases_"), keyby = eval(outstrata)]
      d <- melt(d, id.vars = outstrata)
      d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
      d[cases, on = outstrata, value := value / popsize]
      rm(cases)
      setkey(d, "variable")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(variable)),
             keyby = eval(setdiff(outstrata, "mc"))]
      setnames(d, c(setdiff(outstrata, "mc"), "disease",
                    scales::percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
      setkeyv(d, setdiff(outstrata, "mc"))
      fwrite(d, file.path(tables_dir,
                          paste0("all-cause mortality given disease-", cfg$suffix, " popdenom (not standardised).csv")))
      rm(d)
    }
  }

  # Cleanup scaled_up datasets before ESP processing
  rm(tt_scaled, pp_scaled)
  gc(verbose = FALSE)

  # ---- Standardised (ESP) with disease denominator ----
  if (!is.null(tt_esp)) {
    strata_configs <- make_strata_configs(strata_esp, standardised = TRUE)

    for (cfg in strata_configs) {
      outstrata <- cfg$strata
      d <- tt_esp[, lapply(.SD, sum), .SDcols = patterns("^deaths_|^cases_"), keyby = eval(outstrata)]
      d <- melt(d, id.vars = outstrata)
      cases <- d[grep("^cases_", variable)][, variable := gsub("^cases_", "", variable)]
      d <- d[grep("^deaths_", variable)][, variable := gsub("^deaths_", "", variable)]
      d[cases, on = c(outstrata, "variable"), value := value / i.value]
      rm(cases)
      setkey(d, "variable")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(variable)),
             keyby = eval(setdiff(outstrata, "mc"))]
      setnames(d, c(setdiff(outstrata, "mc"), "disease",
                    scales::percent(prbl, prefix = "all_cause_mrtl_by_disease_rate_")))
      setkeyv(d, setdiff(outstrata, "mc"))
      fwrite(d, file.path(tables_dir,
                          paste0("all-cause mortality given disease-", cfg$suffix,
                                 " (", cfg$std, " standardised).csv")))
      rm(d)
    }
  }

  rm(tt_esp)
  invisible(NULL)
})


# export_disease_characteristics_tables ----
# Generate disease characteristics tables (duration, age metrics, CMS).
Simulation$set("private", "export_disease_characteristics_tables", function(
    prbl,
    summaries_dir,
    tables_dir,
    strata = NULL
) {
  if (self$design$sim_prm$logs) {
    message("Generating disease characteristics tables...")
  }

  tt <- private$read_summary_dataset("dis_characteristics", "scaled_up")
  if (is.null(tt)) return(invisible(NULL))

  # Type conversions (some CMS columns may arrive as integer)
  if ("mean_cms_count_cms1st_cont" %in% names(tt)) {
    tt[, mean_cms_count_cms1st_cont := as.numeric(mean_cms_count_cms1st_cont)]
  }

  # Derive id variables dynamically from requested strata, keeping only
  # columns that actually exist in the data.
  all_strata_vars <- unique(unlist(strata))
  id_vars <- intersect(unique(c("mc", "scenario", all_strata_vars)), names(tt))
  id_pattern <- paste(id_vars, collapse = "|")

  char_prefixes <- "^mean_duration_|^mean_age_incd_|^mean_age_1st_onset_|^mean_age_prvl_|^mean_cms_score_|^mean_cms_count_"

  # Extract case counts for weighting
  d1 <- tt[, .SD, .SDcols = patterns(paste0(id_pattern, "|^cases_"))]
  d1 <- melt(d1, id.vars = id_vars)
  d1 <- unique(d1, by = c(id_vars, "variable"))
  d1[, disease := gsub("^cases_", "", variable)]
  d1[, variable := NULL]

  # Extract characteristics columns
  tt <- tt[, .SD, .SDcols = patterns(paste0(id_pattern, "|", char_prefixes))]

  if ("mean_cms_count_cmsmm1" %in% names(tt)) {
    tt[, mean_cms_count_cmsmm1 := as.double(mean_cms_count_cmsmm1)]
  }

  tt <- melt(tt, id.vars = id_vars)
  tt[, disease := gsub(char_prefixes, "", variable)]
  tt[d1, on = c(id_vars, "disease"), cases := i.value]
  rm(d1) # NOTE mean_age_incd contains NAs

  # Convert user strata to internal format
  make_strata_configs <- function(strata_list) {
    lapply(strata_list, function(s) {
      list(strata = c("mc", s, "scenario"), suffix = paste(s, collapse = "-"))
    })
  }

  strata_configs <- make_strata_configs(strata)

  for (cfg in strata_configs) {
    outstrata <- cfg$strata
    d <- tt[, weighted.mean(value, cases, na.rm = TRUE), keyby = c(outstrata, "variable")] # na.rm for mean_age_incd
    setkey(d, "variable")
    d <- d[, safe_fquantile_byid(V1, prbl, id = as.character(variable)),
           keyby = eval(setdiff(outstrata, "mc"))]
    setnames(d, c(setdiff(outstrata, "mc"), "variable", scales::percent(prbl, prefix = "value_")))

    # Parse variable name to extract disease and type
    d[, disease := gsub(char_prefixes, "", variable)]
    d[grep("^mean_duration_", variable), type := "mean_duration"]
    d[grep("^mean_age_incd_", variable), type := "mean_age_incd"]
    d[grep("^mean_age_1st_onset_", variable), type := "mean_age_1st_onset"]
    d[grep("^mean_age_prvl_", variable), type := "mean_age_prvl"]
    d[grep("^mean_cms_score_", variable), type := "mean_cms_score"]
    d[grep("^mean_cms_count_", variable), type := "mean_cms_count"]
    d[, variable := NULL]
    setkeyv(d, c(setdiff(outstrata, "mc"), "disease", "type"))
    setcolorder(d)

    fwrite(d, file.path(tables_dir,
                        paste0("disease characteristics by ", cfg$suffix, " (not standardised).csv")))
    rm(d)
  }

  rm(tt)
  invisible(NULL)
})


# export_xps_tables ----
# Generate exposure summary tables from the xps20 (non-standardised) and xps5
# (ESP-standardised) parquet datasets.
Simulation$set("private", "export_xps_tables", function(
    prbl,
    output_dir,
    tables_dir,
    strata_ons = NULL,
    strata_esp = NULL
) {
  if (self$design$sim_prm$logs) {
    message("Generating exposure tables...")
  }

  # Build a filter expression from strata: variables in the strata are
  # filtered to != "All", variables not in the strata to == "All" (the
  # groupingsets marginals).
  make_xps_strata_configs <- function(strata_list, filterable_vars, standardised = FALSE) {
    lapply(strata_list, function(s) {
      outstrata <- c("mc", s, "scenario")
      suffix <- paste(setdiff(s, "year"), collapse = "-")
      if (suffix == "") suffix <- "year" else suffix <- paste0("year-", suffix)
      suffix <- gsub("agegrp20", "agegroup", suffix)

      filter_parts <- character(0)
      for (v in filterable_vars) {
        if (v %in% s) {
          filter_parts <- c(filter_parts, paste0(v, " != 'All'"))
        } else {
          filter_parts <- c(filter_parts, paste0(v, " == 'All'"))
        }
      }
      filter_str <- paste(filter_parts, collapse = " & ")
      filter_expr <- if (length(filter_parts) > 0) parse(text = filter_str)[[1]] else quote(TRUE)

      if (standardised) {
        s_for_std <- gsub("agegrp20", "age", s)
        all_std_vars <- gsub("agegrp20", "age", filterable_vars)
        standardised_vars <- setdiff(all_std_vars, s_for_std)
        std_suffix <- paste(standardised_vars, collapse = "-")
        list(strata = outstrata, suffix = suffix, filter_expr = filter_expr, std = std_suffix)
      } else {
        list(strata = outstrata, suffix = suffix, filter_expr = filter_expr)
      }
    })
  }

  # Detect filterable vars: character columns carrying an "All" marginal.
  detect_filterable_vars <- function(dt) {
    candidates <- setdiff(names(dt), c("mc", "scenario", "year"))
    candidates[vapply(candidates, function(v) {
      is.character(dt[[v]]) && "All" %in% dt[[v]]
    }, logical(1))]
  }

  # ---- Non-standardised (xps20) ----
  xps_path <- file.path(output_dir, "xps", "xps20")
  if (dir.exists(xps_path)) {
    xps_tab <- CKutils::read_parquet_dt(xps_path)
    xps_cols <- grep("_curr_xps$", names(xps_tab), value = TRUE)

    filt_vars <- detect_filterable_vars(xps_tab)
    strata_configs <- make_xps_strata_configs(strata_ons, filt_vars, standardised = FALSE)

    for (cfg in strata_configs) {
      outstrata <- cfg$strata
      # Skip strata that reference columns absent from this xps dataset
      # (e.g. agegrp20 when the summary was produced with a different age
      # grouping), rather than erroring.
      missing_cols <- setdiff(setdiff(outstrata, c("mc", "scenario")), names(xps_tab))
      if (length(missing_cols) > 0) {
        if (self$design$sim_prm$logs) {
          message("  skipping xps strata (missing columns): ",
                  paste(missing_cols, collapse = ", "))
        }
        next
      }
      d <- xps_tab[eval(cfg$filter_expr)]
      if (nrow(d) == 0) next
      d <- d[, lapply(.SD, mean), .SDcols = xps_cols, keyby = eval(outstrata)]
      d <- melt(d, id.vars = outstrata)
      setkey(d, "variable")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(variable)),
             keyby = eval(setdiff(outstrata, "mc"))]
      setnames(d, c(setdiff(outstrata, "mc"), "exposure", scales::percent(prbl, prefix = "xps_mean_")))
      setkeyv(d, setdiff(outstrata, "mc"))
      fwrite(d, file.path(tables_dir,
                          paste0("exposures by ", cfg$suffix, " (not standardised).csv")))
      rm(d)
    }
    rm(xps_tab)
    gc(verbose = FALSE)
  }

  # ---- Standardised (xps5 / ESP) ----
  xps_path <- file.path(output_dir, "xps", "xps5")
  if (dir.exists(xps_path)) {
    xps_tab <- CKutils::read_parquet_dt(xps_path)
    xps_cols <- grep("_curr_xps$", names(xps_tab), value = TRUE)

    filt_vars <- detect_filterable_vars(xps_tab)
    strata_configs <- make_xps_strata_configs(strata_esp, filt_vars, standardised = TRUE)

    for (cfg in strata_configs) {
      outstrata <- cfg$strata
      # Skip strata that reference columns absent from this xps dataset.
      missing_cols <- setdiff(setdiff(outstrata, c("mc", "scenario")), names(xps_tab))
      if (length(missing_cols) > 0) {
        if (self$design$sim_prm$logs) {
          message("  skipping xps strata (missing columns): ",
                  paste(missing_cols, collapse = ", "))
        }
        next
      }
      d <- xps_tab[eval(cfg$filter_expr)]
      if (nrow(d) == 0) next
      d <- d[, lapply(.SD, mean), .SDcols = xps_cols, keyby = eval(outstrata)]
      d <- melt(d, id.vars = outstrata)
      setkey(d, "variable")
      d <- d[, safe_fquantile_byid(value, prbl, id = as.character(variable)),
             keyby = eval(setdiff(outstrata, "mc"))]
      setnames(d, c(setdiff(outstrata, "mc"), "exposure", scales::percent(prbl, prefix = "xps_mean_")))
      setkeyv(d, setdiff(outstrata, "mc"))
      fwrite(d, file.path(tables_dir,
                          paste0("exposures by ", cfg$suffix, " (", cfg$std, " standardised).csv")))
      rm(d)
    }
    rm(xps_tab)
  }

  invisible(NULL)
})


# export_cea_tables ----
# Generate cost-effectiveness (ICER / NMB) tables from the qalys and costs
# summaries. For each stratum, perspective (societal / healthcare) and QALY
# scale (EQ5D5L / HUI3) it computes, per Monte-Carlo iteration, the cumulative
# discounted incremental QALYs and costs versus the comparator scenario, then
# the ICER and the net monetary benefit (NMB) at each willingness-to-pay
# threshold, and quantiles those across iterations.
#
# Cost perspectives (cvd_* already aggregate chd + stroke):
#   societal   = cvd_total_costs  (+ all user *_costs columns)
#   healthcare = cvd_direct_costs (+ the user *_costs columns named in
#                custom_costs_in_healthcare)
#
# Discounting: PV = FV / (1 + rate/100)^max(0, year - discount_from_year),
# with separate rates for QALYs and costs. Actual (scaled_up) population only.
Simulation$set("private", "export_cea_tables", function(
    prbl,
    summaries_dir,
    tables_dir,
    comparator_scenario = "sc0",
    baseline_year = 2001L,
    wtp = c(5e6, 7.5e6, 1e7),
    qaly_discount_rate = 2,
    cost_discount_rate = 2,
    discount_from_year = NULL,
    custom_costs_in_healthcare = NULL,
    strata = NULL
) {
  if (self$design$sim_prm$logs) {
    message("Generating cost-effectiveness (ICER/NMB) tables...")
  }

  if (is.null(discount_from_year)) discount_from_year <- baseline_year

  qalys <- private$read_summary_dataset("qalys", "scaled_up")
  costs <- private$read_summary_dataset("costs", "scaled_up")
  if (is.null(qalys) || is.null(costs)) {
    if (self$design$sim_prm$logs) {
      message("  qalys or costs summary missing; skipping CEA tables")
    }
    return(invisible(NULL))
  }

  # Need at least one intervention scenario to compare against the comparator
  non_comparator <- setdiff(unique(qalys$scenario), comparator_scenario)
  if (length(non_comparator) == 0L) {
    if (self$design$sim_prm$logs) {
      message("  no intervention scenarios (only '", comparator_scenario,
              "' found); skipping CEA tables")
    }
    return(invisible(NULL))
  }

  # QALY scales actually present in the summary
  scales_avail <- intersect(c("EQ5D5L", "HUI3"), names(qalys))
  if (length(scales_avail) == 0L) {
    if (self$design$sim_prm$logs) {
      message("  no EQ5D5L/HUI3 columns in qalys summary; skipping CEA tables")
    }
    return(invisible(NULL))
  }

  # Identify built-in vs user-defined cost columns
  all_cost_cols <- grep("_costs$", names(costs), value = TRUE)
  builtin_cost_cols <- grep(
    "^(chd|stroke|cvd)_(direct|productivity|informal|indirect|total)_costs$",
    all_cost_cols, value = TRUE
  )
  custom_cost_cols <- setdiff(all_cost_cols, builtin_cost_cols)

  # Resolve which custom cost columns to add to the healthcare perspective.
  # `custom_costs_in_healthcare` accepts a character vector of (custom) cost
  # column names to include there, in addition to the always-present
  # cvd_direct_costs. For backward compatibility a logical is also honoured:
  # NULL/FALSE -> none (default), TRUE -> all user-defined custom cost columns.
  if (is.null(custom_costs_in_healthcare) ||
      isFALSE(custom_costs_in_healthcare)) {
    healthcare_custom_cols <- character(0)
  } else if (isTRUE(custom_costs_in_healthcare)) {
    healthcare_custom_cols <- custom_cost_cols
  } else {
    requested <- as.character(custom_costs_in_healthcare)
    healthcare_custom_cols <- intersect(requested, custom_cost_cols)
    unknown <- setdiff(requested, custom_cost_cols)
    if (length(unknown) > 0L && self$design$sim_prm$logs) {
      message("  custom_costs_in_healthcare: ignoring name(s) not matching a ",
              "user-defined cost column: ", paste(unknown, collapse = ", "))
    }
  }

  perspective_cols <- list(
    societal = c("cvd_total_costs", custom_cost_cols),
    healthcare = c("cvd_direct_costs", healthcare_custom_cols)
  )

  # WTP -> NMB column-name labels (plain integer form, e.g. 5000000)
  wtp_labels <- paste0(
    "NMB_at_wtp_",
    vapply(wtp, function(w) format(w, scientific = FALSE, trim = TRUE,
                                   big.mark = ""), character(1))
  )

  disc <- function(v, year, rate) {
    v / (1 + rate / 100)^pmax(0, year - discount_from_year)
  }

  for (s in strata) {
    x <- c("mc", "scenario", s) # s always contains "year"

    for (persp in names(perspective_cols)) {
      pcols <- intersect(perspective_cols[[persp]], names(costs))
      if (!any(grepl("^(cvd_total_costs|cvd_direct_costs)$", pcols))) {
        if (self$design$sim_prm$logs) {
          message("  ", persp, ": required cvd cost column missing; skipping")
        }
        next
      }

      # Aggregate (discounted) costs for this perspective, once per stratum
      cc <- copy(costs)
      cc[, .cost := Reduce(`+`, .SD), .SDcols = pcols]
      cc <- cc[, .(C = sum(.cost)), keyby = eval(x)]
      cc[, C := disc(C, year, cost_discount_rate)]

      for (scale in scales_avail) {
        # Aggregate (discounted) QALYs for this scale
        qq <- qalys[, .(Q = sum(get(scale))), keyby = eval(x)]
        qq[, Q := disc(Q, year, qaly_discount_rate)]

        d <- merge(qq, cc, by = x, all = TRUE)
        d[is.na(Q), Q := 0][is.na(C), C := 0]

        # Incremental vs comparator (intervention - comparator), from baseline
        cmp <- d[scenario == comparator_scenario & year >= baseline_year][
          , scenario := NULL]
        d <- d[scenario != comparator_scenario & year >= baseline_year]
        if (nrow(d) == 0L || nrow(cmp) == 0L) next
        d[cmp, on = setdiff(x, "scenario"), `:=`(dQ = Q - i.Q, dC = C - i.C)]
        d <- d[!is.na(dQ) & !is.na(dC)]
        if (nrow(d) == 0L) next

        # Cumulative over year within (mc, scenario, other strata)
        setkeyv(d, c(setdiff(x, "year"), "year"))
        d[, `:=`(dQALYs_cuml = cumsum(dQ), dCosts_cuml = cumsum(dC)),
          by = setdiff(x, "year")]

        # ICER and NMB at each WTP
        d[, ICER := fifelse(dQALYs_cuml == 0, NA_real_,
                            dCosts_cuml / dQALYs_cuml)]
        for (i in seq_along(wtp)) {
          set(d, NULL, wtp_labels[i], wtp[i] * d$dQALYs_cuml - d$dCosts_cuml)
        }

        metric_cols <- c("dCosts_cuml", "dQALYs_cuml", "ICER", wtp_labels)
        dm <- melt(d, id.vars = x, measure.vars = metric_cols,
                   variable.name = "type", value.name = "value")
        # Drop non-finite draws (e.g. ICER when dQALYs_cuml == 0) so the
        # quantile is taken over the finite Monte-Carlo iterations only.
        dm <- dm[is.finite(value)]
        if (nrow(dm) == 0L) next

        setkey(dm, "type")
        out <- dm[, safe_fquantile_byid(value, prbl, id = as.character(type),
                                        rounding = FALSE),
                  keyby = eval(setdiff(x, "mc"))]
        setnames(out, c(setdiff(x, "mc"), "type",
                        scales::percent(prbl, prefix = "value_")))
        setkeyv(out, c("type", setdiff(x, "mc")))
        setcolorder(out, setdiff(x, "mc"))

        suffix <- paste(setdiff(x, c("mc", "scenario")), collapse = "-")
        fwrite(out, file.path(
          tables_dir,
          paste0("cost-effectiveness by ", suffix,
                 " (", persp, "-", scale, ") (not standardised).csv")
        ))
        rm(d, dm, out, qq)
      }
      rm(cc)
    }
  }

  rm(qalys, costs)
  invisible(NULL)
})

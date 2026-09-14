# ---------------------------------------------------------------------------
# Built-in cost columns: the single source of truth
# ---------------------------------------------------------------------------
# Two places used to state the built-in cost column set independently: the 15
# hand-written SUM() lines in `export_costs_summaries()` (Simulation_class.R,
# the block starting at the "Define cost metrics for SELECT statement" comment)
# and a regex in `export_cea_tables()` (Simulation_class_tables.R). When those
# two disagree, a built-in column falls through `setdiff()` into
# `custom_cost_cols` and is then added to the SOCIETAL perspective on top of
# `cvd_total_costs`, which already contains it - a silent double count, with
# nothing anywhere that could notice. Everything in this file exists to make
# that impossible, or where it cannot be made impossible, loud.

#' Cost primitives the cost machinery computes per disease
#'
#' `calc_costs()` computes four per-person primitives for a costed disease and
#' derives everything else from them by addition. These names are internal; they
#' are never column names in the costs summary.
#'
#' @noRd
.cost_primitives <- c("prvl_prdv", "mrtl_prdv", "informal", "direct")

#' Published cost components, each defined as the primitives it sums
#'
#' The five components that reach the costs parquet, in the order they appear
#' there. A component name is NOT a primitive name: `productivity` is never a
#' primitive, it is `prvl_prdv + mrtl_prdv`; `indirect` adds `informal`; `total`
#' adds `direct`. This mirrors the final SELECT of `calc_costs()` exactly.
#'
#' The `<disease>_prvl_prdv_costs` / `<disease>_mrtl_prdv_costs` intermediates
#' exist in the cost view and end in `_costs`, but are deliberately NOT
#' components: they are never exported, and if they ever were they would be
#' double counted inside `<disease>_productivity_costs`.
#'
#' This list also fixes the emitted column ORDER, which is component-major and
#' disease-minor. That order is the on-disk column order of
#' `summaries/costs_esp` and `summaries/costs_scaled_up`, and it becomes the
#' `costs_type` factor level order after the melt in `tbl_smmrs_core()`, so it
#' governs row order in every `costs_*.csv` and `net_costs_*.csv` too.
#'
#' @noRd
.cost_components <- list(
  direct       = "direct",
  productivity = c("prvl_prdv", "mrtl_prdv"),
  informal     = "informal",
  indirect     = c("prvl_prdv", "mrtl_prdv", "informal"),
  total        = c("prvl_prdv", "mrtl_prdv", "informal", "direct")
)

#' Which diseases the cost machinery costs, and with what
#'
#' The single declaration of what `private$calc_costs()` implements. It has to
#' be declared, because the answer exists nowhere else: `inputs/sim_design.yaml`
#' carries no cost configuration of any kind, `Disease_class.R` has no cost
#' fields, and the whole cost machinery is hand-written SQL inside `calc_costs()`
#' that names `chd_dgns` / `stroke_dgns` and the mortality codes inline and
#' embeds its parameters as literal SQL `VALUES` tables.
#'
#' Each entry declares:
#'
#' * `primitives` - the elements of `.cost_primitives` `calc_costs()` computes
#'   for that disease. The available COMPONENTS are derived from this, not
#'   listed: see `cost_disease_components()`.
#' * `aggregate_of` - `NULL` for a disease with its own cost parameters, or the
#'   diseases it is the arithmetic sum of. `cvd` is the only such disease today.
#'   It has no `cvd_dgns` column, no mortality code and no cost parameters;
#'   every `cvd_*` column is literally `chd_* + stroke_*`. It is therefore
#'   available whenever `chd` and `stroke` are - NOT when `cvd` itself is
#'   declared. That matters: `cvd` in the design is a synthetic aggregate
#'   (incidence type 0, diagnosis type 0, mortality `~`), so any predicate built
#'   on disease metadata would drop it, and `cvd_total_costs` /
#'   `cvd_direct_costs` are the two columns both CEA perspectives are anchored
#'   on.
#'
#' This declaration is not taken on trust: `export_costs_summaries()` calls
#' `assert_cost_view_matches_registry()` against the columns DuckDB actually
#' built, every run, and stops if the two have drifted apart in either
#' direction. Adding a costed disease here is therefore the LAST step - make
#' `calc_costs()` emit the columns first.
#'
#' @noRd
.cost_registry <- list(
  chd = list(
    primitives   = c("prvl_prdv", "mrtl_prdv", "informal", "direct"),
    aggregate_of = NULL
  ),
  stroke = list(
    primitives   = c("prvl_prdv", "mrtl_prdv", "informal", "direct"),
    aggregate_of = NULL
  ),
  cvd = list(
    primitives   = character(0),
    aggregate_of = c("chd", "stroke")
  )
)


# cost_components ----
#' Names of the published cost components
#' @return Character vector, in exported-column order.
#' @noRd
cost_components <- function() names(.cost_components)


# cost_disease_requires ----
#' Diseases that must be declared in the run for a costed disease's columns
#'
#' A disease with its own cost parameters requires itself. An aggregate requires
#' the diseases it sums, and NOT itself, because `calc_costs()` builds its
#' columns out of theirs.
#'
#' @param disease Single disease name.
#' @param registry Cost registry. Overridable for testing only.
#' @return Character vector; `character(0)` for an unregistered disease.
#' @noRd
cost_disease_requires <- function(disease, registry = .cost_registry) {
  entry <- registry[[disease]]
  if (is.null(entry)) return(character(0L))
  if (length(entry[["aggregate_of"]]) > 0L) entry[["aggregate_of"]] else disease
}


# cost_disease_components ----
#' Cost components available for one disease
#'
#' Computed from the registry, never listed: a disease has a component when the
#' cost machinery computes EVERY primitive that component sums (a partial sum
#' would be a wrong number, not a smaller one), and an aggregate has a component
#' when all of its constituents do.
#'
#' @param disease Single disease name.
#' @param registry Cost registry. Overridable for testing only.
#' @return Character vector of component names in `cost_components()` order;
#'   `character(0)` for an unregistered disease.
#' @noRd
cost_disease_components <- function(disease, registry = .cost_registry) {
  entry <- registry[[disease]]
  if (is.null(entry)) return(character(0L))
  if (length(entry[["aggregate_of"]]) > 0L) {
    per_member <- lapply(
      entry[["aggregate_of"]], cost_disease_components, registry = registry
    )
    if (length(per_member) == 0L) return(character(0L))
    return(Reduce(intersect, per_member))
  }
  have <- entry[["primitives"]]
  names(Filter(function(prims) all(prims %in% have), .cost_components))
}


# costed_diseases ----
#' Diseases of this model run that have at least one cost component available
#'
#' This is the auto-population rule for the `diseases` argument of
#' `builtin_cost_cols()`. A registered disease is kept when BOTH hold:
#'
#' 1. every disease its cost columns are built from is declared in this run
#'    (`cost_disease_requires()` - itself for a primary costed disease, its
#'    constituents for an aggregate); and
#' 2. at least one cost component is available for it
#'    (`cost_disease_components()`, derived from the primitives the cost
#'    machinery computes).
#'
#' So the set is a function of the run's own disease list: it shrinks when a
#' design declares fewer diseases, and a design disease the cost machinery knows
#' nothing about contributes nothing rather than generating SQL for a column
#' that does not exist.
#'
#' The result is in REGISTRY order, never in the order of `declared`.
#' `Design$new()` topologically re-sorts the disease list, so `declared` order is
#' not stable across designs while the exported column order must be.
#'
#' @param declared Character vector of disease names declared in this run,
#'   normally `names(self$diseases)`.
#' @param registry Cost registry. Overridable for testing only.
#' @return Character vector of costed disease names, possibly empty.
#' @noRd
costed_diseases <- function(declared, registry = .cost_registry) {
  declared <- unique(as.character(declared))
  keep <- vapply(names(registry), function(d) {
    req <- cost_disease_requires(d, registry)
    length(req) > 0L &&
      all(req %in% declared) &&
      length(cost_disease_components(d, registry)) > 0L
  }, logical(1L))
  names(registry)[keep]
}


# builtin_cost_cols ----
#' Built-in cost column names
#'
#' The only statement of the built-in cost column set in the package.
#' `export_costs_summaries()` generates the SQL that creates these columns from
#' it, and `export_cea_tables()` classifies built-in vs user-defined with it, so
#' the two cannot drift apart.
#'
#' Emitted component-major, disease-minor - `chd_direct_costs`,
#' `stroke_direct_costs`, `cvd_direct_costs`, `chd_productivity_costs`, ... -
#' reproducing the order of the hand-written SQL it replaces, so the on-disk
#' column order is unchanged.
#'
#' Coverage is per disease, not a blind cross-product: a `(disease, component)`
#' pair is emitted only when `cost_disease_components()` says the component is
#' available, so a disease costed for only some components can never make the
#' writer emit SQL for a column `calc_costs()` never built.
#'
#' An unregistered disease name is an error, not a silent empty result: pass the
#' run's diseases through `costed_diseases()` first.
#'
#' @param diseases Costed disease names, normally
#'   `costed_diseases(names(self$diseases))`.
#' @param components Components to emit; defaults to all of
#'   `cost_components()`. Narrow it to address one column, e.g.
#'   `builtin_cost_cols("cvd", "total")`.
#' @param registry Cost registry. Overridable for testing only.
#' @return Character vector of column names, possibly empty.
#' @noRd
builtin_cost_cols <- function(diseases,
                              components = cost_components(),
                              registry = .cost_registry) {
  diseases   <- unique(as.character(diseases))
  components <- unique(as.character(components))

  unknown_d <- setdiff(diseases, names(registry))
  if (length(unknown_d) > 0L) {
    stop("builtin_cost_cols(): no cost components are registered for '",
         paste(unknown_d, collapse = "', '"),
         "'. Pass the run's diseases through costed_diseases() first, or add ",
         "them to .cost_registry once calc_costs() emits their columns.",
         call. = FALSE)
  }
  unknown_c <- setdiff(components, cost_components())
  if (length(unknown_c) > 0L) {
    stop("builtin_cost_cols(): unknown cost component(s) '",
         paste(unknown_c, collapse = "', '"), "'. Known components are: ",
         paste(cost_components(), collapse = ", "), ".", call. = FALSE)
  }
  if (length(diseases) == 0L || length(components) == 0L) {
    return(character(0L))
  }

  out <- unlist(lapply(components, function(cmp) {
    avail <- vapply(
      diseases,
      function(d) cmp %in% cost_disease_components(d, registry),
      logical(1L)
    )
    paste0(diseases[avail], "_", cmp, "_costs")
  }), use.names = FALSE)
  if (is.null(out)) character(0L) else out
}


# builtin_cost_col_regex ----
#' Regex matching the built-in cost column names of given diseases
#'
#' Anchored to the disease names it is given, so it cannot swallow a
#' user-defined column such as `statin_direct_costs`. With the default (every
#' registered disease) it is string-identical to the hard-coded regex it
#' replaces in `export_cea_tables()`, and it widens automatically when a disease
#' is registered.
#'
#' @param diseases Disease names to anchor on.
#' @return A length-1 regular expression; `"^$"` when `diseases` is empty.
#' @noRd
builtin_cost_col_regex <- function(diseases = names(.cost_registry)) {
  diseases <- unique(as.character(diseases))
  if (length(diseases) == 0L) return("^$")
  sprintf("^(%s)_(%s)_costs$",
          paste(diseases, collapse = "|"),
          paste(cost_components(), collapse = "|"))
}


# cost_view_builtin_shape ----
#' Regex matching anything shaped like a built-in cost column, for ANY disease
#'
#' Deliberately wider than `builtin_cost_col_regex()`. Use it ONLY against the
#' columns of the cost view built by `calc_costs()`, to detect a column the
#' registry does not claim - never against summary columns, where it would also
#' match a legitimate user-defined name such as `statin_direct_costs`. The
#' disease token forbids `_`, so the never-exported `*_prvl_prdv_costs` /
#' `*_mrtl_prdv_costs` intermediates correctly do not match.
#'
#' @return A length-1 regular expression.
#' @noRd
cost_view_builtin_shape <- function() {
  sprintf("^[A-Za-z0-9]+_(%s)_costs$", paste(cost_components(), collapse = "|"))
}


# assert_cost_view_matches_registry ----
#' Stop unless the cost view and the registry agree
#'
#' Called by `export_costs_summaries()` against the columns DuckDB actually
#' built, before any cost SQL runs. This is what makes "has at least one cost
#' component available" a checked property of the run rather than an assertion
#' in a comment. Both directions are fatal, because both are silent corruption
#' downstream:
#'
#' * a claimed column the view lacks would otherwise become a DuckDB Binder
#'   Error deep inside the per-scenario loop, after the expensive simulation has
#'   already run;
#' * a view column shaped like a built-in that the registry does not claim would
#'   reach the parquet and then be classified user-defined by
#'   `export_cea_tables()` and added to the societal perspective on top of
#'   `cvd_total_costs` - double counted, silently.
#'
#' @param view_cols Columns of a per-scenario cost view, from `dbListFields()`.
#' @param expected Columns `builtin_cost_cols()` generates for this run.
#' @param context Short string naming the caller, used in the message.
#' @return `TRUE`, invisibly. Errors otherwise.
#' @noRd
assert_cost_view_matches_registry <- function(view_cols, expected, context) {
  missing_cols <- setdiff(expected, view_cols)
  if (length(missing_cols) > 0L) {
    stop(context, ": the cost registry claims ",
         paste(missing_cols, collapse = ", "),
         " but calc_costs() did not produce ",
         if (length(missing_cols) == 1L) "it" else "them",
         ". .cost_registry (R/cost_columns.R) and calc_costs() have diverged; ",
         "fix one of them.", call. = FALSE)
  }
  unclaimed <- setdiff(
    grep(cost_view_builtin_shape(), view_cols, value = TRUE),
    expected
  )
  if (length(unclaimed) > 0L) {
    stop(context, ": calc_costs() produced built-in-shaped cost column(s) ",
         paste(unclaimed, collapse = ", "),
         " that .cost_registry does not claim for this run. Exporting them ",
         "would let export_cea_tables() classify them as user-defined and ",
         "double count them into the societal perspective. Register them in ",
         "R/cost_columns.R, or stop emitting them.", call. = FALSE)
  }
  invisible(TRUE)
}

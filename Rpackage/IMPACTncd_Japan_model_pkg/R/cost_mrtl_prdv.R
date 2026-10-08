# Mortality productivity cost: annual lost-output parameter ---------------
#
# A CHD or stroke death costs the economy the output the deceased would have
# produced had they lived. calc_costs() books that loss as an ANNUAL stream:
# one year of lost output per calendar year, from the year of death until the
# deceased would have turned `mrtl_prdv_end_age` (or the simulation horizon,
# whichever comes first). Each year then lands in its own calendar year and is
# discounted there by export_tables(), like every other cost flow.
#
# The national totals the model is calibrated to are NOT annual flows. They are
# the 2014 mortality costs (MtC) of heart disease (2,257bn JPY) and
# cerebrovascular disease (1,352bn JPY) from
#   Matsumoto K, Hanaoka S, Wu Y, Hasegawa T. Comprehensive Cost of Illness of
#   Three Major Diseases in Japan. J Stroke Cerebrovasc Dis 2017;26:1934-40.
#   doi:10.1016/j.jstrokecerebrovasdis.2017.06.022
# which, following the authors' C-COI method, are human-capital LIFETIME
# values: for each death, the income (paid and unpaid work, by sex and 5-year
# age group) from the year of death to life expectancy, discounted to present
# value at 3% (the group's rate in Haga 2013, Matsumoto 2015 and Gochi 2018;
# their 2023 update moved to 2%).
#
# mrtl_prdv_annual_cost_param() inverts that definition. The annual value at
# age a and sex s is k * e(a, s), where e is the employee-count profile already
# used to apportion the totals, and the single scalar k is solved so that the
# source-defined lifetime values of the calibration-year deaths reproduce the
# total:
#
#   total * inflation = k * sum_g D_g * LV_g,
#   LV_g  = mean over the single ages a in g of
#           sum_{t >= 0} e(a + t, s) / (1 + r)^t
#
# The calibration uses the SOURCE's definition (every age with e > 0, to life
# expectancy, at the source's discount rate). The model's own rules - stop at
# `mrtl_prdv_end_age`, stop at the horizon, discount at the export rates - are
# applied afterwards in calc_costs(). Because e(a, s) = 0 from age 75 while
# life expectancy at any age below 75 extends beyond 75, summing to the end of
# the age range is the same as summing to life expectancy. (The source also
# values unpaid work beyond 75; the employee profile cannot represent it, so
# that share is carried by the under-75 values instead.)
#
# calc_costs() uses the same span: mrtl_prdv_end_age = 75, where the employee
# profile ends, so a stream covers exactly the years the source valued. Hence a
# year's deaths, their streams discounted at the source rate to the year of
# death, reproduce total * inflation - up to the uniform-within-agegrp
# assumption in LV_g (actual deaths sit older in each group; about -2%) and any
# truncation of the streams at the simulation horizon. The undiscounted
# streams are larger than the totals, since the totals are present values.


# mrtl_prdv_annual_cost_param ----
# Arguments:
#   employees  data.table(agegrp, sex, employees): annual labour-value profile
#              by 5-year age group ("30-34", ...) and sex.
#   deaths     data.table(agegrp, sex, V1): calibration-year deaths.
#   total_cost Lifetime mortality cost of those deaths in the source (JPY).
#   inflation_factor   Price adjustment applied to `total_cost`.
#   source_discount_rate  Rate (proportion) the source discounted at.
# Returns data.table(agegrp, sex, cost_param): the annual lost output (JPY) of
# one death, per year lost, at an attained age in `agegrp`. All zero when the
# calibration denominator is zero or missing, matching the NULLIF/COALESCE
# behaviour of the other cost parameters in calc_costs().
mrtl_prdv_annual_cost_param <- function(employees,
                                        deaths,
                                        total_cost,
                                        inflation_factor,
                                        source_discount_rate = 0.03) {
  emp <- as.data.table(employees)[, .(
    agegrp = as.character(agegrp),
    sex = as.character(sex),
    employees = as.numeric(employees)
  )]
  dth <- as.data.table(deaths)[, .(
    agegrp = as.character(agegrp),
    sex = as.character(sex),
    V1 = as.numeric(V1)
  )]

  # Single years of age within each group carry the group's profile value
  emp[, `:=`(
    age_lo = as.integer(sub("-.*$", "", agegrp)),
    age_hi = as.integer(sub("^.*-", "", agegrp))
  )]
  single <- emp[, .(age = seq.int(age_lo, age_hi)), by = .(agegrp, sex, employees)]
  setkey(single, sex, age)

  # Lifetime value from each age on, by backward recursion:
  # LV(a) = e(a) + LV(a + 1) / (1 + r)
  dscnt <- 1 / (1 + source_discount_rate)
  single[, lv := {
    out <- numeric(.N)
    acc <- 0
    for (i in rev(seq_len(.N))) {
      acc <- employees[i] + dscnt * acc
      out[i] <- acc
    }
    out
  }, by = sex]

  lv_grp <- single[, .(lv = mean(lv)), keyby = .(agegrp, sex)]
  lv_grp[dth, on = c("agegrp", "sex"), V1 := i.V1]
  denom <- lv_grp[, sum(lv * V1, na.rm = TRUE)]

  k <- if (is.finite(denom) && denom > 0) {
    total_cost * inflation_factor / denom
  } else {
    0
  }

  emp[, .(agegrp, sex, cost_param = k * employees)]
}

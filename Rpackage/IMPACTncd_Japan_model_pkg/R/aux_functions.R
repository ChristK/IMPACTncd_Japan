## IMPACTncdJapan is an implementation of the IMPACTncd framework, developed by Chris
## Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
## Guzman-Castillo, Amandine Robert, and Piotr Bandosz. This work has been
## funded by NIHR  HTA Project: 16/165/01 - IMPACTncdJapan: Health Outcomes
## Research Simulation Environment.  The views expressed are those of the
## authors and not necessarily those of the NHS, the NIHR or the Department of
## Health.
##
## Copyright (C) 2018-2020 University of Liverpool, Chris Kypridemos
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


#' @export
proportional_reduction <- function(exposure, change, weights, penalty = 1){
	c <- (weighted.mean(exposure, weights) - (weighted.mean(exposure, weights) + change)) / weighted.mean(exposure^penalty, weights)
	return(exposure - c * exposure^penalty)
		# exposure: eg. SBP_curr_xps, 
		# chage: -5 mmHg, 
		# penalty: determines how sharply the penalty increases with x. penalty
		# When penalty = 1, the SBP is simply "reduced at the same rate in proportion to the original SBP,
		# When penalty > 1, the distribution is reduced “more” for larger SBP values and “less” for smaller values.
}




# The user provides:
#   •	threshold: A numeric value for exposure.
# •	target_prop: The target proportion (0-1) of values exceeding the threshold after applying reduction.
# •	weights: The weights associated with the exposures.
# •	penalty: A penalty parameter, defaulted to 1 (or any desired numeric value).
# 2.	The function calculates the necessary change by using an optimization method (e.g., uniroot) to find the reduction that achieves the desired proportion.
# 3.	After determining the optimal change, apply it to exposures using the proportional logic you previously defined.

#' @export
proportional_reduction_threshold <- function(exposure, threshold, target_prop, weights, penalty = 1) {

  # Function to compute proportion above threshold given a specific change
  calc_prop_above <- function(change){
    adj_exposure <- proportional_reduction(exposure, change, weights, penalty)
    sum(weights[adj_exposure > threshold]) / sum(weights)
  }

  # Find the change required to achieve the target proportion above threshold
  optimal_change <- uniroot(
    f = function(change) calc_prop_above(change) - target_prop,
    interval = c(-max(exposure)*10, max(exposure)*10),
    extendInt = "yes", tol = .Machine$double.eps, maxiter = 1000, trace = 0
  )$root

  # Apply the optimal change
  proportional_reduction(exposure, optimal_change, weights, penalty)
}







#' @export
inflate <- function(x, percentage_rate, year, baseline_year) {
  x * (1 + percentage_rate / 100) ^ (year - baseline_year)
}
# inflate(1000, 3, 2011:2020, 2013)

#' @export
deflate <- function(x, percentage_rate, year, baseline_year) {
  x * (1 - percentage_rate / 100) ^ (year - baseline_year)
}



# helper func for gamlss::fitDistr
#' @export
distr_best_fit <-
  function(dt,
           var,
           wt,
           distr_family,
           distr_extra = NULL,
           pred = FALSE,
           seed = NULL,
           trace = TRUE) {
    if (pred) {
      print("Selection based on minimum prediction global deviance")
      if (!is.null(seed))
        set.seed(seed)
      lns <- sample(nrow(dt), round(nrow(dt) * 0.8))
      dt_trn   <- dt[lns,] # train dataset
      dt_crv   <- dt[!lns,]  # cross-validation dataset
      marg_distr <- gamlss::fitDistPred(
        dt_trn[[var]],
        type = distr_family,
        weights = dt_trn[[wt]],
        extra = distr_extra,
        try.gamlss = TRUE,
        trace = trace,
        newdata = dt_crv[[var]]
      )
    } else {
      print("Selection based on BIC")
      marg_distr <-
        gamlss::fitDist(
          dt[[var]],
          log(nrow(dt)),
          type = distr_family,
          weights = dt[[wt]],
          extra = distr_extra,
          try.gamlss = TRUE,
          trace = trace
        )
    }
    marg_distr
  }

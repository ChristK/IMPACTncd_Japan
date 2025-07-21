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


`:=` = function(...)
  NULL # due to NSE notes in R CMD check

.onUnload <- function(libpath) {
  library.dynam.unload("IMPACTncdJapan", libpath)
}

# Ensures that when fwrite appends file colnames of file to be written, match
# those already in the file
#' @export
fwrite_safe <- function(x,
                        file,
                        append = TRUE,
                        #threat_safe = append,
                        ...) {
  if (append) {
    if (file.exists(file)) {
      col_names_disk <- names(fread(file, nrows = 0))
      col_names_file <- names(x)
      col_names <- outersect(col_names_disk, col_names_file)
      if (length(col_names) > 0)
        x[, (col_names) := NA]
      setcolorder(x, col_names_disk)
    }
  }

#  # create threat-safe mechanism
#  flock <- paste0(file, ".lock")
#
#
#  while (file.exists(flock)) {
#    Sys.sleep(runif(1))
#  }
#
#    file.create(flock)
     fwrite(x, file, append, ...)
#    on.exit(if (file.exists(flock)) file.remove(flock))

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







# Given a correlation matrix (Pearson), produces a matrix of
# correlated uniforms

#' @export
generate_corr_unifs <- function(n, M) {
  # generate normals, check correlations
  # from http://comisef.wikidot.com/tutorial:correlateduniformvariates
  stopifnot(is.matrix(M))
  # Check that matrix is semi-positive definite
  # NOTE next line crashes frequently!! see
  # https://stat.ethz.ch/pipermail/r-help/2006-March/102703.html for a
  # workaround and https://stat.ethz.ch/pipermail/r-help/2006-March/102647.html
  # for some explanation
  # stopifnot(min(eigen(M, only.values = TRUE)$values) >= 0)

  M_original <- M


  # adjust correlations for uniforms
  for (i in seq_len(dim(M)[[1L]])) {
    for (j in seq_len(dim(M)[[2L]])) {
      if (i != j) {
        M[i, j] <- 2 * sin(pi * M[i, j] / 6)
        M[j, i] <- 2 * sin(pi * M[j, i] / 6)
      }
    }
  }

  X <- matrix(dqrnorm(n * dim(M)[[2]]), n)
  colnames(X) <- colnames(M)

  # induce correlation, check correlations
  Y <- pnorm(X %*% chol(M))

  # message(paste0("Mean square error is: ", signif(sum((cor(Y) - M_original) ^
  # 2), 3)))
  return(Y)
}


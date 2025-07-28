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

#' IMPACTncdJapan: A package with functions for the IMPACTncdJapan model.
#'
#' Description of your package.
#
# @section Foo functions:
# The foo functions ...
#
#' @docType package
#' @author Chris Kypridemos
#' @import Rcpp R6 data.table CKutils
#' @importFrom R6 R6Class
#' @importFrom Rcpp evalCpp
#' @importFrom fst read_fst
#' @importFrom dqrng dqrunif dqsample
#' @importFrom mc2d qpert
#' @importFrom cowplot ggsave2
#' @importFrom gamlss fitDist fitDistPred predictAll
#' @importFrom stats as.formula na.omit qunif var weighted.mean loess predict qbinom
#' @importFrom stats quantile rbinom rpois runif sigma
#' @importFrom utils tail
#' @importFrom digest digest2int
#' @importFrom yaml read_yaml write_yaml
#' @importFrom foreach foreach
#' @importFrom parallelly makeClusterPSOCK
#' @importFrom parallel parLapplyLB
#' @importFrom igraph make_graph is_dag V neighbors all_simple_paths topo_sort
#' @useDynLib IMPACTncdJapan
#' @name IMPACTncdJapan

`:=` = function(...)
  NULL # due to NSE notes in R CMD check

.onUnload <- function(libpath) {
  library.dynam.unload("IMPACTncdJapan", libpath)
}


# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".", ".I", ".N", ".SD"), utils::packageName())

NULL

# cd .\Rpackage\IMPACTncd_Japan_model_pkg\ ; Rscript -e "roxygen2::roxygenise(); tinytest::build_install_test()"
# cd .\Rpackage\IMPACTncd_Japan_model_pkg\ ; Rscript -e "roxygen2::roxygenise()"
# cd .\Rpackage\IMPACTncd_Japan_model_pkg\ ; R CMD INSTALL --preclean .
# cd .\Rpackage\IMPACTncd_Japan_model_pkg\ ; R CMD check --as-cran .
# cd .\Rpackage\IMPACTncd_Japan_model_pkg\ ; Rscript -e "tinytest::test_package(\"CKutils\")"
## IMPACTncd_Japan is an implementation of the IMPACTncd framework, developed by Chris
## Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
## Guzman-Castillo, Amandine Robert, and Piotr Bandosz.
##
## Copyright (C) 2018-2020 University of Liverpool, Chris Kypridemos
##
## IMPACTncd_Japan is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version. This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details. You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/> or write
## to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301 USA.

# file.remove(list.files("./output/", full.names = TRUE, recursive = TRUE))
# file.remove(list.files("./Rpackage/IMPACTncd_Japan_model_pkg/src", full.names = TRUE, recursive = TRUE, pattern = "\\.o$|\\.so$"))

# If segfault from C stack overflow see
# https://github.com/Rdatatable/data.table/issues/1967

if (!file.exists("/.dockerenv")) {
  repos <- getOption("repos")
  if (is.null(repos) || repos["CRAN"] == "@CRAN@") {
    chooseCRANmirror(ind = 1)
  }
}

cat("Initialising IMPACTncd_Japan model...\n\n")

# Ensure 'remotes' is installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Ensure 'CKutils' is installed from GitHub if missing
if (!requireNamespace("CKutils", quietly = TRUE)) {
  remotes::install_github("ChristK/CKutils", upgrade = "ask", force = TRUE)
}
library(CKutils)

# Set development mode flag
dev_mode <- TRUE # Set to FALSE for production

# Environment-specific options
options(rgl.useNULL = TRUE) # suppress error by demography in rstudio server
if (dev_mode) {
  options(future.fork.enable = TRUE) # enable for development only
  options(future.globals.maxSize = +Inf)
  options(future.rng.onMisuse = "ignore") # Remove false warning
}
options(datatable.verbose = FALSE)
options(datatable.showProgress = FALSE)

dependencies(yaml::read_yaml("./dependencies.yaml"), update = FALSE)


installLocalPackageIfChanged(
  pkg_path = "./Rpackage/IMPACTncd_Japan_model_pkg/",
  snapshot_path = "./Rpackage/.IMPACTncd_Japan_model_pkg_snapshot.rds"
)
library(IMPACTncdJapan)


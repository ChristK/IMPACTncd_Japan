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

# Define and ensure the user library path exists and is writable
# This is crucial when running in environments (like Docker) where the default
# system library might not be writable by the current user.
user_lib <- Sys.getenv("R_LIBS_USER")
if (user_lib == "") {
  # Provide a default user library path if R_LIBS_USER is not set
  # Format: ~/R/<platform>/<major>.<minor>
  user_lib <- file.path(
    Sys.getenv("HOME"), "R", paste0(R.version$platform, "-library"),
    paste0(R.version$major, ".", substr(R.version$minor, 1, 1))
  )
}

# Create the directory if it doesn't exist
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}

# Check if the user library is writable
if (file.access(user_lib, mode = 2) != 0) { # mode = 2 checks for write permission
  stop(paste(
    "Error: User library path is not writable:", user_lib,
    "\nPlease check permissions or set the R_LIBS_USER environment variable to a writable path."
  ))
}

# Add the user library to the library paths if not already present
# Prepending ensures it's the default location for installations
if (!user_lib %in% .libPaths()) {
  .libPaths(c(user_lib, .libPaths()))
}

cat("Using library path:", user_lib, "\n")

# --- End: User Library Path Configuration ---

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

# Install missing packages listed in r-packages.txt
# Assumes the working directory is the project root
pkg_list_file <- "docker_setup/r-packages.txt"
if (file.exists(pkg_list_file)) {
  pkg_list <- readLines(pkg_list_file, warn = FALSE)
  pkg_list <- trimws(pkg_list)
  # Filter out empty lines and comments
  pkg_list <- pkg_list[nzchar(pkg_list) & !grepl("^#", pkg_list)]
  if (length(pkg_list) > 0) {
    # update = FALSE prevents updating already installed packages
    CKutils::dependencies(pkg_list, update = FALSE)
  }
  rm(pkg_list, pkg_list_file) # Clean up
} else {
  warning("r-packages.txt not found at: ", pkg_list_file)
}

# Install the local R package if its source code has changed
# Uses a snapshot file to track changes
# Assumes the working directory is the project root
installLocalPackageIfChanged(
  pkg_path = "./Rpackage/IMPACTncd_Japan_model_pkg/",
  snapshot_path = "./Rpackage/.IMPACTncd_Japan_model_pkg_snapshot.rds"
)

library(IMPACTncdJapan)

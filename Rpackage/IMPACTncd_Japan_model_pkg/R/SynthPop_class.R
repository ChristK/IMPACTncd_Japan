## IMPACTncd_Japan is an implementation of the IMPACTncd framework, developed by
## Chris Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
## Guzman-Castillo, Amandine Robert, and Piotr Bandosz.
##
## Copyright (C) 2018-2020 University of Liverpool, Chris Kypridemos
##
## IMPACTncd_Japan is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the Free
## Software Foundation; either version 3 of the License, or (at your option) any
## later version. This program is distributed in the hope that it will be
## useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
## Public License for more details. You should have received a copy of the GNU
## General Public License along with this program; if not, see
## <http://www.gnu.org/licenses/> or write to the Free Software Foundation,
## Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.



# From
# https://stackoverflow.com/questions/33424233/how-do-i-tell-an-r6-class-what-to-do-with-square-brackets
# Allows data.table syntax to the R6class object directly. Assumes it has a
# field 'pop' that is a data.table
#' @export
`[.SynthPop` <- function(x, ...) x$pop[...]

#' R6 Class representing a synthetic population
#'
#' @description
#' A synthpop has a `pop` field that contains the life course of simulants in a
#' `data.table`.
#'
#' @details
#' To be completed...
#'
#' @export
SynthPop <-
  R6::R6Class(
    classname = "SynthPop",

    # public ------------------------------------------------------------------
    public = list(
      #' @field mc The Monte Carlo iteration of the synthetic population fragment. Every
      #'   integer generates a unique synthetic population fragment.
      mc = NA,

      #' @field mc_aggr The Monte Carlo iteration of the synthetic population to
      #'   be used when multiple synthetic population fragments getting
      #'   aggregated. For instance if the synthpop consists of 2 fragments,
      #'   mc_aggr will be the same for both, but mc will differ. It ensures
      #'   correct seeds for the RNGs during the simulation for the RRs and the
      #'   lags.
      mc_aggr = NA,

      #' @field metadata Metadata of the synthpop.
      metadata = NA,

      #' @field pop The data.table that contains the life-course of simulants.
      #'   If the file exists, it is loaded from disk. If it doesn't, it is
      #'   first generated, then saved to disk, and then loaded from disk.
      pop = NA,

      # initialize ----
      #' @description Create a new SynthPop object.
      #' If a synthpop file in \code{\link[fst]{fst-package}} format already
      #' exists, then the synthpop is loaded from there. Otherwise it is
      #' generated from scratch and then saved as `filename` in
      #' \code{\link[fst]{fst-package}} format. Two additional files are saved
      #' for each 'synthpop'. A metadata file, and an index file.
      #' @param mc_ The Monte Carlo iteration of the synthetic population. Each
      #'   integer generates a unique synthetic population. If `mc = 0` an
      #'   object with an empty synthpop is initiated.
      #' @param design_ A \code{\link[IMPACTncdJapan]{Design}} object.
      #' @param synthpop_dir_ The directory where 'SynthPop' objects are stored.
      #'   The synthpop file in \code{\link[fst]{fst-package}} format. If
      #'   `filename` already exists, then the synthpop is loaded from there.
      #'   Otherwise it is generated from scratch and then saved as `filename`
      #'   in \code{\link[fst]{fst-package}} format. Two additional files are
      #'   saved for each 'synthpop'. A metadata file, and an index file.
      #' @return A new `SynthPop` object.
      #' @examples
      #' design <- Design$new("./validation/design_for_trends_validation.yaml")
      #' POP$write_synthpop(1:6, design)
      #' POP <- SynthPop$new(4L, design)
      #' POP$print()
      #' POP$count_synthpop()
      #'
      #' POP$delete_synthpop(1L)
      #' POP$delete_synthpop(5:6)
      #' POP$get_filename()
      initialize = function(mc_, design_) {
        stopifnot(length(mc_) == 1L, is.numeric(mc_), ceiling(mc_) >= 0L)
        stopifnot("Design" %in% class(design_))

        mc_ <- as.integer(ceiling(mc_))
        # Create synthpop_dir if it doesn't exists
        # NOTE code below is duplicated in Simulation class. This is intentional
        if (!dir.exists(design_$sim_prm$synthpop_dir)) {
          dir.create(design_$sim_prm$synthpop_dir, recursive = TRUE)
          message(paste0(
            "Folder ", design_$sim_prm$synthpop_dir,
            " was created"
          ))
        }

        # get unique lsoas
        # lsoas <- private$get_unique_LSOAs(design_)

        private$checksum <- private$gen_checksum(design_)

        self$mc <- mc_
        self$mc_aggr <-
          as.integer(ceiling(mc_ / design_$sim_prm$num_chunks))

        private$design <- design_
        private$synthpop_dir <- design_$sim_prm$synthpop_dir

        if (mc_ > 0) {
          # Logic to reuse a synthpop with larger age range if it exists (an expansion could be used for sim horizon)) (WIP)
          #   if (design_$sim_prm$ageH == 99L) {
          #     private$filename <- private$gen_synthpop_filename(mc_, private$checksum, design_)
          #   } else {
          #     for (age_ in design_$sim_prm$ageH:99)
          #     original_ageH <- design_$sim_prm$ageH
          #     design_$sim_prm$ageH <- age_
          #     new_checksum <- private$gen_checksum(design_)
          #     potential_filename <- private$gen_synthpop_filename(mc_, new_checksum, design_)
          #     design_$sim_prm$ageH <- original_ageH

          #     if (all(sapply(private$filename, file.exists))) {
          #       private$filename <- potential_filename
          #       break
          #     }
          # }
          private$filename <- private$gen_synthpop_filename(mc_, private$checksum, design_)
          # logic for the synthpop load
          files_exist <- sapply(private$filename, file.exists)
          if (all(!files_exist)) {
            # No files exist. Create the synthpop and store the file on disk (no
            # parallelism)
            private$gen_synthpop(
              mc_,
              private$filename,
              design_
            )
          } else if (file.exists(private$filename$metafile) &&
            !all(files_exist)) {
            # Metafile exists but not all files. Bounded wait, then cleanup/regenerate.
            # Default wait time is 0s (no waiting). To enable waiting, set to 600L below.
            max_wait_sec <- 0L
            # max_wait_sec <- 600L  # optional: wait up to 10 minutes

            waited <- 0L
            while (!all(sapply(private$filename, file.exists)) && waited < max_wait_sec) {
              Sys.sleep(1)
              waited <- waited + 1L
              if (design_$sim_prm$logs && waited %% 5L == 0L) {
                message("Waiting for synthpop files to appear (", waited, "s)...")
              }
            }

            if (!all(sapply(private$filename, file.exists))) {
              if (design_$sim_prm$logs) {
                message("Timeout waiting for incomplete synthpop files. Cleaning up and regenerating...")
              }
              self$delete_incomplete_synthpop()
              private$gen_synthpop(
                mc_,
                private$filename,
                design_
              )
            } else {
              # Ensure the file write is complete (size stable) with a bound
              if (design_$sim_prm$logs) message("Synthpop file found. Checking for size stability...")
              sz1 <- tryCatch(file.size(private$filename$synthpop), error = function(e) NA_real_)
              Sys.sleep(1)
              sz2 <- tryCatch(file.size(private$filename$synthpop), error = function(e) NA_real_)
              attempts <- 0L
              while (!is.na(sz1) && !is.na(sz2) && sz1 != sz2 && attempts < 30L) {
                if (design_$sim_prm$logs && attempts %% 5L == 0L) message("Synthpop file growing, waiting to stabilise... (", attempts, ")")
                sz1 <- tryCatch(file.size(private$filename$synthpop), error = function(e) NA_real_)
                Sys.sleep(1)
                sz2 <- tryCatch(file.size(private$filename$synthpop), error = function(e) NA_real_)
                attempts <- attempts + 1L
              }
              if (design_$sim_prm$logs) message("Synthpop file assumed stable.")
            }
          } else if (!file.exists(private$filename$metafile) &&
            !all(files_exist)) {
            # Metafile doesn't exist but some other files exist. In this case
            # delete everything and start from scratch
            self$delete_incomplete_synthpop()
            private$gen_synthpop(
              mc_,
              private$filename,
              design_
            )
          }
          # No need to provision for case when all file present. The following
          # lines handle this case anyway

          if (design_$sim_prm$load_simulants_rn) {
            exclude_cols_ <- c()
          } else { # if not load_simulants_rn = TRUE
            exclude_cols_ <- c(
              "rank_Fruit_vege",
              "rankstat_Smoking_act",
              "rankstat_Smoking_ex",
              "rankstat_Med_HT",
              "rankstat_Med_HL",
              "rankstat_Med_DM",
              "rank_PA_days",
              "rank_BMI",
              "rank_HbA1c",
              "rank_LDLc",
              "rank_SBP",
              "rankstat_Smoking_number"
            )
          }
          self$pop <- private$get_synthpop(exclude_cols = exclude_cols_)
          self$metadata <- yaml::read_yaml(private$filename$metafile)

          if (design_$sim_prm$logs) self$print()
        }
        invisible(self)
      },

      #  update_design ----
      #' @description
      #' Updates the Design object that is stored in the SynthPop object.
      #' @param design_ A design object with the simulation parameters.
      #' @return The invisible self for chaining.

      update_design = function(design_ = design) {
        if (!inherits(design, "Design")) {
          stop("Argument design_ needs to be a Design object.")
        }

        private$design <- design
        invisible(self)
      },

      # update_pop_weights ----
      #' @description
      #' Updates the wt_immrtl to account for mortality in baseline scenario.
      #' @param scenario_nam The scenario name. Logic is different if "sc0".
      #' @return The invisible self for chaining.
      update_pop_weights = function(scenario_nam = "sc0") {
        if (scenario_nam == "sc0") {
          # baseline
          self$pop[, tmp := sum(wt_immrtl), keyby = .(year, age, sex)]
          set(self$pop, NULL, "wt", 0)
          self$pop[
            !is.na(all_cause_mrtl),
            wt := wt_immrtl * tmp / sum(wt_immrtl),
            by = .(year, age, sex)
          ]

          self$pop[, tmp := NULL]
        } else if (scenario_nam != "sc0") {
          # For policy scenarios. 
          # Note that the wt_esp treatment is just an approximation to make them
          # available for policy scenarios. They need to be recalculated
          # properly after the cpp code and with deads removed

          fnam <- file.path(
            private$design$sim_prm$output_dir,
            "lifecourse",
            paste0("mc=", self$mc_aggr),
            "scenario=sc0"
          )

          t0 <- open_dataset(fnam)
          t0 <- as.data.table(t0[, c("pid", "year", "wt", "wt_esp")])

          # For some reason pid and year get read incorrectly as character sometimes
          # TODO check if this is still necessary
          if (!is.integer(t0$pid)) {
            t0[, pid := as.integer(pid)]
          }
          if (!is.integer(self$pop$pid)) {
            self$pop[, pid := as.integer(pid)]
          }
          if (!is.integer(t0$year)) {
            t0[, year := as.integer(year)]
          }
          if (!is.integer(self$pop$year)) {
            self$pop[, year := as.integer(year)]
          }

          setkeyv(t0, c("pid", "year"))
          self$pop[
            t0,
            on = c("pid", "year"),
            `:=`(wt = i.wt, wt_esp = i.wt_esp)
          ]

          # New way of calculating policy scenario population weights
          setkeyv(self$pop, c("pid", "year"))
          setnafill(self$pop, type = "locf", cols = "wt")
          setnafill(self$pop, type = "locf", cols = "wt_esp")
          self$pop[is.na(all_cause_mrtl), wt := 0]
          self$pop[is.na(all_cause_mrtl), wt_esp := 0]

          # Old way of calculating policy scenario population weights
          # self$pop[is.na(all_cause_mrtl), wt := 0]
          # self$pop[is.na(wt), wt := wt_immrtl]
        } else {
          stop(
            "The baseline scenario need to be named 'sc0' and simulated first, before any policy scenarios."
          ) # TODO more informative message
        }

        invisible(self)
      },

      # delete_synthpop ----
      #' @description
      #' Delete (all) synthpop files in the synthpop directory.
      #' @param mc_ If `mc_ = NULL`, delete all files in the synthpop directory.
      #'   If `mc_` is an integer vector delete the specific synthpop files
      #'   including the metadata and index files.
      #' @param check_checksum If  `TRUE` only delete files with the same
      #'   checksum as the synthpop. Only relevant when `mc_ = NULL`.
      #' @param invert If `TRUE` (default is `FALSE`) keeps files with the same
      #'   checksum as the synthpop and deletes all other synthpops. Only
      #'   relevant when `mc_ = NULL` and `check_checksum = TRUE`.
      #' @return The invisible `SynthPop` object.
      delete_synthpop = function(mc_, check_checksum = TRUE, invert = FALSE) {
        if (missing(mc_)) stop("Use mc_ = NULL if you want to delete all synthpop files.")
        if (is.null(mc_)) {
          if (check_checksum) {
            fl <- list.files(
              private$synthpop_dir,
              pattern = paste0("^synthpop_", private$checksum),
              full.names = TRUE,
              recursive = TRUE
            )
            if (invert) {
              fl2 <- list.files(
                private$synthpop_dir,
                pattern = "^synthpop_",
                full.names = TRUE,
                recursive = TRUE
              )
              fl <- setdiff(fl2, fl)
            }
          } else {
            fl <- list.files(
              private$synthpop_dir,
              pattern = "^synthpop_",
              full.names = TRUE,
              recursive = TRUE
            )
          }
          file.remove(fl)
        } else if (length(mc_) == 1L &&
          is.numeric(mc_) && ceiling(mc_) > 0L) {
          fl <- unlist(
            private$gen_synthpop_filename(
              mc_,
              private$checksum,
              private$design
            )
          )
          file.remove(fl)
        } else if (length(mc_) > 1L &&
          all(is.numeric(mc_)) && all(ceiling(mc_) > 0L)) {
          fl <-
            lapply(
              mc_,
              private$gen_synthpop_filename,
              private$checksum,
              private$design
            )
          fl <- unlist(fl)
          file.remove(fl)
        } else {
          message("mc_ need to be NULL or numeric. Nothing was deleted.")
        }

        return(invisible(self))
      },

      # delete_incomplete_synthpop ----
      #' @description
      #' Check that every synthpop file has a metafile and an index file. Delete
      #' any orphan files.
      #' @param check_checksum If  `TRUE` only delete incomplete group files
      #'   with the same checksum as the synthpop.
      #' @return The invisible `SynthPop` object.
      delete_incomplete_synthpop =
        function(check_checksum = TRUE) {
          if (check_checksum) {
            f1 <- paste0("^synthpop_", private$checksum, ".*\\.fst$")
            f2 <- paste0("^synthpop_", private$checksum, ".*_meta\\.yaml$")
          } else {
            f1 <- "^synthpop_.*\\.fst$"
            f2 <- "^synthpop_.*_meta\\.yaml$"
          }

          files <-
            list.files(private$synthpop_dir, f1)
          # remove indx files
          files <- sub("\\.fst$", "", files)
          metafiles <-
            list.files(private$synthpop_dir, f2)
          metafiles <- sub("_meta\\.yaml$", "", metafiles)

          to_remove <- setdiff(metafiles, files)
          if (length(to_remove) > 0) {
            to_remove <- paste0(to_remove, "_meta.yaml")
            file.remove(file.path(private$synthpop_dir, to_remove))
          }

          to_remove <- setdiff(files, metafiles)
          if (length(to_remove) > 0) {
            to_remove2 <- paste0(to_remove, ".fst")
            file.remove(file.path(private$synthpop_dir, to_remove2))
          }

          return(invisible(self))
        },

      # check_integridy ----
      #' @description
      #' Check the integrity of (and optionally delete) .fst files by checking
      #' their metadata are readable.
      #' @param remove_malformed If `TRUE`, delete all malformed .fst files and
      #'   their associated files.
      #' @param check_checksum If  `TRUE` only check files with the same
      #'   checksum as the synthpop.
      #' @return The invisible `SynthPop` object.
      check_integridy =
        function(remove_malformed = FALSE,
                 check_checksum = TRUE) {
          if (check_checksum) {
            pat <- paste0("^synthpop_", private$checksum, ".*\\.fst$")
          } else {
            pat <- "^synthpop_.*\\.fst$"
          }

          files <-
            list.files(private$synthpop_dir,
              pat,
              full.names = TRUE
            )
          if (length(files) > 0L) {
            malformed <- sapply(files, function(x) {
              out <- try(metadata_fst(x), silent = TRUE)
              out <- inherits(out, "try-error")
              out
            }, USE.NAMES = FALSE)


            des <- sum(malformed)

            if (remove_malformed) {
              if (des == 0L) {
                message(paste0(des, " malformed fst file(s)"))
              } else {
                # des != 0L
                message(paste0(des, " malformed fst file(s)..."))
                to_remove <- files[malformed]

                # then remove other files
                to_remove <- gsub(".fst$", "", to_remove)

                # _meta.yaml
                tr <- paste0(to_remove, "_meta.yaml")
                file.remove(tr[file.exists(tr)])
                # .fst
                tr <- paste0(to_remove, ".fst")
                file.remove(tr[file.exists(tr)])

                message("...now deleted!")
              }
            } else {
              # remove_malformed = FALSE
              message(paste0(des, " malformed fst file(s)"))
            }
          } else {
            # if length(files) == 0
            message("no .fst files found.")
          }
          return(invisible(self))
        },



      # count_synthpop ----
      #' @description
      #' Count the synthpop files in a directory. It includes files without
      #' metafiles and index files.
      #' @return The invisible `SynthPop` object.
      count_synthpop =
        function() {
          out <- list()
          # folder size
          files <-
            list.files(private$synthpop_dir, full.names = TRUE)
          if (length(files) > 0L) {
            vect_size <- sapply(files, file.size)
            out$`synthpop folder size (Gb)` <-
              signif(sum(vect_size) / (1024^3), 4) # Gb

            # synthpops with same checksum
            files <- list.files(
              private$synthpop_dir,
              paste0("^synthpop_", private$checksum, ".*\\.fst$")
            )

            out$`synthpop meta files with same checksum` <-
              length(list.files(
                private$synthpop_dir,
                paste0("^synthpop_", private$checksum, ".*_meta\\.yaml$")
              ))

            # synthpops with any checksum
            files <-
              list.files(private$synthpop_dir, "^synthpop_.*\\.fst$")

            out$`synthpop meta files with any checksum` <-
              length(list.files(private$synthpop_dir, "^synthpop_.*_meta\\.yaml$"))


            cat(paste0(names(out), ": ", out, "\n"))
          } else {
            # if length(files) == 0L
            cat("no files found.")
          }
          return(invisible(self))
        },

      # get_checksum ----
      #' @description
      #' Get the synthpop checksum.
      #' @param x One of "all", "synthpop" or "metafile". Can be abbreviated.
      #' @return The invisible `SynthPop` object.
      get_checksum = function() {
        out <- private$checksum
        names(out) <- "Checksum"
        cat(paste0(names(out), ": ", out))
        invisible(self)
      },

      # get_filename ----
      #' @description
      #' Get the synthpop file paths.
      #' @param x One of "all", "synthpop" or "metafile". Can be abbreviated.
      #' @return The invisible `SynthPop` object.
      get_filename = function(x = c("all", "synthpop", "metafile")) {
        if (self$mc == 0L) {
          print("Not relevant because mc = 0L")
        } else {
          x <- match.arg(x)
          switch(x,
            all      = print(private$filename),
            synthpop = print(private$filename[["synthpop"]]),
            metafile = print(private$filename[["metafile"]])
          )
        }
        invisible(self)
      },


      # get_design ----
      #' @description
      #' Get the synthpop design.
      #' @return The invisible `SynthPop` object.
      get_design = function() {
        # print(private$design)
        # invisible(self)
        private$design
      },

      # get_dir ----
      #' @description
      #' Get the synthpop dir.
      #' @return The invisible `SynthPop` object.
      get_dir = function() {
        print(private$synthpop_dir)
        invisible(self)
      },

      # gen_synthpop_demog ----
      #' @description
      #' Generate synthpop sociodemographics, random sample of the population.
      #' @param design_ A Design object,
      #' @param month April or July are accepted. Use July for mid-year
      #'   population estimates.
      #' @return An invisible `data.table` with sociodemographic information.
      # Change-for-IMPACT-NCD-Japan, we use population in October because the official population estimate was baed on population in October
      gen_synthpop_demog =
        function(design_, month = "July") {
          stopifnot("Argument month need to be April (economic year) or July (mid-year)" = month %in% c("April", "July"))
          # Use month = July for mid-year

          # load dt
          if (month == "April") {
            file <- "./inputs/pop_estimates/observed_population_japan.fst" # Change-for-IMPACT-NCD-Japan
          } else {
            file <- "./inputs/pop_estimates/observed_population_japan.fst" # Change-for-IMPACT-NCD-Japan
          }
          dt_meta <- metadata_fst(file)
          stopifnot(
            "Population size file need to be keyed by year" =
              identical("year", dt_meta$keys[1])
          )

          file_indx <- read_fst(file, as.data.table = TRUE, columns = "year")[, .(from = min(.I), to = max(.I)), keyby = "year"][year == 2000L + design_$sim_prm$init_year]

          dt <-
            read_fst(file,
              from = file_indx$from, to = file_indx$to,
              as.data.table = TRUE
            )
          # delete unwanted ages
          dt <- dt[age %in% c(design_$sim_prm$ageL:design_$sim_prm$ageH)] # TODO: Needs fix? Check old version

          dt[, prbl := pops / sum(pops)][, `:=`(reg = NULL, pops = NULL)]

          # I do not explicitly set.seed because I do so in the gen_synthpop()
          dtinit <- dt[sample(.N, design_$sim_prm$n, TRUE, prbl)]

          # Generate the cohorts of 30 year old to enter every year
          # as sim progress these will become 30 yo
          # no population growth here as I will calibrate to dt
          # projections and it fluctuates at +-2% anyways.

          # tt1 <-
          #   read_fst(
          #     "./inputs/pop_estimates_lsoa/national_mid_year_population_estimates.fst",
          #     as.data.table = TRUE
          #   )[age == design_$sim_prm$ageL & year >= design_$sim_prm$init_year]
          # tt2 <-
          #   read_fst("./inputs/pop_projections/national_proj.fst",
          #            as.data.table = TRUE)[age == design_$sim_prm$ageL &
          # year <= (design_$sim_prm$init_year + design_$sim_prm$sim_horizon_max)]
          # doubleyrs <- intersect(unique(tt1$year), unique(tt2$year))
          # if (length(doubleyrs)) {
          #   tt2 <- tt2[!year %in% doubleyrs]
          # }
          # tt <- rbind(tt1, tt2)
          # setkey(tt, year)
          # tt[, growth := shift(pops)/pops, keyby = sex]

          if (design_$sim_prm$logs) {
            message("Generate the cohorts of ", design_$sim_prm$ageL, " year old")
          }

          dt <- dt[age == design_$sim_prm$ageL]
          siz <- dtinit[age == design_$sim_prm$ageL, .N]
          dtfut <- dt[sample(.N, siz * design_$sim_prm$sim_horizon_max, TRUE, prbl)]
          dtfut[, age := age - rep(1:design_$sim_prm$sim_horizon_max, siz)]

          dt <- rbind(dtfut, dtinit)
          dt[, prbl := NULL]

          return(invisible(dt))
        },

      # write_synthpop ----
      #' @description
      #' Generate synthpop files in parallel, using foreach, and writes them to
      #' disk. It skips files that are already on disk.
      #' Note: the backend for foreach needs to be initialised before calling
      #' the function.
      #' @param mc_ An integer vector for the Monte Carlo iteration of the
      #'   synthetic population. Each integer generates a unique synthetic
      #'   population.
      #' @return The invisible `SynthPop` object.
      write_synthpop = function(mc_) {
        stopifnot(all(is.numeric(mc_)), all(ceiling(mc_) > 0L))
        on.exit(self$delete_incomplete_synthpop(), add = TRUE)
        mc_ <- as.integer(ceiling(mc_))

        if (.Platform$OS.type == "windows") {
          # TODO update to make compatible with windows
          cl <-
            makeCluster(private$design$sim_prm$clusternumber) # used for clustering. Windows compatible
          registerDoParallel(cl)
        } else {
          registerDoParallel(private$design$sim_prm$clusternumber) # used for forking. Only Linux/OSX compatible
        }

        foreach(
          mc_iter = mc_,
          .inorder = FALSE,
          .verbose = private$design$sim_prm$logs,
          .packages = c(
            "R6",
            "gamlss.dist",
            # For distr in prevalence.R
            "dqrng",
            "qs2",
            "fst",
            "CKutils",
            "IMPACTncdJapan",
            "data.table"
          ),
          .export = NULL,
          .noexport = NULL # c("time_mark")
        ) %dopar%
          {
            data.table::setDTthreads(1L)
            fst::threads_fst(1L)
            filename <-
              private$gen_synthpop_filename(
                mc_iter,
                private$checksum,
                private$design
              )

            # logic for the synthpop load
            files_exist <- sapply(filename, file.exists)
            if (all(!files_exist)) {
              # No files exist. Create the synthpop and store
              # the file on disk
              private$gen_synthpop(
                mc_iter,
                filename,
                private$design
              )
            } else if (
              file.exists(filename$metafile) &&
                !all(files_exist)
            ) {
              # Metafile exists but not all files. Bounded wait, then cleanup/regenerate.
              # Default wait time is 0s (no waiting). To enable waiting, set to 600L below.
              max_wait_sec <- 0L
              # max_wait_sec <- 600L  # optional: wait up to 10 minutes

              waited <- 0L
              while (!all(sapply(filename, file.exists)) && waited < max_wait_sec) {
                Sys.sleep(1)
                waited <- waited + 1L
              }

              if (!all(sapply(filename, file.exists))) {
                self$delete_incomplete_synthpop()
                private$gen_synthpop(
                  mc_iter,
                  filename,
                  private$design
                )
              } else {
                # Ensure the file write is complete (size stable) with a bound
                sz1 <- tryCatch(file.size(filename$synthpop), error = function(e) NA_real_)
                Sys.sleep(1)
                sz2 <- tryCatch(file.size(filename$synthpop), error = function(e) NA_real_)
                attempts <- 0L
                while (!is.na(sz1) && !is.na(sz2) && sz1 != sz2 && attempts < 30L) {
                  sz1 <- tryCatch(file.size(filename$synthpop), error = function(e) NA_real_)
                  Sys.sleep(1)
                  sz2 <- tryCatch(file.size(filename$synthpop), error = function(e) NA_real_)
                  attempts <- attempts + 1L
                }
              }
            } else if (
              !file.exists(filename$metafile) &&
                !all(files_exist)
            ) {
              # Metafile doesn't exist but some other files exist. In this case
              # delete everything and start from scratch
              self$delete_incomplete_synthpop()
              private$gen_synthpop(
                mc_iter,
                filename,
                private$design
              )
            }
            # No need to provision for case when all files present.

            return(NULL)
          }
        if (exists("cl")) {
          stopCluster(cl)
        }

        invisible(self)
      },

      # get_risks ----
      #' @description Get the risks for all individuals in a synthetic
      #'   population for a disease.
      #' @param disease_nam The disease that the risks will be returned.
      #' @return A data.table with columns for pid, year, and all associated
      #'   risks if disease_nam is specified. Else a list of data.tables for all
      #'   diseases.
      get_risks = function(disease_nam) {
        if (missing(disease_nam)) {
          return(private$risks)
        } else {
          stopifnot(is.character(disease_nam))
          return(private$risks[[disease_nam]])
        }
      },

      # store_risks ----
      #' @description Stores the disease risks for all individuals in a synthetic
      #'   population in a private list.
      #' @param disease_nam The disease that the risks will be stored.
      #' @return The invisible self for chaining.
      store_risks = function(disease_nam) {
        stopifnot(is.character(disease_nam))

        nam <- grep("_rr$", names(self$pop), value = TRUE)

        private$risks[[disease_nam]] <-
          self$pop[, .SD, .SDcols = c("pid", "year", nam)]

        self$pop[, (nam) := NULL]
        invisible(self)
      },

      # print ----
      #' @description
      #' Prints the synthpop object metadata.
      #' @return The invisible `SynthPop` object.
      print = function() {
        print(c(
          "path" = ifelse(self$mc == 0L,
            "Not relevant because mc = 0L",
            private$filename$synthpop
          ),
          "checksum" = private$checksum,
          "mc" = self$mc,
          self$metadata
        ))
        invisible(self)
      }
    ),



    # private -----------------------------------------------------------------
    private = list(
      filename = NA,
      checksum = NA,
      # The design object with the simulation parameters.
      design = NA,
      synthpop_dir = NA,
      risks = list(), # holds the risks for all individuals

      # Special deep copy for data.table. Use POP$clone(deep = TRUE) to
      # dispatch. Otherwise a reference is created
      deep_clone = function(name, value) {
        if ("data.table" %in% class(value)) {
          data.table::copy(value)
        } else if ("R6" %in% class(value)) {
          value$clone()
        } else {
          # For everything else, just return it. This results in a shallow
          # copy of s3.
          value
        }
      },

      # get a smaller design list only with characteristics that are important
      # for synthpop creation and define the uniqueness of the object. I.e. if
      # these parameters are different the synthpop has to have different
      # filename and vice-versa
      get_unique_characteristics = function(design_) {
        design_$sim_prm[c(
          "n",
          "sim_horizon_max",
          "init_year_long",
          "maxlag",
          "ageL",
          "ageH",
          "jumpiness"
        )]
      },

      # gen_checksum ----
      # gen synthpop unique checksum for the given set of inputs
      gen_checksum =
        function(design_) {
          # get a md5 checksum based on function arguments
          # First get function call arguments
          fcall <- private$get_unique_characteristics(design_)

          years_age_id <-
            digest(paste(fcall, sep = ",", collapse = ","),
              serialize = FALSE
            )
          return(years_age_id)
        },

      # gen synthpop filename for the given set of inputs
      gen_synthpop_filename =
        function(mc_,
                 checksum_,
                 design_) {
          return(
            list(
              "synthpop" = normalizePath(
                paste0(
                  design_$sim_prm$synthpop_dir,
                  "/synthpop_",
                  checksum_,
                  "_",
                  mc_,
                  ".fst"
                ),
                mustWork = FALSE
              ),
              "metafile" = normalizePath(
                paste0(
                  design_$sim_prm$synthpop_dir,
                  "/synthpop_",
                  checksum_,
                  "_",
                  mc_,
                  "_meta.yaml"
                ),
                mustWork = FALSE
              )
            )
          )
        },
      del_incomplete = function(filename_) {
        if (file.exists(filename_$metafile) &&
          (!file.exists(filename_$synthpop)
          )) {
          suppressWarnings(sapply(filename_, file.remove))
        }
      },

      # gen_synthpop ----
      gen_synthpop = # returns NULL. Writes synthpop on disk
        function(mc_,
                 filename_,
                 design_) {
          # increase design_$sim_prm$jumpiness for more erratic jumps in
          # trajectories

          # In Shiny app this function runs as a future. It is not
          # straightforward to check whether the future has been resolved or
          # not. To circumvent the problem I will save the metafile here (almost
          # function beginning) and the synthpop file at the end. So if both
          # files exist the function has finished. If only metafile exists the
          # function probably still runs.

          # Save synthpop metadata
          if (!file.exists(filename_$metafile)) {
            yaml::write_yaml(
              private$get_unique_characteristics(design_),
              filename_$metafile
            )
          }
          # NOTE In shiny app if 2 users click the  button at the same time, 2
          # functions will run almost concurrently with potential race condition

          # To avoid edge cases when the function stopped prematurely and a
          # metafile was created while the file was not. On.exit ensures that
          # either both files exist or none.

          on.exit(private$del_incomplete(filename_), add = TRUE)

          dqRNGkind("pcg64")
          SEED <-
            2121870L # sample(1e7, 1) # Hard-coded for reproducibility
          set.seed(SEED + mc_)
          dqset.seed(SEED, mc_)

          # Generate synthpops with sociodemographic and exposures information.

          dt <- self$gen_synthpop_demog(design_, month = "July")

          # NOTE!! from now on year in the short form i.e. 13 not 2013
          dt[, `:=`(pid = .I)]
          new_n <- nrow(dt)


          # Generate correlated ranks for the individuals ----
          if (design_$sim_prm$logs) {
            message("Generate correlated ranks for the individuals")
          }

          cm_mean <- as.matrix(
            read_fst(
              "./inputs/exposure_distributions/exposure_corr_mean.fst", # Change-for-IMPACT-NCD-Japan
              as.data.table = TRUE
            ),
            rownames = "rn"
          )


          # ??generate_corr_unifs(new_n, cm_mean)
          # Change-for-IMPACT-NCD-Japan
          rank_mtx <- generate_corr_unifs(new_n, cm_mean)
          if (design_$sim_prm$logs) message("generate correlated uniforms")

          # Restrict the range of some RNs to avoid unrealistic exposures
          # This scaling does not affect correlations
          # /0.999 because I multiplied all the columns below

          ## Change-for-IMPACT-NCD-Japan
          # rank_mtx <- data.table(rank_mtx)
          # rank_mtx[, colnames(rank_mtx) := lapply(.SD, function(x){x * 0.999 * 0.95 / 0.999}), .SDcols = colnames(rank_mtx)]

          # rank_mtx[, "Fruit_vege_r"] <- rank_mtx[, "Fruit_vege_r"] * 0.95 / 0.999
          # rank_mtx[, "Smoking_r"] <- rank_mtx[, "Smoking_r"] * 0.95 / 0.999
          # rank_mtx[, "Smoking_number_r"] <- rank_mtx[, "Smoking_number_r"] * 0.95 / 0.999
          # rank_mtx[, "Med_HT_r"] <- rank_mtx[, "Med_HT_r"] * 0.95 / 0.999
          # rank_mtx[, "Med_HL_r"] <- rank_mtx[, "Med_HL_r"] * 0.95 / 0.999
          # rank_mtx[, "Med_DM_r"] <- rank_mtx[, "Med_DM_r"] * 0.95 / 0.999
          # rank_mtx[, "PA_days_r"] <- rank_mtx[, "PA_days_r"] * 0.95 / 0.999
          # rank_mtx[, "BMI_r"] <- rank_mtx[, "BMI_r"] * 0.95 / 0.999
          # rank_mtx[, "HbA1c_r"] <- rank_mtx[, "HbA1c_r"] * 0.95 / 0.999
          # rank_mtx[, "LDLc_r"] <- rank_mtx[, "LDLc_r"] * 0.95 / 0.999
          # rank_mtx[, "SBP_r"] <- rank_mtx[, "SBP_r"] * 0.95 / 0.999

          rank_mtx <- data.table(rank_mtx)


          # sum((cor(rank_mtx) - cm_mean) ^ 2)
          if (design_$sim_prm$logs) message("correlated ranks matrix to data.table")


          # ????? 20230206
          # NOTE rankstat_* is unaffected by the RW (random walk). Stay constant through the lifecourse
          # Change-for-IMPACT-NCD-Japan
          # dt <- cbind(dt, rank_mtx)
          # setnames(dt, colnames(rank_mtx), paste0("rank_", str_sub(colnames(rank_mtx), start = 1, end = -3)))
          dt[, c(
            "rank_Fruit_vege",
            "rankstat_Smoking_act",
            "rankstat_Smoking_ex",
            "rankstat_Med_HT",
            "rankstat_Med_HL",
            "rankstat_Med_DM",
            "rank_PA_days",
            "rank_BMI",
            "rank_HbA1c",
            "rank_LDLc",
            "rank_SBP"
          ) := rank_mtx[, list(
            Fruit_vege_r,
            Smoking_act_r,
            Smoking_ex_r,
            Med_HT_r,
            Med_HL_r,
            Med_DM_r,
            PA_days_r, BMI_r, HbA1c_r, LDLc_r, SBP_r
          ), ]]

          # ????? 20230206 NOT RW variables to change the variable name for rankstat
          # add non-correlated RNs
          # Change-for-IMPACT-NCD-Japan
          rank_cols <- c("rankstat_Smoking_number")


          for (nam in rank_cols) {
            set(dt, NULL, nam, dqrunif(new_n))
          } # NOTE do not replace with generate_rns function.

          # Project forward for simulation and back project for lags  ----
          if (design_$sim_prm$logs) message("Project forward and back project")

          dt <-
            clone_dt(
              dt,
              design_$sim_prm$sim_horizon_max +
                design_$sim_prm$maxlag + 1L
            )

          dt[.id <= design_$sim_prm$maxlag, `:=`(
            age = age - .id,
            year = year - .id
          )]
          dt[.id > design_$sim_prm$maxlag, `:=`(
            age  = age + .id - design_$sim_prm$maxlag - 1L,
            year = year + .id - design_$sim_prm$maxlag - 1L
          )]
          # dt <-
          #   dt[between(age, design_$sim_prm$ageL - design_$sim_prm$maxlag, design_$sim_prm$ageH)]
          # delete unnecessary ages
          del_dt_rows(
            dt,
            !between(
              dt$age,
              design_$sim_prm$ageL - design_$sim_prm$maxlag,
              design_$sim_prm$ageH
            ),
            environment()
          )

          dt[, `:=`(.id = NULL)]

          # Change-for-IMPACT-NCD-Japan
          if (max(dt$age) >= 100L) {
            dt[, age100 := age]
            dt[age >= 100L, age := 100L]
          }

          # to_agegrp(dt, 20L, 85L, "age", "agegrp20", to_factor = TRUE)
          # to_agegrp(dt, 10L, 85L, "age", "agegrp10", to_factor = TRUE)
          # to_agegrp(dt,  5L, 85L, "age", "agegrp5" , to_factor = TRUE)

          # Simulate exposures -----

          # Random walk for ranks ----
          if (design_$sim_prm$logs) message("Random walk for ranks")

          setkeyv(dt, c("pid", "year"))
          setindexv(dt, c("year", "age", "sex")) # STRATA

          dt[, pid_mrk := mk_new_simulant_markers(pid)]

          dt[, lapply(
            .SD,
            fscramble_trajectories,
            pid_mrk,
            design_$sim_prm$jumpiness
          ),
          .SDcols = patterns("^rank_")
          ]
          # ggplot2::qplot(year, rank_ssb, data = dt[pid %in% sample(1e1, 1)], ylim = c(0,1))


          # Change-for-IMPACT-NCD-Japan
          # Set limit age ranges
          # Temp <- read_fst("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/HSE_ts.fst", as.data.table = TRUE)[between(Age, 20L, max(dt$age))]
          # limit_age <- Temp[, .(min = min(Age), max = max(Age))]
          # rm(Temp)
          limit_age <- data.table(min = min(dt$age), max = max(dt$age))




          # Generate Fruit_vege ----
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Fruit_vege", ".qs"))
          # Model_gamlss$parameters
          # Model_gamlss$family[1]
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate Fruit_vege")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_Fruit_vege.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]




          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          # } else {
          absorb_dt(dt, tbl)
          # }

          # ????? 20230206 I cannot find my_ function
          # For now, we use q___ insted of my_
          # Change-for-IMPACT-NCD-Japan
          dt[, Fruit_vege := fqZINBI(rank_Fruit_vege, mu, sigma, nu), ] # , n_cpu = design_$sim_prm$n_cpu)]

          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rank_Fruit_vege")
          dt[, c(col_nam) := NULL]





          # Generate Smoking ----
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Smoking", ".qs"))
          # Model_gamlss$parameters
          # Model_gamlss$family[1]
          # rm(Model_gamlss)
          # (never or ex smokers = 0) vs current(=1) using data between 2003 and 2019

          # The coding rule of smoking status was 3 = current, 2 = ever, 1 = never

          if (design_$sim_prm$logs) message("Generate Smoking")

          dt[, tax_tabaco := fcase(
            year < 2006L,                 0L,
            year >= 2006L & year < 2010L, 1L,
            year >= 2010L & year < 2018L, 2L,
            year >= 2018L,                3L
          )]
          dt[, tax_tabaco := factor(tax_tabaco, 0:3, 0:3)]

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_Smoking_NevEx_vs_current.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          # } else {
          absorb_dt(dt, tbl)
          # }

          # ????? 20230206 I cannot find my_ function
          # For now, we use q___ insted of my_
          # Change-for-IMPACT-NCD-Japan
          dt[, Smoking := as.integer(rankstat_Smoking_act < mu) * 2L] # 0 = never smoker or ex, 2 = current
          dt[, c(col_nam) := NULL]

          # Never (=0) vs Ex (=1) smokers using data between 2003 and 2012 Note that I did not use tabaco tax because data were limitted to 2003 and 2012

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_Smoking_never_vs_ex.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]
          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          absorb_dt(dt, tbl)
          # range01 <- function(x) {
          #   if (length(x) > 1L) {
          #     (x - min(x)) / (max(x) - min(x))
          #   } else {
          #     x
          #   }
          # }
          # dt[Smoking == 0L, Smoking := as.integer(range01(rankstat_Smoking) < mu), by = .(year)] # 0 = never smoker, 1=ex, 2=current
          dt[Smoking == 0L, Smoking := as.integer(rankstat_Smoking_ex < mu)] # 0 = never smoker, 1=ex, 2=current


          dt[, Smoking := factor(Smoking + 1L)]

          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rankstat_Smoking_act", "rankstat_Smoking_ex")
          dt[, c(col_nam, "tax_tabaco") := NULL]





          # Generate Smoking_number
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Smoking_number", ".qs"))
          # Model_gamlss
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate Smoking_number")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_Smoking_number.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]



          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          # } else {
          absorb_dt(dt, tbl)
          # }




          dt[
            Smoking == 3,
            Smoking_number_grp := (rankstat_Smoking_number > pa0) +
              (rankstat_Smoking_number > pa1) +
              (rankstat_Smoking_number > pa2) +
              (rankstat_Smoking_number > pa3) +
              (rankstat_Smoking_number > pa4) +
              (rankstat_Smoking_number > pa5) +
              (rankstat_Smoking_number > pa6) +
              (rankstat_Smoking_number > pa7)
          ]



          ##### meeting on Feb 23 2023
          # system.time({dt[, Smoking_number := fcase(
          #   Smoking_number_grp == 0L, 5L,
          #   Smoking_number_grp == 1L, 10L,
          #   Smoking_number_grp == 2L, 15L,
          #   Smoking_number_grp == 3L, 20L,
          #   Smoking_number_grp == 4L, 25L,
          #   Smoking_number_grp == 5L, 30L,
          #   Smoking_number_grp == 6L, 35L,
          #   Smoking_number_grp == 7L, 40L,
          #   Smoking_number_grp == 8L, sample(c(50L, 60L, 80L), .N, TRUE, prob = c(0.4, 0.45, 0.15))
          # )]}) # NOTE not faster than the below

          dt[Smoking_number_grp == 0L, Smoking_number := 5L]
          dt[Smoking_number_grp == 1L, Smoking_number := 10L]
          dt[Smoking_number_grp == 2L, Smoking_number := 15L]
          dt[Smoking_number_grp == 3L, Smoking_number := 20L]
          dt[Smoking_number_grp == 4L, Smoking_number := 25L]
          dt[Smoking_number_grp == 5L, Smoking_number := 30L]
          dt[Smoking_number_grp == 6L, Smoking_number := 35L]
          dt[Smoking_number_grp == 7L, Smoking_number := 40L]
          # I do not explicitly set.seed because I do so at the beginning of gen_synthpop()
          dt[Smoking_number_grp == 8L, Smoking_number := sample(c(50L, 60L, 80L), .N, TRUE, prob = c(0.4, 0.45, 0.15))]

          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rankstat_Smoking_number")
          dt[, c(col_nam, "Smoking_number_grp") := NULL]

          # Generate Med_HT
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Med_HT", ".qs"))
          # Model_gamlss$parameters
          # Model_gamlss$family[1]
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate Med_HT")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_Med_HT.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]




          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          dt[, trueyear := year]
          dt[age >= 70 & trueyear > 2030L, year := 2030L]
          absorb_dt(dt, tbl)
          dt[, `:=`(year = trueyear, trueyear = NULL)]

          # ????? 20230206 I cannot find my_ function
          # For now, we use q___ insted of my_
          # Change-for-IMPACT-NCD-Japan
          dt[, Med_HT := as.integer(rankstat_Med_HT > (1 - mu))] # , n_cpu = design_$sim_prm$n_cpu)]
          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rankstat_Med_HT")
          dt[, c(col_nam) := NULL]








          # Generate Med_HL
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Med_HL", ".qs"))
          # Model_gamlss$parameters
          # Model_gamlss$family[1]
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate Med_HL")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_Med_HL.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]



          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          dt[, trueyear := year]
          dt[age >= 70 & trueyear > 2030L, year := 2030L]
          absorb_dt(dt, tbl)
          dt[, `:=`(year = trueyear, trueyear = NULL)]

          # ????? 20230206 I cannot find my_ function
          # For now, we use q___ insted of my_
          # Change-for-IMPACT-NCD-Japan
          dt[, Med_HL := as.integer(rankstat_Med_HL > (1 - mu))] # , n_cpu = design_$sim_prm$n_cpu)]
          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rankstat_Med_HL")
          dt[, c(col_nam) := NULL]







          # Generate Med_DM
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "Med_DM", ".qs"))
          # Model_gamlss$parameters
          # Model_gamlss$family[1]
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate Med_DM")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_Med_DM.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]



          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          # Tame unrealistic trends
          dt[, trueyear := year]
          dt[age >= 70 & trueyear > 2030L, year := 2030L]
          absorb_dt(dt, tbl)
          dt[, `:=`(year = trueyear, trueyear = NULL)]

          # ????? 20230206 I cannot find my_ function
          # For now, we use q___ insted of my_
          # Change-for-IMPACT-NCD-Japan
          dt[, Med_DM := as.integer(rankstat_Med_DM > (1 - mu))] # , n_cpu = design_$sim_prm$n_cpu)]
          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rankstat_Med_DM")
          dt[, c(col_nam) := NULL]




          # Generate PA_days ----
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "PA_days", ".qs"))
          # Model_gamlss
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate PA_days")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_PA_days.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]



          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          # } else {

          # Tame unrealistic trends
          dt[, trueyear := year]
          dt[age >= 70 & trueyear > 2025L, year := 2025L]
          dt[age >= 50 & sex == "men" & trueyear < 2010L, year := 2010L]
          dt[age >= 70 & sex == "women" & trueyear < 2015L, year := 2015L]

          absorb_dt(dt, tbl)
          dt[, `:=`(year = trueyear, trueyear = NULL)]



          dt[
            ,
            PA_days := factor(
              (rank_PA_days > pa0) +
                (rank_PA_days > pa1) +
                (rank_PA_days > pa2) +
                (rank_PA_days > pa3) +
                (rank_PA_days > pa4) +
                (rank_PA_days > pa5) +
                (rank_PA_days > pa6),
              levels = 0:7, labels = 0:7, ordered = TRUE
            )
          ]

          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rank_PA_days")
          dt[, c(col_nam) := NULL]






          # Generate BMI ----
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "BMI", ".qs"))
          # Model_gamlss$parameters
          # Model_gamlss$family[1]
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate BMI")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_BMI.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

          ### Make PA days category 
          dt[, pa_3cat := fifelse(
            PA_days %in% as.character(0:1), 1L,
            fifelse(
              PA_days %in% as.character(2:4), 2L,
              fifelse(PA_days %in% as.character(5:7), 3L, NA_integer_)
            )
          )]
          # dt[,table(PA_3cat, PA_days, useNA="always"),]


          dt[, pa_3cat := factor(pa_3cat)]
          # table(dt$PA_3cat, useNA = "always")
         
          # simulate trancated distribution
          tbl[, maxq := (pBCTo(rep(70, .N), mu, sigma, nu, tau))]
          tbl[, minq := (pBCTo(rep(14, .N), mu, sigma, nu, tau))]

          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          # } else {
          absorb_dt(dt, tbl)
          # }

          # ????? 20230206 I cannot find my_ function
          # For now, we use q___ insted of my_
          # Change-for-IMPACT-NCD-Japan
          dt[, BMI := qBCTo(minq + rank_BMI * (maxq - minq), mu, sigma, nu, tau), ] # , n_cpu = design_$sim_prm$n_cpu)]

          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rank_BMI")
          dt[, c(col_nam, "pa_3cat") := NULL]





          # Generate HbA1c ----
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "HbA1c", ".qs"))
          # Model_gamlss$parameters
          # Model_gamlss$family[1]
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate HbA1c")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_HbA1c.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]


          setnames(tbl, c("Age", "Sex", "Year", "BMI"), c("age", "sex", "year", "BMI_round"))
          tbl[, sex := factor(sex, 0:1, c("men", "women"))]

          tbl[, BMI_round := as.integer(10 * BMI_round)]
          dt[, BMI_round := as.integer(round(10 * BMI, 0))]


          # simulate trancated distribution
          tbl[, maxq := (pBCT(rep(18, .N), mu, sigma, nu, tau))]
          tbl[, minq := (pBCT(rep(0.04, .N), mu, sigma, nu, tau))]


          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          # } else {
          absorb_dt(dt, tbl)
          # }


          # ????? 20230206 I cannot find my_ function
          # For now, we use q___ insted of my_
          # Change-for-IMPACT-NCD-Japan
          dt[, HbA1c := fqBCT(minq + rank_HbA1c * (maxq - minq), mu, sigma, nu, tau), ] # , n_cpu = design_$sim_prm$n_cpu)]
          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rank_HbA1c")
          dt[, c(col_nam) := NULL]





          # Generate LDLc ----
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "LDLc", ".qs"))
          # Model_gamlss$parameters
          # Model_gamlss$family[1]
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate LDLc")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_LDLc.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, c("Age", "Sex", "Year", "BMI"), c("age", "sex", "year", "BMI_round"))
          tbl[, sex := factor(sex, 0:1, c("men", "women"))]

          tbl[, BMI_round := as.integer(10 * BMI_round)]
          # dt[, BMI_round := as.integer(round(10 * BMI, 0))] # Created above in HbA1c


          # simulate trancated distribution
          tbl[, maxq := (pBCT(rep(350, .N), mu, sigma, nu, tau))]
          tbl[, minq := (pBCT(rep(15, .N), mu, sigma, nu, tau))]


          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          # } else {
          absorb_dt(dt, tbl)

          # }

          # ????? 20230206 I cannot find my_ function
          # For now, we use q___ insted of my_
          # Change-for-IMPACT-NCD-Japan
          dt[, LDLc := fqBCT(minq + rank_LDLc * (maxq - minq), mu, sigma, nu, tau)] # , n_cpu = design_$sim_prm$n_cpu)]
          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rank_LDLc")
          dt[, c(col_nam, "BMI_round") := NULL] # del BMI_round because SBP rounds at different precision


          # Generate SBP ----
          # Change-for-IMPACT-NCD-Japan
          # Model_gamlss <- qread(paste0("/home/rstudio/IMPACT_NCD_data/NHNS_data/Output_data_organized/GAMLSS_created/GAMLSS_model_", "SBP", ".qs"))
          # Model_gamlss$parameters
          # Model_gamlss$family[1]
          # rm(Model_gamlss)


          if (design_$sim_prm$logs) message("Generate SBP")

          tbl <-
            read_fst("./inputs/exposure_distributions/Table_SBP.fst",
              as.data.table = TRUE
            )[between(Age, limit_age$min, limit_age$max)]
          setnames(tbl, c("Age", "Sex", "Year", "BMI", "Smoking"), c("age", "sex", "year", "BMI_round", "smoking_tmp"))
          tbl[, `:=`(
            sex = factor(sex, 0:1, c("men", "women")),
            smoking_tmp = as.integer(smoking_tmp),
            BMI_round = as.integer(BMI_round)
          )] # TODO update the saved file so we don't have to do these slow conversions every time


          dt[, `:=`(
            BMI_round = as.integer(round(BMI)), # TODO consider Rfast::Round to speedup
            smoking_tmp = as.integer(Smoking == "3")
          )] # 1 = smoker

                    # simulate trancated distribution
          tbl[, maxq := (pBCPE(rep(250, .N), mu, sigma, nu, tau))]
          tbl[, minq := (pBCPE(rep(75, .N), mu, sigma, nu, tau))]


          col_nam <-
            setdiff(names(tbl), intersect(names(dt), names(tbl)))
          # if (.Platform$OS.type == "unix") {
          #  lookup_dt(dt, tbl, check_lookup_tbl_validity = FALSE) #TODO: Lookup_dt
          # } else {

          absorb_dt(dt, tbl)
          # }

          # ????? 20230206 I cannot find my_ function
          # For now, we use q___ insted of my_
          # Change-for-IMPACT-NCD-Japan
          dt[, SBP := qBCPE(minq + rank_SBP * (maxq - minq), mu, sigma, nu, tau)] # , n_cpu = design_$sim_prm$n_cpu)]
          if (!design_$sim_prm$keep_simulants_rn) col_nam <- c(col_nam, "rank_SBP")
          dt[, c(col_nam, "BMI_round", "smoking_tmp") := NULL]




          ## --------------------------------------------------

          dt[, `:=`(
            pid_mrk = NULL
            # to be recreated when loading synthpop
          )]


          # ????? 20230206  # all exposure names  we do not need rank_ rankstat_
          xps_tolag <- c(
            "Smoking_number",
            "Smoking",
            "SBP",
            "PA_days",
            "Med_HT",
            "Med_DM",
            "Med_HL",
            "LDLc",
            "HbA1c",
            "Fruit_vege",
            "BMI"
          )
          xps_nam <- paste0(xps_tolag, "_curr_xps")
          setnames(dt, xps_tolag, xps_nam)

          if ("age100" %in% names(dt)) {
            dt[, age := NULL]
            setnames(dt, "age100", "age")
          }
          dt[, sex := factor(sex)]
          dt[, year := as.integer(year)]
          setkey(dt, pid, year) # Just in case
          setcolorder(dt, c("pid", "year", "age", "sex")) # STRATA
          setindexv(dt, c("year", "age", "sex")) # STRATA
          if (design_$sim_prm$logs) message("Writing synthpop to disk")
          write_fst(
            dt,
            filename_$synthpop,
            90
          ) # 100 is too slow
          return(invisible(NULL))
        },

      # get_synthpop ----
      # Load a synthpop file from disk in full or in chunks.
      get_synthpop =
        function(exclude_cols = c()) {
          mm_synthpop <- metadata_fst(private$filename$synthpop)
          mm_synthpop <- setdiff(mm_synthpop$columnNames, exclude_cols)


          # Read synthpop

          dt <- read_fst(private$filename$synthpop,
            columns = mm_synthpop,
            as.data.table = TRUE
          )
          dt <- dt[between(
            year - 2000L,
            private$design$sim_prm$init_year - private$design$sim_prm$maxlag,
            private$design$sim_prm$init_year + private$design$sim_prm$sim_horizon_fromGUI
          ) &
            between(
              age,
              private$design$sim_prm$ageL - private$design$sim_prm$maxlag,
              private$design$sim_prm$ageH
            )]

          # Ensure pid does not overlap for files from different mc
          new_n <-
            it <- as.integer(ceiling(self$mc %% private$design$sim_prm$num_chunks))
          if ((max(dt$pid) + it * 1e8) >= .Machine$integer.max) stop("pid larger than int32 limit.")
          dt[, pid := as.integer(pid + it * 1e8)]

          dt[, pid_mrk := mk_new_simulant_markers(pid)] # TODO Do I need this?

          # dt[, pid_mrk := mk_new_simulant_markers(pid)] # TODO Do I need this?

          # Ensure pid does not overlap for files from different mc
          # new_n <- uniqueN(dt$pid)
          # it <- as.integer(ceiling(self$mc %% private$design$sim_prm$num_chunks))
          # it[it == 0L] <- private$design$sim_prm$num_chunks
          # it <- it - 1L
          # if (max(dt$pid + (private$design$sim_prm$num_chunks - 1) * new_n) < .Machine$integer.max) {
          #  dt[, pid := as.integer(pid + it * new_n)]
          # } else stop("pid larger than int32 limit.")

          # generate population weights
          private$gen_pop_weights(dt, private$design)

          set(dt, NULL, "all_cause_mrtl", 0L)
          set(dt, NULL, "cms_score", 0) # CMS score of diagnosed conditions
          set(dt, NULL, "cms_count", 0L) # Count of diagnosed CMS conditions

          setkey(dt, pid, year)
          # dt[, dead := identify_longdead(all_cause_mrtl, pid_mrk)]
          # dt[, ncc := clamp(
          #   ncc - (chd_prvl > 0) - (stroke_prvl > 0) -
          #     (poststroke_dementia_prvl > 0) -
          #     (htn_prvl > 0) - (t2dm_prvl > 0) - (af_prvl > 0) -
          #     (copd_prvl > 0) - (lung_ca_prvl > 0) -
          #     (colon_ca_prvl > 0) -
          #     (breast_ca_prvl > 0),
          #   0L,
          #   10L
          # )]
          # to be added back in the qaly fn. Otherwise when I prevent disease
          # the ncc does not decrease.


          invisible(dt)
        },

      # gen_pop_weights ----
      # Calculate weights so that their sum is the population of the area based
      # on ONS. It takes into account synthpop aggregation. So you need to sum
      # all the synthpops belong to the same aggregation to reach the total pop.
      # NOTE Wt are still incomplete because they assume everyone remains alive.
      # So baseline population underestimated as clearly some die
      gen_pop_weights = function(dt, design) {
        tt <-
          read_fst("./inputs/pop_projections/combined_population_japan.fst", as.data.table = TRUE) # Change-for-IMPACT-NCD-Japan

        tt <- tt[
          between(age, min(dt$age), max(dt$age)) &
            between(year, min(dt$year), max(dt$year)),
          .(pops = sum(pops)),
          keyby = .(year, age, sex)
        ]
        dt[, wt_immrtl := .N, by = .(year, age, sex)]
        absorb_dt(dt, tt)
        dt[, wt_immrtl := pops / (wt_immrtl * design$sim_prm$num_chunks)]
        dt[, pops := NULL]

        invisible(dt)
      }
    )
  )

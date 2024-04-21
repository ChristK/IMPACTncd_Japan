## IMPACTncdJapan is an implementation of the IMPACTncd framework, developed by Chris
## Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
## Guzman-Castillo, Amandine Robert, and Piotr Bandosz.
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



# From
# https://stackoverflow.com/questions/33424233/how-do-i-tell-an-r6-class-what-to-do-with-square-brackets
# Allows data.table syntax to the R6class object directly. Assumes it has a
# field 'output' that is a data.table

#' @export
`[.Simulation` <- function(x, ...) x$output[...]

#' R6 Class representing a simulation environment
#' @description A simulation environment.
#' @details To be completed...
#' @export
Simulation <-
  R6::R6Class(
    classname = "Simulation",
    lock_objects = TRUE, # allows primary prevention scenario to be updated
    lock_class = TRUE,
    # public ------------------------------------------------------------------
    public = list(
      #' @field design A Design object.
      design = NA,

      #' @field diseases A list of Disease objects.
      diseases = NA,

      #' @field RR A list of RR for the simulated exposures.
      RR = NA,

      #' @field scenarios A list of scenario objects.
      scenarios = NA,

      # initialise ----
      #' @description Create a new simulation object.
      #' @param sim_prm Either a path to a yaml file or a Design object.
      #' @return A new `Simulation` object.
      initialize = function(sim_prm) {
        if (is.character(sim_prm)) {
          self$design <- Design$new(sim_prm)
        } else if (inherits(sim_prm, "Design")) {
          self$design <- sim_prm$clone(deep = TRUE)
        } else {
          stop("sim_prm need to be a path to an appropriate yaml file or a Design object")
        }

        data.table::setDTthreads(
          threads = self$design$sim_prm$clusternumber,
          restore_after_fork = NULL
        )
        fst::threads_fst(
          nr_of_threads = self$design$sim_prm$clusternumber,
          reset_after_fork = NULL
        )


        # Create folders if don't exist
        # TODO write hlp function and use lapply
        message("Creating output subfolders.")
        private$create_new_folder(self$design$sim_prm$output_dir, self$design$sim_prm$logs)
        private$create_new_folder(private$output_dir("summaries/"), self$design$sim_prm$logs)
        private$create_new_folder(private$output_dir("tables/"), self$design$sim_prm$logs)
        private$create_new_folder(private$output_dir("plots/"), self$design$sim_prm$logs)
        private$create_new_folder(private$output_dir("lifecourse/"), self$design$sim_prm$logs)
        if (self$design$sim_prm$export_PARF) {
          private$create_new_folder(private$output_dir("parf/"), self$design$sim_prm$logs)
        }
        if (self$design$sim_prm$export_xps) {
          private$create_new_folder(private$output_dir("xps/"), self$design$sim_prm$logs)
        }
        if (self$design$sim_prm$logs) {
          private$create_new_folder(private$output_dir("logs/"), self$design$sim_prm$logs)
        }

        # NOTE code below is duplicated in Synthpop class. This is intentional
        private$create_new_folder(self$design$sim_prm$synthpop_dir, self$design$sim_prm$logs)

        private$create_empty_calibration_prms_file(replace = FALSE)

        message("Loading exposures.")
        # RR Create a named list of Exposure objects for the files in
        # ./inputs/RR
        fl <- list.files(path = "./inputs/RR", pattern = ".csvy$", full.names = TRUE)
        # RR <- future_lapply(fl, Exposure$new, future.seed = 950480304L)
        self$RR <- lapply(fl, Exposure$new, design = self$design)
        names(self$RR) <- sapply(self$RR, function(x) x$get_name())
        # invisible(future_lapply(RR, function(x) {
        #   x$gen_stochastic_effect(design, overwrite = FALSE, smooth = FALSE)
        # }, future.seed = 627524136L))
        invisible(lapply(self$RR, function(x) {
          x$gen_stochastic_effect(self$design, overwrite = FALSE, smooth = FALSE)
        }))
        # NOTE smooth cannot be exported to Design for now, because the first
        # time this parameter changes we need logic to overwrite unsmoothed
        # files
        rm(fl)

        # Generate diseases
        message("Loading diseases.")
        self$diseases <- lapply(self$design$sim_prm$diseases, function(x) {
          x[["design_"]] <- self$design
          x[["RR"]] <- self$RR
          do.call(Disease$new, x)
        })
        names(self$diseases) <- sapply(self$design$sim_prm$diseases, `[[`, "name")

        message("Generating microsimulation structure.")
        # Generate the graph with the causality structure
        ds <- unlist(strsplit(names(self$RR), "~"))
        ds[grep("^smok_", ds)] <- "smoking"
        ds <- gsub("_prvl$", "", ds)

        ds1 <- ds[as.logical(seq_along(ds) %% 2)]
        ds2 <- ds[!as.logical(seq_along(ds) %% 2)]
        ds <- unique(data.table(ds1, ds2))

        private$causality_structure <- make_graph(unlist(transpose(ds)),
          directed = TRUE
        )

        # Japanise standardised population 2015 (esp) weights
        tt <- data.table(
          agegrp = agegrp_name(0, 99),
          wt_esp = c(
            978000, 4048000, 5369000, 5711000, 6053000, 6396000, 6738000, 7081000,
            7423000, 7766000, 8108000, 8451000, 8793000, 9135000, 9246000, 7892000,
            6306000, 4720000, 3134000, 1548000, 423000
          )
        )
        esp <- CJ(
          agegrp = agegrp_name(0, 99),
          sex = c("men", "women")
        )

        private$esp_weights <- copy(absorb_dt(esp, tt))

        private$death_codes <- unlist(lapply(self$diseases, function(x) {
          x$meta$mortality$code
        }))
        private$death_codes[["alive"]] <- 0L

        private$primary_prevention_scn <- function(synthpop) NULL # default for baseline scenario
        private$secondary_prevention_scn <- function(synthpop) NULL # default for baseline scenario

        invisible(self)
      },


      # update_primary_prevention_scn ----
      #' @description Updates the primary prevention policy scenario
      #' @param method a function with synthpop as an argument that models the primary prevention policy.
      #' @return The invisible self for chaining.
      update_primary_prevention_scn = function(method) {
        private$primary_prevention_scn <- method
        environment(private$primary_prevention_scn) <- environment(private$update_primary_prevention_scn)
      },

      # get_primary_prevention_scn ----
      #' @description Get the primary prevention policy scenario
      #' @return The primary prevention policy scenario.
      get_primary_prevention_scn = function() {
        private$primary_prevention_scn
      },

      # update_secondary_prevention_scn ----
      #' @description Updates the secondary prevention policy scenario
      #' @param method a function with synthpop as an argument that models the secondary prevention policy.
      #' @return The invisible self for chaining.
      update_secondary_prevention_scn = function(method) {
        private$secondary_prevention_scn <- method
        environment(private$secondary_prevention_scn) <- environment(private$update_secondary_prevention_scn)
      },

      # get_secondary_prevention_scn ----
      #' @description Get the secondary prevention policy scenario
      #' @return The secondary prevention policy scenario.
      get_secondary_prevention_scn = function() {
        private$secondary_prevention_scn
      },

      # run ----
      #' @description Runs a simulation
      #' @param mc A positive sequential integer vector with the Monte Carlo
      #'   iterations of synthetic population to simulate, or a scalar.
      #' @param multicore If TRUE run the simulation in parallel.
      #' @param scenario_nam A string for the scenario name (i.e. sc1)
      #' @return The invisible self for chaining.
      run = function(mc, multicore = TRUE, scenario_nam) {
        if (!is.integer(mc)) stop("mc need to be an integer")
        if (any(mc <= 0)) stop("mc need to be positive integer")

        # recombine the chunks of large files
        # TODO logic to delete these files
        self$reconstruct_large_files()

        # check if sequential vector. Necessary if
        # design$sim_prm$n_synthpop_aggregation > 1
        if (anyNA(mc) || any(is.infinite(mc)) || length(mc) < 1L ||
          (length(mc) > 1L && diff(mc[1:2]) == 0) ||
          (length(mc) > 1L && diff(range(diff(mc))) > sqrt(.Machine$double.eps))) {
          stop("mc need to be a sequential integer vector, or a scalar")
        }
        # NOTE mc is in fact mc_aggr. mc_ is the mc of the synthpop
        mc_sp <-
          (
            min(mc) * self$design$sim_prm$n_synthpop_aggregation -
              self$design$sim_prm$n_synthpop_aggregation + 1L
          ):(max(mc) * self$design$sim_prm$n_synthpop_aggregation)



        if (any(file.exists( # TODO fix when lifecourse is not saved
          file.path(
            self$design$sim_prm$output_dir,
            "lifecourse",
            paste0(mc, "_lifecourse.cs")
          )
        ))) {
          # stop("Results from a previous simulation exists in the output
          #      folder. Please remove them before run a new one.")
          message(
            "Results from a previous simulation exists in the output folder. Please remove them if this was unintentional."
          )
        }



        # Generate PARF files if they don't exist. Note that generation is
        # multicore
        lapply(self$diseases, function(x) {
          x$gen_parf_files(self$design, self$diseases)
        })

        if (multicore) {
          if (self$design$sim_prm$logs) private$time_mark("Start of parallelisation")

          if (.Platform$OS.type == "windows") {
            cl <-
              makeClusterPSOCK(
                self$design$sim_prm$clusternumber,
                dryrun = FALSE,
                quiet = FALSE,
                rscript_startup = quote(local({
                  library(CKutils)
                  library(IMPACTncdJapan)
                  library(digest)
                  library(fst)
                  library(qs)
                  library(wrswoR)
                  library(gamlss.dist)
                  library(dqrng)
                  library(data.table)
                })),
                rscript_args = c(
                  "--no-init-file",
                  "--no-site-file",
                  "--no-environ"
                ),
                setup_strategy = "parallel"
              ) # used for clustering. Windows compatible

            on.exit(if (exists("cl")) stopCluster(cl))

            xps_dt <- parLapplyLB(
              cl = cl,
              X = mc_sp,
              fun = function(x) private$run_sim(mc_ = x, scenario_nam)
            )
          } else {
            # used for forking. Only Linux/OSX compatible
            registerDoParallel(self$design$sim_prm$clusternumber)

            xps_dt <- foreach(
              mc_iter = mc_sp,
              .inorder = FALSE,
              .options.multicore = list(preschedule = FALSE),
              .verbose = self$design$sim_prm$logs,
              .packages = c(
                "R6",
                "digest",
                "qs",
                "wrswoR",
                "gamlss.dist",
                "dqrng",
                "CKutils",
                "IMPACTncdJapan",
                "fst",
                "data.table"
              ),
              .export = ls(envir = globalenv()),
              .noexport = NULL # c("time_mark")
            ) %dopar% {
              private$run_sim(mc_ = mc_iter, scenario_nam)
            }


            # xps_dt <- foreach(
            #   mc_iter = mc_sp,
            #   .inorder = FALSE,
            #   .options.multicore = list(preschedule = FALSE),
            #   .verbose = self$design$sim_prm$logs,
            #   .packages = c(
            #     "R6",
            #     "gamlss.dist",
            #     "dqrng",
            #     "CKutils",
            #     "IMPACTncdJapan",
            #     "fst",
            #     "data.table"
            #   ),
            #   .export = NULL,
            #   .noexport = NULL # c("time_mark")
            # ) %dopar% {

            #   private$run_sim(mc_ = mc_iter, scenario_nam)

            # }
          }

          if (self$design$sim_prm$logs) private$time_mark("End of parallelisation")
        } else { # if multicore = FALSE
          if (self$design$sim_prm$logs) {
            private$time_mark("Start of single-core run")
          }

          lapply(mc_sp, private$run_sim, scenario_nam)

          if (self$design$sim_prm$logs) {
            private$time_mark("End of single-core run")
          }
        }

        if (self$design$sim_prm$avoid_appending_csv) {
          message("Collecting the fragmented lifecourse files. This may take some time. Please be patient...")
          private$collect_files("lifecourse", "_lifecourse.csv$", to_mc_aggr = TRUE)

          if (self$design$sim_prm$export_xps) {
            private$collect_files("xps", "_xps20.csv$", to_mc_aggr = FALSE)
            private$collect_files("xps", "_xps_esp.csv$", to_mc_aggr = FALSE)
          }

          if (self$design$sim_prm$logs) {
            private$time_mark("End of collecting mc lifecourse files")
          }
        }

        while (sink.number() > 0L) sink()

        invisible(self)
      },


      # The trends in incidence by sex alone seem a bit off. This is because I
      # calibrate using a single year of age, and there is bias there that is
      # compounded rather than cancelling out. I think I can fix this by adding a
      # final step in the calibration after the calibration by a single year of age
      # finish. The prevalence at older ages fluctuates a lot. This is because the
      # initial values we get from GBD are not aligned with the mortality rates we
      # use. The only way to fix this is by using DISMOD to align the initial
      # prevalence for the given incidence and mortality. Please remind me if you have
      # used DISMOD before so you can do this. It would be very helpful. Nonmodelled,
      # CHD and stroke mortalities are underestimated. This is most likely because I
      # calibrate them independently from one another while the risk of mortality is
      # not independent but competing. I think I can change the calibration algorithm
      # to consider competing risks. Another possibility is that it is the bias
      # introduced by the use of beta distribution for the uncertainty. From memory,
      # when I checked it, that bias was much smaller than the one observed in these
      # plots, but I will double-check to make sure.


      # calibrate_incd_ftlt ----
      #' @description generates new calibration parameters and ovwrites old ones.
      #' @param mc A positive sequential integer vector with the Monte Carlo
      #'   iterations of synthetic population to simulate, or a scalar.
      #' @param replace If TRUE the calibration deletes the previous calibration file and starts from scratch. Else it continues from the last age.
      #' @return The invisible self for chaining.
      calibrate_incd_ftlt = function(mc, replace = FALSE) {
        # recombine the chunks of large files
        # TODO logic to delete these files
        self$reconstruct_large_files()

        export_xps <- self$design$sim_prm$export_xps # save the original value to be restored later
        self$design$sim_prm$export_xps <- FALSE # turn off export_xps to speed up the calibration
        private$create_empty_calibration_prms_file(replace = replace)
        clbr <- fread("./simulation/calibration_prms.csv",
          colClasses = list(numeric = c(
            "chd_incd_clbr_fctr", "stroke_incd_clbr_fctr",
            "chd_ftlt_clbr_fctr", "stroke_ftlt_clbr_fctr",
            "nonmodelled_ftlt_clbr_fctr"
          ))
        )

        memedian <- function(x) {
          out <- median(x)
          if (out == 0L) out <- mean(x)
          out
        }
        if (replace) {
          age_start <- self$design$sim_prm$ageL
        } else { # if replace == FALSE
          # if all ages exist skip calibration
          if (dim(clbr[chd_incd_clbr_fctr == 1 | stroke_incd_clbr_fctr == 1 | chd_ftlt_clbr_fctr == 1 | stroke_ftlt_clbr_fctr == 1 | nonmodelled_ftlt_clbr_fctr == 1])[1] == 0) {
            message("All ages have been calibrated. Skipping calibration.")
            return(invisible(self))
          }
          age_start <- clbr[chd_incd_clbr_fctr == 1 | stroke_incd_clbr_fctr == 1 | chd_ftlt_clbr_fctr == 1 | stroke_ftlt_clbr_fctr == 1 | nonmodelled_ftlt_clbr_fctr == 1, min(age)] # Unsafe but rarely
          message(paste0("Starting calibration from age ", age_start, "."))
        }

        # Run the simulation from min to max age
        for (age_ in age_start:self$design$sim_prm$ageH) {
          # Run the simulation and export summaries. TODO restrict ages for efficiency.
          self$
            del_logs()$
            del_outputs()$
            run(mc, multicore = TRUE, "sc0")$
            export_summaries(multicore = TRUE, type = c("incd", "prvl", "dis_mrtl"), single_year_of_age = TRUE) #

          # Incidence calibration
          # load the uncalibrated results
          unclbr <- fread(file.path(self$design$sim_prm$output_dir, "summaries", "incd_scaled_up.csv.gz"),
            select = c("year", "age", "sex", "mc", "popsize", "chd_incd", "stroke_incd")
          )
          unclbr <- unclbr[age == age_, .(chd_incd = chd_incd / popsize, stroke_incd = stroke_incd / popsize), keyby = .(age, sex, year, mc)][, .(chd_incd = memedian(chd_incd), stroke_incd = memedian(stroke_incd)), keyby = .(age, sex, year)]

          # for CHD
          # fit a log-log linear model to the uncalibrated results and store the coefficients
          unclbr[chd_incd > 0, c("intercept_unclbr", "trend_unclbr") := as.list(coef(lm(log(chd_incd) ~ log(year)))), by = sex]
          unclbr[, intercept_unclbr := nafill(intercept_unclbr, "const", max(intercept_unclbr, na.rm = TRUE)), by = sex] # NOTE I use max just to return a value. It doesn't matter what value it is.
          unclbr[, trend_unclbr := nafill(trend_unclbr, "const", max(trend_unclbr, na.rm = TRUE)), by = sex] # NOTE I use max just to return a value. It doesn't matter what value it is.
          # load benchmark
          benchmark <- read_fst(file.path("./inputs/disease_burden", "chd_incd.fst"), columns = c("age", "sex", "year", "mu"), as.data.table = TRUE)[age == age_, ]
          # fit a log-log linear model to the benchmark incidence and store the coefficients
          benchmark[year >= self$design$sim_prm$init_year_long, c("intercept_bnchmrk", "trend_bnchmrk") := as.list(coef(lm(log(mu) ~ log(year)))), by = sex]
          # calculate the calibration factors that the uncalibrated log-log model
          # need to be multiplied with so it can match the benchmark log-log model
          unclbr[benchmark[year == max(year)], chd_incd_clbr_fctr := exp(intercept_bnchmrk + trend_bnchmrk * log(year)) / exp(intercept_unclbr + trend_unclbr * log(year)), on = c("age", "sex")] # Do not join on year!
          unclbr[, c("intercept_unclbr", "trend_unclbr") := NULL]

          # Repeat for stroke
          unclbr[stroke_incd > 0, c("intercept_unclbr", "trend_unclbr") := as.list(coef(lm(log(stroke_incd) ~ log(year)))), by = sex]
          unclbr[, intercept_unclbr := nafill(intercept_unclbr, "const", max(intercept_unclbr, na.rm = TRUE)), by = sex] # NOTE I use max just to return a value. It doesn't matter what value it is.
          unclbr[, trend_unclbr := nafill(trend_unclbr, "const", max(trend_unclbr, na.rm = TRUE)), by = sex] # NOTE I use max just to return a value. It doesn't matter what value it is.
          benchmark <- read_fst(file.path("./inputs/disease_burden", "stroke_incd.fst"), columns = c("age", "sex", "year", "mu"), as.data.table = TRUE)[age == age_, ]
          benchmark[year >= self$design$sim_prm$init_year_long, c("intercept_bnchmrk", "trend_bnchmrk") := as.list(coef(lm(log(mu) ~ log(year)))), by = sex]
          unclbr[benchmark[year == max(year)], stroke_incd_clbr_fctr := exp(intercept_bnchmrk + trend_bnchmrk * log(year)) / exp(intercept_unclbr + trend_unclbr * log(year)), on = c("age", "sex")] # Do not join on year!

          # keep only year, age, sex, and calibration factors (to be multiplied
          # with p0)
          unclbr[, `:=`(
            chd_prvl_correction = chd_incd * (chd_incd_clbr_fctr - 1),
            stroke_prvl_correction = stroke_incd * (stroke_incd_clbr_fctr - 1),
            chd_incd = NULL, stroke_incd = NULL, intercept_unclbr = NULL, trend_unclbr = NULL
          )]
          clbr[unclbr, on = c("year", "age", "sex"), `:=`(
            chd_incd_clbr_fctr = i.chd_incd_clbr_fctr,
            stroke_incd_clbr_fctr = i.stroke_incd_clbr_fctr
          )]

          # Case fatality calibration
          # Because we do incd and case fatality correction in the same step, we
          # need to estimate the expected changes on prvl because of the incd
          # calibration, before we proceed with the case fatality calibration.
          # Note that the calibration factor (multiplier) is 1/prvl as we
          # currently have mortality rates in the ftlt files.

          prvl <- fread(file.path(self$design$sim_prm$output_dir, "summaries", "prvl_scaled_up.csv.gz"),
            select = c("year", "age", "sex", "mc", "popsize", "chd_prvl", "stroke_prvl")
          )[age == age_, ]

          # prvl <- prvl[, `:=` (
          #   chd_ftlt_clbr_fctr = (chd_prvl - chd_prvl*((stroke_mrtl + nonmodelled_mrtl)/popsize) + chd_prvl_correction * popsize)/chd_mrtl,
          #   stroke_ftlt_clbr_fctr = (stroke_prvl - stroke_prvl*((chd_mrtl + nonmodelled_mrtl)/popsize) + stroke_prvl_correction * popsize)/stroke_mrtl,
          #   nonmodelled_ftlt_clbr_fctr = popsize/(popsize - chd_mrtl - stroke_mrtl)
          #   )][, .(chd_ftlt_clbr_fctr = mean(chd_ftlt_clbr_fctr),
          #                 stroke_ftlt_clbr_fctr = mean(stroke_ftlt_clbr_fctr),
          #                 nonmodelled_ftlt_clbr_fctr = mean(nonmodelled_ftlt_clbr_fctr)),
          #                  keyby = .(age, sex, year)]

          prvl <- prvl[, .(
            chd_prvl = chd_prvl / popsize,
            stroke_prvl = stroke_prvl / popsize,
            popsize, age, sex, year, mc
          )][, .(
            chd_prvl = memedian(chd_prvl),
            stroke_prvl = memedian(stroke_prvl),
            popsize = memedian(popsize)
          ), keyby = .(age, sex, year)]
          prvl[unclbr, on = c("year", "age", "sex"), `:=`(
            chd_prvl_correction = i.chd_prvl_correction, # Note corrections for prvl are rates
            stroke_prvl_correction = i.stroke_prvl_correction
          )]
          benchmark <- read_fst(file.path("./inputs/disease_burden", "chd_ftlt.fst"), columns = c("age", "sex", "year", "mu2"), as.data.table = TRUE)[age == age_, ]
          prvl[benchmark, on = c("age", "sex", "year"), chd_mrtl := mu2]
          benchmark <- read_fst(file.path("./inputs/disease_burden", "stroke_ftlt.fst"), columns = c("age", "sex", "year", "mu2"), as.data.table = TRUE)[age == age_, ]
          prvl[benchmark, on = c("age", "sex", "year"), stroke_mrtl := mu2]
          benchmark <- read_fst(file.path("./inputs/disease_burden", "nonmodelled_ftlt.fst"), columns = c("age", "sex", "year", "mu2"), as.data.table = TRUE)[age == age_, ]
          prvl[benchmark, on = c("age", "sex", "year"), nonmodelled_mrtl := mu2]



          prvl[, `:=`(
            chd_ftlt_clbr_fctr = 1 / (chd_prvl + chd_prvl_correction), #  - stroke_mrtl - nonmodelled_mrtl
            stroke_ftlt_clbr_fctr = 1 / (stroke_prvl + stroke_prvl_correction), #  - chd_mrtl - nonmodelled_mrtl
            nonmodelled_ftlt_clbr_fctr = 1 / (1 - chd_mrtl - stroke_mrtl)
          )]

          # Fix the calibration factors for the ages that have been calibrated
          if (age_ > age_start) {
            # NOTE here age is age_1L
            mrtl <- fread(file.path(self$design$sim_prm$output_dir, "summaries", "dis_mrtl_scaled_up.csv.gz"),
              select = c("year", "age", "sex", "mc", "popsize", "chd_deaths", "stroke_deaths", "nonmodelled_deaths")
            )[age == age_ - 1L, ]
            mrtl <- mrtl[, .(
              chd_mrtl = chd_deaths / popsize,
              stroke_mrtl = stroke_deaths / popsize,
              nonmodelled_mrtl = nonmodelled_deaths / popsize,
              popsize, age, sex, year, mc
            )][, .(
              chd_mrtl = memedian(chd_mrtl),
              stroke_mrtl = memedian(stroke_mrtl),
              nonmodelled_mrtl = memedian(nonmodelled_mrtl),
              popsize = memedian(popsize)
            ), keyby = .(age, sex, year)]
            benchmark <- read_fst(file.path("./inputs/disease_burden", "chd_ftlt.fst"), columns = c("age", "sex", "year", "mu2"), as.data.table = TRUE)[age == age_ - 1L, ]
            mrtl[benchmark, on = c("age", "sex", "year"), chd_ftlt_clbr_fctr := mu2 / chd_mrtl]
            benchmark <- read_fst(file.path("./inputs/disease_burden", "stroke_ftlt.fst"), columns = c("age", "sex", "year", "mu2"), as.data.table = TRUE)[age == age_ - 1L, ]
            mrtl[benchmark, on = c("age", "sex", "year"), stroke_ftlt_clbr_fctr := mu2 / stroke_mrtl]
            benchmark <- read_fst(file.path("./inputs/disease_burden", "nonmodelled_ftlt.fst"), columns = c("age", "sex", "year", "mu2"), as.data.table = TRUE)[age == age_ - 1L, ]
            mrtl[benchmark, on = c("age", "sex", "year"), nonmodelled_ftlt_clbr_fctr := mu2 / nonmodelled_mrtl]
            mrtl[chd_ftlt_clbr_fctr == Inf, chd_ftlt_clbr_fctr := 1] # to avoid Inf through division by 0
            mrtl[stroke_ftlt_clbr_fctr == Inf, stroke_ftlt_clbr_fctr := 1] # to avoid Inf through division by 0
            mrtl[nonmodelled_ftlt_clbr_fctr == Inf, nonmodelled_ftlt_clbr_fctr := 1] # to avoid Inf through division by 0

            clbr[mrtl, on = c("year", "age", "sex"), `:=`(
              chd_ftlt_clbr_fctr = i.chd_ftlt_clbr_fctr * chd_ftlt_clbr_fctr,
              stroke_ftlt_clbr_fctr = i.stroke_ftlt_clbr_fctr * stroke_ftlt_clbr_fctr,
              nonmodelled_ftlt_clbr_fctr = i.nonmodelled_ftlt_clbr_fctr * nonmodelled_ftlt_clbr_fctr
            )]
          }

          if (age_ == self$design$sim_prm$ageH) {
            # shortcut for age == 99 hopefully with tiny bias
            mrtl[, age := age + 1L]
            prvl[mrtl, on = c("year", "age", "sex"), `:=`(
              chd_ftlt_clbr_fctr = i.chd_ftlt_clbr_fctr * chd_ftlt_clbr_fctr,
              stroke_ftlt_clbr_fctr = i.stroke_ftlt_clbr_fctr * stroke_ftlt_clbr_fctr,
              nonmodelled_ftlt_clbr_fctr = i.nonmodelled_ftlt_clbr_fctr * nonmodelled_ftlt_clbr_fctr
            )]
          }

          clbr[prvl, on = c("year", "age", "sex"), `:=`(
            chd_ftlt_clbr_fctr = i.chd_ftlt_clbr_fctr,
            stroke_ftlt_clbr_fctr = i.stroke_ftlt_clbr_fctr,
            nonmodelled_ftlt_clbr_fctr = i.nonmodelled_ftlt_clbr_fctr
          )]

          fwrite(clbr, "./simulation/calibration_prms.csv") # NOTE this needs to be inside the loop so it influences the simulation during the loop over ages
        } # end loop over ages

        self$design$sim_prm$export_xps <- export_xps # restore the original value
        invisible(self)
      },

      # export_summaries ----

      #' @description Process the lifecourse files
      #' @param multicore If TRUE run the simulation in parallel.
      #' @param type The type of summary to extract.
      #' @param single_year_of_age Export summaries by single year of age. Useful for the calibration proccess.
      #' @return The invisible self for chaining.
      export_summaries = function(multicore = TRUE,
                                  type = c(
                                    "le", "hle", "dis_char", "prvl",
                                    "incd", "dis_mrtl", "mrtl",
                                    "allcause_mrtl_by_dis", "cms", "qalys", "costs"
                                  ),
                                  single_year_of_age = FALSE) {
        fl <- list.files(private$output_dir("lifecourse"), full.names = TRUE)

        # logic to avoid inappropriate dual processing of already processed mcs
        # TODO take into account scenarios
        if ("le" %in% type) {
          file_pth <- private$output_dir("summaries/le_scaled_up.csv.gz")
        } else if ("hle" %in% type) {
          file_pth <- private$output_dir("summaries/hle_1st_cond_scaled_up.csv.gz")
        } else if ("cms" %in% type) {
          file_pth <- private$output_dir("summaries/cms_count_scaled_up.csv.gz")
        } else if ("mrtl" %in% type) {
          file_pth <- private$output_dir("summaries/mrtl_scaled_up.csv.gz")
        } else if ("dis_mrtl" %in% type) {
          file_pth <- private$output_dir("summaries/dis_mrtl_scaled_up.csv.gz")
        } else if ("dis_char" %in% type) {
          file_pth <- private$output_dir("summaries/dis_characteristics_scaled_up.csv.gz")
        } else if ("incd" %in% type) {
          file_pth <- private$output_dir("summaries/incd_scaled_up.csv.gz")
        } else if ("prvl" %in% type) {
          file_pth <- private$output_dir("summaries/prvl_scaled_up.csv.gz")
        } else if ("allcause_mrtl_by_dis" %in% type) {
          file_pth <- private$output_dir("summaries/all_cause_mrtl_by_dis_scaled_up.csv.gz")
        } else if ("qalys" %in% type) {
          file_pth <- private$output_dir("summaries/qalys_scaled_up.csv.gz")
        } else if ("costs" %in% type) {
          file_pth <- private$output_dir("summaries/costs_scaled_up.csv.gz")
        } else {
          stop("Unknown type of summary")
        }


        if (file.exists(file_pth)) {
          tt <- unique(fread(file_pth, select = "mc")$mc)
          for (i in seq_along(tt)) {
            fl <- grep(paste0("/", tt[[i]], "_lifecourse.csv.gz$"), fl,
              value = TRUE, invert = TRUE
            )
          }
        }
        # end of logic

        if (multicore) {
          if (self$design$sim_prm$logs) {
            private$time_mark("Start exporting summaries")
          }

          if (.Platform$OS.type == "windows") {
            cl <-
              makeClusterPSOCK(
                self$design$sim_prm$clusternumber_export,
                dryrun = FALSE,
                quiet = FALSE,
                rscript_startup = quote(local({
                  library(CKutils)
                  library(IMPACTncdJapan)
                  library(digest)
                  library(fst)
                  library(qs)
                  library(wrswoR)
                  library(gamlss.dist)
                  library(dqrng)
                  library(data.table)
                })),
                rscript_args = c(
                  "--no-init-file",
                  "--no-site-file",
                  "--no-environ"
                ),
                setup_strategy = "parallel"
              ) # used for clustering. Windows compatible

            on.exit(if (exists("cl")) stopCluster(cl))

            parLapplyLB(
              cl = cl,
              X = seq_along(fl),
              fun = function(i) {
                lc <- fread(fl[i], stringsAsFactors = TRUE, key = c("scenario", "pid", "year"))
                private$export_summaries_hlpr(lc, type = type, single_year_of_age = single_year_of_age)
                NULL
              }
            )
          } else {
            registerDoParallel(self$design$sim_prm$clusternumber_export) # used for forking. Only Linux/OSX compatible
            xps_dt <- foreach(
              i = seq_along(fl),
              .inorder = TRUE,
              .options.multicore = list(preschedule = FALSE),
              .verbose = self$design$sim_prm$logs,
              .packages = c(
                "R6",
                "CKutils",
                "IMPACTncdJapan",
                "data.table"
              ),
              .export = NULL,
              .noexport = NULL # c("time_mark")
            ) %dopar% {
              lc <- fread(fl[i], stringsAsFactors = TRUE, key = c("scenario", "pid", "year"))
              private$export_summaries_hlpr(lc, type = type, single_year_of_age = single_year_of_age)
              NULL
            }
          }





          if (self$design$sim_prm$logs) {
            private$time_mark("End of exporting summuries")
          }
        } else {
          if (self$design$sim_prm$logs) {
            private$time_mark("Start of single-core run")
          }

          lapply(seq_along(fl), function(i) {
            lc <- fread(fl[i], stringsAsFactors = TRUE, key = c("pid", "year"))
            private$export_summaries_hlpr(lc, type = type, single_year_of_age = single_year_of_age)
            NULL
          })

          if (self$design$sim_prm$logs) {
            private$time_mark("End of single-core run")
          }
        }

        if (self$design$sim_prm$avoid_appending_csv) {
          # collect the summary fragmentrd file
          if ("le" %in% type) {
            private$collect_files("summaries", "_le_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_le_esp.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_le60_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_le60_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("hle" %in% type) {
            private$collect_files("summaries", "_hle_1st_cond_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_hle_1st_cond_esp.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_hle_cmsmm1.5_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_hle_cmsmm1.5_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("cms" %in% type) {
            private$collect_files("summaries", "_cms_score_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_cms_score_esp.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_cms_score_by_age_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_cms_score_by_age_esp.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_cms_count_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_cms_count_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("mrtl" %in% type) {
            private$collect_files("summaries", "_mrtl_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_mrtl_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("dis_mrtl" %in% type) {
            private$collect_files("summaries", "_dis_mrtl_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_dis_mrtl_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("dis_char" %in% type) {
            private$collect_files("summaries", "_dis_characteristics_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_dis_characteristics_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("incd" %in% type) {
            private$collect_files("summaries", "_incd_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_incd_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("prvl" %in% type) {
            private$collect_files("summaries", "_prvl_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_prvl_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("allcause_mrtl_by_dis" %in% type) {
            private$collect_files("summaries", "_all_cause_mrtl_by_dis_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_all_cause_mrtl_by_dis_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("qalys" %in% type) {
            private$collect_files("summaries", "_qalys_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_qalys_esp.csv$", to_mc_aggr = FALSE)
          }
          if ("costs" %in% type) {
            private$collect_files("summaries", "_costs_scaled_up.csv$", to_mc_aggr = FALSE)
            private$collect_files("summaries", "_costs_esp.csv$", to_mc_aggr = FALSE)
          }

          if (self$design$sim_prm$logs) {
            private$time_mark("End of collecting mc_aggr summary files")
          }
        }

        while (sink.number() > 0L) sink()

        invisible(self)
      },

      # get_causal_structure ----

      #' @description Returns the causality matrix and optionally plots the
      #'   causality structure.
      #' @param processed If `TRUE` generates the causality matrix from the
      #'   graph.
      #' @param print_plot If `TRUE` prints the causal structure graph.
      #' @param focus If missing the whole causal structure is returned.
      #'  Otherwise, if a named node only the subgraph of the 1st order
      #'  neighbours that point to the given vertrice is returned.
      #' @return The processed causality matrix if `processed = TRUE` or the
      #'   graph otherwise.
      get_causal_structure = function(processed = TRUE, print_plot = FALSE, focus = FALSE) {
        if (missing(focus)) {
          graph <- private$causality_structure
        } else {
          if (length(focus) > 1L) stop("focus need to be scalar string.")
          if (!focus %in% self$get_node_names()) stop("focus need to be a node name. Use get_node_names() to get the list of eligible values.")
          graph <- make_ego_graph(private$causality_structure, order = 1, nodes = focus, mode = "in")[[1]]
        }
        if (print_plot) {
          print(
            plot(
              graph,
              vertex.shape = "none",
              edge.arrow.size = .3,
              vertex.label.font = 2,
              vertex.label.color = "gray40",
              edge.arrow.width = .5,
              vertex.label.cex = .7,
              edge.color = "gray85",
              layout = layout_components
            )
          )
        }

        if (processed) {
          graph <- as.matrix(as_adjacency_matrix(graph))
          n <- sapply(self$diseases, `[[`, "name")
          graph <- graph[rowSums(graph) > 0, colnames(graph) %in% n]
        }

        return(graph)
      },

      # get_node_names ----

      #' @description Returns the names of all exposures and diseases.
      #' @return A string vector.
      get_node_names = function() {
        return(V(private$causality_structure)$name)
      },

      # get_causal_path ----

      #' @description Returns the causal paths between an exposure and an outcome (disease).
      #' @param from the beginning of the path (an exposure) as a string. Use `get_node_names` for available nodes.
      #' @param to the end of the path (a disease) as a string. Use `get_node_names` for available nodes.
      #' @param shortest_paths Boolean. If true, only returns the paths with the smallest number of nodes. Else, all possible paths (excluding multiple and loop edges) are returned.
      #' @return A list with all the possible paths between exposure and disease.
      get_causal_path = function(from, to, shortest_paths = FALSE) {
        nm <- V(private$causality_structure)$name
        from <- which(nm == from)
        to <- which(nm == to)
        if (shortest_paths) {
          out <- get.all.shortest.paths(private$causality_structure, from, to, mode = "out")
        } else {
          out <- all_simple_paths(private$causality_structure, from, to, mode = "out")
        }
        return(out)
      },

      # update_design ----

      #' @description Updates the Design object that is stored in the Simulation
      #'   object.
      #' @param new_design A design object with the simulation parameters.
      #' @return The invisible self for chaining.
      update_design = function(new_design) {
        if (!inherits(new_design, "Design")) {
          stop("Argument new_design needs to be a Design object.")
        }

        self$design <- new_design

        invisible(self)
      },

      # del_outputs ----

      #' @description Delete all output files.
      #' @return The invisible self for chaining.
      del_outputs = function() {
        if (dir.exists(self$design$sim_prm$output_dir)) {
          fl <- list.files(self$design$sim_prm$output_dir,
            full.names = TRUE,
            recursive = TRUE
          )

          file.remove(fl)

          if (length(fl) > 0 && self$design$sim_prm$logs) {
            message("Output files deleted.")
          }
        } else {
          message("Output folder doesn't exist.")
        }

        invisible(self)
      },

      # del_logs ----
      #' @description Delete log files.
      #' @return The invisible self for chaining.
      del_logs = function() {
        fl <- list.files(private$output_dir("logs/"), full.names = TRUE)

        file.remove(fl)

        if (length(fl) > 0 && self$design$sim_prm$logs) {
          message("Log files deleted.")
        }

        invisible(self)
      },

      # del_parfs ----
      #' @description Delete all files in the ./simulation/parf folder.
      #' @return The invisible self for chaining.
      del_parfs = function() {
        fl <- list.files("./simulation/parf", full.names = TRUE)

        file.remove(fl)

        if (length(fl) > 0 && self$design$sim_prm$logs) {
          message("Parf files deleted.")
        }

        invisible(self)
      },

      # del_RR_cache ----
      #' @description Delete all files in the ./simulation/rr folder.
      #' @return The invisible self for chaining.
      del_RR_cache = function() {
        fl <- list.files("./simulation/rr", full.names = TRUE)

        file.remove(fl)

        if (length(fl) > 0 && self$design$sim_prm$logs) {
          message("RR cache files deleted.")
        }

        invisible(self)
      },

      # del_synthpops ----
      #' @description Delete all files in the synthpop folder.
      #' @return The invisible self for chaining.
      del_synthpops = function() {
        fl <- list.files(self$design$sim_prm$synthpop_dir, full.names = TRUE)

        file.remove(fl)

        if (length(fl) > 0 && self$design$sim_prm$logs) {
          message("Sythpop files deleted.")
        }

        invisible(self)
      },

      # get_esp ----

      #' @description Get the European Standardised Population 2013 by sex and
      #'   dimd.
      #' @return A data.table with the European Standardised Population 2013.
      get_esp = function() {
        private$esp_weights
      },

      # get_mm_weights ----

      #' @description Get the disease multimorbidity weights (i.e. Cambridge
      #'   Morbidity Score weights).
      #' @return A named vector with disease weights.
      get_mm_weights = function() {
        unlist(sapply(self$diseases, function(x) x$meta$diagnosis$mm_wt))
      },

      #' @description Internal validation of the disease burden.
      #' @return The invisible self for chaining.
      # validate ----
      validate = function() {
        HEIGHT <- 5
        WIDTH <- 10


        data_pop <- read_fst("./inputs/pop_projections/combined_population_japan.fst", columns = c("year", "age", "sex", "pops"), as.data.table = TRUE)
        data_pop_agegrp <- copy(data_pop)
        to_agegrp(data_pop_agegrp, 5, 99)
        data_pop_agegrp <- data_pop_agegrp[, .(pops = sum(pops)), keyby = .(year, agegrp, sex)]

        # MRTL
        mdd <- fread(file.path(self$design$sim_prm$output_dir, "summaries", "dis_mrtl_scaled_up.csv.gz"))
        mdd[, `:=`(
          nonmodelled_mrtl_rate = nonmodelled_deaths / popsize,
          chd_mrtl_rate = chd_deaths / popsize,
          stroke_mrtl_rate = stroke_deaths / popsize
        )]
        mdd <- mdd[scenario == "sc0", .(
          nonmodelled_mrtl_rate = quantile(nonmodelled_mrtl_rate, p = 0.500),
          nonmodelled_mrtl_rate_low = quantile(nonmodelled_mrtl_rate, p = 0.025),
          nonmodelled_mrtl_rate_upp = quantile(nonmodelled_mrtl_rate, p = 0.975),
          chd_mrtl_rate = quantile(chd_mrtl_rate, p = 0.500),
          chd_mrtl_rate_low = quantile(chd_mrtl_rate, p = 0.025),
          chd_mrtl_rate_upp = quantile(chd_mrtl_rate, p = 0.957),
          stroke_mrtl_rate = quantile(stroke_mrtl_rate, p = 0.500),
          stroke_mrtl_rate_low = quantile(stroke_mrtl_rate, p = 0.025),
          stroke_mrtl_rate_upp = quantile(stroke_mrtl_rate, p = 0.975),
          type = "modelled"
        ), keyby = .(year, agegrp, sex)]

        obs <- read_fst(paste0("./inputs/disease_burden/", "chd_ftlt.fst"), columns = c("age", "year", "sex", "mu2", "mu_lower", "mu_upper"), as.data.table = TRUE)
        setnames(obs, c("mu2", "mu_lower", "mu_upper"), c("chd_mrtl_rate", "chd_mrtl_rate_low", "chd_mrtl_rate_upp"))
        tt <- read_fst(paste0("./inputs/disease_burden/", "stroke_ftlt.fst"), columns = c("age", "year", "sex", "mu2", "mu_lower", "mu_upper"), as.data.table = TRUE)
        setnames(tt, c("mu2", "mu_lower", "mu_upper"), c("stroke_mrtl_rate", "stroke_mrtl_rate_low", "stroke_mrtl_rate_upp"))
        absorb_dt(obs, tt)
        tt <- read_fst(paste0("./inputs/disease_burden/", "nonmodelled_ftlt.fst"), columns = c("age", "year", "sex", "mu2", "mu_lower", "mu_upper"), as.data.table = TRUE)
        setnames(tt, c("mu2", "mu_lower", "mu_upper"), c("nonmodelled_mrtl_rate", "nonmodelled_mrtl_rate_low", "nonmodelled_mrtl_rate_upp"))
        absorb_dt(obs, tt)
        absorb_dt(obs, data_pop)
        to_agegrp(obs, 5, 99)
        obs <- obs[, lapply(.SD, weighted.mean, w = pops), .SDcols = -c("pops", "age"), keyby = .(agegrp, year, sex)]
        obs[, type := "observed"]
        dt <- rbindlist(list(obs, mdd), use.names = TRUE)

        p <- ggplot() +
          geom_line(data = dt[sex == "men"], aes(x = year, y = chd_mrtl_rate, color = type)) +
          geom_line(data = dt[sex == "men"], aes(x = year, y = chd_mrtl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "men"], aes(x = year, y = chd_mrtl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD mrtl rate", "Men")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "CHD_as_men_mrtl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "women"], aes(x = year, y = chd_mrtl_rate, color = type)) +
          geom_line(data = dt[sex == "women"], aes(x = year, y = chd_mrtl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "women"], aes(x = year, y = chd_mrtl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD mrtl rate", "Women")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "CHD_as_women_mrtl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "men"], aes(x = year, y = stroke_mrtl_rate, color = type)) +
          geom_line(data = dt[sex == "men"], aes(x = year, y = stroke_mrtl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "men"], aes(x = year, y = stroke_mrtl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke mrtl rate", "Men")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "stroke_as_men_mrtl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "women"], aes(x = year, y = stroke_mrtl_rate, color = type)) +
          geom_line(data = dt[sex == "women"], aes(x = year, y = stroke_mrtl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "women"], aes(x = year, y = stroke_mrtl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke mrtl rate", "Women")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "stroke_as_women_mrtl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "men"], aes(x = year, y = nonmodelled_mrtl_rate, color = type)) +
          geom_line(data = dt[sex == "men"], aes(x = year, y = nonmodelled_mrtl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "men"], aes(x = year, y = nonmodelled_mrtl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Nonmodelled mrtl rate", "Men")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "nonmodelled_as_men_mrtl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "women"], aes(x = year, y = nonmodelled_mrtl_rate, color = type)) +
          geom_line(data = dt[sex == "women"], aes(x = year, y = nonmodelled_mrtl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "women"], aes(x = year, y = nonmodelled_mrtl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Nonmodelled mrtl rate", "Women")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "nonmodelled_as_women_mrtl.jpg"), p, height = HEIGHT, width = WIDTH)

        # MRTL by sex alone
        absorb_dt(dt, data_pop_agegrp)
        dt <- dt[, lapply(.SD, weighted.mean, w = pops), .SDcols = -c("pops", "agegrp"), keyby = .(type, year, sex)]

        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = chd_mrtl_rate, color = type)) +
          geom_line(data = dt, aes(x = year, y = chd_mrtl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt, aes(x = year, y = chd_mrtl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD mrtl rate", "By sex")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "CHD_s_mrtl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = stroke_mrtl_rate, color = type)) +
          geom_line(data = dt, aes(x = year, y = stroke_mrtl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt, aes(x = year, y = stroke_mrtl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke mrtl rate", "By sex")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "stroke_s_mrtl.jpg"), p, height = HEIGHT, width = WIDTH)


        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = nonmodelled_mrtl_rate, color = type)) +
          geom_line(data = dt, aes(x = year, y = nonmodelled_mrtl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt, aes(x = year, y = nonmodelled_mrtl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Nonmodelled mrtl rate", "By sex")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "nonmodelled_s_mrtl.jpg"), p, height = HEIGHT, width = WIDTH)



        # INCD

        mdd <- fread(file.path(self$design$sim_prm$output_dir, "summaries", "incd_scaled_up.csv.gz"), select = c("mc", "scenario", "year", "agegrp", "sex", "popsize", "chd_incd", "stroke_incd"))
        mdd[, `:=`(
          chd_incd_rate = chd_incd / popsize,
          stroke_incd_rate = stroke_incd / popsize
        )]
        mdd <- mdd[scenario == "sc0", .(
          chd_incd_rate = quantile(chd_incd_rate, p = 0.500),
          chd_incd_rate_low = quantile(chd_incd_rate, p = 0.025),
          chd_incd_rate_upp = quantile(chd_incd_rate, p = 0.957),
          stroke_incd_rate = quantile(stroke_incd_rate, p = 0.500),
          stroke_incd_rate_low = quantile(stroke_incd_rate, p = 0.025),
          stroke_incd_rate_upp = quantile(stroke_incd_rate, p = 0.975),
          type = "modelled"
        ), keyby = .(year, agegrp, sex)]

        obs <- read_fst(paste0("./inputs/disease_burden/", "chd_incd.fst"), columns = c("age", "year", "sex", "mu", "mu_lower", "mu_upper"), as.data.table = TRUE)
        setnames(obs, c("mu", "mu_lower", "mu_upper"), c("chd_incd_rate", "chd_incd_rate_low", "chd_incd_rate_upp"))
        tt <- read_fst(paste0("./inputs/disease_burden/", "stroke_incd.fst"), columns = c("age", "year", "sex", "mu", "mu_lower", "mu_upper"), as.data.table = TRUE)
        setnames(tt, c("mu", "mu_lower", "mu_upper"), c("stroke_incd_rate", "stroke_incd_rate_low", "stroke_incd_rate_upp"))
        absorb_dt(obs, tt)
        absorb_dt(obs, data_pop)
        to_agegrp(obs, 5, 99)
        obs <- obs[, lapply(.SD, weighted.mean, w = pops), .SDcols = -c("pops", "age"), keyby = .(agegrp, year, sex)]
        obs[, type := "observed"]
        dt <- rbindlist(list(obs, mdd), use.names = TRUE)

        p <- ggplot() +
          geom_line(data = dt[sex == "men"], aes(x = year, y = chd_incd_rate, color = type)) +
          geom_line(data = dt[sex == "men"], aes(x = year, y = chd_incd_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "men"], aes(x = year, y = chd_incd_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD incd rate", "Men")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "CHD_as_men_incd.jpg"), p, height = HEIGHT, width = WIDTH)


        p <- ggplot() +
          geom_line(data = dt[sex == "women"], aes(x = year, y = chd_incd_rate, color = type)) +
          geom_line(data = dt[sex == "women"], aes(x = year, y = chd_incd_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "women"], aes(x = year, y = chd_incd_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD incd rate", "Women")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "CHD_as_women_incd.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "men"], aes(x = year, y = stroke_incd_rate, color = type)) +
          geom_line(data = dt[sex == "men"], aes(x = year, y = stroke_incd_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "men"], aes(x = year, y = stroke_incd_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke incd rate", "Men")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "stroke_as_men_incd.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "women"], aes(x = year, y = stroke_incd_rate, color = type)) +
          geom_line(data = dt[sex == "women"], aes(x = year, y = stroke_incd_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "women"], aes(x = year, y = stroke_incd_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke incd rate", "Women")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "stroke_as_women_incd.jpg"), p, height = HEIGHT, width = WIDTH)

        # INCD by sex alone
        absorb_dt(dt, data_pop_agegrp)
        dt <- dt[, lapply(.SD, weighted.mean, w = pops), .SDcols = -c("pops", "agegrp"), keyby = .(type, year, sex)]

        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = chd_incd_rate, color = type)) +
          geom_line(data = dt, aes(x = year, y = chd_incd_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt, aes(x = year, y = chd_incd_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD incd rate", "By sex")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "CHD_s_incd.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = stroke_incd_rate, color = type)) +
          geom_line(data = dt, aes(x = year, y = stroke_incd_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt, aes(x = year, y = stroke_incd_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke incd rate", "By sex")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "stroke_s_incd.jpg"), p, height = HEIGHT, width = WIDTH)


        # PRVL
        mdd <- fread(file.path(self$design$sim_prm$output_dir, "summaries", "prvl_scaled_up.csv.gz"), select = c("mc", "scenario", "year", "agegrp", "sex", "popsize", "chd_prvl", "stroke_prvl"))
        mdd[, `:=`(
          chd_prvl_rate = chd_prvl / popsize,
          stroke_prvl_rate = stroke_prvl / popsize
        )]
        mdd <- mdd[scenario == "sc0", .(
          chd_prvl_rate = quantile(chd_prvl_rate, p = 0.500),
          chd_prvl_rate_low = quantile(chd_prvl_rate, p = 0.025),
          chd_prvl_rate_upp = quantile(chd_prvl_rate, p = 0.957),
          stroke_prvl_rate = quantile(stroke_prvl_rate, p = 0.500),
          stroke_prvl_rate_low = quantile(stroke_prvl_rate, p = 0.025),
          stroke_prvl_rate_upp = quantile(stroke_prvl_rate, p = 0.975),
          type = "modelled"
        ), keyby = .(year, agegrp, sex)]

        obs <- read_fst(paste0("./inputs/disease_burden/", "chd_prvl.fst"), columns = c("age", "year", "sex", "mu", "mu_lower", "mu_upper", "prvl_mltp"), as.data.table = TRUE)
        setnames(obs, c("mu", "mu_lower", "mu_upper"), c("chd_prvl_rate", "chd_prvl_rate_low", "chd_prvl_rate_upp"))
        obs[, `:=`(
          chd_prvl_rate = chd_prvl_rate * prvl_mltp,
          chd_prvl_rate_low = chd_prvl_rate_low * prvl_mltp,
          chd_prvl_rate_upp = chd_prvl_rate_upp * prvl_mltp,
          prvl_mltp = NULL
        )]
        tt <- read_fst(paste0("./inputs/disease_burden/", "stroke_prvl.fst"), columns = c("age", "year", "sex", "mu", "mu_lower", "mu_upper", "prvl_mltp"), as.data.table = TRUE)
        setnames(tt, c("mu", "mu_lower", "mu_upper"), c("stroke_prvl_rate", "stroke_prvl_rate_low", "stroke_prvl_rate_upp"))
        tt[, `:=`(
          stroke_prvl_rate = stroke_prvl_rate * prvl_mltp,
          stroke_prvl_rate_low = stroke_prvl_rate_low * prvl_mltp,
          stroke_prvl_rate_upp = stroke_prvl_rate_upp * prvl_mltp,
          prvl_mltp = NULL
        )]
        absorb_dt(obs, tt)
        absorb_dt(obs, data_pop)
        to_agegrp(obs, 5, 99)
        obs <- obs[, lapply(.SD, weighted.mean, w = pops), .SDcols = -c("pops", "age"), keyby = .(agegrp, year, sex)]
        obs[, type := "observed"]
        dt <- rbindlist(list(obs, mdd), use.names = TRUE)

        p <- ggplot() +
          geom_line(data = dt[sex == "men"], aes(x = year, y = chd_prvl_rate, color = type)) +
          geom_line(data = dt[sex == "men"], aes(x = year, y = chd_prvl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "men"], aes(x = year, y = chd_prvl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD prvl rate", "Men")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "CHD_as_men_prvl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "women"], aes(x = year, y = chd_prvl_rate, color = type)) +
          geom_line(data = dt[sex == "women"], aes(x = year, y = chd_prvl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "women"], aes(x = year, y = chd_prvl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD prvl rate", "Women")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "CHD_as_women_prvl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "men"], aes(x = year, y = stroke_prvl_rate, color = type)) +
          geom_line(data = dt[sex == "men"], aes(x = year, y = stroke_prvl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "men"], aes(x = year, y = stroke_prvl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke prvl rate", "Men")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "stroke_as_men_prvl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt[sex == "women"], aes(x = year, y = stroke_prvl_rate, color = type)) +
          geom_line(data = dt[sex == "women"], aes(x = year, y = stroke_prvl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt[sex == "women"], aes(x = year, y = stroke_prvl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke prvl rate", "Women")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "stroke_as_women_prvl.jpg"), p, height = HEIGHT, width = WIDTH)

        # prvl by sex alone
        absorb_dt(dt, data_pop_agegrp)
        dt <- dt[, lapply(.SD, weighted.mean, w = pops), .SDcols = -c("pops", "agegrp"), keyby = .(type, year, sex)]

        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = chd_prvl_rate, color = type)) +
          geom_line(data = dt, aes(x = year, y = chd_prvl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt, aes(x = year, y = chd_prvl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD prvl rate", "By sex")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "CHD_s_prvl.jpg"), p, height = HEIGHT, width = WIDTH)

        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = stroke_prvl_rate, color = type)) +
          geom_line(data = dt, aes(x = year, y = stroke_prvl_rate_low, color = type), linetype = "dashed") +
          geom_line(data = dt, aes(x = year, y = stroke_prvl_rate_upp, color = type), linetype = "dashed") +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke prvl rate", "By sex")
        ggsave(file.path(self$design$sim_prm$output_dir, "plots", "stroke_s_prvl.jpg"), p, height = HEIGHT, width = WIDTH)
        invisible(self)
      },

      # split_large_files ----
      #' @description Splits files larger than 50Mb into chunks of 49Mb.
      #' @details The function splits files larger than 50Mb into chunks of 49Mb
      #' so they can be tracked by GitHub. The large files are deleted and an
      #' index is created at "./simulation/large_files_indx.csv" so they can be
      #' reconstructed. The function also adds the large files to `.gitignore`.
      #' It works on Linux and Windows. Untested on Mac.
      #' @return The invisible `Simulation` object.
      split_large_files = function() {
        # identify large files
        fl <- list.files(".", full.names = TRUE, recursive = TRUE)
        fl <- sort(fl[file.size(fl) / (1024^2) >= 50])
        fl <- grep("/synthpop/|/outputs/", fl, ignore.case = TRUE, value = TRUE, invert = TRUE)      
        if (length(fl) == 0) { # no large files. Early escape.
          return(invisible(self))
        }

        # Merge with existing large files from previous uploads. This ensures
        # that if any previously large files are now smaller than 50MB, they are
        # still processed as if they are still more than 50Mb. This is perhaps
        # inefficient but has less side effects.  Otherwise code for special
        # case of files above the threshold that shrinked for whatever reason
        # below the threshold is needed (i.e. remove them from .gitignore).
        if (file.exists("./simulation/large_files_indx.csv")) {
          fl <- sort(unique(c(fread("./simulation/large_files_indx.csv")$pths, fl)))
        }
        fwrite(list(pths = fl), "./simulation/large_files_indx.csv")

        # add large files to .gitignore
        excl <- readLines("./.gitignore")
        for (i in 1:length(fl)) {
          file <- gsub("^./", "", fl[i])
          if (file %in% excl) next
          write(file, file = "./.gitignore", append = TRUE)
        }

        # split the files into 50MB chunks
        for (i in 1:length(fl)) {
          file <- fl[i]
          if (!file.exists(file)) next

          # split the file into 49MB chunks
          if (.Platform$OS.type == "unix") {
            system(paste0("split -b 49m ", file, " ", file, ".chunk"))
          } else if (.Platform$OS.type == "windows") {
            # For windows split and cat are from https://unxutils.sourceforge.net/
            shell(paste0("split -b 49m ", file, " ", file, ".chunk"))
          } else {
            stop("Operating system is not supported.")
          }
          # remove the original file
          file.remove(file)
        }

        invisible(self)
      },

      # reconstruct_large_files ----
      #' @description Reconstructs large files from chunks.
      #' @details The function reconstructs large files from chunks. The path of
      #' the files are stored in "./simulation/large_files_indx.csv". It works
      #' on Linux and Windows. Untested on Mac.
      #' @return The invisible `Simulation` object.
      reconstruct_large_files = function() {
        if (file.exists("./simulation/large_files_indx.csv")) {
          fl <- fread("./simulation/large_files_indx.csv")$pths
          for (i in 1:length(fl)) {
            if (file.exists(fl[i])) next
            file <- fl[i]
            # recombine the chunks
            if (.Platform$OS.type == "unix") {
              system(paste0("cat ", file, ".chunk?? > ", file, ""))
            } else if (.Platform$OS.type == "windows") {
              # For windows split and cat are from https://unxutils.sourceforge.net/
              shell(paste0("cat ", file, ".chunk?? > ", file, ""))
            } else {
              stop("Operating system is not supported.")
            }
          }
        }
        invisible(self)
      },

      # del_large_files ----
      #' @description Deletes large files.
      #' @details The function deletes large files that are stored in chunks.
      #' The path of the files are stored in
      #' "./simulation/large_files_indx.csv".
      #' @return The invisible `Simulation` object.
      del_large_files = function() {
        if (file.exists("./simulation/large_files_indx.csv")) {
          fl <- fread("./simulation/large_files_indx.csv")$pths
          file.remove(fl)
        }
        invisible(self)
      },

      # print ----
      #' @description Prints the simulation object metadata.
      #' @return The invisible `Simulation` object.
      print = function() {
        print(c(
          "TODO..."
        ))
        invisible(self)
      }
    ),



    # private -----------------------------------------------------------------
    private = list(
      synthpop_dir = NA,
      causality_structure = NA,
      death_codes = NA,
      # diseasenam_hlp = NA,
      esp_weights = data.table(),
      # Models a primary prevention policy scenario
      primary_prevention_scn = NULL,
      # Models a secondary prevention policy scenario
      secondary_prevention_scn = NULL,

      # run_sim ----
      # Runs the simulation in one core. mc is scalar
      run_sim = function(mc_, scenario_nam = "") {
        if (!nzchar(scenario_nam)) scenario_nam <- "sc0"

        if (self$design$sim_prm$logs) {
          private$time_mark(paste0("Start mc iteration ", mc_))
          sink(
            file = private$output_dir(paste0("logs/log", mc_, ".txt")),
            append = TRUE,
            type = "output",
            split = FALSE
          )
        }

        sp <- SynthPop$new(mc_, self$design)

        # From Karl, somehow scenario_fn() makes init_prvl different. The
        # following code solves the problem.
        # TODO: investigate the root cause


        lapply(self$diseases, function(x) {
          if (self$design$sim_prm$logs) print(x$name)
          x$
            gen_parf(sp, self$design, self$diseases)$
            set_init_prvl(sp = sp, design_ = self$design)
        })

        private$primary_prevention_scn(sp) # apply primary pevention scenario

        lapply(self$diseases, function(x) {
          x$set_rr(sp, self$design)$
            set_incd_prb(sp, self$design)$
            set_dgns_prb(sp, self$design)$
            set_mrtl_prb(sp, self$design)
        })

        private$secondary_prevention_scn(sp) # apply secondary pevention scenario

        # ds <- copy(self$diseases) # Necessary for parallelisation
        # lapply(self$diseases, function(x) {
        #   if (self$design$sim_prm$logs) print(x$name)
        #   x$gen_parf(sp, self$design, self$diseases)$
        #     set_init_prvl(sp, self$design)$
        #     set_rr(sp, self$design)$
        #     set_incd_prb(sp, self$design)$
        #     set_dgns_prb(sp, self$design)$
        #     set_mrtl_prb(sp, self$design)
        # })

        l <- private$mk_scenario_init(sp, scenario_nam)
        simcpp(sp$pop, l, sp$mc)
        # it doesn't matter if mc or mc_aggr is used in the above, because it is
        # only used for the RNG stream and the pid are different in each mc_aggr
        # pop

        sp$update_pop_weights(scenario_nam)

        # Prune pop (NOTE that assignment in the function env makes this
        # data.table local)
        sp$pop <- sp$pop[all_cause_mrtl >= 0L &
          year >= self$design$sim_prm$init_year_long &
          between(age, self$design$sim_prm$ageL, self$design$sim_prm$ageH), ]
        setkey(sp$pop, pid, year)
        sp$pop[, pid_mrk := mk_new_simulant_markers(pid)]

        # apply ESP weights
        to_agegrp(sp$pop, 5, 99)
        absorb_dt(sp$pop, private$esp_weights)
        sp$pop[, wt_esp := wt_esp * unique(wt_esp) / sum(wt_esp),
          by = .(year, agegrp, sex)
        ] # NOTE keyby changes the key

        if (self$design$sim_prm$export_xps) {
          if (self$design$sim_prm$logs) message("Exporting exposures...")
          private$export_xps(sp, scenario_nam)
        }

        nam <- c(
          self$design$sim_prm$cols_for_output,
          grep("^cms_|_prvl$|_dgns$|_mrtl$", names(sp$pop), value = TRUE)
        )
        nam <- grep("^prb_", nam, value = TRUE, invert = TRUE) # exclude prb_ ... _dgns
        sp$pop[, setdiff(names(sp$pop), nam) := NULL]
        sp$pop[, mc := sp$mc_aggr]


        # TODO add logic for the years of having MM. Currently 1 is not the real
        # incidence. It is still prevalence
        sp$pop[, `:=`(
          cms1st_cont_prvl = carry_forward_incr(as.integer(cms_count == 1),
            pid_mrk, TRUE, 1L,
            byref = TRUE
          ),
          cmsmm0_prvl = carry_forward_incr(as.integer(cms_score > 0),
            pid_mrk, TRUE, 1L,
            byref = TRUE
          ),
          cmsmm1_prvl = carry_forward_incr(as.integer(cms_score > 1),
            pid_mrk, TRUE, 1L,
            byref = TRUE
          ),
          cmsmm1.5_prvl = carry_forward_incr(as.integer(cms_score > 1.5),
            pid_mrk, TRUE, 1L,
            byref = TRUE
          ),
          cmsmm2_prvl = carry_forward_incr(as.integer(cms_score > 2),
            pid_mrk, TRUE, 1L,
            byref = TRUE
          )
        )]

        sp$pop[, scenario := scenario_nam]

        setkeyv(sp$pop, c("pid", "year"))

        # Write lifecourse
        if (self$design$sim_prm$logs) message("Exporting lifecourse...")

        if (self$design$sim_prm$avoid_appending_csv) {
          fnam <- private$output_dir(paste0(
            "lifecourse/", sp$mc_aggr, "_", sp$mc, "_lifecourse.csv"
          ))
        } else {
          fnam <- private$output_dir(paste0(
            "lifecourse/", sp$mc_aggr, "_lifecourse.csv.gz"
          ))
        }
        fwrite_safe(sp$pop, fnam)


        if (self$design$sim_prm$logs) {
          private$time_mark(paste0("End mc iteration ", mc_))
          sink()
        }

        NULL
      },

      # creates the list that is used in c++ side sp is needed for sp$mc_aggr in
      # to_cpp()

      # mk_scenario_init ----
      mk_scenario_init = function(sp, scenario_name) {
        # scenario_suffix_for_pop <- paste0("_", scenario_name)

        # TODO the next line counteracts the commented line above. This is
        # intentional until we finalise the scenario mechanism
        scenario_suffix_for_pop <- ""

        list(
          "exposures" = self$design$sim_prm$exposures,
          "scenarios" = self$design$sim_prm$scenarios, # to be generated programmatically
          "scenario" = scenario_name,
          "kismet" = self$design$sim_prm$kismet, # If TRUE random numbers are the same for each scenario.
          "init_year" = self$design$sim_prm$init_year_long,
          "pids" = "pid",
          "years" = "year",
          "ages" = "age",
          "sexs" = "sex",
          "ageL" = self$design$sim_prm$ageL,
          "all_cause_mrtl" = paste0("all_cause_mrtl", scenario_suffix_for_pop),
          "cms_score" = paste0("cms_score", scenario_suffix_for_pop),
          "cms_count" = paste0("cms_count", scenario_suffix_for_pop),
          # "strata_for_outputs" = c("pid", "year", "age", "sex", "dimd"),
          "diseases" = lapply(self$diseases, function(x) {
            x$to_cpp(sp, self$design, scenario_name, scenario_suffix_for_pop)
          })
        )
      },

      # export xps ----
      export_xps = function(sp, scenario_nam) {
        # NOTE no need to check validity of inputs here as it is only used
        # internally

        to_agegrp(sp$pop,
          grp_width = 20L, max_age = self$design$sim_prm$ageH,
          min_age = self$design$sim_prm$ageL, age_colname = "age",
          agegrp_colname = "agegrp20", to_factor = TRUE
        )

        sp$pop[, smok_never_curr_xps := fifelse(Smoking_curr_xps == "1", 1L, 0L)]
        sp$pop[, smok_active_curr_xps := fifelse(Smoking_curr_xps == "3", 1L, 0L)]
        sp$pop[, pa567_curr_xps := fifelse(PA_days_curr_xps %in% c("5", "6", "7"), 1L, 0L)]

        xps <- grep("_curr_xps$", names(sp$pop), value = TRUE)
        xps <- grep("_prvl_curr_xps$", xps, value = TRUE, invert = TRUE)
        xps <- xps[-which(xps %in% c("Smoking_curr_xps", "PA_days_curr_xps"))]
        sp$pop[Smoking_curr_xps != "3", `:=`(
          Smoking_number_curr_xps = NA
        )]

        out_xps20 <- groupingsets(
          sp$pop[all_cause_mrtl >= 0L &
            year >= self$design$sim_prm$init_year_long &
            age >= self$design$sim_prm$ageL, ],
          j = lapply(.SD, weighted.mean, wt, na.rm = TRUE),
          by = c("year", "sex", "agegrp20"), # "ethnicity", "sha"
          .SDcols = xps,
          sets = list(
            "year",
            c("year", "agegrp20"),
            c("year", "sex"),
            c("year", "agegrp20", "sex")
            # c("year", "ethnicity"),
            # c("year", "sha")
          )
        )[, `:=`(mc = sp$mc, scenario = scenario_nam)]
        # TODO above mc could also be mc_aggr. Getting the uncertainty right here is tricky

        for (j in names(out_xps20)[-which(names(out_xps20) %in% xps)]) {
          set(out_xps20, which(is.na(out_xps20[[j]])), j, "All")
        }
        setkey(out_xps20, year)
        if (self$design$sim_prm$avoid_appending_csv) {
          fwrite_safe(out_xps20, private$output_dir(paste0("xps/", sp$mc, "_xps20.csv")))
        } else {
          fwrite_safe(out_xps20, private$output_dir("xps/xps20.csv.gz"))
        }

        # TODO link strata in the outputs to the design.yaml
        out_xps5 <- groupingsets(
          sp$pop[all_cause_mrtl >= 0L &
            year >= self$design$sim_prm$init_year_long &
            age >= self$design$sim_prm$ageL, ],
          j = lapply(.SD, weighted.mean, wt_esp, na.rm = TRUE), # TODO avoid append option
          by = c("year", "sex"), # "ethnicity", "sha"
          .SDcols = xps,
          sets = list(
            "year",
            c("year", "sex")
            # c("year", "ethnicity"),
            # c("year", "sha")
          )
        )[, `:=`(year = year, mc = sp$mc, scenario = scenario_nam)]
        for (j in names(out_xps5)[-which(names(out_xps5) %in% xps)]) {
          set(out_xps5, which(is.na(out_xps5[[j]])), j, "All")
        }
        setkey(out_xps5, year)
        if (self$design$sim_prm$avoid_appending_csv) {
          fwrite_safe(out_xps5, private$output_dir(paste0("xps/", sp$mc, "_xps_esp.csv")))
        } else {
          fwrite_safe(out_xps5, private$output_dir("xps/xps_esp.csv.gz"))
        }

        # Tidy up
        sp$pop[, c(
          "agegrp20",
          "smok_never_curr_xps",
          "smok_active_curr_xps",
          "pa567_curr_xps"
        ) := NULL]
        sp$pop[Smoking_curr_xps != "3", `:=`(
          Smoking_number_curr_xps = 0L
        )]

        NULL
      },


      # Function for timing log
      time_mark = function(text_id) {
        sink(
          file = private$output_dir("logs/times.txt"),
          append = TRUE,
          type = "output",
          split = FALSE
        )
        cat(paste0(text_id, " at: ", Sys.time(), "\n"))
        sink()
      },
      output_dir = function(x = "") {
        file.path(self$design$sim_prm$output_dir, x)
      },

      # function to export summaries from lifecourse files.
      # lc is a lifecourse file
      # single_year_of_age export summaries by single year of age to be used for calibration
      # export_summaries_hlpr ----
      export_summaries_hlpr = function(lc, type = c(
                                         "le", "hle", "dis_char",
                                         "prvl", "incd", "mrtl", "dis_mrtl",
                                         "allcause_mrtl_by_dis", "cms", "qalys", "costs"
                                       ),
                                       single_year_of_age = FALSE) {
        if (self$design$sim_prm$logs) message("Exporting summaries...")

        strata <- c("mc", self$design$sim_prm$strata_for_output)
        strata_noagegrp <- c(
          "mc",
          setdiff(self$design$sim_prm$strata_for_output, c("agegrp"))
        )
        strata_age <- c(strata_noagegrp, "age")

        if (single_year_of_age) strata <- strata_age # used for calibrate_incd_ftlt

        setkeyv(lc, c("scenario", "pid", "year")) # necessary for age_onset

        mcaggr <- ifelse(self$design$sim_prm$avoid_appending_csv, paste0(lc$mc[1], "_"), "")
        ext <- ifelse(self$design$sim_prm$avoid_appending_csv, ".csv", ".csv.gz")


        # Life expectancy ----
        # NOTE for scaled_up LE weights need to apply from the very beginning.
        # Also note that currently this ignores the deaths for people younger
        # than min_age so not a true LE at birth
        if ("le" %in% type) {
          # fwrite_safe(lc[all_cause_mrtl > 0, .("popsize" = (.N), LE = mean(age)),
          #                keyby = strata_noagegrp],
          #             private$output_dir(paste0("summaries/", "le_out.csv.gz"
          #             )))
          fwrite_safe(
            lc[all_cause_mrtl > 0, .("popsize" = sum(wt), LE = weighted.mean(age, wt)), keyby = strata_noagegrp],
            private$output_dir(paste0("summaries/", mcaggr, "le_scaled_up", ext))
          )
          fwrite_safe(
            lc[all_cause_mrtl > 0, .("popsize" = sum(wt_esp), LE = weighted.mean(age, wt_esp)), keyby = strata_noagegrp],
            private$output_dir(paste0("summaries/", mcaggr, "le_esp", ext))
          )
          # Life expectancy at 60 ----

          if (self$design$sim_prm$ageL < 60L && self$design$sim_prm$ageH > 60L) {
            # fwrite_safe(lc[all_cause_mrtl > 0 & age > 60, .("popsize" = (.N), LE60 = mean(age)),  keyby = strata_noagegrp],
            #             private$output_dir(paste0("summaries/", "le60_out", ext
            #             )))
            fwrite_safe(
              lc[all_cause_mrtl > 0 & age > 60, .("popsize" = sum(wt), LE60 = weighted.mean(age, wt)), keyby = strata_noagegrp],
              private$output_dir(paste0("summaries/", mcaggr, "le60_scaled_up", ext))
            )
            fwrite_safe(
              lc[all_cause_mrtl > 0 & age > 60, .("popsize" = sum(wt_esp), LE60 = weighted.mean(age, wt_esp)), keyby = strata_noagegrp],
              private$output_dir(paste0("summaries/", mcaggr, "le60_esp", ext))
            )
          }
          # Note: for less aggregation use wtd.mean with popsize i.e le_out[,
          # weighted.mean(LE, popsize), keyby = year]
        }

        # Healthy life expectancy ----
        if ("hle" %in% type) {
          # TODO currently some individuals are counted more than once because
          # disease counter and score can be reduced.
          # Ideally only the first reach to the threshold should be counted
          # fwrite_safe(lc[cms_count == 1L, .("popsize" = (.N), HLE = mean(age)),
          #                keyby = strata_noagegrp],
          #             private$output_dir(paste0("summaries/", "hle_1st_cond_out", ext)))
          fwrite_safe(
            lc[cms_count == 1L,
              .("popsize" = sum(wt), HLE = weighted.mean(age, wt)),
              keyby = strata_noagegrp
            ],
            private$output_dir(paste0(
              "summaries/", mcaggr, "hle_1st_cond_scaled_up", ext
            ))
          )
          fwrite_safe(
            lc[cms_count == 1L,
              .("popsize" = sum(wt_esp), HLE = weighted.mean(age, wt_esp)),
              keyby = strata_noagegrp
            ],
            private$output_dir(paste0("summaries/", mcaggr, "hle_1st_cond_esp", ext))
          )

          # fwrite_safe(lc[cmsmm1.5_prvl == 1L, .("popsize" = (.N), HLE = mean(age)),
          #                keyby = strata_noagegrp],
          #             private$output_dir(paste0("summaries/", "hle_cmsmm1.5_out", ext)))
          fwrite_safe(
            lc[cmsmm1.5_prvl == 1L,
              .("popsize" = sum(wt), HLE = weighted.mean(age, wt)),
              keyby = strata_noagegrp
            ],
            private$output_dir(paste0(
              "summaries/", mcaggr, "hle_cmsmm1.5_scaled_up", ext
            ))
          )
          fwrite_safe(
            lc[cmsmm1.5_prvl == 1L,
              .("popsize" = sum(wt_esp), HLE = weighted.mean(age, wt_esp)),
              keyby = strata_noagegrp
            ],
            private$output_dir(paste0("summaries/", mcaggr, "hle_cmsmm1.5_esp", ext))
          )
        }

        # Disease characteristics----
        if ("dis_char" %in% type) {
          nm <- grep("_prvl$", names(lc), value = TRUE)

          # tt <- rbindlist(lapply(nm, function(x) {
          #   # sr are the rows the 1st episode occurs per pid
          #   # Need to be sorted on year
          #   sr <- lc[get(x) > 0L, .I[match(1L, get(x))], by = .(pid, scenario)]$V1
          #   sr <- sr[!is.na(sr)]
          #   lc[sr, age_onset := age] # age at 1st ever event
          #
          #   lc[get(x) > 0L, .("disease" = gsub("_prvl$", "", x),
          #                     "cases" = (.N),
          #                     "mean_age_incd" = mean(age[get(x) == 1L]),
          #                     "mean_age_1st_onset" = mean(age_onset, na.rm = TRUE),
          #                     "mean_age_prvl" = mean(age),
          #                     "mean_duration" = mean(get(x)), # Note get(x) very slow here. Implementation with .SDcols also slow because of cases
          #                     "mean_cms_score" = mean(cms_score),
          #                     "mean_cms_count" = mean(cms_count)),
          #      keyby = strata_noagegrp]
          #   lc[, age_onset := NULL]
          # }))
          # tt <- rbindlist(lapply(nm, function(x) {
          #   lc[get(x) > 0L, lapply(.SD, mean), .SDcols = c(x, "age", "cms_score", "cms_count"), keyby = strata_noagegrp]
          # })) # Fast but without cases
          # tt <-
          #   dcast(tt, as.formula(paste0(paste(strata_noagegrp, collapse = "+"), "~disease")),
          #         fill = 0L, value.var = c("cases", "mean_duration", "mean_age_incd",
          #                                  "mean_age_prvl", "mean_cms_score",
          #                                  "mean_cms_count"))
          # fwrite_safe(tt,
          #             private$output_dir(paste0("summaries/", "dis_characteristics_out", ext
          #             )))

          tt <- rbindlist(lapply(nm, function(x) {
            # sr are the rows the 1st episode occurs per pid
            # Need to be sorted on year
            sr <- lc[get(x) > 0L, .I[match(1L, get(x))], by = .(pid, scenario)]$V1
            sr <- sr[!is.na(sr)]
            lc[, wt1st := 0]
            lc[sr, `:=`(age_onset = age, wt1st = wt)] # age at 1st ever event

            ans <- lc[get(x) > 0L, .(
              "disease" = gsub("_prvl$", "", x),
              "cases" = sum(wt),
              "mean_age_incd" = weighted.mean(
                age[get(x) == 1L],
                wt[get(x) == 1L]
              ),
              "mean_age_1st_onset" = weighted.mean(age_onset, wt1st, na.rm = TRUE),
              "mean_age_prvl" = weighted.mean(age, wt),
              "mean_duration" = weighted.mean(get(x), wt), # Note get(x) very slow here. Implementation with .SDcols also slow because of cases
              "mean_cms_score" = weighted.mean(cms_score, wt),
              "mean_cms_count" = weighted.mean(cms_count, wt)
            ),
            keyby = strata_noagegrp
            ]
            lc[, c("age_onset", "wt1st") := NULL]
            ans
          }))
          tt <-
            dcast(tt, as.formula(paste0(paste(strata_noagegrp, collapse = "+"), "~disease")),
              fill = 0L, value.var = c(
                "cases", "mean_duration", "mean_age_incd",
                "mean_age_1st_onset",
                "mean_age_prvl", "mean_cms_score", "mean_cms_count"
              )
            )
          fwrite_safe(
            tt,
            private$output_dir(paste0("summaries/", mcaggr, "dis_characteristics_scaled_up", ext))
          )

          tt <- rbindlist(lapply(nm, function(x) {
            # sr are the rows the 1st episode occurs per pid
            # Need to be sorted on year
            sr <- lc[get(x) > 0L, .I[match(1L, get(x))], by = .(pid, scenario)]$V1
            sr <- sr[!is.na(sr)]
            lc[, wt1st := 0]
            lc[sr, `:=`(age_onset = age, wt1st = wt_esp)] # age at 1st ever event

            ans <- lc[get(x) > 0L, .(
              "disease" = gsub("_prvl$", "", x),
              "cases" = sum(wt_esp),
              "mean_age_incd" = weighted.mean(
                age[get(x) == 1L],
                wt_esp[get(x) == 1L]
              ),
              "mean_age_1st_onset" = weighted.mean(age_onset, wt1st, na.rm = TRUE),
              "mean_age_prvl" = weighted.mean(age, wt_esp),
              "mean_duration" = weighted.mean(get(x), wt_esp), # Note get(x) very slow here. Implementation with .SDcols also slow because of cases
              "mean_cms_score" = weighted.mean(cms_score, wt_esp),
              "mean_cms_count" = weighted.mean(cms_count, wt_esp)
            ),
            keyby = strata_noagegrp
            ]
            lc[, c("age_onset", "wt1st") := NULL]
            ans
          }))
          tt <-
            dcast(tt, as.formula(paste0(paste(strata_noagegrp, collapse = "+"), "~disease")),
              fill = 0L, value.var = c(
                "cases", "mean_duration", "mean_age_incd",
                "mean_age_1st_onset",
                "mean_age_prvl", "mean_cms_score",
                "mean_cms_count"
              )
            )
          fwrite_safe(
            tt,
            private$output_dir(paste0("summaries/", mcaggr, "dis_characteristics_esp", ext))
          )
          rm(tt)
        }

        # prvl ----
        if ("prvl" %in% type) {
          # Note for mortality this exports qx directly. mx is defined as the
          # number of deaths during the year divided by the average number alive
          # during the year, i.e. This differs slightly from qx , which is the
          # number of deaths during the year divided by the number alive at the
          # beginning of the year.
          # fwrite_safe(lc[, c("popsize" = (.N),
          #                    lapply(.SD, function(x) sum(x > 0))),
          #                .SDcols = patterns("_prvl$"), keyby = strata],
          #             private$output_dir(paste0("summaries/", "prvl_out", ext
          #             )))
          fwrite_safe(
            lc[, c(
              "popsize" = sum(wt),
              lapply(.SD, function(x, wt) sum((x > 0) * wt), wt)
            ),
            .SDcols = patterns("_prvl$"), keyby = strata
            ],
            private$output_dir(paste0("summaries/", mcaggr, "prvl_scaled_up", ext))
          )
          fwrite_safe(
            lc[, c(
              "popsize" = sum(wt_esp),
              lapply(.SD, function(x, wt) sum((x > 0) * wt), wt_esp)
            ),
            .SDcols = patterns("_prvl$"), keyby = strata
            ],
            private$output_dir(paste0("summaries/", mcaggr, "prvl_esp", ext))
          )
        }

        # incd ----
        if ("incd" %in% type) {
          # NOTE incd includes prevalent cases in denominator
          # fwrite_safe(lc[, c("popsize" = (.N),
          #                    lapply(.SD, function(x) sum(x == 1))),
          #                .SDcols = patterns("_prvl$"), keyby = strata],
          #             private$output_dir(paste0("summaries/", "incd_out", ext
          #             )))
          incdtbl <- lc[, c(
            "popsize" = sum(wt),
            lapply(.SD, function(x, wt) sum((x == 1) * wt), wt)
          ),
          .SDcols = patterns("_prvl$"), keyby = strata
          ]
          nm <- grep("_prvl$", names(incdtbl), value = TRUE)
          setnames(incdtbl, nm, gsub("_prvl$", "_incd", nm))
          fwrite_safe(
            incdtbl,
            private$output_dir(paste0("summaries/", mcaggr, "incd_scaled_up", ext))
          )

          incdtbl <- lc[, c(
            "popsize" = sum(wt_esp),
            lapply(.SD, function(x, wt) sum((x == 1) * wt), wt_esp)
          ),
          .SDcols = patterns("_prvl$"), keyby = strata
          ]
          nm <- grep("_prvl$", names(incdtbl), value = TRUE)
          setnames(incdtbl, nm, gsub("_prvl$", "_incd", nm))
          fwrite_safe(
            incdtbl,
            private$output_dir(paste0("summaries/", mcaggr, "incd_esp", ext))
          )

          rm(incdtbl, nm)
        }

        # mrtl ----
        if ("mrtl" %in% type) {
          # fwrite_safe(lc[, .("popsize" = (.N),
          #                    "all_cause_mrtl" = sum(all_cause_mrtl > 0)),
          #                keyby = strata],
          #             private$output_dir(paste0("summaries/", "mrtl_out", ext
          #             )))
          fwrite_safe(
            lc[, .(
              "popsize" = sum(wt),
              "all_cause_mrtl" = sum((all_cause_mrtl > 0) * wt)
            ),
            keyby = strata
            ],
            private$output_dir(paste0("summaries/", mcaggr, "mrtl_scaled_up", ext))
          )
          fwrite_safe(
            lc[, .(
              "popsize" = sum(wt_esp),
              "all_cause_mrtl" = sum((all_cause_mrtl > 0) * wt_esp)
            ),
            keyby = strata
            ],
            private$output_dir(paste0("summaries/", mcaggr, "mrtl_esp", ext))
          )
        }

        # disease specific mortality ----
        if ("dis_mrtl" %in% type) {
          # dis_mrtl_out <-
          #   dcast(
          #     lc[, .("deaths" = (.N)),
          #        keyby = c(strata, "all_cause_mrtl")],
          #     formula = as.formula(paste0(
          #       paste(strata, collapse = "+"), "~all_cause_mrtl"
          #     )),
          #     fill = 0L,
          #     value.var = "deaths"
          #   )
          #
          # setnames(dis_mrtl_out, as.character(private$death_codes),
          #          names(private$death_codes), skip_absent = TRUE)
          # dis_mrtl_out[, `:=` (
          #   popsize = Reduce(`+`, .SD),
          #   alive = NULL
          # ), .SDcols = !strata]
          # fwrite_safe(dis_mrtl_out,
          #             private$output_dir(paste0("summaries/", "dis_mrtl_out", ext
          #             )))

          dis_mrtl_out <- # scale up
            dcast(
              lc[, .("deaths" = sum(wt)),
                keyby = c(strata, "all_cause_mrtl")
              ],
              formula = as.formula(paste0(
                paste(strata, collapse = "+"), "~all_cause_mrtl"
              )),
              fill = 0L,
              value.var = "deaths"
            )

          setnames(dis_mrtl_out, as.character(private$death_codes),
            paste0(names(private$death_codes), "_deaths"),
            skip_absent = TRUE
          )
          dis_mrtl_out[, `:=`(
            popsize = Reduce(`+`, .SD), # it includes alive so it is the pop at the start of the year
            alive_deaths = NULL
          ), .SDcols = !strata]
          fwrite_safe(
            dis_mrtl_out,
            private$output_dir(paste0("summaries/", mcaggr, "dis_mrtl_scaled_up", ext))
          )

          dis_mrtl_out <- # scale up esp
            dcast(
              lc[, .("deaths" = sum(wt_esp)),
                keyby = c(strata, "all_cause_mrtl")
              ],
              formula = as.formula(paste0(
                paste(strata, collapse = "+"), "~all_cause_mrtl"
              )),
              fill = 0L,
              value.var = "deaths"
            )

          setnames(dis_mrtl_out, as.character(private$death_codes),
            paste0(names(private$death_codes), "_deaths"),
            skip_absent = TRUE
          )
          dis_mrtl_out[, `:=`(
            popsize = Reduce(`+`, .SD),
            alive_deaths = NULL
          ), .SDcols = !strata]
          fwrite_safe(
            dis_mrtl_out,
            private$output_dir(paste0("summaries/", mcaggr, "dis_mrtl_esp", ext))
          )
          rm(dis_mrtl_out)
        }

        # All-cause mrtl by disease ----
        if ("allcause_mrtl_by_dis" %in% type) {
          nm <- grep("_prvl$", names(lc), value = TRUE)

          # tt <- lapply(nm, function(x) {
          #   lc[get(x) > 0L, .("disease" = gsub("_prvl$", "", x), "cases" = (.N), "deaths" = sum(all_cause_mrtl > 0)), keyby = strata]
          # })
          # tt <-
          # dcast(rbindlist(tt), as.formula(paste0(paste(strata, collapse = "+"), "~disease")),
          #       fill = 0L, value.var = c("deaths", "cases"))
          # fwrite_safe(tt,
          #             private$output_dir(paste0("summaries/", "all_cause_mrtl_by_dis_out", ext
          #             )))

          tt <- lapply(nm, function(x) {
            lc[get(x) > 0L, .("disease" = gsub("_prvl$", "", x), "cases" = sum(wt), "deaths" = sum(wt * (all_cause_mrtl > 0))), keyby = strata]
          })
          tt <-
            dcast(rbindlist(tt), as.formula(paste0(paste(strata, collapse = "+"), "~disease")),
              fill = 0L, value.var = c("deaths", "cases")
            )
          fwrite_safe(
            tt,
            private$output_dir(paste0("summaries/", mcaggr, "all_cause_mrtl_by_dis_scaled_up", ext))
          )

          tt <- lapply(nm, function(x) {
            lc[get(x) > 0L, .("disease" = gsub("_prvl$", "", x), "cases" = sum(wt_esp), "deaths" = sum(wt_esp * (all_cause_mrtl > 0))), keyby = strata]
          })
          tt <-
            dcast(rbindlist(tt), as.formula(paste0(paste(strata, collapse = "+"), "~disease")),
              fill = 0L, value.var = c("deaths", "cases")
            )
          fwrite_safe(
            tt,
            private$output_dir(paste0("summaries/", mcaggr, "all_cause_mrtl_by_dis_esp", ext))
          )
          rm(tt)
        }


        # CMS mean ----
        if ("cms" %in% type) {
          # fwrite_safe(lc[, .("popsize" = (.N), cms_score = mean(cms_score)), keyby = strata],
          #             private$output_dir(paste0("summaries/", "cms_score_out", ext
          #             )))
          fwrite_safe(
            lc[, .("popsize" = sum(wt), cms_score = weighted.mean(cms_score, wt)), keyby = strata],
            private$output_dir(paste0("summaries/", mcaggr, "cms_score_scaled_up", ext))
          )
          fwrite_safe(
            lc[, .("popsize" = sum(wt), cms_score = weighted.mean(cms_score, wt)), keyby = strata_age],
            private$output_dir(paste0("summaries/", mcaggr, "cms_score_by_age_scaled_up", ext))
          )

          fwrite_safe(
            lc[, .("popsize" = sum(wt_esp), cms_score = weighted.mean(cms_score, wt_esp)), keyby = strata],
            private$output_dir(paste0("summaries/", mcaggr, "cms_score_esp", ext))
          )

          fwrite_safe(
            lc[, .("popsize" = sum(wt_esp), cms_score = weighted.mean(cms_score, wt_esp)), keyby = strata_age],
            private$output_dir(paste0("summaries/", mcaggr, "cms_score_by_age_esp", ext))
          )

          # CMS count ----
          # fwrite_safe(lc[, .("popsize" = (.N), cms_count = mean(cms_count)), keyby = strata],
          #             private$output_dir(paste0("summaries/", "cms_count_out", ext
          #             )))
          fwrite_safe(
            lc[, .("popsize" = sum(wt), cms_count = weighted.mean(cms_count, wt)), keyby = strata],
            private$output_dir(paste0("summaries/", mcaggr, "cms_count_scaled_up", ext))
          )
          fwrite_safe(
            lc[, .("popsize" = sum(wt_esp), cms_count = weighted.mean(cms_count, wt_esp)), keyby = strata],
            private$output_dir(paste0("summaries/", mcaggr, "cms_count_esp", ext))
          )
        }

        # QALYs ----
        if ("qalys" %in% type) {
          private$calc_QALYs(lc, include_non_significant = FALSE) # create cols EQ5D5L and HUI3
          fwrite_safe(
            lc[, c(
              # "popsize" = sum(wt),
              lapply(.SD, function(x, wt) sum(x * wt), wt)
            ),
            .SDcols = patterns("^EQ5D5L$|^HUI3$"), keyby = strata
            ],
            private$output_dir(paste0("summaries/", mcaggr, "qalys_scaled_up", ext))
          )
          fwrite_safe(
            lc[, c(
              # "popsize" = sum(wt_esp),
              lapply(.SD, function(x, wt) sum(x * wt), wt_esp)
            ),
            .SDcols = patterns("^EQ5D5L$|^HUI3$"), keyby = strata
            ],
            private$output_dir(paste0("summaries/", mcaggr, "qalys_esp", ext))
          )
        }

        # costs ----
        if ("costs" %in% type) {
          private$calc_costs(lc) # create cols
          fwrite_safe(
            lc[, c(
              # "popsize" = sum(wt),
              lapply(.SD, function(x, wt) sum(x * wt), wt)
            ),
            .SDcols = patterns("_costs$"), keyby = strata
            ],
            private$output_dir(paste0("summaries/", mcaggr, "costs_scaled_up", ext))
          )
          fwrite_safe(
            lc[, c(
              # "popsize" = sum(wt_esp),
              lapply(.SD, function(x, wt) sum(x * wt), wt_esp)
            ),
            .SDcols = patterns("_costs$"), keyby = strata
            ],
            private$output_dir(paste0("summaries/", mcaggr, "costs_esp", ext))
          )
        }

        if (!self$design$sim_prm$keep_lifecourse) file.remove(pth)

        return(invisible(self))
      },

      # deep_clone ----
      # Special deep copy for data.table. Use POP$clone(deep = TRUE) to
      # dispatch. Otherwise a reference is created
      deep_clone = function(name, value) {
        if ("data.table" %in% class(value)) {
          data.table::copy(value)
        } else if ("R6" %in% class(value)) {
          value$clone()
        } else {
          # For everything else, just return it. This results in a shallow copy
          # of s3.
          value
        }
      },

      # calc_QALYs ----
      # Calculate QALYs based on Table 4 of the paper, Shiroiwa 2021, titled with
      # "Japanese Population Norms of EQ-5D-5L and Health Utilities Index Mark 3:
      # Disutility Catalog by Disease and Symptom in Community Settings"

      # NOTE an implementation with join is slower because the prvl cols need to be transformed to 0 and 1
      # Even with lc[, lapply(.SD, fclamp_int, inplace = TRUE), .SDcols = patterns("_prvl")] this is slow,
      # and destrucive to the original data

      # lc = a lifecourse file
      # include_non_significant = whether to include non-significant desreaments (obesity, sbp, ldl) in the calculation
      calc_QALYs = function(lc, include_non_significant = FALSE) {
        lc[, `:=`(
          EQ5D5L = 0.989 +
            fcase( # age decreaments
              # Scores for 90-99 years old were assumed to be same as those for 80-89.
              agegrp == "20-24", -0.018,
              agegrp == "25-29", -0.018,
              agegrp == "30-34", -0.019,
              agegrp == "35-39", -0.019,
              agegrp == "40-44", -0.018,
              agegrp == "45-49", -0.018,
              agegrp == "50-54", -0.028,
              agegrp == "55-59", -0.028,
              agegrp == "60-64", -0.021,
              agegrp == "65-69", -0.021,
              agegrp == "70-74", -0.057,
              agegrp == "75-79", -0.057,
              agegrp == "80-84", -0.129,
              agegrp == "85-89", -0.129,
              agegrp == "90-94", -0.129,
              agegrp == "95-99", -0.129,
              default = 0
            ) +
            fifelse(sex == "women", -0.011, 0) +
            fifelse(chd_prvl == 0L, 0, -0.073) +
            fifelse(stroke_prvl == 0L, 0, -0.265) +
            fifelse(t2dm_prvl == 0L, 0, -0.046),
          HUI3 = 0.897 +
            fcase( # age decreaments
              # Scores for 90-99 years old were assumed to be same as those for 80-89.
              agegrp == "20-24", -0.023,
              agegrp == "25-29", -0.023,
              agegrp == "30-34", -0.018,
              agegrp == "35-39", -0.018,
              agegrp == "40-44", -0.004,
              agegrp == "45-49", -0.004,
              agegrp == "50-54", -0.021,
              agegrp == "55-59", -0.021,
              agegrp == "60-64", -0.013,
              agegrp == "65-69", -0.013,
              agegrp == "70-74", -0.042,
              agegrp == "75-79", -0.042,
              agegrp == "80-84", -0.145,
              agegrp == "85-89", -0.145,
              agegrp == "90-94", -0.145,
              agegrp == "95-99", -0.145,
              default = 0
            ) +
            fifelse(sex == "women", 0.011, 0) +
            fifelse(chd_prvl == 0L, 0, -0.081) +
            fifelse(stroke_prvl == 0L, 0, -0.293) +
            fifelse(t2dm_prvl == 0L, 0, -0.055)
        )]

        # TODO add hyperlipidaemia if needed
        if (!include_non_significant == TRUE) {
          lc[, `:=`(
            EQ5D5L = EQ5D5L + fifelse(htn_prvl == 0L, 0, -0.005) +
              fifelse(obesity_prvl == 0L, 0, -0.034),
            HUI3 = HUI3 + fifelse(htn_prvl == 0L, 0, -0.006) +
              fifelse(obesity_prvl == 0L, 0, 0.019)
          )]
        }

        NULL
      },


      # calc_costs ----
      calc_costs = function(lc) {
        # All in 2021 Yen prices
        # productivity costs
        inflation_factor <- 1.025
        chd_prvl_2016 <- lc[year == 2016 & scenario == "sc0", round(sum((chd_dgns > 0) * wt)), keyby = .(agegrp, sex)]
        chd_prvl_2019 <- lc[year == 2019 & scenario == "sc0", round(sum((chd_dgns > 0) * wt)), keyby = .(agegrp, sex)]
        stroke_prvl_2016 <- lc[year == 2016 & scenario == "sc0", round(sum((stroke_dgns > 0) * wt)), keyby = .(agegrp, sex)]
        stroke_prvl_2019 <- lc[year == 2019 & scenario == "sc0", round(sum((stroke_dgns > 0) * wt)), keyby = .(agegrp, sex)]
        chd_mrtl_2016 <- lc[year == 2016 & scenario == "sc0", round(sum((all_cause_mrtl == 2L) * wt)), keyby = .(agegrp, sex)]
        tt <- read_fst("inputs/disease_burden/chd_ftlt.fst", as.data.table = TRUE)[year == 2016]
        ttt <- read_fst("inputs/pop_estimates/observed_population_japan.fst", as.data.table = TRUE)[year == 2016]
        tt[ttt, on = c("age", "year", "sex"), `:=`(deaths = mu2 * pops)]
        to_agegrp(tt, 5, 99L)
        tt <- tt[, .(deaths = sum(deaths)), keyby = .(agegrp, sex)]
        chd_mrtl_2016[tt, on = c("agegrp", "sex"), deaths := round(i.deaths)]
        chd_mrtl_2016[V1 == 0, V1 := deaths]
        chd_mrtl_2016[, deaths := NULL]
        stroke_mrtl_2016 <- lc[year == 2016 & scenario == "sc0", round(sum((all_cause_mrtl == 3L) * wt)), keyby = .(agegrp, sex)]
        tt <- read_fst("inputs/disease_burden/stroke_ftlt.fst", as.data.table = TRUE)[year == 2016]
        tt[ttt, on = c("age", "year", "sex"), `:=`(deaths = mu2 * pops)]
        to_agegrp(tt, 5, 99L)
        tt <- tt[, .(deaths = sum(deaths)), keyby = .(agegrp, sex)]
        stroke_mrtl_2016[tt, on = c("agegrp", "sex"), deaths := round(i.deaths)]
        stroke_mrtl_2016[V1 == 0, V1 := deaths]
        stroke_mrtl_2016[, deaths := NULL]

        chd_prvl_prdv_costs <- data.table(
          agegrp = rep(c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), 2),
          sex = rep(c("men", "women"), each = 14),
          employees = c(
            1683780, 1829610, 2174550, 2057710, 1702470,
            1425510, 963430, 369640, 106850, 0, 0, 0, 0, 0,
            919700, 894770, 1049490, 1037140, 854970,
            685040, 376370, 132470, 44050, 0, 0, 0, 0, 0
          )
        )
        absorb_dt(chd_prvl_prdv_costs, chd_prvl_2016)
        chd_prvl_prdv_costs[, chd_prvl_prdv_wt := employees * V1]
        chd_prvl_prdv_costs[, tcost := 141000000000.00 * chd_prvl_prdv_wt / sum(chd_prvl_prdv_wt)]
        chd_prvl_prdv_costs[, chd_prvl_prdv_cost_percase := tcost * inflation_factor / V1]
        chd_prvl_prdv_costs[, c("employees", "V1", "chd_prvl_prdv_wt", "tcost") := NULL]
        absorb_dt(lc, chd_prvl_prdv_costs)
        lc[chd_dgns == 0L, chd_prvl_prdv_cost_percase := 0]

        stroke_prvl_prdv_costs <- data.table(
          agegrp = rep(c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), 2),
          sex = rep(c("men", "women"), each = 14),
          employees = c(
            1683780, 1829610, 2174550, 2057710, 1702470,
            1425510, 963430, 369640, 106850, 0, 0, 0, 0, 0,
            919700, 894770, 1049490, 1037140, 854970,
            685040, 376370, 132470, 44050, 0, 0, 0, 0, 0
          )
        )
        absorb_dt(stroke_prvl_prdv_costs, stroke_prvl_2016)
        stroke_prvl_prdv_costs[, stroke_prvl_prdv_wt := employees * V1]
        stroke_prvl_prdv_costs[, tcost := 322000000000.00 * stroke_prvl_prdv_wt / sum(stroke_prvl_prdv_wt)]
        stroke_prvl_prdv_costs[, stroke_prvl_prdv_cost_percase := tcost * inflation_factor / V1]
        stroke_prvl_prdv_costs[, c("employees", "V1", "stroke_prvl_prdv_wt", "tcost") := NULL]
        absorb_dt(lc, stroke_prvl_prdv_costs)
        lc[stroke_dgns == 0L, stroke_prvl_prdv_cost_percase := 0]

        chd_mrtl_prdv_costs <- data.table(
          agegrp = rep(c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), 2),
          sex = rep(c("men", "women"), each = 14),
          employees = c(
            1683780, 1829610, 2174550, 2057710, 1702470,
            1425510, 963430, 369640, 106850, 0, 0, 0, 0, 0,
            919700, 894770, 1049490, 1037140, 854970,
            685040, 376370, 132470, 44050, 0, 0, 0, 0, 0
          )
        )
        absorb_dt(chd_mrtl_prdv_costs, chd_mrtl_2016)
        chd_mrtl_prdv_costs[, chd_mrtl_prdv_wt := employees * V1]
        chd_mrtl_prdv_costs[, tcost := 2257000000000.00 * chd_mrtl_prdv_wt / sum(chd_mrtl_prdv_wt)]
        chd_mrtl_prdv_costs[, chd_mrtl_prdv_cost_percase := tcost * inflation_factor / V1]
        chd_mrtl_prdv_costs[, c("employees", "V1", "chd_mrtl_prdv_wt", "tcost") := NULL]
        absorb_dt(lc, chd_mrtl_prdv_costs)
        lc[all_cause_mrtl != 2L, chd_mrtl_prdv_cost_percase := 0]

        stroke_mrtl_prdv_costs <- data.table(
          agegrp = rep(c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), 2),
          sex = rep(c("men", "women"), each = 14),
          employees = c(
            1683780, 1829610, 2174550, 2057710, 1702470,
            1425510, 963430, 369640, 106850, 0, 0, 0, 0, 0,
            919700, 894770, 1049490, 1037140, 854970,
            685040, 376370, 132470, 44050, 0, 0, 0, 0, 0
          )
        )
        absorb_dt(stroke_mrtl_prdv_costs, stroke_mrtl_2016)
        stroke_mrtl_prdv_costs[, stroke_mrtl_prdv_wt := employees * V1]
        stroke_mrtl_prdv_costs[, tcost := 1352000000000.00 * stroke_mrtl_prdv_wt / sum(stroke_mrtl_prdv_wt)]
        stroke_mrtl_prdv_costs[, stroke_mrtl_prdv_cost_percase := tcost * inflation_factor / V1]
        stroke_mrtl_prdv_costs[, c("employees", "V1", "stroke_mrtl_prdv_wt", "tcost") := NULL]
        absorb_dt(lc, stroke_mrtl_prdv_costs)
        lc[all_cause_mrtl != 3L, stroke_mrtl_prdv_cost_percase := 0]

        lc[, `:=`(
          chd_productivity_costs = chd_prvl_prdv_cost_percase + chd_mrtl_prdv_cost_percase,
          chd_prvl_prdv_cost_percase = NULL,
          chd_mrtl_prdv_cost_percase = NULL,
          stroke_productivity_costs = stroke_prvl_prdv_cost_percase + stroke_mrtl_prdv_cost_percase,
          stroke_prvl_prdv_cost_percase = NULL,
          stroke_mrtl_prdv_cost_percase = NULL
        )]

        # Informal costs
        chd_prvl_infm_costs <- data.table(
          agegrp = rep(c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), 2),
          sex = rep(c("men", "women"), each = 14),
          infm_care_hrs = c(
            0.030, 0.030, 0.030, 0.030, 0.030, 0.030, 0.030,
            0.200, 0.200, 0.200, 0, 0, 0, 0,
            0.030, 0.030, 0.030, 0.030, 0.030, 0.030,
            0.030, 0.200, 0.200, 0.200, 0, 0, 0, 0
          )
        )
        absorb_dt(chd_prvl_infm_costs, chd_prvl_2016)
        chd_prvl_infm_costs[, chd_prvl_infm_wt := infm_care_hrs * V1]
        chd_prvl_infm_costs[, tcost := 291000000000 * chd_prvl_infm_wt / sum(chd_prvl_infm_wt)]
        chd_prvl_infm_costs[, chd_informal_costs := tcost * inflation_factor / V1]
        chd_prvl_infm_costs[, c("infm_care_hrs", "V1", "chd_prvl_infm_wt", "tcost") := NULL]
        absorb_dt(lc, chd_prvl_infm_costs)
        lc[chd_dgns == 0L, chd_informal_costs := 0]

        stroke_prvl_infm_costs <- data.table(
          agegrp = rep(c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"), 2),
          sex = rep(c("men", "women"), each = 14),
          infm_care_hrs = c(
            5.20, 5.20, 5.20, 5.20, 5.20, 5.20, 5.20,
            5.03, 5.03, 5.03, 9.23, 9.23, 9.23, 9.23,
            5.20, 5.20, 5.20, 5.20, 5.20, 5.20, 5.20,
            5.03, 5.03, 5.03, 9.23, 9.23, 9.23, 9.23
          )
        )
        absorb_dt(stroke_prvl_infm_costs, stroke_prvl_2016)
        stroke_prvl_infm_costs[, stroke_prvl_infm_wt := infm_care_hrs * V1]
        stroke_prvl_infm_costs[, tcost := 1651000000000.00 * stroke_prvl_infm_wt / sum(stroke_prvl_infm_wt)]
        stroke_prvl_infm_costs[, stroke_informal_costs := tcost * inflation_factor / V1]
        stroke_prvl_infm_costs[, c("infm_care_hrs", "V1", "stroke_prvl_infm_wt", "tcost") := NULL]
        absorb_dt(lc, stroke_prvl_infm_costs)
        lc[stroke_dgns == 0L, stroke_informal_costs := 0]


        #  direct costs
        inflation_factor <- 99.6 / 99.7
        chd_prvl_2019[, agegrp2 := fcase(
          agegrp == "30-34", "30-44",
          agegrp == "35-39", "30-44",
          agegrp == "40-44", "30-44",
          agegrp == "45-49", "45-64",
          agegrp == "50-54", "45-64",
          agegrp == "55-59", "45-64",
          agegrp == "60-64", "45-64",
          agegrp == "65-69", "65-69",
          agegrp == "70-74", "70-74",
          agegrp == "75-79", "75-99",
          agegrp == "80-84", "75-99",
          agegrp == "85-89", "75-99",
          agegrp == "90-94", "75-99",
          agegrp == "95-99", "75-99"
        )]
        tt <- chd_prvl_2019[, .(V1 = sum(V1)), by = .(agegrp2, sex)]
        chd_prvl_dir_costs <- data.table(
          agegrp2 = rep(c("30-44", "45-64", "65-69", "70-74", "75-99"), 2),
          sex = rep(c("men", "women"), each = 5),
          tcost = c(
            10500 - 200, 121000, 72300, 90100, 197000,
            2600 - 100, 22500, 18900, 30300, 132800
          ) * 1e6
        )
        absorb_dt(chd_prvl_dir_costs, tt)
        chd_prvl_dir_costs[, chd_direct_costs := tcost * inflation_factor / V1]
        chd_prvl_dir_costs <- chd_prvl_2019[chd_prvl_dir_costs, on = c("agegrp2", "sex"), .(agegrp, sex, chd_direct_costs)]
        absorb_dt(lc, chd_prvl_dir_costs)
        lc[chd_dgns == 0L, chd_direct_costs := 0]

        stroke_prvl_2019[, agegrp2 := fcase(
          agegrp == "30-34", "30-44",
          agegrp == "35-39", "30-44",
          agegrp == "40-44", "30-44",
          agegrp == "45-49", "45-64",
          agegrp == "50-54", "45-64",
          agegrp == "55-59", "45-64",
          agegrp == "60-64", "45-64",
          agegrp == "65-69", "65-69",
          agegrp == "70-74", "70-74",
          agegrp == "75-79", "75-99",
          agegrp == "80-84", "75-99",
          agegrp == "85-89", "75-99",
          agegrp == "90-94", "75-99",
          agegrp == "95-99", "75-99"
        )]
        tt <- stroke_prvl_2019[, .(V1 = sum(V1)), by = .(agegrp2, sex)]
        stroke_prvl_dir_costs <- data.table(
          agegrp2 = rep(c("30-44", "45-64", "65-69", "70-74", "75-99"), 2),
          sex = rep(c("men", "women"), each = 5),
          tcost = c(
            26600 - 1700, 186400, 109000, 144100, 465600,
            19700 - 1700, 106800, 60900, 95700, 606800
          ) * 1e6
        )
        absorb_dt(stroke_prvl_dir_costs, tt)
        stroke_prvl_dir_costs[, stroke_direct_costs := tcost * inflation_factor / V1]
        stroke_prvl_dir_costs <- stroke_prvl_2019[stroke_prvl_dir_costs, on = c("agegrp2", "sex"), .(agegrp, sex, stroke_direct_costs)]
        absorb_dt(lc, stroke_prvl_dir_costs)
        lc[stroke_dgns == 0L, stroke_direct_costs := 0]

        lc[, `:=`(
          chd_total_costs = chd_productivity_costs + chd_informal_costs + chd_direct_costs,
          stroke_total_costs = stroke_productivity_costs + stroke_informal_costs + stroke_direct_costs,
          chd_indirect_costs = chd_productivity_costs + chd_informal_costs,
          stroke_indirect_costs = stroke_productivity_costs + stroke_informal_costs
        )]
        lc[, `:=`(
          cvd_total_costs = chd_total_costs + stroke_total_costs,
          cvd_indirect_costs = chd_indirect_costs + stroke_indirect_costs,
          cvd_direct_costs = chd_direct_costs + stroke_direct_costs,
          cvd_productivity_costs = chd_productivity_costs + stroke_productivity_costs,
          cvd_informal_costs = chd_informal_costs + stroke_informal_costs
        )]

        NULL
      },


      # collect_files ----
      # Collect files written by mc_aggr or mc_aggr_mc in a folder and combine
      # them into one file
      collect_files = function(folder_name, pattern = NULL, to_mc_aggr = FALSE) {
        if (self$design$sim_prm$logs) message("Collecting mc files...")
        if (to_mc_aggr) {
          string1 <- "_[0-9]+_"
          string2 <- "_"
        } else {
          string1 <- "[0-9]+_"
          string2 <- ""
        }
        sapply(
          list.files(path = private$output_dir(folder_name), pattern = pattern, full.names = TRUE),
          function(fnam) {
            fwrite_safe(fread(fnam), file = sub(string1, string2, fnam))
            file.remove(fnam)
          }
        )
        # gzip the .csv files to .csv.gz (faster than using gzip() and same speed/compression as with fst 80. But fst reads faster)
        if (self$design$sim_prm$logs) message("Compressing aggregated files...")
        sapply(
          list.files(path = private$output_dir(folder_name), pattern = sub("^_", "", pattern), full.names = TRUE),
          function(fnam) {
            fwrite_safe(fread(fnam), file = gsub(".csv$", ".csv.gz", fnam))
            file.remove(fnam)
          }
        )
        NULL
      },

      # create_empty_calibration_prms_file ----
      # if replace is FALSE then it creates a calibration parameters when it is
      # missing, file filed with 1. If replace = TRUE it overwrites the existin
      # file
      # returns invisible(self)
      # TODO Automate based on diseases in design.yaml
      create_empty_calibration_prms_file = function(replace = FALSE) {
        if (replace || !file.exists("./simulation/calibration_prms.csv")) {
          clbr <- CJ(
            year = self$design$sim_prm$init_year_long:(self$design$sim_prm$init_year_long + self$design$sim_prm$sim_horizon_max),
            age = self$design$sim_prm$ageL:self$design$sim_prm$ageH,
            sex = c("men", "women"),
            chd_incd_clbr_fctr = 1,
            stroke_incd_clbr_fctr = 1,
            chd_ftlt_clbr_fctr = 1,
            stroke_ftlt_clbr_fctr = 1,
            nonmodelled_ftlt_clbr_fctr = 1
          )
          fwrite(clbr, "./simulation/calibration_prms.csv")
        }
        invisible(self)
      },

      # create_new_folder ----
      # @description Create folder if doesn't exist. Stops on failure.
      # @param sDirPathName String folder path and name.
      # @param bReport Bool report folder creation.
      create_new_folder = function(sDirPathName, bReport) {
        if (!dir.exists(sDirPathName)) {
          bSuccess <- dir.create(sDirPathName, recursive = TRUE)
          if (!bSuccess) stop(paste("Failed creating directory", sDirPathName))
          if (bReport) message(paste0("Folder ", sDirPathName, " was created"))
        }
      }
    )
  )

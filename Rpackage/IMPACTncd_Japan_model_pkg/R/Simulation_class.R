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
          stop(
            "sim_prm need to be a path to an appropriate yaml file or a Design object"
          )
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
        private$create_new_folder(
          self$design$sim_prm$output_dir,
          self$design$sim_prm$logs
        )
        private$create_new_folder(
          private$output_dir("summaries/"),
          self$design$sim_prm$logs
        )
        private$create_new_folder(
          private$output_dir("tables/"),
          self$design$sim_prm$logs
        )
        private$create_new_folder(
          private$output_dir("plots/"),
          self$design$sim_prm$logs
        )
        private$create_new_folder(
          private$output_dir("lifecourse/"),
          self$design$sim_prm$logs
        )
        if (self$design$sim_prm$export_PARF) {
          private$create_new_folder(
            private$output_dir("parf/"),
            self$design$sim_prm$logs
          )
        }
        if (self$design$sim_prm$export_xps) {
          private$create_new_folder(
            private$output_dir("xps/"),
            self$design$sim_prm$logs
          )
        }
        if (self$design$sim_prm$logs) {
          private$create_new_folder(
            private$output_dir("logs/"),
            self$design$sim_prm$logs
          )
        }

        # NOTE code below is duplicated in Synthpop class. This is intentional
        private$create_new_folder(
          self$design$sim_prm$synthpop_dir,
          self$design$sim_prm$logs
        )

        private$create_empty_calibration_prms_file(replace = FALSE)

        message("Loading exposures.")
        # RR Create a named list of Exposure objects for the files in
        # ./inputs/RR
        fl <- list.files(
          path = "./inputs/RR",
          pattern = ".csvy$",
          full.names = TRUE
        )
        # RR <- future_lapply(fl, Exposure$new, future.seed = 950480304L)
        self$RR <- lapply(fl, Exposure$new, design = self$design)
        names(self$RR) <- sapply(self$RR, function(x) x$get_name())
        # invisible(future_lapply(RR, function(x) {
        #   x$gen_stochastic_effect(design, overwrite = FALSE, smooth = FALSE)
        # }, future.seed = 627524136L))
        invisible(lapply(self$RR, function(x) {
          x$gen_stochastic_effect(
            self$design,
            overwrite = FALSE,
            smooth = FALSE
          )
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
        names(self$diseases) <- sapply(
          self$design$sim_prm$diseases,
          `[[`,
          "name"
        )

        message("Generating microsimulation structure.")
        # Generate the graph with the causality structure
        ds <- unlist(strsplit(names(self$RR), "~"))
        ds[grep("^smok_", ds)] <- "smoking"
        ds <- gsub("_prvl$", "", ds)

        ds1 <- ds[as.logical(seq_along(ds) %% 2)]
        ds2 <- ds[!as.logical(seq_along(ds) %% 2)]
        ds <- unique(data.table(ds1, ds2))

        private$causality_structure <- make_graph(
          unlist(transpose(ds)),
          directed = TRUE
        )

        # Japanise standardised population 2015 (esp) weights
        tt <- data.table(
          agegrp = agegrp_name(0, 99),
          wt_esp = c(
            978000,
            4048000,
            5369000,
            5711000,
            6053000,
            6396000,
            6738000,
            7081000,
            7423000,
            7766000,
            8108000,
            8451000,
            8793000,
            9135000,
            9246000,
            7892000,
            6306000,
            4720000,
            3134000,
            1548000,
            423000
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
        environment(private$primary_prevention_scn) <- environment(
          private$update_primary_prevention_scn
        )
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
        environment(private$secondary_prevention_scn) <- environment(
          private$update_secondary_prevention_scn
        )
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
        if (
          anyNA(mc) ||
            any(is.infinite(mc)) ||
            length(mc) < 1L ||
            (length(mc) > 1L && diff(mc[1:2]) == 0) ||
            (length(mc) > 1L &&
              diff(range(diff(mc))) > sqrt(.Machine$double.eps))
        ) {
          stop("mc need to be a sequential integer vector, or a scalar")
        }
        # NOTE mc is in fact mc_aggr. mc_ is the mc of the synthpop
        mc_sp <-
          (min(mc) *
            self$design$sim_prm$n_synthpop_aggregation -
            self$design$sim_prm$n_synthpop_aggregation +
            1L):(max(mc) * self$design$sim_prm$n_synthpop_aggregation)

        if (
          any(file.exists(
            # TODO fix when lifecourse is not saved
            file.path(
              self$design$sim_prm$output_dir,
              "lifecourse",
              paste0(mc, "_lifecourse.cs")
            )
          ))
        ) {
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
          if (self$design$sim_prm$logs)
            private$time_mark("Start of parallelisation")

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
            ) %dopar%
              {
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

          if (self$design$sim_prm$logs)
            private$time_mark("End of parallelisation")
        } else {
          # if multicore = FALSE
          if (self$design$sim_prm$logs) {
            private$time_mark("Start of single-core run")
          }

          lapply(mc_sp, private$run_sim, scenario_nam)

          if (self$design$sim_prm$logs) {
            private$time_mark("End of single-core run")
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
        clbr <- fread(
          "./simulation/calibration_prms.csv",
          colClasses = list(
            numeric = c(
              "chd_incd_clbr_fctr",
              "stroke_incd_clbr_fctr",
              "chd_ftlt_clbr_fctr",
              "stroke_ftlt_clbr_fctr",
              "nonmodelled_ftlt_clbr_fctr"
            )
          )
        )

        memedian <- function(x) {
          out <- median(x)
          if (out == 0L) out <- mean(x)
          out
        }
        if (replace) {
          age_start <- self$design$sim_prm$ageL
        } else {
          # if replace == FALSE
          # if all ages exist skip calibration
          if (
            dim(clbr[
              chd_incd_clbr_fctr == 1 |
                stroke_incd_clbr_fctr == 1 |
                chd_ftlt_clbr_fctr == 1 |
                stroke_ftlt_clbr_fctr == 1 |
                nonmodelled_ftlt_clbr_fctr == 1
            ])[1] ==
              0
          ) {
            message("All ages have been calibrated. Skipping calibration.")
            return(invisible(self))
          }
          age_start <- clbr[
            chd_incd_clbr_fctr == 1 |
              stroke_incd_clbr_fctr == 1 |
              chd_ftlt_clbr_fctr == 1 |
              stroke_ftlt_clbr_fctr == 1 |
              nonmodelled_ftlt_clbr_fctr == 1,
            min(age)
          ] # Unsafe but rarely
          message(paste0("Starting calibration from age ", age_start, "."))
        }

        # Run the simulation from min to max age
        for (age_ in age_start:self$design$sim_prm$ageH) {
          # Run the simulation and export summaries. TODO restrict ages for efficiency.
          self$del_logs()$del_outputs()$run(
            mc,
            multicore = TRUE,
            "sc0"
          )$export_summaries(
            multicore = TRUE,
            type = c("incd", "prvl", "dis_mrtl"),
            single_year_of_age = TRUE
          ) #

          # Incidence calibration
          # load the uncalibrated results
          unclbr <- open_dataset(file.path(
            self$design$sim_prm$output_dir,
            "summaries",
            "incd_scaled_up"
          )) %>%
            filter(age == age_) %>%
            select(
              "year",
              "age",
              "sex",
              "mc",
              "popsize",
              "chd_incd",
              "stroke_incd"
            ) %>%
            collect()
          setDT(unclbr)

          unclbr <- unclbr[,
            .(
              chd_incd = chd_incd / popsize,
              stroke_incd = stroke_incd / popsize
            ),
            keyby = .(age, sex, year, mc)
          ][,
            .(
              chd_incd = memedian(chd_incd),
              stroke_incd = memedian(stroke_incd)
            ),
            keyby = .(age, sex, year)
          ]

          # for CHD
          # fit a log-log linear model to the uncalibrated results and store the coefficients
          unclbr[
            chd_incd > 0,
            c("intercept_unclbr", "trend_unclbr") := as.list(coef(lm(
              log(chd_incd) ~ log(year)
            ))),
            by = sex
          ]
          unclbr[,
            intercept_unclbr := nafill(
              intercept_unclbr,
              "const",
              max(intercept_unclbr, na.rm = TRUE)
            ),
            by = sex
          ] # NOTE I use max just to return a value. It doesn't matter what value it is.
          unclbr[,
            trend_unclbr := nafill(
              trend_unclbr,
              "const",
              max(trend_unclbr, na.rm = TRUE)
            ),
            by = sex
          ] # NOTE I use max just to return a value. It doesn't matter what value it is.
          # load benchmark
          benchmark <- read_fst(
            file.path("./inputs/disease_burden", "chd_incd.fst"),
            columns = c("age", "sex", "year", "mu"),
            as.data.table = TRUE
          )[age == age_, ]
          # fit a log-log linear model to the benchmark incidence and store the coefficients
          benchmark[
            year >= self$design$sim_prm$init_year_long,
            c("intercept_bnchmrk", "trend_bnchmrk") := as.list(coef(lm(
              log(mu) ~ log(year)
            ))),
            by = sex
          ]
          # calculate the calibration factors that the uncalibrated log-log model
          # need to be multiplied with so it can match the benchmark log-log model
          unclbr[
            benchmark[year == max(year)],
            chd_incd_clbr_fctr := exp(
              intercept_bnchmrk + trend_bnchmrk * log(year)
            ) /
              exp(intercept_unclbr + trend_unclbr * log(year)),
            on = c("age", "sex")
          ] # Do not join on year!
          unclbr[, c("intercept_unclbr", "trend_unclbr") := NULL]

          # Repeat for stroke
          unclbr[
            stroke_incd > 0,
            c("intercept_unclbr", "trend_unclbr") := as.list(coef(lm(
              log(stroke_incd) ~ log(year)
            ))),
            by = sex
          ]
          unclbr[,
            intercept_unclbr := nafill(
              intercept_unclbr,
              "const",
              max(intercept_unclbr, na.rm = TRUE)
            ),
            by = sex
          ] # NOTE I use max just to return a value. It doesn't matter what value it is.
          unclbr[,
            trend_unclbr := nafill(
              trend_unclbr,
              "const",
              max(trend_unclbr, na.rm = TRUE)
            ),
            by = sex
          ] # NOTE I use max just to return a value. It doesn't matter what value it is.
          benchmark <- read_fst(
            file.path("./inputs/disease_burden", "stroke_incd.fst"),
            columns = c("age", "sex", "year", "mu"),
            as.data.table = TRUE
          )[age == age_, ]
          benchmark[
            year >= self$design$sim_prm$init_year_long,
            c("intercept_bnchmrk", "trend_bnchmrk") := as.list(coef(lm(
              log(mu) ~ log(year)
            ))),
            by = sex
          ]
          unclbr[
            benchmark[year == max(year)],
            stroke_incd_clbr_fctr := exp(
              intercept_bnchmrk + trend_bnchmrk * log(year)
            ) /
              exp(intercept_unclbr + trend_unclbr * log(year)),
            on = c("age", "sex")
          ] # Do not join on year!

          # keep only year, age, sex, and calibration factors (to be multiplied
          # with p0)
          unclbr[, `:=`(
            chd_prvl_correction = chd_incd * (chd_incd_clbr_fctr - 1),
            stroke_prvl_correction = stroke_incd * (stroke_incd_clbr_fctr - 1),
            chd_incd = NULL,
            stroke_incd = NULL,
            intercept_unclbr = NULL,
            trend_unclbr = NULL
          )]
          clbr[
            unclbr,
            on = c("year", "age", "sex"),
            `:=`(
              chd_incd_clbr_fctr = i.chd_incd_clbr_fctr,
              stroke_incd_clbr_fctr = i.stroke_incd_clbr_fctr
            )
          ]

          # Case fatality calibration
          # Because we do incd and case fatality correction in the same step, we
          # need to estimate the expected changes on prvl because of the incd
          # calibration, before we proceed with the case fatality calibration.
          # Note that the calibration factor (multiplier) is 1/prvl as we
          # currently have mortality rates in the ftlt files.
          prvl <- open_dataset(file.path(
            self$design$sim_prm$output_dir,
            "summaries",
            "prvl_scaled_up"
          )) %>%
            filter(age == age_) %>%
            select(
              "year",
              "age",
              "sex",
              "mc",
              "popsize",
              "chd_prvl",
              "stroke_prvl"
            ) %>%
            collect()
          setDT(prvl)

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
            popsize,
            age,
            sex,
            year,
            mc
          )][,
            .(
              chd_prvl = memedian(chd_prvl),
              stroke_prvl = memedian(stroke_prvl),
              popsize = memedian(popsize)
            ),
            keyby = .(age, sex, year)
          ]
          prvl[
            unclbr,
            on = c("year", "age", "sex"),
            `:=`(
              chd_prvl_correction = i.chd_prvl_correction, # Note corrections for prvl are rates
              stroke_prvl_correction = i.stroke_prvl_correction
            )
          ]
          benchmark <- read_fst(
            file.path("./inputs/disease_burden", "chd_ftlt.fst"),
            columns = c("age", "sex", "year", "mu2"),
            as.data.table = TRUE
          )[age == age_, ]
          prvl[benchmark, on = c("age", "sex", "year"), chd_mrtl := mu2]
          benchmark <- read_fst(
            file.path("./inputs/disease_burden", "stroke_ftlt.fst"),
            columns = c("age", "sex", "year", "mu2"),
            as.data.table = TRUE
          )[age == age_, ]
          prvl[benchmark, on = c("age", "sex", "year"), stroke_mrtl := mu2]
          benchmark <- read_fst(
            file.path("./inputs/disease_burden", "nonmodelled_ftlt.fst"),
            columns = c("age", "sex", "year", "mu2"),
            as.data.table = TRUE
          )[age == age_, ]
          prvl[benchmark, on = c("age", "sex", "year"), nonmodelled_mrtl := mu2]

          prvl[, `:=`(
            chd_ftlt_clbr_fctr = 1 / (chd_prvl + chd_prvl_correction), #  - stroke_mrtl - nonmodelled_mrtl
            stroke_ftlt_clbr_fctr = 1 / (stroke_prvl + stroke_prvl_correction), #  - chd_mrtl - nonmodelled_mrtl
            nonmodelled_ftlt_clbr_fctr = 1 / (1 - chd_mrtl - stroke_mrtl)
          )]

          # Fix the calibration factors for the ages that have been calibrated
          if (age_ > age_start) {
            # NOTE here age is age - 1L
            mrtl <- open_dataset(file.path(
              self$design$sim_prm$output_dir,
              "summaries",
              "mrtl_scaled_up"
            )) %>%
              filter(age == age_ - 1L) %>%
              select(
                "year",
                "age",
                "sex",
                "mc",
                "popsize",
                "chd_deaths",
                "stroke_deaths",
                "nonmodelled_deaths"
              ) %>%
              collect()
            setDT(mrtl)

            mrtl <- mrtl[, .(
              chd_mrtl = chd_deaths / popsize,
              stroke_mrtl = stroke_deaths / popsize,
              nonmodelled_mrtl = nonmodelled_deaths / popsize,
              popsize,
              age,
              sex,
              year,
              mc
            )][,
              .(
                chd_mrtl = memedian(chd_mrtl),
                stroke_mrtl = memedian(stroke_mrtl),
                nonmodelled_mrtl = memedian(nonmodelled_mrtl),
                popsize = memedian(popsize)
              ),
              keyby = .(age, sex, year)
            ]
            benchmark <- read_fst(
              file.path("./inputs/disease_burden", "chd_ftlt.fst"),
              columns = c("age", "sex", "year", "mu2"),
              as.data.table = TRUE
            )[age == age_ - 1L, ]
            mrtl[
              benchmark,
              on = c("age", "sex", "year"),
              chd_ftlt_clbr_fctr := mu2 / chd_mrtl
            ]
            benchmark <- read_fst(
              file.path("./inputs/disease_burden", "stroke_ftlt.fst"),
              columns = c("age", "sex", "year", "mu2"),
              as.data.table = TRUE
            )[age == age_ - 1L, ]
            mrtl[
              benchmark,
              on = c("age", "sex", "year"),
              stroke_ftlt_clbr_fctr := mu2 / stroke_mrtl
            ]
            benchmark <- read_fst(
              file.path("./inputs/disease_burden", "nonmodelled_ftlt.fst"),
              columns = c("age", "sex", "year", "mu2"),
              as.data.table = TRUE
            )[age == age_ - 1L, ]
            mrtl[
              benchmark,
              on = c("age", "sex", "year"),
              nonmodelled_ftlt_clbr_fctr := mu2 / nonmodelled_mrtl
            ]
            mrtl[chd_ftlt_clbr_fctr == Inf, chd_ftlt_clbr_fctr := 1] # to avoid Inf through division by 0
            mrtl[stroke_ftlt_clbr_fctr == Inf, stroke_ftlt_clbr_fctr := 1] # to avoid Inf through division by 0
            mrtl[
              nonmodelled_ftlt_clbr_fctr == Inf,
              nonmodelled_ftlt_clbr_fctr := 1
            ] # to avoid Inf through division by 0

            clbr[
              mrtl,
              on = c("year", "age", "sex"),
              `:=`(
                chd_ftlt_clbr_fctr = i.chd_ftlt_clbr_fctr * chd_ftlt_clbr_fctr,
                stroke_ftlt_clbr_fctr = i.stroke_ftlt_clbr_fctr *
                  stroke_ftlt_clbr_fctr,
                nonmodelled_ftlt_clbr_fctr = i.nonmodelled_ftlt_clbr_fctr *
                  nonmodelled_ftlt_clbr_fctr
              )
            ]
          }

          if (age_ == self$design$sim_prm$ageH) {
            # shortcut for age == 99 hopefully with tiny bias
            mrtl[, age := age + 1L]
            prvl[
              mrtl,
              on = c("year", "age", "sex"),
              `:=`(
                chd_ftlt_clbr_fctr = i.chd_ftlt_clbr_fctr * chd_ftlt_clbr_fctr,
                stroke_ftlt_clbr_fctr = i.stroke_ftlt_clbr_fctr *
                  stroke_ftlt_clbr_fctr,
                nonmodelled_ftlt_clbr_fctr = i.nonmodelled_ftlt_clbr_fctr *
                  nonmodelled_ftlt_clbr_fctr
              )
            ]
          }

          clbr[
            prvl,
            on = c("year", "age", "sex"),
            `:=`(
              chd_ftlt_clbr_fctr = i.chd_ftlt_clbr_fctr,
              stroke_ftlt_clbr_fctr = i.stroke_ftlt_clbr_fctr,
              nonmodelled_ftlt_clbr_fctr = i.nonmodelled_ftlt_clbr_fctr
            )
          ]

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
      export_summaries = function(
        multicore = TRUE,
        type = c(
          "le",
          "hle",
          "dis_char",
          "prvl",
          "incd",
          "dis_mrtl",
          "mrtl",
          "all_cause_mrtl_by_dis",
          "cms",
          "qalys",
          "costs"
        ),
        single_year_of_age = FALSE
      ) {
        lc <- open_dataset(private$output_dir("lifecourse"))

        # Connect DuckDB and register the Arrow dataset as a DuckDB view
        con <- dbConnect(duckdb::duckdb(), ":memory:", read_only = TRUE)
        on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
        duckdb::duckdb_register_arrow(con, "lc_table", lc)

        mc_set <- dbGetQuery(con, "SELECT DISTINCT mc FROM lc_table")$mc

        # logic to avoid inappropriate dual processing of already processed mc iterations
        # TODO take into account scenarios
        if ("le" %in% type) {
          file_pth <- private$output_dir("summaries/le_scaled_up")
        } else if ("hle" %in% type) {
          file_pth <- private$output_dir("summaries/hle_1st_cond_scaled_up")
        } else if ("cms" %in% type) {
          file_pth <- private$output_dir("summaries/cms_count_scaled_up")
        } else if ("mrtl" %in% type) {
          file_pth <- private$output_dir("summaries/mrtl_scaled_up")
        } else if ("dis_mrtl" %in% type) {
          file_pth <- private$output_dir("summaries/dis_mrtl_scaled_up")
        } else if ("dis_char" %in% type) {
          file_pth <- private$output_dir(
            "summaries/dis_characteristics_scaled_up"
          )
        } else if ("incd" %in% type) {
          file_pth <- private$output_dir("summaries/incd_scaled_up")
        } else if ("prvl" %in% type) {
          file_pth <- private$output_dir("summaries/prvl_scaled_up")
        } else if ("all_cause_mrtl_by_dis" %in% type) {
          file_pth <- private$output_dir(
            "summaries/all_cause_mrtl_by_dis_scaled_up"
          )
        } else if ("qalys" %in% type) {
          file_pth <- private$output_dir("summaries/qalys_scaled_up")
        } else if ("costs" %in% type) {
          file_pth <- private$output_dir("summaries/costs_scaled_up")
        } else {
          stop("Unknown type of summary")
        }

        if (file.exists(file_pth) && length(list.files(file_pth)) > 0) {
          con2 <- dbConnect(duckdb::duckdb(), ":memory:", read_only = TRUE)
          duckdb::duckdb_register_arrow(
            con2,
            "tbl",
            open_dataset(file_pth, format = "parquet")
          )
          mc_toexclude <- dbGetQuery(con2, "SELECT DISTINCT mc FROM tbl")$mc
          dbDisconnect(con2, shutdown = TRUE)
          mc_set <- mc_set[!mc_set %in% mc_toexclude]
        }

        if (length(mc_set) == 0) {
          message(
            "All required Monte Carlo iterations already processed for type: ",
            paste(type, collapse = ", "),
            ". Skipping summary export."
          )
          return(invisible(self)) # Exit if nothing left to process
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
                  library(R6)
                  library(arrow)
                  library(duckdb)
                  library(data.table)
                })),
                rscript_args = c(
                  "--no-init-file",
                  "--no-site-file",
                  "--no-environ"
                ),
                setup_strategy = "parallel"
              ) # used for clustering. Windows compatible

            on.exit(if (exists("cl")) stopCluster(cl), add = TRUE)

            parLapplyLB(
              cl = cl,
              X = seq_along(mc_set),
              fun = function(i) {
                private$export_summaries_hlpr(
                  con,
                  mcaggr = i,
                  type = type,
                  single_year_of_age = single_year_of_age
                )
                NULL
              }
            )
          } else {
            registerDoParallel(self$design$sim_prm$clusternumber_export) # used for forking. Only Linux/OSX compatible
            xps_dt <- foreach(
              i = seq_along(mc_set),
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
            ) %dopar%
              {
                private$export_summaries_hlpr(
                  con,
                  mcaggr = i,
                  type = type,
                  single_year_of_age = single_year_of_age
                )
                NULL
              }
          }

          if (self$design$sim_prm$logs) {
            private$time_mark("End of exporting summuries")
          }
        } else {
          # if multicore = FALSE
          if (self$design$sim_prm$logs) {
            private$time_mark("Start of single-core summaries export")
          }

          lapply(seq_along(mc_set), function(i) {
            private$export_summaries_hlpr(
              con,
              mcaggr = i,
              type = type,
              single_year_of_age = single_year_of_age
            )
            NULL
          })

          if (self$design$sim_prm$logs) {
            private$time_mark("End of single-core summaries export")
          }
        } # end of multicore = FALSE

        while (sink.number() > 0L) sink()

        invisible(self)
      },

      # export_tables ----
      #' @description
      #' Export summary tables for the simulation results.
      #'
      #' This method generates and exports summary tables for the main simulation outputs,
      #' including prevalence, incidence, mortality, disease characteristics, and exposures.
      #' It calls modular helper methods for each type of summary, ensuring output directories
      #' are created as needed and that all tables are written to the appropriate locations.
      #'
      #' @param baseline_year_for_change_outputs Integer. The baseline year to use for change outputs (default: 2019L).
      #' @param prbl Numeric vector. The quantiles to use for summary statistics (default: c(0.5, 0.025, 0.975, 0.1, 0.9)).
      #'
      #' @details
      #' This method is a high-level wrapper that orchestrates the export of all main summary tables.
      #' It delegates the actual export logic to the following private helper methods:
      #' - \code{private$export_main_tables}
      #' - \code{private$export_all_cause_mrtl_tables}
      #' - \code{private$export_disease_characteristics_tables}
      #' - \code{private$export_xps_tables}
      #'
      #' Each helper method is responsible for a specific set of outputs and ensures that
      #' the results are saved in the correct format and location.
      #'
      #' @return The invisible self for chaining.
      #'
      #' @examples
      #' IMPACTncd$export_tables()
      export_tables = function(
        baseline_year_for_change_outputs = 2019L,
        prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)
      ) {
        private$export_main_tables(
          prbl,
          baseline_year_for_change_outputs,
          private$output_dir()
        )
        private$export_all_cause_mrtl_tables(
          prbl,
          private$output_dir("summaries"),
          private$output_dir("tables")
        )
        private$export_disease_characteristics_tables(
          prbl,
          private$output_dir("summaries"),
          private$output_dir("tables")
        )
        private$export_xps_tables(
          prbl,
          private$output_dir(),
          private$output_dir("tables")
        )

        invisible(self)
      }, # end of export_tables

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
      get_causal_structure = function(
        processed = TRUE,
        print_plot = FALSE,
        focus = FALSE
      ) {
        if (missing(focus)) {
          graph <- private$causality_structure
        } else {
          if (length(focus) > 1L) stop("focus need to be scalar string.")
          if (!focus %in% self$get_node_names())
            stop(
              "focus need to be a node name. Use get_node_names() to get the list of eligible values."
            )
          graph <- make_ego_graph(
            private$causality_structure,
            order = 1,
            nodes = focus,
            mode = "in"
          )[[1]]
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
          out <- get.all.shortest.paths(
            private$causality_structure,
            from,
            to,
            mode = "out"
          )
        } else {
          out <- all_simple_paths(
            private$causality_structure,
            from,
            to,
            mode = "out"
          )
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
          fl <- list.files(
            self$design$sim_prm$output_dir,
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

      #' @description Delete all output summary files.
      #' @return The invisible self for chaining.
      del_summaries = function() {
        pth <- file.path(self$design$sim_prm$output_dir, "summaries")
        if (dir.exists(pth)) {
          fl <- list.files(pth, full.names = TRUE, recursive = TRUE)

          file.remove(fl)

          if (length(fl) > 0 && self$design$sim_prm$logs) {
            message("Output summary files deleted.")
          }
        } else {
          message("Output summaries folder doesn't exist.")
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

        data_pop <- read_fst(
          "./inputs/pop_projections/combined_population_japan.fst",
          columns = c("year", "age", "sex", "pops"),
          as.data.table = TRUE
        )
        data_pop_agegrp <- copy(data_pop)
        to_agegrp(data_pop_agegrp, 5, 99)
        data_pop_agegrp <- data_pop_agegrp[,
          .(pops = sum(pops)),
          keyby = .(year, agegrp, sex)
        ]

        # MRTL
        mdd <- open_dataset(file.path(
          self$design$sim_prm$output_dir,
          "summaries",
          "dis_mrtl_scaled_up"
        )) %>%
          filter(scenario == "sc0") %>%
          collect()
        setDT(mdd)
        mdd[, `:=`(
          nonmodelled_mrtl_rate = nonmodelled_deaths / popsize,
          chd_mrtl_rate = chd_deaths / popsize,
          stroke_mrtl_rate = stroke_deaths / popsize
        )]
        mdd <- mdd[,
          .(
            nonmodelled_mrtl_rate = quantile(nonmodelled_mrtl_rate, p = 0.500),
            nonmodelled_mrtl_rate_low = quantile(
              nonmodelled_mrtl_rate,
              p = 0.025
            ),
            nonmodelled_mrtl_rate_upp = quantile(
              nonmodelled_mrtl_rate,
              p = 0.975
            ),
            chd_mrtl_rate = quantile(chd_mrtl_rate, p = 0.500),
            chd_mrtl_rate_low = quantile(chd_mrtl_rate, p = 0.025),
            chd_mrtl_rate_upp = quantile(chd_mrtl_rate, p = 0.957),
            stroke_mrtl_rate = quantile(stroke_mrtl_rate, p = 0.500),
            stroke_mrtl_rate_low = quantile(stroke_mrtl_rate, p = 0.025),
            stroke_mrtl_rate_upp = quantile(stroke_mrtl_rate, p = 0.975),
            type = "modelled"
          ),
          keyby = .(year, agegrp, sex)
        ]

        obs <- read_fst(
          paste0("./inputs/disease_burden/", "chd_ftlt.fst"),
          columns = c("age", "year", "sex", "mu2", "mu_lower", "mu_upper"),
          as.data.table = TRUE
        )
        setnames(
          obs,
          c("mu2", "mu_lower", "mu_upper"),
          c("chd_mrtl_rate", "chd_mrtl_rate_low", "chd_mrtl_rate_upp")
        )
        tt <- read_fst(
          paste0("./inputs/disease_burden/", "stroke_ftlt.fst"),
          columns = c("age", "year", "sex", "mu2", "mu_lower", "mu_upper"),
          as.data.table = TRUE
        )
        setnames(
          tt,
          c("mu2", "mu_lower", "mu_upper"),
          c("stroke_mrtl_rate", "stroke_mrtl_rate_low", "stroke_mrtl_rate_upp")
        )
        absorb_dt(obs, tt)
        tt <- read_fst(
          paste0("./inputs/disease_burden/", "nonmodelled_ftlt.fst"),
          columns = c("age", "year", "sex", "mu2", "mu_lower", "mu_upper"),
          as.data.table = TRUE
        )
        setnames(
          tt,
          c("mu2", "mu_lower", "mu_upper"),
          c(
            "nonmodelled_mrtl_rate",
            "nonmodelled_mrtl_rate_low",
            "nonmodelled_mrtl_rate_upp"
          )
        )
        absorb_dt(obs, tt)
        absorb_dt(obs, data_pop)
        to_agegrp(obs, 5, 99)
        obs <- obs[,
          lapply(.SD, weighted.mean, w = pops),
          .SDcols = -c("pops", "age"),
          keyby = .(agegrp, year, sex)
        ]
        obs[, type := "observed"]
        dt <- rbindlist(list(obs, mdd), use.names = TRUE)

        p <- ggplot() +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = chd_mrtl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = chd_mrtl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = chd_mrtl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD mrtl rate", "Men")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "CHD_as_men_mrtl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = chd_mrtl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = chd_mrtl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = chd_mrtl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD mrtl rate", "Women")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "CHD_as_women_mrtl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = stroke_mrtl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = stroke_mrtl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = stroke_mrtl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke mrtl rate", "Men")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "stroke_as_men_mrtl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = stroke_mrtl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = stroke_mrtl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = stroke_mrtl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke mrtl rate", "Women")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "stroke_as_women_mrtl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = nonmodelled_mrtl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = nonmodelled_mrtl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = nonmodelled_mrtl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Nonmodelled mrtl rate", "Men")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "nonmodelled_as_men_mrtl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = nonmodelled_mrtl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = nonmodelled_mrtl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = nonmodelled_mrtl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Nonmodelled mrtl rate", "Women")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "nonmodelled_as_women_mrtl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        # MRTL by sex alone
        absorb_dt(dt, data_pop_agegrp)
        dt <- dt[,
          lapply(.SD, weighted.mean, w = pops),
          .SDcols = -c("pops", "agegrp"),
          keyby = .(type, year, sex)
        ]

        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = chd_mrtl_rate, color = type)) +
          geom_line(
            data = dt,
            aes(x = year, y = chd_mrtl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = chd_mrtl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD mrtl rate", "By sex")
        ggsave(
          file.path(self$design$sim_prm$output_dir, "plots", "CHD_s_mrtl.jpg"),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt,
            aes(x = year, y = stroke_mrtl_rate, color = type)
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = stroke_mrtl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = stroke_mrtl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke mrtl rate", "By sex")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "stroke_s_mrtl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt,
            aes(x = year, y = nonmodelled_mrtl_rate, color = type)
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = nonmodelled_mrtl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = nonmodelled_mrtl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Nonmodelled mrtl rate", "By sex")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "nonmodelled_s_mrtl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        # INCD
        mdd <- open_dataset(file.path(
          self$design$sim_prm$output_dir,
          "summaries",
          "incd_scaled_up"
        )) %>%
          filter(scenario == "sc0") %>%
          select(
            "mc",
            "scenario",
            "year",
            "agegrp",
            "sex",
            "popsize",
            "chd_incd",
            "stroke_incd"
          ) %>%
          collect()
        setDT(mdd)
        mdd[, `:=`(
          chd_incd_rate = chd_incd / popsize,
          stroke_incd_rate = stroke_incd / popsize
        )]
        mdd <- mdd[,
          .(
            chd_incd_rate = quantile(chd_incd_rate, p = 0.500),
            chd_incd_rate_low = quantile(chd_incd_rate, p = 0.025),
            chd_incd_rate_upp = quantile(chd_incd_rate, p = 0.957),
            stroke_incd_rate = quantile(stroke_incd_rate, p = 0.500),
            stroke_incd_rate_low = quantile(stroke_incd_rate, p = 0.025),
            stroke_incd_rate_upp = quantile(stroke_incd_rate, p = 0.975),
            type = "modelled"
          ),
          keyby = .(year, agegrp, sex)
        ]

        obs <- read_fst(
          paste0("./inputs/disease_burden/", "chd_incd.fst"),
          columns = c("age", "year", "sex", "mu", "mu_lower", "mu_upper"),
          as.data.table = TRUE
        )
        setnames(
          obs,
          c("mu", "mu_lower", "mu_upper"),
          c("chd_incd_rate", "chd_incd_rate_low", "chd_incd_rate_upp")
        )
        tt <- read_fst(
          paste0("./inputs/disease_burden/", "stroke_incd.fst"),
          columns = c("age", "year", "sex", "mu", "mu_lower", "mu_upper"),
          as.data.table = TRUE
        )
        setnames(
          tt,
          c("mu", "mu_lower", "mu_upper"),
          c("stroke_incd_rate", "stroke_incd_rate_low", "stroke_incd_rate_upp")
        )
        absorb_dt(obs, tt)
        absorb_dt(obs, data_pop)
        to_agegrp(obs, 5, 99)
        obs <- obs[,
          lapply(.SD, weighted.mean, w = pops),
          .SDcols = -c("pops", "age"),
          keyby = .(agegrp, year, sex)
        ]
        obs[, type := "observed"]
        dt <- rbindlist(list(obs, mdd), use.names = TRUE)

        p <- ggplot() +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = chd_incd_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = chd_incd_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = chd_incd_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD incd rate", "Men")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "CHD_as_men_incd.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = chd_incd_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = chd_incd_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = chd_incd_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD incd rate", "Women")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "CHD_as_women_incd.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = stroke_incd_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = stroke_incd_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = stroke_incd_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke incd rate", "Men")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "stroke_as_men_incd.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = stroke_incd_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = stroke_incd_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = stroke_incd_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke incd rate", "Women")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "stroke_as_women_incd.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        # INCD by sex alone
        absorb_dt(dt, data_pop_agegrp)
        dt <- dt[,
          lapply(.SD, weighted.mean, w = pops),
          .SDcols = -c("pops", "agegrp"),
          keyby = .(type, year, sex)
        ]

        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = chd_incd_rate, color = type)) +
          geom_line(
            data = dt,
            aes(x = year, y = chd_incd_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = chd_incd_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD incd rate", "By sex")
        ggsave(
          file.path(self$design$sim_prm$output_dir, "plots", "CHD_s_incd.jpg"),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt,
            aes(x = year, y = stroke_incd_rate, color = type)
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = stroke_incd_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = stroke_incd_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke incd rate", "By sex")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "stroke_s_incd.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        # PRVL
        mdd <- open_dataset(file.path(
          self$design$sim_prm$output_dir,
          "summaries",
          "prvl_scaled_up"
        )) %>%
          filter(scenario == "sc0") %>%
          select(
            "mc",
            "scenario",
            "year",
            "agegrp",
            "sex",
            "popsize",
            "chd_prvl",
            "stroke_prvl"
          ) %>%
          collect()
        setDT(mdd)

        mdd[, `:=`(
          chd_prvl_rate = chd_prvl / popsize,
          stroke_prvl_rate = stroke_prvl / popsize
        )]
        mdd <- mdd[,
          .(
            chd_prvl_rate = quantile(chd_prvl_rate, p = 0.500),
            chd_prvl_rate_low = quantile(chd_prvl_rate, p = 0.025),
            chd_prvl_rate_upp = quantile(chd_prvl_rate, p = 0.957),
            stroke_prvl_rate = quantile(stroke_prvl_rate, p = 0.500),
            stroke_prvl_rate_low = quantile(stroke_prvl_rate, p = 0.025),
            stroke_prvl_rate_upp = quantile(stroke_prvl_rate, p = 0.975),
            type = "modelled"
          ),
          keyby = .(year, agegrp, sex)
        ]

        obs <- read_fst(
          paste0("./inputs/disease_burden/", "chd_prvl.fst"),
          columns = c(
            "age",
            "year",
            "sex",
            "mu",
            "mu_lower",
            "mu_upper",
            "prvl_mltp"
          ),
          as.data.table = TRUE
        )
        setnames(
          obs,
          c("mu", "mu_lower", "mu_upper"),
          c("chd_prvl_rate", "chd_prvl_rate_low", "chd_prvl_rate_upp")
        )
        obs[, `:=`(
          chd_prvl_rate = chd_prvl_rate * prvl_mltp,
          chd_prvl_rate_low = chd_prvl_rate_low * prvl_mltp,
          chd_prvl_rate_upp = chd_prvl_rate_upp * prvl_mltp,
          prvl_mltp = NULL
        )]
        tt <- read_fst(
          paste0("./inputs/disease_burden/", "stroke_prvl.fst"),
          columns = c(
            "age",
            "year",
            "sex",
            "mu",
            "mu_lower",
            "mu_upper",
            "prvl_mltp"
          ),
          as.data.table = TRUE
        )
        setnames(
          tt,
          c("mu", "mu_lower", "mu_upper"),
          c("stroke_prvl_rate", "stroke_prvl_rate_low", "stroke_prvl_rate_upp")
        )
        tt[, `:=`(
          stroke_prvl_rate = stroke_prvl_rate * prvl_mltp,
          stroke_prvl_rate_low = stroke_prvl_rate_low * prvl_mltp,
          stroke_prvl_rate_upp = stroke_prvl_rate_upp * prvl_mltp,
          prvl_mltp = NULL
        )]
        absorb_dt(obs, tt)
        absorb_dt(obs, data_pop)
        to_agegrp(obs, 5, 99)
        obs <- obs[,
          lapply(.SD, weighted.mean, w = pops),
          .SDcols = -c("pops", "age"),
          keyby = .(agegrp, year, sex)
        ]
        obs[, type := "observed"]
        dt <- rbindlist(list(obs, mdd), use.names = TRUE)

        p <- ggplot() +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = chd_prvl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = chd_prvl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = chd_prvl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD prvl rate", "Men")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "CHD_as_men_prvl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = chd_prvl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = chd_prvl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = chd_prvl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD prvl rate", "Women")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "CHD_as_women_prvl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = stroke_prvl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = stroke_prvl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "men"],
            aes(x = year, y = stroke_prvl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke prvl rate", "Men")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "stroke_as_men_prvl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = stroke_prvl_rate, color = type)
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = stroke_prvl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt[sex == "women"],
            aes(x = year, y = stroke_prvl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(agegrp), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke prvl rate", "Women")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "stroke_as_women_prvl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        # prvl by sex alone
        absorb_dt(dt, data_pop_agegrp)
        dt <- dt[,
          lapply(.SD, weighted.mean, w = pops),
          .SDcols = -c("pops", "agegrp"),
          keyby = .(type, year, sex)
        ]

        p <- ggplot() +
          geom_line(data = dt, aes(x = year, y = chd_prvl_rate, color = type)) +
          geom_line(
            data = dt,
            aes(x = year, y = chd_prvl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = chd_prvl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("CHD prvl rate", "By sex")
        ggsave(
          file.path(self$design$sim_prm$output_dir, "plots", "CHD_s_prvl.jpg"),
          p,
          height = HEIGHT,
          width = WIDTH
        )

        p <- ggplot() +
          geom_line(
            data = dt,
            aes(x = year, y = stroke_prvl_rate, color = type)
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = stroke_prvl_rate_low, color = type),
            linetype = "dashed"
          ) +
          geom_line(
            data = dt,
            aes(x = year, y = stroke_prvl_rate_upp, color = type),
            linetype = "dashed"
          ) +
          facet_wrap(. ~ factor(sex), scales = "free") +
          theme_bw() +
          theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
          ggtitle("Stroke prvl rate", "By sex")
        ggsave(
          file.path(
            self$design$sim_prm$output_dir,
            "plots",
            "stroke_s_prvl.jpg"
          ),
          p,
          height = HEIGHT,
          width = WIDTH
        )
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
        fl <- grep(
          "/synthpop/|/outputs/",
          fl,
          ignore.case = TRUE,
          value = TRUE,
          invert = TRUE
        )
        if (length(fl) == 0) {
          # no large files. Early escape.
          return(invisible(self))
        }

        # Merge with existing large files from previous uploads. This ensures
        # that if any previously large files are now smaller than 50MB, they are
        # still processed as if they are still more than 50Mb. This is perhaps
        # inefficient but has less side effects.  Otherwise code for special
        # case of files above the threshold that shrinked for whatever reason
        # below the threshold is needed (i.e. remove them from .gitignore).
        if (file.exists("./simulation/large_files_indx.csv")) {
          fl <- sort(unique(c(
            fread("./simulation/large_files_indx.csv")$pths,
            fl
          )))
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
          x$gen_parf(sp, self$design, self$diseases)$set_init_prvl(
            sp = sp,
            design_ = self$design
          )
        })

        private$primary_prevention_scn(sp) # apply primary pevention scenario

        lapply(self$diseases, function(x) {
          x$set_rr(sp, self$design)$set_incd_prb(sp, self$design)$set_dgns_prb(
            sp,
            self$design
          )$set_mrtl_prb(sp, self$design)
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
        sp$pop <- sp$pop[
          all_cause_mrtl >= 0L &
            year >= self$design$sim_prm$init_year_long &
            between(age, self$design$sim_prm$ageL, self$design$sim_prm$ageH),
        ]
        setkey(sp$pop, pid, year)
        sp$pop[, pid_mrk := mk_new_simulant_markers(pid)]

        # apply ESP weights
        to_agegrp(sp$pop, 5, 99)
        absorb_dt(sp$pop, private$esp_weights)
        sp$pop[,
          wt_esp := wt_esp * unique(wt_esp) / sum(wt_esp),
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
        # sp$pop[, mc := sp$mc_aggr]

        # TODO add logic for the years of having MM. Currently 1 is not the real
        # incidence. It is still prevalence
        sp$pop[, `:=`(
          cms1st_cont_prvl = carry_forward_incr(
            as.integer(cms_count == 1),
            pid_mrk,
            TRUE,
            1L,
            byref = TRUE
          ),
          cmsmm0_prvl = carry_forward_incr(
            as.integer(cms_score > 0),
            pid_mrk,
            TRUE,
            1L,
            byref = TRUE
          ),
          cmsmm1_prvl = carry_forward_incr(
            as.integer(cms_score > 1),
            pid_mrk,
            TRUE,
            1L,
            byref = TRUE
          ),
          cmsmm1.5_prvl = carry_forward_incr(
            as.integer(cms_score > 1.5),
            pid_mrk,
            TRUE,
            1L,
            byref = TRUE
          ),
          cmsmm2_prvl = carry_forward_incr(
            as.integer(cms_score > 2),
            pid_mrk,
            TRUE,
            1L,
            byref = TRUE
          )
        )]

        # sp$pop[, scenario := scenario_nam]

        setkeyv(sp$pop, c("pid", "year"))

        # Write lifecourse
        if (self$design$sim_prm$logs) message("Exporting lifecourse...")

        fileformat <- "parquet"
        fnam <- private$output_dir(paste0(
          "lifecourse/",
          "scenario=",
          scenario_nam,
          "/",
          "mc=",
          sp$mc_aggr,
          "/",
          sp$mc,
          "_lifecourse.",
          fileformat
        ))
        # NOTE parquet format about 30 times smaller but about 50% slower in writting to disk
        write_dataset(dataset = sp$pop, path = fnam, format = fileformat)

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

      # export_xps ----
      export_xps = function(sp, scenario_nam) {
        # NOTE no need to check validity of inputs here as it is only used
        # internally

        to_agegrp(
          sp$pop,
          grp_width = 20L,
          max_age = self$design$sim_prm$ageH,
          min_age = self$design$sim_prm$ageL,
          age_colname = "age",
          agegrp_colname = "agegrp20",
          to_factor = TRUE
        )

        sp$pop[,
          smok_never_curr_xps := fifelse(Smoking_curr_xps == "1", 1L, 0L)
        ]
        sp$pop[,
          smok_active_curr_xps := fifelse(Smoking_curr_xps == "3", 1L, 0L)
        ]
        sp$pop[,
          pa567_curr_xps := fifelse(
            PA_days_curr_xps %in% c("5", "6", "7"),
            1L,
            0L
          )
        ]

        xps <- grep("_curr_xps$", names(sp$pop), value = TRUE)
        xps <- grep("_prvl_curr_xps$", xps, value = TRUE, invert = TRUE)
        xps <- xps[-which(xps %in% c("Smoking_curr_xps", "PA_days_curr_xps"))]
        sp$pop[
          Smoking_curr_xps != "3",
          `:=`(
            Smoking_number_curr_xps = NA
          )
        ]

        out_xps20 <- groupingsets(
          sp$pop[
            all_cause_mrtl >= 0L &
              year >= self$design$sim_prm$init_year_long &
              age >= self$design$sim_prm$ageL,
          ],
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

        fileformat <- "parquet"
        fnam <- private$output_dir(paste0(
          "xps/xps20/",
          "scenario=",
          scenario_nam,
          "/",
          "mc=",
          sp$mc_aggr,
          "/",
          sp$mc,
          "_xps20.",
          fileformat
        ))
        # NOTE parquet format about 30 times smaller but about 50% slower in writting to disk
        write_dataset(dataset = out_xps20, path = fnam, format = fileformat)

        # TODO link strata in the outputs to the design.yaml
        out_xps5 <- groupingsets(
          sp$pop[
            all_cause_mrtl >= 0L &
              year >= self$design$sim_prm$init_year_long &
              age >= self$design$sim_prm$ageL,
          ],
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

        fnam <- private$output_dir(paste0(
          "xps/xps5/",
          "scenario=",
          scenario_nam,
          "/",
          "mc=",
          sp$mc_aggr,
          "/",
          sp$mc,
          "_xps_esp.",
          fileformat
        ))
        # NOTE parquet format about 30 times smaller but about 50% slower in writting to disk
        write_dataset(dataset = out_xps5, path = fnam, format = fileformat)

        # Tidy up
        sp$pop[,
          c(
            "agegrp20",
            "smok_never_curr_xps",
            "smok_active_curr_xps",
            "pa567_curr_xps"
          ) := NULL
        ]
        sp$pop[
          Smoking_curr_xps != "3",
          `:=`(
            Smoking_number_curr_xps = 0L
          )
        ]

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
      # duckdb_con is the duckdb connection to the partitioned parquet lifecourse file
      # mcaggr is the mc iteration
      # single_year_of_age export summaries by single year of age to be used for calibration
      # export_summaries_hlpr ----
      export_summaries_hlpr = function(
        duckdb_con,
        mcaggr,
        type = c(
          "le",
          "hle",
          "dis_char",
          "prvl",
          "incd",
          "mrtl",
          "dis_mrtl",
          "all_cause_mrtl_by_dis",
          "cms",
          "qalys",
          "costs"
        ),
        single_year_of_age = FALSE
      ) {
        if (self$design$sim_prm$logs) message("Exporting summaries...")

        strata <- c("mc", self$design$sim_prm$strata_for_output)
        strata_noagegrp <- c(
          "mc",
          setdiff(self$design$sim_prm$strata_for_output, c("agegrp"))
        )
        strata_age <- c(strata_noagegrp, "age")

        if (single_year_of_age) strata <- strata_age # used for calibrate_incd_ftlt

        ext <- "parquet"

        if ("le" %in% type)
          private$export_le_summaries(duckdb_con, mcaggr, strata_noagegrp, ext)
        if ("hle" %in% type)
          private$export_hle_summaries(duckdb_con, mcaggr, strata_noagegrp, ext)
        if ("dis_char" %in% type)
          private$export_dis_char_summaries(
            duckdb_con,
            mcaggr,
            strata_noagegrp,
            ext
          )
        if ("prvl" %in% type)
          private$export_prvl_summaries(duckdb_con, mcaggr, strata, ext)
        if ("incd" %in% type)
          private$export_incd_summaries(duckdb_con, mcaggr, strata, ext)
        if ("mrtl" %in% type)
          private$export_mrtl_summaries(duckdb_con, mcaggr, strata, ext)
        if ("dis_mrtl" %in% type)
          private$export_dis_mrtl_summaries(duckdb_con, mcaggr, strata, ext)
        if ("all_cause_mrtl_by_dis" %in% type)
          private$export_all_cause_mrtl_by_dis_summaries(
            duckdb_con,
            mcaggr,
            strata,
            ext
          )
        if ("cms" %in% type)
          private$export_cms_summaries(
            duckdb_con,
            mcaggr,
            strata,
            strata_age,
            ext
          )
        if ("qalys" %in% type)
          private$export_qaly_summaries(duckdb_con, mcaggr, strata, ext)
        if ("costs" %in% type)
          private$export_costs_summaries(duckdb_con, mcaggr, strata, ext)

        return(invisible(self))
      }, # end of export_summaries_hlpr

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

      # Calculates QALYs (EQ5D5L and HUI3) and creates a new temporary view in DuckDB
      # with these additional columns.
      #
      # Args:
      #   duckdb_con: A DuckDB connection object.
      #   input_table_name: Name of the input table/view in DuckDB (e.g., "lc_table").
      #   output_view_name: Name for the temporary output view that will include QALY columns.
      #   include_non_significant: Boolean, whether to include non-significant decrements.
      #
      # Returns:
      #   NULL. Modifies DuckDB state by creating output_view_name.
      calc_QALYs = function(
        duckdb_con,
        mcaggr,
        input_table_name,
        output_view_name,
        include_non_significant = FALSE
      ) {
        eq5d5l_expr <- "
          0.989
          + CASE agegrp
              WHEN '20-24' THEN -0.018 WHEN '25-29' THEN -0.018
              WHEN '30-34' THEN -0.019 WHEN '35-39' THEN -0.019
              WHEN '40-44' THEN -0.018 WHEN '45-49' THEN -0.018
              WHEN '50-54' THEN -0.028 WHEN '55-59' THEN -0.028
              WHEN '60-64' THEN -0.021 WHEN '65-69' THEN -0.021
              WHEN '70-74' THEN -0.057 WHEN '75-79' THEN -0.057
              WHEN '80-84' THEN -0.129 WHEN '85-89' THEN -0.129
              WHEN '90-94' THEN -0.129 WHEN '95-99' THEN -0.129
              ELSE 0.0
            END
          + CASE WHEN sex = 'women' THEN -0.011 ELSE 0.0 END
          + CASE WHEN chd_prvl = 0 THEN 0.0 ELSE -0.073 END
          + CASE WHEN stroke_prvl = 0 THEN 0.0 ELSE -0.265 END
          + CASE WHEN t2dm_prvl = 0 THEN 0.0 ELSE -0.046 END
        "

        hui3_expr <- "
          0.897
          + CASE agegrp
              WHEN '20-24' THEN -0.023 WHEN '25-29' THEN -0.023
              WHEN '30-34' THEN -0.018 WHEN '35-39' THEN -0.018
              WHEN '40-44' THEN -0.004 WHEN '45-49' THEN -0.004
              WHEN '50-54' THEN -0.021 WHEN '55-59' THEN -0.021
              WHEN '60-64' THEN -0.013 WHEN '65-69' THEN -0.013
              WHEN '70-74' THEN -0.042 WHEN '75-79' THEN -0.042
              WHEN '80-84' THEN -0.145 WHEN '85-89' THEN -0.145
              WHEN '90-94' THEN -0.145 WHEN '95-99' THEN -0.145
              ELSE 0.0
            END
          + CASE WHEN sex = 'women' THEN 0.011 ELSE 0.0 END
          + CASE WHEN chd_prvl = 0 THEN 0.0 ELSE -0.081 END
          + CASE WHEN stroke_prvl = 0 THEN 0.0 ELSE -0.293 END
          + CASE WHEN t2dm_prvl = 0 THEN 0.0 ELSE -0.055 END
        "

        if (!include_non_significant) {
          # Original logic: if (!include_non_significant == TRUE)
          eq5d5l_expr <- paste0(
            eq5d5l_expr,
            " + CASE WHEN htn_prvl = 0 THEN 0.0 ELSE -0.005 END",
            " + CASE WHEN obesity_prvl = 0 THEN 0.0 ELSE -0.034 END"
          )
          hui3_expr <- paste0(
            hui3_expr,
            " + CASE WHEN htn_prvl = 0 THEN 0.0 ELSE -0.006 END",
            " + CASE WHEN obesity_prvl = 0 THEN 0.0 ELSE 0.019 END"
          )
        }

        create_view_sql <- sprintf(
          "
          CREATE OR REPLACE TEMP VIEW %s AS
          SELECT
            *,
            (%s) AS EQ5D5L,
            (%s) AS HUI3
          FROM %s
          WHERE mc = %d;
        ",
          output_view_name,
          eq5d5l_expr,
          hui3_expr,
          input_table_name,
          mcaggr
        )

        dbExecute(duckdb_con, create_view_sql)

        NULL
      },

      # calc_costs ----
      # Refactored version of calc_costs to use DuckDB SQL.
      # Creates a temporary view named 'output_view_name' in DuckDB,
      # which is the 'input_table_name' (filtered by mcaggr) augmented with calculated cost columns.
      calc_costs = function(
        duckdb_con,
        mcaggr,
        input_table_name,
        output_view_name
      ) {
        # Helper function to execute SQL, with error reporting
        execute_sql <- function(sql, context = "") {
          # For debugging: message(paste("Executing SQL for", context, ":\n", sql))
          tryCatch(
            {
              dbExecute(duckdb_con, sql)
            },
            error = function(e) {
              stop(paste(
                "Error executing SQL for",
                context,
                ":",
                e$message,
                "\nSQL:\n",
                sql
              ))
            }
          )
        }

        # Helper to register a data.frame/data.table as a DuckDB temp table
        register_df_as_table <- function(df, table_name, con = duckdb_con) {
          duckdb::dbWriteTable(
            con,
            table_name,
            as.data.frame(df),
            overwrite = TRUE
          )
        }

        # --- Inflation Factors ---
        prod_informal_inflation_factor <- 1.025
        direct_costs_inflation_factor <- 99.6 / 99.7

        # --- Step 1: Initial Aggregations from lc_table ---
        # These views are filtered by year, scenario 'sc0', and mcaggr.
        base_agg_sql <- "
          CREATE OR REPLACE TEMP VIEW %s AS
          SELECT agegrp, sex, ROUND(SUM(CASE WHEN %s THEN wt ELSE 0 END)) AS V1
          FROM %s WHERE year = %d AND scenario = 'sc0' AND mc = %d GROUP BY agegrp, sex;
        "
        execute_sql(
          sprintf(
            base_agg_sql,
            "chd_prvl_2016_agg_view",
            "chd_dgns > 0",
            input_table_name,
            2016,
            mcaggr
          ),
          "chd_prvl_2016_agg_view"
        )
        execute_sql(
          sprintf(
            base_agg_sql,
            "chd_prvl_2019_agg_view",
            "chd_dgns > 0",
            input_table_name,
            2019,
            mcaggr
          ),
          "chd_prvl_2019_agg_view"
        )
        execute_sql(
          sprintf(
            base_agg_sql,
            "stroke_prvl_2016_agg_view",
            "stroke_dgns > 0",
            input_table_name,
            2016,
            mcaggr
          ),
          "stroke_prvl_2016_agg_view"
        )
        execute_sql(
          sprintf(
            base_agg_sql,
            "stroke_prvl_2019_agg_view",
            "stroke_dgns > 0",
            input_table_name,
            2019,
            mcaggr
          ),
          "stroke_prvl_2019_agg_view"
        )
        execute_sql(
          sprintf(
            base_agg_sql,
            "chd_mrtl_2016_initial_view",
            "all_cause_mrtl = 2",
            input_table_name,
            2016,
            mcaggr
          ),
          "chd_mrtl_2016_initial_view"
        )
        execute_sql(
          sprintf(
            base_agg_sql,
            "stroke_mrtl_2016_initial_view",
            "all_cause_mrtl = 3",
            input_table_name,
            2016,
            mcaggr
          ),
          "stroke_mrtl_2016_initial_view"
        )

        # --- Step 2: Load external data and update mortality views ---
        # (Handling to_agegrp by pre-aggregation in R before loading into DuckDB)
        obs_pop_df_2016 <- read_fst(
          "inputs/pop_estimates/observed_population_japan.fst",
          as.data.table = TRUE
        )[year == 2016]

        # CHD Mortality Update
        chd_ftlt_df_2016 <- read_fst(
          "inputs/disease_burden/chd_ftlt.fst",
          as.data.table = TRUE
        )[year == 2016]
        chd_ftlt_joined_2016 <- chd_ftlt_df_2016[
          obs_pop_df_2016,
          on = c("age", "year", "sex"),
          nomatch = 0L
        ][, deaths_calc := mu2 * pops]
        to_agegrp(chd_ftlt_joined_2016, 5, 99L) # Assumes to_agegrp is in scope
        chd_ftlt_agg_ext_2016 <- chd_ftlt_joined_2016[,
          .(calculated_deaths = round(sum(deaths_calc))),
          keyby = .(agegrp, sex)
        ]
        register_df_as_table(chd_ftlt_agg_ext_2016, "chd_ftlt_ext_2016_table")

        execute_sql(
          "
          CREATE OR REPLACE TEMP VIEW chd_mrtl_2016_agg_view AS
          SELECT i.agegrp, i.sex, CASE WHEN i.V1 = 0 THEN COALESCE(f.calculated_deaths, i.V1) ELSE i.V1 END AS V1
          FROM chd_mrtl_2016_initial_view i LEFT JOIN chd_ftlt_ext_2016_table f ON i.agegrp = f.agegrp AND i.sex = f.sex;
        ",
          "chd_mrtl_2016_agg_view (updated)"
        )

        # Stroke Mortality Update
        stroke_ftlt_df_2016 <- read_fst(
          "inputs/disease_burden/stroke_ftlt.fst",
          as.data.table = TRUE
        )[year == 2016]
        stroke_ftlt_joined_2016 <- stroke_ftlt_df_2016[
          obs_pop_df_2016,
          on = c("age", "year", "sex"),
          nomatch = 0L
        ][, deaths_calc := mu2 * pops]
        to_agegrp(stroke_ftlt_joined_2016, 5, 99L)
        stroke_ftlt_agg_ext_2016 <- stroke_ftlt_joined_2016[,
          .(calculated_deaths = round(sum(deaths_calc))),
          keyby = .(agegrp, sex)
        ]
        register_df_as_table(
          stroke_ftlt_agg_ext_2016,
          "stroke_ftlt_ext_2016_table"
        )

        execute_sql(
          "
          CREATE OR REPLACE TEMP VIEW stroke_mrtl_2016_agg_view AS
          SELECT i.agegrp, i.sex, CASE WHEN i.V1 = 0 THEN COALESCE(f.calculated_deaths, i.V1) ELSE i.V1 END AS V1
          FROM stroke_mrtl_2016_initial_view i LEFT JOIN stroke_ftlt_ext_2016_table f ON i.agegrp = f.agegrp AND i.sex = f.sex;
        ",
          "stroke_mrtl_2016_agg_view (updated)"
        )

        # --- Step 3: Define Cost Parameter Tables & Views ---
        # Helper for productivity/informal cost parameter calculation
        calc_cost_param_sql <- "
          CREATE OR REPLACE TEMP VIEW %s AS
          WITH joined_data AS (
            SELECT p.agegrp, p.sex, p.%s AS factor_col, agg.V1 FROM %s p JOIN %s agg ON p.agegrp = agg.agegrp AND p.sex = agg.sex
          ), weighted_data AS (
            SELECT *, (factor_col * V1) AS weighted_factor FROM joined_data
          ), total_weighted_sum AS (
            SELECT SUM(weighted_factor) AS total_wt_sum FROM weighted_data
          )
          SELECT wd.agegrp, wd.sex, (%f * wd.weighted_factor / tws.total_wt_sum) * %f / NULLIF(wd.V1, 0) AS cost_param
          FROM weighted_data wd, total_weighted_sum tws;
        "
        # Productivity Prevalence Costs
        employee_params_df <- data.table(
          agegrp = rep(
            c(
              "30-34",
              "35-39",
              "40-44",
              "45-49",
              "50-54",
              "55-59",
              "60-64",
              "65-69",
              "70-74",
              "75-79",
              "80-84",
              "85-89",
              "90-94",
              "95-99"
            ),
            2
          ),
          sex = rep(c("men", "women"), each = 14),
          employees = c(
            1683780,
            1829610,
            2174550,
            2057710,
            1702470,
            1425510,
            963430,
            369640,
            106850,
            0,
            0,
            0,
            0,
            0,
            919700,
            894770,
            1049490,
            1037140,
            854970,
            685040,
            376370,
            132470,
            44050,
            0,
            0,
            0,
            0,
            0
          )
        )
        register_df_as_table(employee_params_df, "employee_params_table")
        execute_sql(
          sprintf(
            calc_cost_param_sql,
            "chd_prvl_prdv_cost_param_view",
            "employees",
            "employee_params_table",
            "chd_prvl_2016_agg_view",
            141000000000.00,
            prod_informal_inflation_factor
          ),
          "chd_prvl_prdv_cost_param_view"
        )
        execute_sql(
          sprintf(
            calc_cost_param_sql,
            "stroke_prvl_prdv_cost_param_view",
            "employees",
            "employee_params_table",
            "stroke_prvl_2016_agg_view",
            322000000000.00,
            prod_informal_inflation_factor
          ),
          "stroke_prvl_prdv_cost_param_view"
        )

        # Productivity Mortality Costs
        execute_sql(
          sprintf(
            calc_cost_param_sql,
            "chd_mrtl_prdv_cost_param_view",
            "employees",
            "employee_params_table",
            "chd_mrtl_2016_agg_view",
            2257000000000.00,
            prod_informal_inflation_factor
          ),
          "chd_mrtl_prdv_cost_param_view"
        )
        execute_sql(
          sprintf(
            calc_cost_param_sql,
            "stroke_mrtl_prdv_cost_param_view",
            "employees",
            "employee_params_table",
            "stroke_mrtl_2016_agg_view",
            1352000000000.00,
            prod_informal_inflation_factor
          ),
          "stroke_mrtl_prdv_cost_param_view"
        )

        # Informal Costs
        chd_infm_care_df <- data.table(
          agegrp = rep(
            c(
              "30-34",
              "35-39",
              "40-44",
              "45-49",
              "50-54",
              "55-59",
              "60-64",
              "65-69",
              "70-74",
              "75-79",
              "80-84",
              "85-89",
              "90-94",
              "95-99"
            ),
            2
          ),
          sex = rep(c("men", "women"), each = 14),
          infm_care_hrs = c(
            0.030,
            0.030,
            0.030,
            0.030,
            0.030,
            0.030,
            0.030,
            0.200,
            0.200,
            0.200,
            0,
            0,
            0,
            0,
            0.030,
            0.030,
            0.030,
            0.030,
            0.030,
            0.030,
            0.030,
            0.200,
            0.200,
            0.200,
            0,
            0,
            0,
            0
          )
        )
        register_df_as_table(chd_infm_care_df, "chd_infm_care_table")
        execute_sql(
          sprintf(
            calc_cost_param_sql,
            "chd_informal_cost_param_view",
            "infm_care_hrs",
            "chd_infm_care_table",
            "chd_prvl_2016_agg_view",
            291000000000.00,
            prod_informal_inflation_factor
          ),
          "chd_informal_cost_param_view"
        )

        stroke_infm_care_df <- data.table(
          agegrp = rep(
            c(
              "30-34",
              "35-39",
              "40-44",
              "45-49",
              "50-54",
              "55-59",
              "60-64",
              "65-69",
              "70-74",
              "75-79",
              "80-84",
              "85-89",
              "90-94",
              "95-99"
            ),
            2
          ),
          sex = rep(c("men", "women"), each = 14),
          infm_care_hrs = c(
            5.20,
            5.20,
            5.20,
            5.20,
            5.20,
            5.20,
            5.20,
            5.03,
            5.03,
            5.03,
            9.23,
            9.23,
            9.23,
            9.23,
            5.20,
            5.20,
            5.20,
            5.20,
            5.20,
            5.20,
            5.20,
            5.03,
            5.03,
            5.03,
            9.23,
            9.23,
            9.23,
            9.23
          )
        )
        register_df_as_table(stroke_infm_care_df, "stroke_infm_care_table")
        execute_sql(
          sprintf(
            calc_cost_param_sql,
            "stroke_informal_cost_param_view",
            "infm_care_hrs",
            "stroke_infm_care_table",
            "stroke_prvl_2016_agg_view",
            1651000000000.00,
            prod_informal_inflation_factor
          ),
          "stroke_informal_cost_param_view"
        )

        # Direct Costs
        direct_cost_param_sql <- "
          CREATE OR REPLACE TEMP VIEW %s AS
          WITH lc_with_agegrp2 AS (
            SELECT *, CASE agegrp
              WHEN '30-34' THEN '30-44' WHEN '35-39' THEN '30-44' WHEN '40-44' THEN '30-44'
              WHEN '45-49' THEN '45-64' WHEN '50-54' THEN '45-64' WHEN '55-59' THEN '45-64' WHEN '60-64' THEN '45-64'
              WHEN '65-69' THEN '65-69' WHEN '70-74' THEN '70-74' ELSE '75-99' END AS agegrp2
            FROM %s
          ), agg_by_agegrp2 AS (
            SELECT agegrp2, sex, SUM(V1) AS V1_sum FROM lc_with_agegrp2 GROUP BY agegrp2, sex
          ), joined_tcost AS (
            SELECT agg.agegrp2, agg.sex, agg.V1_sum, tc.tcost_val FROM agg_by_agegrp2 agg JOIN %s tc ON agg.agegrp2 = tc.agegrp2 AND agg.sex = tc.sex
          )
          SELECT orig.agegrp, orig.sex, (jt.tcost_val * %f / NULLIF(jt.V1_sum, 0)) AS cost_param
          FROM %s orig
          JOIN lc_with_agegrp2 lwa ON orig.agegrp = lwa.agegrp AND orig.sex = lwa.sex
          JOIN joined_tcost jt ON lwa.agegrp2 = jt.agegrp2 AND lwa.sex = jt.sex
          GROUP BY orig.agegrp, orig.sex, jt.tcost_val, jt.V1_sum;
        "
        chd_direct_tcost_df <- data.table(
          agegrp2 = rep(c("30-44", "45-64", "65-69", "70-74", "75-99"), 2),
          sex = rep(c("men", "women"), each = 5),
          tcost_val = c(
            10500 - 200,
            121000,
            72300,
            90100,
            197000,
            2600 - 100,
            22500,
            18900,
            30300,
            132800
          ) *
            1e6
        )
        register_df_as_table(chd_direct_tcost_df, "chd_direct_tcost_table")
        execute_sql(
          sprintf(
            direct_cost_param_sql,
            "chd_direct_cost_param_view",
            "chd_prvl_2019_agg_view",
            "chd_direct_tcost_table",
            direct_costs_inflation_factor,
            "chd_prvl_2019_agg_view"
          ),
          "chd_direct_cost_param_view"
        )

        stroke_direct_tcost_df <- data.table(
          agegrp2 = rep(c("30-44", "45-64", "65-69", "70-74", "75-99"), 2),
          sex = rep(c("men", "women"), each = 5),
          tcost_val = c(
            26600 - 1700,
            186400,
            109000,
            144100,
            465600,
            19700 - 1700,
            106800,
            60900,
            95700,
            606800
          ) *
            1e6
        )
        register_df_as_table(
          stroke_direct_tcost_df,
          "stroke_direct_tcost_table"
        )
        execute_sql(
          sprintf(
            direct_cost_param_sql,
            "stroke_direct_cost_param_view",
            "stroke_prvl_2019_agg_view",
            "stroke_direct_tcost_table",
            direct_costs_inflation_factor,
            "stroke_prvl_2019_agg_view"
          ),
          "stroke_direct_cost_param_view"
        )

        # --- Step 4: Create Final Output View with All Cost Columns ---
        final_view_creation_sql <- sprintf(
          "
          CREATE OR REPLACE TEMP VIEW %s AS
          WITH base_filtered AS (SELECT * FROM %s WHERE mc = %d),
          chd_prod_prvl_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM chd_prvl_prdv_cost_param_view),
          chd_prod_mrtl_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM chd_mrtl_prdv_cost_param_view),
          stroke_prod_prvl_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM stroke_prvl_prdv_cost_param_view),
          stroke_prod_mrtl_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM stroke_mrtl_prdv_cost_param_view),
          chd_inf_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM chd_informal_cost_param_view),
          stroke_inf_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM stroke_informal_cost_param_view),
          chd_dir_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM chd_direct_cost_param_view),
          stroke_dir_cost AS (SELECT agegrp, sex, COALESCE(cost_param, 0) AS val FROM stroke_direct_cost_param_view)
          SELECT
            m.*,
            CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END AS chd_prvl_prdv_costs,
            CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END AS chd_mrtl_prdv_costs,
            (CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) AS chd_productivity_costs,

            CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END AS stroke_prvl_prdv_costs,
            CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END AS stroke_mrtl_prdv_costs,
            (CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) AS stroke_productivity_costs,

            CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END AS chd_informal_costs,
            CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END AS stroke_informal_costs,

            CASE WHEN m.chd_dgns > 0 THEN cdc.val ELSE 0 END AS chd_direct_costs,
            CASE WHEN m.stroke_dgns > 0 THEN sdc.val ELSE 0 END AS stroke_direct_costs,

            -- Totals and Indirect
            ((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) + CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END + CASE WHEN m.chd_dgns > 0 THEN cdc.val ELSE 0 END) AS chd_total_costs,
            ((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) + CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END) AS chd_indirect_costs,

            ((CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END + CASE WHEN m.stroke_dgns > 0 THEN sdc.val ELSE 0 END) AS stroke_total_costs,
            ((CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END) AS stroke_indirect_costs,

            -- CVD Costs (Sum of CHD and Stroke costs)
            (((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) + CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END + CASE WHEN m.chd_dgns > 0 THEN cdc.val ELSE 0 END) +
             ((CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END + CASE WHEN m.stroke_dgns > 0 THEN sdc.val ELSE 0 END)
            ) AS cvd_total_costs,
            (((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) + CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END) +
             ((CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END) + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END)
            ) AS cvd_indirect_costs,
            (CASE WHEN m.chd_dgns > 0 THEN cdc.val ELSE 0 END + CASE WHEN m.stroke_dgns > 0 THEN sdc.val ELSE 0 END) AS cvd_direct_costs,
            ((CASE WHEN m.chd_dgns > 0 THEN cppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 2 THEN cpmc.val ELSE 0 END) +
             (CASE WHEN m.stroke_dgns > 0 THEN sppc.val ELSE 0 END + CASE WHEN m.all_cause_mrtl = 3 THEN spmc.val ELSE 0 END)
            ) AS cvd_productivity_costs,
            (CASE WHEN m.chd_dgns > 0 THEN cic.val ELSE 0 END + CASE WHEN m.stroke_dgns > 0 THEN sic.val ELSE 0 END) AS cvd_informal_costs
          FROM base_filtered m
          LEFT JOIN chd_prod_prvl_cost cppc ON m.agegrp = cppc.agegrp AND m.sex = cppc.sex
          LEFT JOIN chd_prod_mrtl_cost cpmc ON m.agegrp = cpmc.agegrp AND m.sex = cpmc.sex
          LEFT JOIN stroke_prod_prvl_cost sppc ON m.agegrp = sppc.agegrp AND m.sex = sppc.sex
          LEFT JOIN stroke_prod_mrtl_cost spmc ON m.agegrp = spmc.agegrp AND m.sex = spmc.sex
          LEFT JOIN chd_inf_cost cic ON m.agegrp = cic.agegrp AND m.sex = cic.sex
          LEFT JOIN stroke_inf_cost sic ON m.agegrp = sic.agegrp AND m.sex = sic.sex
          LEFT JOIN chd_dir_cost cdc ON m.agegrp = cdc.agegrp AND m.sex = cdc.sex
          LEFT JOIN stroke_dir_cost sdc ON m.agegrp = sdc.agegrp AND m.sex = sdc.sex;
        ",
          output_view_name,
          input_table_name,
          mcaggr
        )
        execute_sql(
          final_view_creation_sql,
          paste("Final cost view:", output_view_name)
        )

        # Clean up intermediate tables/views (optional, as they are TEMP)
        # Example: dbExecute(duckdb_con, "DROP VIEW IF EXISTS chd_prvl_2016_agg_view;")

        return(invisible(NULL))
      }, # end calc_costs

      # create_empty_calibration_prms_file ----
      # if replace is FALSE then it creates a calibration parameters when it is
      # missing, file filed with 1. If replace = TRUE it overwrites the existin
      # file
      # returns invisible(self)
      # TODO Automate based on diseases in design.yaml
      create_empty_calibration_prms_file = function(replace = FALSE) {
        if (replace || !file.exists("./simulation/calibration_prms.csv")) {
          clbr <- CJ(
            year = self$design$sim_prm$init_year_long:(self$design$sim_prm$init_year_long +
              self$design$sim_prm$sim_horizon_max),
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
      # @param bReport Bool report folder creation. Default is design$sim_prm$logs.
      create_new_folder = function(
        sDirPathName,
        bReport = self$design$sim_prm$logs
      ) {
        if (!dir.exists(sDirPathName)) {
          bSuccess <- dir.create(sDirPathName, recursive = TRUE)
          if (!bSuccess) stop(paste("Failed creating directory", sDirPathName))
          if (bReport) message(paste0("Folder ", sDirPathName, " was created"))
        }
      },

      # Life Expectancy (LE) Export Section
      #
      # This block exports life expectancy (LE) at minAge and life expectancy at age 60 (LE60)
      # summaries for each Monte Carlo (mc) iteration. It uses a configuration list
      # and a loop to avoid code repetition. Each configuration specifies:
      #   - prefix: output file prefix ("le" or "le60")
      #   - age_filter: SQL WHERE clause for age (e.g., only ages > 60 for LE60)
      #   - weight_col: which weight column to use ("wt" or "wt_esp")
      #   - suffix: output file suffix (e.g., "_scaled_up" or "_esp")
      #
      # The code dynamically constructs the SQL query and output path for each
      # configuration, then executes the query and writes the result to a Parquet file.
      #
      # Directories for all output types are created if they do not exist.
      # The LE60 calculation is skipped if the simulation age range does not cover age 60.
      #
      # Args:
      #   type: character vector, must include "le" to trigger this block
      #   duckdb_con: DuckDB connection to the lifecourse data
      #   mc: Monte Carlo iteration number
      #   strata_noagegrp: grouping columns for summary
      #   ext: file extension (usually "parquet")
      #
      # Output:
      #   Writes LE and LE60 summary files to the appropriate summaries/ subfolders.
      #
      # Example output files:
      #   summaries/le_scaled_up/1_le_scaled_up.parquet
      #   summaries/le_esp/1_le_esp.parquet
      #   summaries/le60_scaled_up/1_le60_scaled_up.parquet
      #   summaries/le60_esp/1_le60_esp.parquet
      #
      # @description Export Life Expectancy (LE) and LE60 summaries for a given Monte Carlo iteration.
      # @param duckdb_con DuckDB connection to the lifecourse data.
      # @param mcaggr Monte Carlo iteration number.
      # @param strata_noagegrp Grouping columns as a character vector (for output path construction).
      # @param ext File extension (usually "parquet").
      # @return None. Writes LE and LE60 summary files to the appropriate summaries/ subfolders.
      # --- End documentation ---
      # export_le_summaries ----
      export_le_summaries = function(duckdb_con, mcaggr, strata_noagegrp, ext) {
        lapply(
          paste0(rep(c("le", "le60"), each = 2), "_", c("scaled_up", "esp")),
          function(subdir) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir
            )))
          }
        )

        group_by_cols <- paste(strata_noagegrp, collapse = ", ")

        # Define configurations for LE calculations
        le_configs <- list(
          list(
            prefix = "le",
            age_filter = "",
            weight_col = "wt",
            suffix = "_scaled_up"
          ),
          list(
            prefix = "le",
            age_filter = "",
            weight_col = "wt_esp",
            suffix = "_esp"
          ),
          list(
            prefix = "le60",
            age_filter = "AND age > 60",
            weight_col = "wt",
            suffix = "_scaled_up"
          ),
          list(
            prefix = "le60",
            age_filter = "AND age > 60",
            weight_col = "wt_esp",
            suffix = "_esp"
          )
        )

        for (config in le_configs) {
          # Skip LE60 calculation if age range doesn't cover 60
          if (
            startsWith(config$prefix, "le60") &&
              !(self$design$sim_prm$ageL < 60L &&
                self$design$sim_prm$ageH > 60L)
          ) {
            next
          }

          query <- sprintf(
            "SELECT %s, SUM(%s) AS popsize, SUM(age * %s) / SUM(%s) AS LE
            FROM lc_table
            WHERE mc == %d AND all_cause_mrtl > 0 %s
            GROUP BY %s
            ORDER BY %s",
            group_by_cols,
            config$weight_col,
            config$weight_col,
            config$weight_col, # Use weight_col 3 times
            mcaggr,
            config$age_filter,
            group_by_cols,
            group_by_cols
          )

          output_path <- private$output_dir(
            paste0(
              "summaries/",
              config$prefix,
              config$suffix,
              "/",
              mcaggr,
              "_",
              config$prefix,
              config$suffix,
              ".",
              ext
            )
          )

          # Execute query and write result
          dbExecute(
            duckdb_con,
            sprintf(
              "COPY (%s) TO '%s' (FORMAT PARQUET);",
              query,
              output_path
            )
          )
          NULL
        }
      }, # end export_le_summaries

      # Healthy Life Expectancy (HLE) Export Section
      #
      # This section exports healthy life expectancy (HLE) summaries for each Monte Carlo (mc)
      # iteration, using different health state definitions and weighting schemes.
      # It uses a configuration list and a loop to avoid code repetition.
      #
      # For each configuration, the following parameters are specified:
      #   - prefix: output file prefix (e.g., "hle_1st_cond", "hle_cmsmm1.5")
      #   - condition: SQL WHERE clause defining the 'healthy' state (e.g., "cms_count = 1")
      #   - weight_col: which weight column to use ("wt" or "wt_esp")
      #   - suffix: output file suffix (e.g., "_scaled_up" or "_esp")
      #
      # The code dynamically constructs the SQL query and output path for each configuration,
      # executes the query using DuckDB, and writes the result to a Parquet file.
      # Output directories are created if they do not exist.
      #
      # Args:
      #   type: character vector, must include "hle" to trigger this block
      #   duckdb_con: DuckDB connection to the lifecourse data
      #   mc: Monte Carlo iteration number
      #   strata_noagegrp: grouping columns for summary
      #   ext: file extension (usually "parquet")
      #
      # Output:
      #   Writes HLE summary files to the appropriate summaries/ subfolders.
      #
      # Example output files:
      #   summaries/hle_1st_cond_scaled_up/1_hle_1st_cond_scaled_up.parquet
      #   summaries/hle_1st_cond_esp/1_hle_1st_cond_esp.parquet
      #   summaries/hle_cmsmm1.5_scaled_up/1_hle_cmsmm1.5_scaled_up.parquet
      #   summaries/hle_cmsmm1.5_esp/1_hle_cmsmm1.5_esp.parquet
      #
      # --- End documentation ---
      # export_hle_summaries ----
      export_hle_summaries = function(
        duckdb_con,
        mcaggr,
        strata_noagegrp,
        ext
      ) {
        # TODO currently some individuals are counted more than once because
        # disease counter and score can be reduced.
        # Ideally only the first reach to the threshold should be counted
        lapply(
          paste0(
            rep(c("hle_1st_cond", "hle_cmsmm1.5"), each = 2),
            "_",
            c("scaled_up", "esp")
          ),
          function(subdir) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir
            )))
          }
        )

        group_by_cols <- paste(strata_noagegrp, collapse = ", ")

        # Define configurations for HLE calculations
        hle_configs <- list(
          list(
            prefix = "hle_1st_cond",
            condition = "cms_count = 1",
            weight_col = "wt",
            suffix = "_scaled_up"
          ),
          list(
            prefix = "hle_1st_cond",
            condition = "cms_count = 1",
            weight_col = "wt_esp",
            suffix = "_esp"
          ),
          list(
            prefix = "hle_cmsmm1.5",
            condition = '"cmsmm1.5_prvl" = 1',
            weight_col = "wt",
            suffix = "_scaled_up"
          ),
          list(
            prefix = "hle_cmsmm1.5",
            condition = '"cmsmm1.5_prvl" = 1',
            weight_col = "wt_esp",
            suffix = "_esp"
          )
        )

        for (config in hle_configs) {
          query <- sprintf(
            "SELECT %s, SUM(%s) AS popsize, SUM(age * %s) / SUM(%s) AS HLE
               FROM lc_table
               WHERE mc = %d AND %s
               GROUP BY %s
               ORDER BY %s",
            group_by_cols,
            config$weight_col,
            config$weight_col,
            config$weight_col,
            mcaggr,
            config$condition,
            group_by_cols,
            group_by_cols
          )

          output_path <- private$output_dir(
            paste0(
              "summaries/",
              config$prefix,
              config$suffix,
              "/",
              mcaggr,
              "_",
              config$prefix,
              config$suffix,
              ".",
              ext
            )
          )

          dbExecute(
            duckdb_con,
            sprintf(
              "COPY (%s) TO '%s' (FORMAT PARQUET);",
              query,
              output_path
            )
          )
          NULL
        }
      }, # end export_hle_summaries

      # The code block is responsible for exporting disease
      # characteristics summaries for each Monte Carlo (mc) iteration. It
      # dynamically queries the DuckDB lifecourse table to calculate, for each
      # disease (identified by columns ending in _prvl), the number of cases,
      # mean age at incidence, mean age at first onset, mean age at
      # prevalence, mean duration, mean CMS score, and mean CMS count, grouped
      # by the specified strata. The results are written to Parquet files in
      # the summaries/dis_characteristics_scaled_up and
      # summaries/dis_characteristics_esp directories, using both standard and
      # ESP weights. The code uses SQL PIVOT to reshape the output so that
      # each disease's metrics become columns, and the output is suitable for
      # further analysis or reporting. This process is repeated for both
      # standard and ESP-weighted results.
      # export_dis_char_summaries ----
      export_dis_char_summaries = function(
        duckdb_con,
        mcaggr,
        strata_noagegrp,
        ext
      ) {
        lapply(
          paste0(
            rep(c("dis_characteristics"), each = 2),
            "_",
            c("scaled_up", "esp")
          ),
          function(subdir) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir
            )))
          }
        )

        # Get disease prevalence columns from DuckDB schema
        lc_table_name <- "lc_table" # Assuming the view/table name in DuckDB is lc_table

        # Use dbListFields for a more direct way to get column names
        all_cols <- dbListFields(duckdb_con, lc_table_name)
        nm <- grep("_prvl$", all_cols, value = TRUE)

        # Both SELECT and GROUP BY need the 't.' prefix due to the join ambiguity
        select_and_group_by_cols_sql <- paste0(
          "t.",
          strata_noagegrp,
          collapse = ", "
        )
        # Columns needed for the final dcast formula in R (includes mcaggr, no prefix needed here)
        cols <- paste0(strata_noagegrp, collapse = ", ")

        # --- Calculate disease characteristics using DuckDB (ALL DISEASES, UNION ALL) ---
        if (length(nm) > 0) {
          # Build all queries as subqueries and combine with UNION ALL
          union_queries <- lapply(nm, function(disease_col) {
            disease_name <- gsub("_prvl$", "", disease_col)
            quoted_disease_col <- paste0('"', disease_col, '"')
            sprintf(
              "SELECT * FROM (
                       WITH FirstOnset AS (
                       SELECT pid, scenario, MIN(year) AS first_onset_year
                       FROM %s
                       WHERE mc = %d AND %s = 1
                       GROUP BY pid, scenario
                       ),
                       FirstOnsetDetails AS (
                       SELECT t_fod.pid, t_fod.scenario, t_fod.year, t_fod.age AS age_onset, t_fod.wt AS wt1st
                       FROM %s t_fod
                       JOIN FirstOnset fo ON t_fod.pid = fo.pid AND t_fod.scenario = fo.scenario AND t_fod.year = fo.first_onset_year
                       WHERE t_fod.mc = %d
                       )
                       SELECT %s, '%s' AS disease,
                       SUM(t.wt) AS cases,
                       SUM(CASE WHEN t.%s = 1 THEN t.age * t.wt ELSE 0 END) / NULLIF(SUM(CASE WHEN t.%s = 1 THEN t.wt ELSE 0 END), 0) AS mean_age_incd,
                       SUM(CASE WHEN fod.pid IS NOT NULL THEN fod.age_onset * fod.wt1st ELSE 0 END) / NULLIF(SUM(CASE WHEN fod.pid IS NOT NULL THEN fod.wt1st ELSE 0 END), 0) AS mean_age_1st_onset,
                       SUM(t.age * t.wt) / NULLIF(SUM(t.wt), 0) AS mean_age_prvl,
                       SUM(t.%s * t.wt) / NULLIF(SUM(t.wt), 0) AS mean_duration,
                       SUM(t.cms_score * t.wt) / NULLIF(SUM(t.wt), 0) AS mean_cms_score,
                       SUM(t.cms_count * t.wt) / NULLIF(SUM(t.wt), 0) AS mean_cms_count
                       FROM %s t
                       LEFT JOIN FirstOnsetDetails fod ON t.pid = fod.pid AND t.scenario = fod.scenario AND t.year = fod.year
                       WHERE t.mc = %d AND t.%s > 0
                       GROUP BY %s
                       ) AS disease_part_%s",
              lc_table_name,
              mcaggr,
              quoted_disease_col,
              lc_table_name,
              mcaggr,
              select_and_group_by_cols_sql,
              disease_name,
              quoted_disease_col,
              quoted_disease_col,
              quoted_disease_col,
              lc_table_name,
              mcaggr,
              quoted_disease_col,
              select_and_group_by_cols_sql,
              gsub("[^A-Za-z0-9_]", "_", disease_name)
            )
          })
          full_union_query <- paste(union_queries, collapse = " UNION ALL ")

          pivot_query <- sprintf(
            "
           SELECT *
           FROM (
             %s
           )
           PIVOT (
             COALESCE(SUM(cases), 0) AS ___cases,
             COALESCE(AVG(mean_duration), 0) AS ___mean_duration,
             COALESCE(AVG(mean_age_incd), 0) AS ___mean_age_incd,
             COALESCE(AVG(mean_age_1st_onset), 0) AS ___mean_age_1st_onset,
             COALESCE(AVG(mean_age_prvl), 0) AS ___mean_age_prvl,
             COALESCE(AVG(mean_cms_score), 0) AS ___mean_cms_score,
             COALESCE(AVG(mean_cms_count), 0) AS ___mean_cms_count
             FOR disease IN (%s)
           )
           ",
            full_union_query,
            paste0("'", gsub("_prvl$", "", nm), "'", collapse = ", ")
          )

          wrapped_sql <- sprintf(
            "
           SELECT
             %s,
             COLUMNS('(.*)____(.*)') AS '\\2_\\1'
           FROM (
             %s
           )
           ORDER BY %s
           ",
            cols,
            pivot_query,
            cols
          )

          output_path <- private$output_dir(
            paste0(
              "summaries/dis_characteristics_scaled_up/",
              mcaggr,
              "_dis_characteristics_scaled_up.",
              ext
            )
          )

          # Execute query and write result
          dbExecute(
            duckdb_con,
            sprintf(
              "COPY (%s) TO '%s' (FORMAT PARQUET);",
              wrapped_sql,
              output_path
            )
          )

          wrapped_sql_esp <- gsub("wt", "wt_esp", wrapped_sql)
          output_path_esp <- private$output_dir(
            paste0(
              "summaries/dis_characteristics_esp/",
              mcaggr,
              "_dis_characteristics_esp.",
              ext
            )
          )

          # Execute query and write result
          dbExecute(
            duckdb_con,
            sprintf(
              "COPY (%s) TO '%s' (FORMAT PARQUET);",
              wrapped_sql_esp,
              output_path_esp
            )
          )
          NULL
        } # length(nm) > 0
      }, # end export_dis_char_summaries

      # export_prvl_summaries ----
      export_prvl_summaries = function(duckdb_con, mcaggr, strata, ext) {
        # TODO currently some individuals are counted more than once because
        # disease counter and score can be reduced.
        # Ideally only the first reach to the threshold should be counted
        lapply(
          paste0(rep(c("prvl"), each = 2), "_", c("scaled_up", "esp")),
          function(subdir) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir
            )))
          }
        )

        # Get disease prevalence columns from DuckDB schema
        lc_table_name <- "lc_table" # Assuming the view/table name in DuckDB is lc_table
        all_cols <- dbListFields(duckdb_con, lc_table_name)
        nm_prvl <- grep("_prvl$", all_cols, value = TRUE)

        # Construct the SQL query dynamically
        select_cols <- paste(strata, collapse = ", ")
        sum_cases_cols <- paste(
          sprintf(
            'SUM(CASE WHEN "%s" > 0 THEN wt ELSE 0 END) AS "%s"',
            nm_prvl,
            nm_prvl
          ),
          collapse = ", "
        )

        sql_query <- sprintf(
          "SELECT %s, SUM(wt) AS popsize, %s
                  FROM %s
                  WHERE mc = %d
                  GROUP BY %s
                  ORDER BY %s",
          select_cols,
          sum_cases_cols,
          lc_table_name,
          mcaggr,
          select_cols,
          select_cols
        )

        output_path <- private$output_dir(
          paste0("summaries/prvl_scaled_up/", mcaggr, "_prvl_scale_up.", ext)
        )

        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query,
            output_path
          )
        )

        # esp
        sql_query_esp <- gsub("wt", "wt_esp", sql_query)
        output_path_esp <- private$output_dir(
          paste0("summaries/prvl_esp/", mcaggr, "_prvl_esp.", ext)
        )
        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query_esp,
            output_path_esp
          )
        )

        NULL
      }, # end of export_prvl_summaries

      # export_incd_summaries ----
      export_incd_summaries = function(duckdb_con, mcaggr, strata, ext) {
        # TODO currently some individuals are counted more than once because
        # disease counter and score can be reduced.
        # Ideally only the first reach to the threshold should be counted
        lapply(
          paste0(rep(c("incd"), each = 2), "_", c("scaled_up", "esp")),
          function(subdir) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir
            )))
          }
        )

        # Get disease prevalence columns from DuckDB schema
        lc_table_name <- "lc_table" # Assuming the view/table name in DuckDB is lc_table
        all_cols <- dbListFields(duckdb_con, lc_table_name)
        nm_prvl <- grep("_prvl$", all_cols, value = TRUE)

        # Construct the SQL query dynamically
        select_cols <- paste(strata, collapse = ", ")
        sum_cases_cols <- paste(
          sprintf(
            'SUM(CASE WHEN "%s" = 1 THEN wt ELSE 0 END) AS "%s"',
            nm_prvl,
            gsub("_prvl$", "_incd", nm_prvl)
          ),
          collapse = ", "
        )

        sql_query <- sprintf(
          "SELECT %s, SUM(wt) AS popsize, %s
                  FROM %s
                  WHERE mc = %d
                  GROUP BY %s
                  ORDER BY %s",
          select_cols,
          sum_cases_cols,
          lc_table_name,
          mcaggr,
          select_cols,
          select_cols
        )

        output_path <- private$output_dir(
          paste0("summaries/incd_scaled_up/", mcaggr, "_incd_scale_up.", ext)
        )

        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query,
            output_path
          )
        )

        # esp
        sql_query_esp <- gsub("wt", "wt_esp", sql_query)
        output_path_esp <- private$output_dir(
          paste0("summaries/incd_esp/", mcaggr, "_incd_esp.", ext)
        )
        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query_esp,
            output_path_esp
          )
        )

        NULL
      }, # end of export_incd_summaries

      # export_mrtl_summaries ----
      export_mrtl_summaries = function(duckdb_con, mcaggr, strata, ext) {
        lapply(
          paste0(rep(c("mrtl"), each = 2), "_", c("scaled_up", "esp")),
          function(subdir) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir
            )))
          }
        )

        # Get disease prevalence columns from DuckDB schema
        lc_table_name <- "lc_table" # Assuming the view/table name in DuckDB is lc_table
        # Construct the SQL query dynamically for mortality summary
        select_cols_mrtl <- paste(strata, collapse = ", ")
        sql_query <- sprintf(
          "SELECT %s,
                    SUM(wt) AS popsize,
                    SUM(CASE WHEN all_cause_mrtl > 0 THEN wt ELSE 0 END) AS all_cause_mrtl
             FROM %s
             WHERE mc = %d
             GROUP BY %s
             ORDER BY %s", # Add ORDER BY to match data.table's keyby behavior
          select_cols_mrtl,
          lc_table_name,
          mcaggr,
          select_cols_mrtl,
          select_cols_mrtl # Order by the grouping columns
        )
        output_path <- private$output_dir(
          paste0("summaries/mrtl_scaled_up/", mcaggr, "_mrtl_scale_up.", ext)
        )

        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query,
            output_path
          )
        )

        # esp
        sql_query_esp <- gsub("wt", "wt_esp", sql_query)
        output_path_esp <- private$output_dir(
          paste0("summaries/mrtl_esp/", mcaggr, "_mrtl_esp.", ext)
        )
        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query_esp,
            output_path_esp
          )
        )

        NULL
      }, # end of export_mrtl_summaries

      # export_dis_mrtl_summaries ----
      # this exports cause specific mortality summaries for each Monte Carlo (mc)
      # iteration. It dynamically queries the DuckDB lifecourse table.
      export_dis_mrtl_summaries = function(duckdb_con, mcaggr, strata, ext) {
        lapply(
          paste0(rep(c("dis_mrtl"), each = 2), "_", c("scaled_up", "esp")),
          function(subdir) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir
            )))
          }
        )

        # Get disease prevalence columns from DuckDB schema
        lc_table_name <- "lc_table" # Assuming the view/table name in DuckDB is lc_table
        # Construct the SQL query dynamically for mortality summary
        # Define strata and death codes for the query
        # Quote strata columns for safety
        quoted_strata <- paste0('"', strata, '"')
        strata_cols_sql <- paste(quoted_strata, collapse = ", ")

        # All pivoted column names, including potentially 'alive_deaths'
        pivoted_death_col_names_all <- paste0(
          names(private$death_codes),
          "_deaths"
        )
        quoted_pivoted_death_col_names_all <- paste0(
          '"',
          pivoted_death_col_names_all,
          '"'
        )

        # Correctly format the IN clause with quoted aliases for PIVOT (includes 'alive_deaths')
        death_codes_pivot_sql <- paste0(
          "'",
          private$death_codes,
          "' AS ", # Use single quotes for values, double for aliases
          quoted_pivoted_death_col_names_all, # Use quoted aliases
          collapse = ", "
        )

        # Pivoted column names EXCLUDING 'alive_deaths' for the final SELECT list
        pivoted_death_col_names_final <- setdiff(
          pivoted_death_col_names_all,
          "alive_deaths"
        )
        quoted_pivoted_death_col_names_final <- paste0(
          '"',
          pivoted_death_col_names_final,
          '"'
        )

        # Create COALESCE expressions for the final SELECT list (excludes 'alive_deaths')
        # Use quoted column names and aliases
        coalesce_select_sql_final <- paste0(
          "COALESCE(t.",
          quoted_pivoted_death_col_names_final,
          ", 0) AS ",
          quoted_pivoted_death_col_names_final, # Use quoted aliases
          collapse = ", "
        )

        # Sum of ALL coalesced columns for popsize (includes 'alive_deaths')
        # Note: This uses quoted_pivoted_death_col_names_all
        death_cols_sum_sql <- paste0(
          "COALESCE(t.",
          quoted_pivoted_death_col_names_all,
          ", 0)",
          collapse = " + "
        )

        # Select strata columns prefixed with t. and quoted
        select_strata_sql <- paste0("t.", quoted_strata, collapse = ", ")

        # Construct the PIVOT SQL query (inner query) - uses all death codes
        sql_query <- sprintf(
          "WITH AggregatedDeaths AS (
           SELECT
             %s,
             all_cause_mrtl,
             SUM(wt) AS deaths
           FROM %s
           WHERE mc = %d -- Filter by mc
           GROUP BY %s, all_cause_mrtl
           )
           PIVOT AggregatedDeaths
           ON all_cause_mrtl IN (%s) -- Specify codes and quoted aliases to pivot
           USING SUM(deaths) -- Aggregation function
           GROUP BY %s -- Columns to keep (quoted)
           ", # Removed trailing semicolon
          strata_cols_sql, # Quoted strata cols for SELECT
          lc_table_name,
          mcaggr,
          strata_cols_sql, # Quoted strata cols for GROUP BY
          death_codes_pivot_sql, # Use pivot definition including quoted aliases
          strata_cols_sql # Quoted strata cols for GROUP BY
        )

        # Construct the final SQL query with COALESCE (excluding alive_deaths) and popsize calculation (including alive_deaths)
        sql_query_final <- sprintf(
          "SELECT
           %s, -- Select quoted strata columns
           %s, -- Select coalesced death columns (excluding alive_deaths, quoted)
           %s AS popsize -- Calculate popsize from ALL coalesced columns (including alive_deaths, quoted)
           FROM (%s) AS t
           ORDER BY %s", # Order by quoted strata columns, no semicolon
          select_strata_sql,
          coalesce_select_sql_final,
          death_cols_sum_sql,
          sql_query,
          strata_cols_sql
        ) # Use quoted strata_cols_sql for ORDER BY

        output_path <- private$output_dir(
          paste0(
            "summaries/dis_mrtl_scaled_up/",
            mcaggr,
            "_dis_mrtl_scale_up.",
            ext
          )
        )

        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query_final,
            output_path
          )
        )

        # esp
        sql_query_esp <- gsub("wt", "wt_esp", sql_query_final)
        output_path_esp <- private$output_dir(
          paste0("summaries/dis_mrtl_esp/", mcaggr, "_dis_mrtl_esp.", ext)
        )
        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query_esp,
            output_path_esp
          )
        )

        NULL
      }, # end of export_mrtl_summaries

      # export_all_cause_mrtl_by_dis_summaries ----
      export_all_cause_mrtl_by_dis_summaries = function(
        duckdb_con,
        mcaggr,
        strata,
        ext
      ) {
        lapply(
          paste0(
            rep(c("all_cause_mrtl_by_dis"), each = 2),
            "_",
            c("scaled_up", "esp")
          ),
          function(subdir) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir
            )))
          }
        )

        # Get disease prevalence columns from DuckDB schema
        lc_table_name <- "lc_table"
        schema <- dbGetQuery(duckdb_con, sprintf("DESCRIBE %s;", lc_table_name))
        prvl_cols <- schema$column_name[endsWith(schema$column_name, "_prvl")]
        disease_names <- gsub("_prvl$", "", prvl_cols)

        # Quote strata columns for safety
        quoted_strata <- paste0('"', strata, '"')
        strata_cols_sql <- paste(quoted_strata, collapse = ", ")

        # Construct CASE WHEN statements for each disease's cases and deaths
        case_statements <- sapply(disease_names, function(dis) {
          prvl_col <- paste0('"', dis, '_prvl"')
          sprintf(
            'SUM(CASE WHEN %s > 0 THEN wt ELSE 0 END) AS "cases_%s"',
            prvl_col,
            dis
          )
        })

        death_statements <- sapply(disease_names, function(dis) {
          prvl_col <- paste0('"', dis, '_prvl"')
          sprintf(
            'SUM(CASE WHEN %s > 0 AND all_cause_mrtl > 0 THEN wt ELSE 0 END) AS "deaths_%s"',
            prvl_col,
            dis
          )
        })

        # Combine all select parts
        select_parts_sql <- paste(
          c(strata_cols_sql, case_statements, death_statements),
          collapse = ",\n  "
        )

        # Construct the full SQL query
        sql_query_dis_char <- sprintf(
          "
        SELECT
          %s
        FROM %s
        WHERE mc = %d -- Filter for the specific mc iteration if needed
        GROUP BY %s
        ORDER BY %s
        ",
          select_parts_sql,
          lc_table_name,
          mcaggr,
          strata_cols_sql,
          strata_cols_sql
        )

        output_path <- private$output_dir(
          paste0(
            "summaries/all_cause_mrtl_by_dis_scaled_up/",
            mcaggr,
            "_all_cause_mrtl_by_dis_scale_up.",
            ext
          )
        )

        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query_dis_char,
            output_path
          )
        )

        # esp
        sql_query_esp <- gsub("wt", "wt_esp", sql_query_dis_char)
        output_path_esp <- private$output_dir(
          paste0(
            "summaries/all_cause_mrtl_by_dis_esp/",
            mcaggr,
            "_all_cause_mrtl_by_dis_esp.",
            ext
          )
        )
        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            sql_query_esp,
            output_path_esp
          )
        )

        NULL
      }, # end of export_mrtl_summaries

      # export_cms_summaries ----
      export_cms_summaries = function(
        duckdb_con,
        mcaggr,
        strata,
        strata_age,
        ext
      ) {
        lapply(
          paste0(
            rep(c("cms_score", "cms_score_by_age", "cms_count"), each = 2),
            "_",
            c("scaled_up", "esp")
          ),
          function(subdir) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir
            )))
          }
        )

        # Define configurations for CMS calculations
        # strata_sql is strata; strata_age_sql is strata_age
        strata_sql <- paste(strata, collapse = ", ")
        strata_age_sql <- paste(strata_age, collapse = ", ")

        cms_configs <- list(
          list(
            metric = "cms_score",
            group_cols_sql = strata_sql,
            group_cols_r = strata,
            weight_col = "wt",
            suffix = "_scaled_up",
            file_group_suffix = ""
          ),
          list(
            metric = "cms_score",
            group_cols_sql = strata_age_sql,
            group_cols_r = strata_age,
            weight_col = "wt",
            suffix = "_by_age_scaled_up",
            file_group_suffix = "_by_age"
          ),
          list(
            metric = "cms_score",
            group_cols_sql = strata_sql,
            group_cols_r = strata,
            weight_col = "wt_esp",
            suffix = "_esp",
            file_group_suffix = ""
          ),
          list(
            metric = "cms_score",
            group_cols_sql = strata_age_sql,
            group_cols_r = strata_age,
            weight_col = "wt_esp",
            suffix = "_by_age_esp",
            file_group_suffix = "_by_age"
          ),
          list(
            metric = "cms_count",
            group_cols_sql = strata_sql,
            group_cols_r = strata,
            weight_col = "wt",
            suffix = "_scaled_up",
            file_group_suffix = ""
          ),
          list(
            metric = "cms_count",
            group_cols_sql = strata_sql,
            group_cols_r = strata,
            weight_col = "wt_esp",
            suffix = "_esp",
            file_group_suffix = ""
          )
        )

        lc_table_name <- "lc_table"

        for (config in cms_configs) {
          query <- sprintf(
            "SELECT %s, SUM(%s) AS popsize, SUM(%s * %s) / SUM(%s) AS %s
             FROM %s
             WHERE mc = %d
             GROUP BY %s
             ORDER BY %s",
            config$group_cols_sql,
            config$weight_col,
            config$metric,
            config$weight_col,
            config$weight_col,
            config$metric,
            lc_table_name,
            mcaggr,
            config$group_cols_sql,
            config$group_cols_sql
          )

          output_path <- private$output_dir(
            paste0(
              "summaries/",
              config$metric,
              config$file_group_suffix,
              ifelse(config$weight_col == "wt_esp", "_esp", "_scaled_up"),
              "/",
              mcaggr,
              "_",
              config$metric,
              config$file_group_suffix,
              ifelse(config$weight_col == "wt_esp", "_esp", "_scaled_up"),
              ".",
              ext
            )
          )

          # Ensure output directory for the specific file exists (handles cases like cms_count_scaled_up)
          private$create_new_folder(dirname(output_path))

          dbExecute(
            duckdb_con,
            sprintf(
              "COPY (%s) TO '%s' (FORMAT PARQUET);",
              query,
              output_path
            )
          )
        }

        NULL
      }, # end of export_cms_summaries

      # export_qaly_summaries ----
      export_qaly_summaries = function(duckdb_con, mcaggr, strata, ext) {
        lapply(
          paste0(rep(c("qalys"), each = 2), "_", c("scaled_up", "esp")),
          function(subdir_suffix) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir_suffix
            )))
          }
        )

        lc_table_name <- "lc_table"

        # Define the name for the temporary view that calc_QALYs will create
        qaly_view_name <- "lc_with_qalys_view"

        # Call calc_QALYs to create/replace the temporary view with EQ5D5L and HUI3 columns.
        # This view will be based on lc_table and filtered for the current mcaggr.
        private$calc_QALYs(
          duckdb_con = duckdb_con,
          mcaggr = mcaggr,
          input_table_name = lc_table_name,
          output_view_name = qaly_view_name,
          include_non_significant = FALSE
        )

        # Prepare strata columns for SQL query (quoted)
        # 'strata' is defined earlier in export_summaries_hlpr
        quoted_strata_cols_sql <- paste(
          sprintf('"%s"', strata),
          collapse = ", "
        )

        # Define QALY metrics for SELECT statement
        qaly_metrics_select_wt <- 'SUM("EQ5D5L" * wt) AS "EQ5D5L", SUM("HUI3" * wt) AS "HUI3"'
        qaly_metrics_select_wt_esp <- 'SUM("EQ5D5L" * wt_esp) AS "EQ5D5L", SUM("HUI3" * wt_esp) AS "HUI3"'

        # --- Scaled-up QALYs ---
        query_scaled_up <- sprintf(
          "SELECT %s, SUM(wt) AS popsize, %s
             FROM %s -- This view is already filtered by mcaggr by calc_QALYs
             GROUP BY %s
             ORDER BY %s",
          quoted_strata_cols_sql,
          qaly_metrics_select_wt,
          qaly_view_name,
          quoted_strata_cols_sql,
          quoted_strata_cols_sql
        )
        output_path_scaled_up <- private$output_dir(
          paste0("summaries/qalys_scaled_up/", mcaggr, "_qalys_scaled_up.", ext)
        )
        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            query_scaled_up,
            output_path_scaled_up
          )
        )

        # --- ESP QALYs ---
        query_esp <- sprintf(
          "SELECT %s, SUM(wt_esp) AS popsize, %s
             FROM %s -- This view is already filtered by mcaggr by calc_QALYs
             GROUP BY %s
             ORDER BY %s",
          quoted_strata_cols_sql,
          qaly_metrics_select_wt_esp,
          qaly_view_name,
          quoted_strata_cols_sql,
          quoted_strata_cols_sql
        )
        output_path_esp <- private$output_dir(
          paste0("summaries/qalys_esp/", mcaggr, "_qalys_esp.", ext)
        )
        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            query_esp,
            output_path_esp
          )
        )

        # drop the temporary view if it's no longer needed for this mcaggr
        dbExecute(
          duckdb_con,
          sprintf("DROP VIEW IF EXISTS %s;", qaly_view_name)
        )

        NULL
      }, # end of export_qaly_summaries

      # export_costs_summaries ----
      export_costs_summaries = function(duckdb_con, mcaggr, strata, ext) {
        # Create output directories for scaled-up and ESP-weighted summaries
        lapply(
          paste0(rep(c("costs"), each = 2), "_", c("scaled_up", "esp")),
          function(subdir_suffix) {
            private$create_new_folder(private$output_dir(paste0(
              "summaries/",
              subdir_suffix
            )))
          }
        )

        lc_table_name <- "lc_table"

        # Define the name for the temporary view that calc_costs will create
        costs_view_name <- "lc_with_costs_view"

        # Call calc_costs to create/replace the temporary view with cost columns.
        # This view will be based on lc_table and filtered for the current mcaggr.
        private$calc_costs(
          duckdb_con = duckdb_con,
          mcaggr = mcaggr,
          input_table_name = lc_table_name,
          output_view_name = costs_view_name
        )

        # Prepare strata columns for SQL query (quoted)
        quoted_strata_cols_sql <- paste(
          sprintf('"%s"', strata),
          collapse = ", "
        )

        # Define cost metrics for SELECT statement
        cost_metrics_select_wt <- paste(
          'SUM("chd_productivity_costs" * wt) AS "chd_productivity_costs"',
          'SUM("stroke_productivity_costs" * wt) AS "stroke_productivity_costs"',
          'SUM("chd_informal_costs" * wt) AS "chd_informal_costs"',
          'SUM("stroke_informal_costs" * wt) AS "stroke_informal_costs"',
          'SUM("chd_direct_costs" * wt) AS "chd_direct_costs"',
          'SUM("stroke_direct_costs" * wt) AS "stroke_direct_costs"',
          'SUM("chd_indirect_costs" * wt) AS "chd_indirect_costs"',
          'SUM("stroke_indirect_costs" * wt) AS "stroke_indirect_costs"',
          'SUM("chd_total_costs" * wt) AS "chd_total_costs"',
          'SUM("stroke_total_costs" * wt) AS "stroke_total_costs"',
          'SUM("cvd_productivity_costs" * wt) AS "cvd_productivity_costs"',
          'SUM("cvd_informal_costs" * wt) AS "cvd_informal_costs"',
          'SUM("cvd_direct_costs" * wt) AS "cvd_direct_costs"',
          'SUM("cvd_indirect_costs" * wt) AS "cvd_indirect_costs"',
          'SUM("cvd_total_costs" * wt) AS "cvd_total_costs"',
          sep = ", "
        )

        cost_metrics_select_wt_esp <- gsub(
          "wt",
          "wt_esp",
          cost_metrics_select_wt
        )

        # --- Scaled-up Costs ---
        query_scaled_up <- sprintf(
          "SELECT %s, SUM(wt) AS popsize, %s
       FROM %s
       GROUP BY %s
       ORDER BY %s",
          quoted_strata_cols_sql,
          cost_metrics_select_wt,
          costs_view_name,
          quoted_strata_cols_sql,
          quoted_strata_cols_sql
        )
        output_path_scaled_up <- private$output_dir(
          paste0("summaries/costs_scaled_up/", mcaggr, "_costs_scaled_up.", ext)
        )
        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            query_scaled_up,
            output_path_scaled_up
          )
        )

        # --- ESP Costs ---
        query_esp <- sprintf(
          "SELECT %s, SUM(wt_esp) AS popsize, %s
       FROM %s
       GROUP BY %s
       ORDER BY %s",
          quoted_strata_cols_sql,
          cost_metrics_select_wt_esp,
          costs_view_name,
          quoted_strata_cols_sql,
          quoted_strata_cols_sql
        )
        output_path_esp <- private$output_dir(
          paste0("summaries/costs_esp/", mcaggr, "_costs_esp.", ext)
        )
        dbExecute(
          duckdb_con,
          sprintf(
            "COPY (%s) TO '%s' (FORMAT PARQUET);",
            query_esp,
            output_path_esp
          )
        )

        # Drop the temporary view if it's no longer needed for this mcaggr
        dbExecute(
          duckdb_con,
          sprintf("DROP VIEW IF EXISTS %s;", costs_view_name)
        )

        NULL
      }
    ) # End of private methods
  ) # Endo of class

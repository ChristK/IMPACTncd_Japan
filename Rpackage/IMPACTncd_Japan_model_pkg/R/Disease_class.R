## IMPACTncdJapan is an implementation of the IMPACTncd framework, developed by
## Chris Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
## Guzman-Castillo, Amandine Robert, and Piotr Bandosz. This work has been
## funded by NIHR  HTA Project: 16/165/01 - IMPACTncdJapan: Health Outcomes
## Research Simulation Environment.  The views expressed are those of the
## authors and not necessarily those of the NHS, the NIHR or the Department of
## Health.
##
## Copyright (C) 2018-2022 University of Liverpool, Chris Kypridemos
##
## IMPACTncdJapan is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the Free
## Software Foundation; either version 3 of the License, or (at your option) any
## later version. This program is distributed in the hope that it will be
## useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
## Public License for more details. You should have received a copy of the GNU
## General Public License along with this program; if not, see
## <http://www.gnu.org/licenses/> or write to the Free Software Foundation,
## Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#' R6 Class representing an disease
#'
#' @description
#' A disease has a sim_prm list that holds the simulation parameters.
#'
#' @details
#' To be completed...
#'
#' @export
Disease <-
  R6::R6Class(
    classname = "Disease",
    # cloneable = FALSE, # cloneable is necessary for multi threading
    # public ------------------------------------------------------------------
    public = list(
      #' @field name The name of the disease.
      name = NA_character_,
      #' @field friendly_name A friendly name for the disease.
      friendly_name = NA_character_,
      #' @field meta Disease metadata including type.
      meta = NA_integer_,
      #' @field notes Any notes regarding the disease.
      notes = NA_character_,

      #' @description Create a new disease object.
      #' @param name A string with disease name.
      #' @param friendly_name A string with disease friendly name.
      #' @param RR A list of exposure objects.
      #' @param meta A list with the disease type and other information for
      #'   incidence, diagnosis, and mortality.
      #' @param notes A string with any notes.
      #' @param design_ A design object with the simulation parameters.
      #' @return A new `Disease` object.

      # initialise ----
      initialize = function(
        name,
        friendly_name,
        meta,
        notes = NA_character_,
        design_,
        RR
      ) {
        if (!inherits(design_, "Design")) {
          stop("Argument design needs to be a Design object.")
        }
        if (!(is.character(name) && is.character(friendly_name))) {
          stop("Both arguments need to be strings.")
        }
        if (!all(sapply(RR, inherits, "Exposure"))) {
          stop("Argument RR needs to be a list of exposure object.")
        }

        self$name <- name
        self$friendly_name <- friendly_name
        self$meta <- meta
        self$notes <- notes

        # Generate unique name using the relevant RR and lags
        rr <- RR[sapply(RR, `[[`, "outcome") == self$name]
        # above only contains exposures for this disease object
        # Reorder risk so smok_status & smok_cig is calculated before quit_yrs (irrelevant currently)
        private$rr <-
          rr[order(match(
            sapply(rr, `[[`, "name"),
            c("Smoking", "Smoking_number")
          ))]

        dqRNGkind("pcg64")
        private$seed <- abs(digest2int(name, seed = 230565490L))

        private$sDiseaseBurdenDirPath <- file.path(
          getwd(),
          "inputs",
          "disease_burden"
        )
        vsFileTypes <- vector("character")
        if (is.numeric(meta$incidence$type) && meta$incidence$type > 1L) {
          private$filenams$incd <- file.path(
            private$sDiseaseBurdenDirPath,
            paste0(self$name, "_incd", ".fst") # incidence
          )
          private$filenams$incd_indx <- file.path(
            private$sDiseaseBurdenDirPath,
            paste0(self$name, "_incd_indx", ".fst")
          )
          private$filenams$prvl <- file.path(
            private$sDiseaseBurdenDirPath,
            paste0(self$name, "_prvl", ".fst") # prevalence
          )
          private$filenams$prvl_indx <- file.path(
            private$sDiseaseBurdenDirPath,
            paste0(self$name, "_prvl_indx", ".fst")
          )
          private$filenams$dur <- file.path(
            private$sDiseaseBurdenDirPath,
            paste0(self$name, "_dur", ".fst") # disease duration
          )
          vsFileTypes <- c(vsFileTypes, "incd", "prvl")
        }

        if (is.numeric(meta$mortality$type)) {
          private$filenams$ftlt <- file.path(
            private$sDiseaseBurdenDirPath,
            paste0(self$name, "_ftlt", ".fst") # fatality probability
          )
          private$filenams$ftlt_indx <- file.path(
            private$sDiseaseBurdenDirPath,
            paste0(self$name, "_ftlt_indx", ".fst")
          )
          vsFileTypes <- c(vsFileTypes, "ftlt")
        }

        # TODO add check for stop('For type 1 incidence aggregation of RF need
        # to be "any" or "all".')

        private$incd_colnam <- paste0("prb_", self$name, "_incd")
        private$dgns_colnam <- paste0("prb_", self$name, "_dgns")
        private$mrtl_colnam2 <- paste0("prb_", self$name, "_mrtl2") # Only for mrtl 2

        private$chksum <-
          digest(list(
            design_$sim_prm[c(
              "init_year",
              "sim_horizon_max",
              "ageL",
              "ageH",
              "apply_RR_to_mrtl2",
              "model_trends_in_redidual_incd"
            )],
            lapply(private$rr, function(x) {
              x$get_input_rr()
            }),
            lapply(private$rr, `[[`, "lag"),
            lapply(private$rr, `[[`, "distribution")
          ))

        private$parf_dir <- file.path(getwd(), "simulation", "parf")
        if (!dir.exists(private$parf_dir)) {
          dir.create(private$parf_dir)
        }
        private$parf_filenam <- file.path(
          private$parf_dir,
          paste0("PARF_", self$name, "_", private$chksum, ".fst")
        )

        keys <- sapply(
          private$filenams[names(private$filenams) %in% vsFileTypes],
          function(x) metadata_fst(x)$keys[[1]]
        )

        if (length(keys) > 0 && !all(sapply(keys, identical, "year"))) {
          # TODO ?remove
          warning("If present, 1st key needs to be year")
        } # TODO fix message if mc present

        # update each _indx file on snapshot change, prior to creation of new snapshot
        bSnapshotChange <- private$UpdateDiseaseSnapshotIfInvalid(
          FALSE,
          function() {
            for (sFileType in vsFileTypes) {
              sIndexFileType <- paste0(sFileType, "_indx")
              if (file.exists(private$filenams[[sIndexFileType]])) {
                file.remove(private$filenams[[sIndexFileType]])
              }

              # write each [year]'s min and max row index to _indx file
              private[[sIndexFileType]] <- read_fst(
                private$filenams[[sFileType]],
                as.data.table = TRUE,
                columns = "year"
              )[, .(from = min(.I), to = max(.I)), keyby = "year"]
              write_fst(
                private[[sIndexFileType]],
                private$filenams[[sIndexFileType]],
                100L
              )
            }
          }
        )

        if (!bSnapshotChange) {
          # if indx up to date
          for (sFileType in vsFileTypes) {
            private[[paste0(sFileType, "_indx")]] <-
              read_fst(
                private$filenams[[paste0(sFileType, "_indx")]],
                as.data.table = TRUE
              )
          }
        }

        invisible(self)
      },

      #' @description Generates PARF and stores it to disk if one doesn not
      #'   exists already.
      #' @param design_ A design object with the simulation parameters.
      #' @param diseases_ A list of Disease objects.
      #' @param popsize The population size for each stratum.
      #' @param check Check for NAs in parf_dt.
      #' @param keep_intermediate_file Whether to keep the intermediate synthpop file.
      #' @param bUpdateExistingDiseaseSnapshot bool, update existing disease PARF and snapshot files as necessary.
      #' @return The PARF data.table if it was created, otherwise `NULL`.

      # gen_parf_files ----
      gen_parf_files = function(
        design_ = design,
        diseases_ = diseases,
        popsize = 100,
        check = design_$sim_prm$logs,
        keep_intermediate_file = TRUE,
        bUpdateExistingDiseaseSnapshot = TRUE
      ) {
        if (
          (is.numeric(self$meta$incidence$type) &&
            self$meta$incidence$type < 2L) ||
            length(private$rr) == 0L
        ) {
          # Early break for type 1 incidence and diseases with no RF
          return(NULL)
        }

        if (!inherits(design_, "Design")) {
          stop("Argument design_ needs to be a Design object.")
        }
        if (!all(sapply(diseases_, inherits, "Disease"))) {
          stop("Argument diseases_ needs to be a list of disease object.")
        }

        # delete disease PARF file and update snapshot if necessary
        if (bUpdateExistingDiseaseSnapshot) {
          private$UpdateDiseaseSnapshotIfInvalid(TRUE, function() {
            self$del_parf_file()
          })
        }

        if (file.exists(private$parf_filenam)) {
          return(NULL)
        } # nothing to do

        tmpfile <- file.path(
          private$parf_dir,
          paste0(
            "PARF_",
            self$name,
            "_",
            digest(
              list(
                lapply(private$rr, function(x) {
                  x$get_input_rr()
                }),
                lapply(private$rr, `[[`, "lag"),
                lapply(private$rr, `[[`, "distribution")
              )
            ),
            ".qs"
          )
        )

        if (file.exists(tmpfile)) {
          if (design_$sim_prm$logs) {
            message("Reading file from cache.")
          }
          ans <- qs_read(tmpfile, nthreads = 1L)
          setDT(ans$pop)
        } else {
          if (design_$sim_prm$logs) {
            message("No available cached file.")
          }

          self$del_parf_file(invert = TRUE) # Delete old versions

          # start if file not exist
          if (sum(dim(private$incd_indx)) > 0) {
            ff <- self$get_incd(
              design_$sim_prm$init_year_long,
              design_ = design_
            )
          } else {
            ff <- self$get_ftlt(
              design_$sim_prm$init_year_long,
              design_ = design_
            )
          }

          ff <- CJ(
            age = seq(design_$sim_prm$ageL, design_$sim_prm$ageH),
            sex = ff$sex,
            year = design_$sim_prm$init_year_long,
            unique = TRUE
          )

          ff <- clone_dt(ff, 10, idcol = NULL)

          # NOTE future and mclapply do not work here for some reason
          if (.Platform$OS.type == "windows") {
            cl <-
              makeClusterPSOCK(
                design_$sim_prm$clusternumber,
                dryrun = FALSE,
                quiet = FALSE,
                rscript_startup = quote(local({
                  library(CKutils)
                  library(IMPACTncdJapan)
                  library(digest)
                  library(fst)
                  library(qs2)
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
              X = seq(1, (popsize / 10L)),
              fun = function(x) {
                private$gen_sp_forPARF(
                  x,
                  ff = ff,
                  design_ = design_,
                  diseases_ = diseases_
                )
              }
            )
          } else {
            registerDoParallel(design_$sim_prm$clusternumber) # used for forking. Only Linux/OSX compatible
          }
          xps_dt <- foreach(
            mc_iter = seq(1, (popsize / 10L)),
            .inorder = FALSE,
            .options.multicore = list(preschedule = FALSE),
            .verbose = design_$sim_prm$logs,
            .packages = c(
              "R6",
              "gamlss.dist",
              "dqrng",
              "CKutils",
              "IMPACTncdJapan",
              "data.table"
            ),
            .export = NULL,
            .noexport = NULL # c("time_mark")
          ) %dopar%
            {
              private$gen_sp_forPARF(
                mc_iter,
                ff = ff,
                design_ = design_,
                diseases_ = diseases_
              )
            }

          if (design_$sim_prm$logs) {
            message("End of parallelisation for PARF.")
          }

          ans <- SynthPop$new(mc = 0L, design_)
          ans$pop <- rbindlist(xps_dt)
          ans$pop[, `:=`(pid = .I, pid_mrk = TRUE)] # TODO add check to avoid intmax limit
          ans$mc <- 0L
          ans$mc_aggr <- 0L # TODO: Check with Chris. Needed so that PARF files can read prevalence
          #       for diseases that act as exposures!

          # NOTE xps_dt does not contain disease init prevalence. I simulate
          # here as set_init_prvl expects a synthpop and not a data.table as
          # input. All relevant risk factors for the diseases need to be
          # available in xps_dt.

          # Generate diseases that act as exposures
          xps_dep <- private$get_xps_dependency_tree(
            x = self$name,
            dssl = diseases_
          )
          xps_dep <- xps_dep[grepl("_prvl$", xpscol)]
          setkey(xps_dep, xpscol)

          for (xps in paste0(names(diseases_), "_prvl")) {
            if (xps %in% xps_dep$xpscol) {
              lag <- xps_dep[xps, max(lag)] # See note below in line ~ 1681 about max
              ans$pop[, year := year - lag]
              design_$sim_prm$init_year <-
                design_$sim_prm$init_year - lag
              disnam <- gsub("_prvl$", "", xps)
              diseases_[[disnam]]$set_init_prvl(ans, design_)
              design_$sim_prm$init_year <-
                design_$sim_prm$init_year + lag
              ans$pop[, year := year + lag]
            }
          }
          if (design_$sim_prm$logs) {
            message("Saving parf cache.")
          }
          qs_save(ans, tmpfile, nthreads = 1L) # NOTE multithreaded qs_save requires TBB https://cran.r-project.org/web/packages/qs2/vignettes/vignette.html
        } # end tmpfile bypass

        self$set_rr(ans, design_, forPARF = TRUE)

        nam <- grep("_rr$", names(ans$pop), value = TRUE)
        # risks <- ans$pop[, .SD, .SDcols = c("pid", "year", nam)]

        parf_dt <-
          ans$pop[
            between(age, design_$sim_prm$ageL, design_$sim_prm$ageH),
            .(parf = 1 - .N / sum(Reduce(`*`, .SD))), # mbirkett: #PARF
            keyby = .(age, sex),
            .SDcols = nam
          ]

        if (sum(dim(private$incd_indx)) > 0) {
          if (design_$sim_prm$logs) {
            message("Estimating p0.")
          }

          yrs <- design_$sim_prm$init_year_long

          tt <- self$get_incd(yrs, 0L, design_ = design_) # Note this is the median incd. It is updated stochastically for each iteration with gen_parf()
          # nam <- "p0"

          absorb_dt(parf_dt, tt)
          setnafill(parf_dt, "c", fill = 0, cols = "mu") # fix for prostate and breast cancer

          parf_dt[, p0 := mu * (1 - parf)]
          parf_dt[, ("mu") := NULL]
        }

        if (sum(dim(private$ftlt_indx)) > 0) {
          if (design_$sim_prm$logs) {
            message("Estimating m0.")
          }

          # Re-estimate parf without exposures and only for diseases when
          # apply_rr_to_mrtl2 is FALSE
          if (!design_$sim_prm$apply_RR_to_mrtl2) {
            riskcolnam <- grep(
              paste0(
                "^((?!",
                paste(
                  self$meta$mortality$influenced_by_disease_name,
                  collapse = "|"
                ),
                ").)*_rr$"
              ),
              names(nam),
              value = TRUE,
              perl = TRUE
            )

            if (length(riskcolnam) > 0) {
              parf_dt_mrtl <-
                ans$pop[
                  between(age, design_$sim_prm$ageL, design_$sim_prm$ageH),
                  .(parf_mrtl = 1 - .N / sum(Reduce(`*`, .SD))),
                  keyby = .(age, sex),
                  .SDcols = riskcolnam
                ]
              absorb_dt(parf_dt, parf_dt_mrtl)
            }
          }

          yrs <- seq(
            design_$sim_prm$init_year_long,
            design_$sim_prm$init_year_long + design_$sim_prm$sim_horizon_max
          )
          tt <- self$get_ftlt(yrs, 0L, design_ = design_) # Note this is the median ftlt. It is updated stochastically for each iteration with gen_parf()

          if ("mu" %in% names(tt)) {
            stop("mu in ftlt file. Please rename to mu2.")
          }

          setnames(tt, "mu2", "mu")
          if ("mu1" %in% names(tt)) {
            tt[, mu1 := NULL]
          }
          # nam <- "m0"

          if (!all(yrs %in% unique(parf_dt$years))) {
            # TODO safer logic here
            parf_dt <- clone_dt(parf_dt, length(yrs))
            parf_dt[, year := .id - 1L + design_$sim_prm$init_year_long]
            parf_dt[, .id := NULL]
          }
          absorb_dt(parf_dt, tt)
          setnafill(parf_dt, "c", fill = 0, cols = "mu") # fix for prostate and breast cancer

          if ("parf_mrtl" %in% names(parf_dt)) {
            parf_dt[, m0 := mu * (1 - parf_mrtl)]
          } else {
            parf_dt[, m0 := mu * (1 - parf)]
          }
          parf_dt[, c("mu") := NULL]
        }

        if (uniqueN(parf_dt$year) == 1L) {
          parf_dt[, year := NULL]
        }

        if (check && anyNA(parf_dt)) {
          warning("NAs in parf.")
          print(summary(parf_dt))
          # parf_dt[is.na(get(nam)), (nam) := mu]
        }

        if (design_$sim_prm$logs) {
          message("Saving parf file ", private$parf_filenam)
        }

        write_fst(parf_dt, private$parf_filenam, 100L)

        if (!keep_intermediate_file) {
          file.remove(tmpfile)
        }

        invisible(parf_dt)
      },

      # gen_parf ----
      #' @description Read PARF file from disk. If missing, generates PARF and
      #'   writes it to disk.
      #' @param sp A synthpop object
      #' @param design_ A design object with the simulation parameters.
      #' @param diseases_ A list of Disease objects
      #' @param popsize The population size for each stratum
      #' @param check Check for NAs in parf_dt.
      #' @param keep_intermediate_file Whether to keep the intermediate synthpop file
      #' @return The invisible self for chaining.

      gen_parf = function(
        sp = sp,
        design_ = design,
        diseases_ = diseases,
        popsize = 100,
        check = design_$sim_prm$logs,
        keep_intermediate_file = TRUE
      ) {
        # TODO add logic to delete the intermediate synthpop file outside this
        # function

        if (
          (is.numeric(self$meta$incidence$type) &&
            self$meta$incidence$type == 1L) ||
            length(private$rr) == 0L
        ) {
          # Early break for type 1 incidence and diseases with no RF
          return(invisible(self))
        }

        if (!inherits(sp, "SynthPop")) {
          stop("Argument sp needs to be a SynthPop object.")
        }
        if (!inherits(design_, "Design")) {
          stop("Argument design_ needs to be a Design object.")
        }
        if (!all(sapply(diseases_, inherits, "Disease"))) {
          stop("Argument diseases_ needs to be a list of disease object.")
        }

        # Logic to ensure parf files are regenerated when disease incidence
        # change.
        private$UpdateDiseaseSnapshotIfInvalid(TRUE, function() {
          self$del_parf_file()
        })

        if (file.exists(private$parf_filenam)) {
          if (design_$sim_prm$logs) {
            message("Reading parf file from disk.")
          }

          parf_dt <- read_fst(private$parf_filenam, as.data.table = TRUE)
        } else {
          # if file not exist
          if (design_$sim_prm$logs) {
            message("Generating new parf file.")
          }

          # shortcut for when parallel part is successful but function crashes
          # after it. This logic caches the synthpop for parf that is
          # independent of the RR but only depends on the causal structure.
          # Hence this is using another checksam that only depends on the names
          # of xps stored in rr

          parf_dt <- self$gen_parf_files(
            design_,
            diseases_,
            popsize,
            check,
            keep_intermediate_file,
            bUpdateExistingDiseaseSnapshot = FALSE
          )
        } # end if file not exist

        # Update p0 and m0 using stochastic incidence incd and mrtl
        if (sum(dim(private$incd_indx)) > 0) {
          # This excludes nonmodelled diseases
          if (design_$sim_prm$logs) {
            message("Estimating p0.")
          }

          yrs <- design_$sim_prm$init_year_long

          tt <- self$get_incd(yrs, sp$mc_aggr, design_ = design_)[,
            year := NULL
          ]
          # nam <- "p0"

          absorb_dt(parf_dt, tt) # now mu is incd
          setnafill(parf_dt, "c", fill = 0, cols = "mu") # fix for prostate and breast cancer

          # Nonmodelled is excluded in this branch
          absorb_dt(
            parf_dt,
            fread(
              "./simulation/calibration_prms.csv",
              select = c(
                "year",
                "age",
                "sex",
                paste0(self$name, "_incd_clbr_fctr")
              )
            )
          )

          if (!design_$sim_prm$calibrate_to_incd_trends) {
            parf_dt[, paste0(self$name, "_incd_clbr_fctr") := 1]
          }

          # TODO check if this is correct, Rony is using the commented code further below as a fix
          setnames(
            parf_dt,
            paste0(self$name, "_incd_clbr_fctr"),
            "incd_clbr_fctr"
          )

          parf_dt[, p0 := incd_clbr_fctr * mu * (1 - parf)] # now p0 incorporates the calibration
          parf_dt[, c("mu", "incd_clbr_fctr") := NULL]

          # if (self$name * "nonmodelled") {
          #   absorb_dt(parf_dt, fread("./simulation/calibration_prms.csv",
          #     select = c("year", "age", "sex", paste0(self$name, "_incd_clbr_fctr"))
          #   ))
          # }
        }

        if (sum(dim(private$ftlt_indx)) > 0) {
          if (design_$sim_prm$logs) {
            message("Estimating m0.")
          }

          # Re-estimate parf without exposures and only for diseases when
          # apply_rr_to_mrtl2 is FALSE
          if (!design_$sim_prm$apply_RR_to_mrtl2) {
            riskcolnam <- grep(
              paste0(
                "^((?!",
                paste(
                  self$meta$mortality$influenced_by_disease_name,
                  collapse = "|"
                ),
                ").)*_rr$"
              ),
              names(nam),
              value = TRUE,
              perl = TRUE
            )

            if (length(riskcolnam) > 0) {
              parf_dt_mrtl <-
                ans$pop[
                  between(age, design_$sim_prm$ageL, design_$sim_prm$ageH),
                  .(parf_mrtl = 1 - .N / sum(Reduce(`*`, .SD))),
                  keyby = .(age, sex),
                  .SDcols = riskcolnam
                ]
              absorb_dt(parf_dt, parf_dt_mrtl)
            }
          }

          yrs <- seq(
            design_$sim_prm$init_year_long,
            design_$sim_prm$init_year_long + design_$sim_prm$sim_horizon_max
          )
          tt <- self$get_ftlt(yrs, sp$mc_aggr, design_ = design_) # Note this is the median ftlt. Needs to be updated stochastically for each iteration

          if ("mu" %in% names(tt)) {
            stop("mu in ftlt file. Please rename to mu2.")
          }

          setnames(tt, "mu2", "mu")
          if ("mu1" %in% names(tt)) {
            tt[, mu1 := NULL]
          }

          if (!all(yrs %in% unique(parf_dt$year))) {
            # TODO safer logic here
            parf_dt <- clone_dt(parf_dt, length(yrs))
            parf_dt[, year := .id - 1L + design_$sim_prm$init_year_long]
            parf_dt[, .id := NULL]
          }
          absorb_dt(parf_dt, tt) # mu is still mrtl, not ftlt
          setnafill(parf_dt, "c", fill = 0, cols = "mu") # fix for prostate and breast cancer

          absorb_dt(
            parf_dt,
            fread(
              "./simulation/calibration_prms.csv",
              select = c(
                "year",
                "age",
                "sex",
                paste0(self$name, "_ftlt_clbr_fctr")
              )
            )
          )

          if (!design_$sim_prm$calibrate_to_ftlt_trends) {
            parf_dt[, paste0(self$name, "_ftlt_clbr_fctr") := 1]
          }

          setnames(
            parf_dt,
            paste0(self$name, "_ftlt_clbr_fctr"),
            "ftlt_clbr_fctr"
          )

          if ("parf_mrtl" %in% names(parf_dt)) {
            parf_dt[, m0 := ftlt_clbr_fctr * mu * (1 - parf_mrtl)]
          } else {
            parf_dt[, m0 := ftlt_clbr_fctr * mu * (1 - parf)]
          }
          parf_dt[, c("mu", "ftlt_clbr_fctr") := NULL]
        }

        colnam <-
          setdiff(names(parf_dt), intersect(names(sp$pop), names(parf_dt)))
        private$parf <- parf_dt[sp$pop, on = .NATURAL, ..colnam]

        invisible(self)
      },

      #  set_init_prvl -----
      #' @description Set disease prevalence & diagnosis in a new col in sp$pop.
      #' @param sp A synthetic population.
      #' @param design_ A design object with the simulation parameters.
      #' @return The invisible self for chaining.
      set_init_prvl = function(sp, design_ = design) {
        # TODO correlate with other diseases prevalence
        if (is.numeric(self$meta$incidence$type)) {
          if (!inherits(sp, "SynthPop")) {
            stop("Argument sp needs to be a SynthPop object.")
          }
          if (!inherits(design_, "Design")) {
            stop("Argument design_ needs to be a Design object.")
          }

          namprvl <- paste0(self$name, "_prvl")
          if (namprvl %in% names(sp$pop)) {
            stop(
              "A column named ",
              namprvl,
              " already exists in sp$pop.
              Please delete it and run set_init_prvl() afterwards."
            )
          }

          if (self$meta$incidence$type == 0L) {
            set(sp$pop, NULL, namprvl, 0L)
          } else if (self$meta$incidence$type == 1L) {
            self$set_rr(sp, design_, forPARF = FALSE)
            riskcolnam <- grep(
              "_rr$",
              names(sp$get_risks(self$name)),
              value = TRUE,
              perl = TRUE
            )
            if (length(riskcolnam) == 1L) {
              thresh <- as.integer(sp$get_risks(self$name)[[riskcolnam]])
            }
            if (
              length(riskcolnam) > 1L &&
                self$meta$incidence$aggregation == "any"
            ) {
              thresh <- as.integer(sp$get_risks(self$name)[,
                do.call(pmax, .SD),
                .SDcols = riskcolnam
              ])
            }
            if (
              length(riskcolnam) > 1L &&
                self$meta$incidence$aggregation == "all"
            ) {
              thresh <- as.integer(sp$get_risks(self$name)[,
                Reduce(`*`, .SD),
                .SDcols = riskcolnam
              ])
            }

            set(sp$pop, NULL, namprvl, thresh)

            sp$pop[
              year > design_$sim_prm$init_year_long &
                age > design_$sim_prm$ageL,
              (namprvl) := 0L
            ]

            sp$pop[, carry_forward_incr(
              get((namprvl)),
              pid_mrk,
              recur = self$meta$incidence$can_recur,
              y = 1L,
              byref = TRUE
            )]
            invisible(self)
          } else if (self$meta$incidence$type > 1L) {
            # if incidence type not 0 or 1

            dqRNGkind("pcg64")
            dqset.seed(private$seed, stream = sp$mc * 10 + 1L) # not mc_aggr
            set.seed(private$seed + sp$mc * 10 + 1L) # for sample_int_expj

            # First find out how many prevalent cases by pop subgroup
            tbl <- self$get_prvl(
              design_$sim_prm$init_year_long,
              mc_ = sp$mc_aggr,
              design_
            )[between(age, design_$sim_prm$ageL, design_$sim_prm$ageH)]

            tt <- self$get_incd(
              design_$sim_prm$init_year_long,
              mc_ = sp$mc_aggr,
              design_
            )[between(age, design_$sim_prm$ageL, design_$sim_prm$ageH)]

            strata <- setdiff(names(tbl), c("mu"))
            tbl[tt, on = strata, mu := mu - i.mu]

            #lookup_dt(sp$pop, tbl, check_lookup_tbl_validity = FALSE) #TODO: lookup_dt
            absorb_dt(sp$pop, tbl)

            setnafill(sp$pop, "c", fill = 0, cols = "mu") # fix for prostate and breast cancer
            sp$pop[
              year == design_$sim_prm$init_year_long,
              (namprvl) := as.integer(dqrunif(.N) < mu)
            ]
            sp$pop[, mu := NULL]

            # For future 30yo cohorts, we need to project prevalence assuming linear trends
            # NOTE below assumes prvl avail for years 2010 - 2019
            tbl <- self$get_prvl(seq(2010L, 2019L), mc_ = sp$mc_aggr, design_)[
              age == design_$sim_prm$ageL
            ]

            # assume linear projection for year post 2019 based on data since 2010 (trends different before)
            m1 <- tbl[sex == "men", lm(mu ~ year)]
            m2 <- tbl[sex == "women", lm(mu ~ year)]

            tbl <- self$get_prvl(
              seq(design_$sim_prm$init_year_long + 1L, 2019L),
              mc_ = sp$mc_aggr,
              design_
            )[age == design_$sim_prm$ageL]
            tt1 <- data.table(
              age = 30L,
              year = 2020:(design_$sim_prm$init_year_long +
                design_$sim_prm$sim_horizon_max),
              sex = "men"
            )
            tt1[, mu := predict(m1, newdata = .SD)]
            tbl <- rbind(tbl, tt1, fill = TRUE)
            tt1 <- data.table(
              age = 30L,
              year = 2020:(design_$sim_prm$init_year_long +
                design_$sim_prm$sim_horizon_max),
              sex = "women"
            )
            tt1[, mu := predict(m2, newdata = .SD)]
            tbl <- rbind(tbl, tt1, fill = TRUE)

            #lookup_dt(sp$pop, tbl, check_lookup_tbl_validity = FALSE) #TODO: lookup_dt
            absorb_dt(sp$pop, tbl)
            setnafill(sp$pop, "c", fill = 0, cols = "mu") # fix for prostate and breast cancer

            sp$pop[
              year > design_$sim_prm$init_year_long &
                age == design_$sim_prm$ageL,
              (namprvl) := as.integer(dqrunif(.N) < mu)
            ]
            setnafill(sp$pop, "c", 0L, cols = namprvl)
            sp$pop[, mu := NULL]

            # Then select individuals with higher risk to have higher probability
            # to be a prevalent case. I estimate weights based on relevant RF for
            # each disease. Note that RR and sampling weights are equivalent here.
            # The RR are for incident. One would expect prevalent cases to be
            # healthier because of survival of the fittest and because some may
            # have better RF control post diagnosis. For that I will arbitrarily
            # assume that the risk for prevalence is half of that for incidence.

            if (length(private$rr) > 0L && any(sp$pop[[namprvl]] > 0L)) {
              # ncases is the number of prevalent cases expected in each stratum
              sp$pop[
                year >= design_$sim_prm$init_year_long,
                ncases := sum(get(namprvl), na.rm = TRUE),
                by = eval(strata)
              ]

              # Generate unique name using the relevant RR and lags

              # TODO below should apply only to strata with ncases > 0 for efficiency
              self$set_rr(
                sp,
                design_,
                forPARF = TRUE,
                checkNAs = design_$sim_prm$logs
              )

              nam <- grep("_rr$", names(sp$pop), value = TRUE) # necessary because above forPARF = TRUE

              sp$pop[, disease_wt := (Reduce(`*`, .SD)), .SDcols = nam]
              sp$pop[, (nam) := NULL]
              # adjust for prevalent risk half of incident risk

              sp$pop[, disease_wt := ((disease_wt - 1) * 0.5) + 1]

              ss <- sp$pop[ncases > 0, ][,
                .(
                  "pid" = pid[sample_int_expj(
                    unique(.N),
                    unique(ncases),
                    disease_wt
                  )]
                ),
                by = eval(strata)
              ]
              sp$pop[, (namprvl) := 0L]
              sp$pop[ss, on = c("pid", "year"), (namprvl) := 1L]
              sp$pop[, c("ncases", "disease_wt") := NULL]
            }

            print("set duration")
            dqset.seed(private$seed, stream = sp$mc * 10 + 2L) # not mc_aggr

            tbl <- read_fst(private$filenams$dur, as.data.table = TRUE)
            if ("qimd" %in% names(tbl)) {
              tbl <- tbl[qimd == 3][, qimd := NULL]
            }

            col_nam <- setdiff(names(tbl), intersect(names(sp$pop), names(tbl)))
            tbl[, (namprvl) := 1L]

            if (sp$pop[, max(age)] > tbl[, max(age)]) {
              tt1 <- tbl[age == max(age)]
              tt1 <- clone_dt(tt1, sp$pop[, max(age)] - tbl[, max(age)])
              tt1[, `:=`(age = age + .id, .id = NULL)]
              tbl <- rbind(tbl, tt1)
            }

            #lookup_dt(sp$pop, tbl, check_lookup_tbl_validity = FALSE)
            absorb_dt(sp$pop, tbl)

            fn <- paste0("q", self$meta$diagnosis$duration_distr)

            if (self$name %in% c("chd", "stroke")) {
              sp$pop[
                get(namprvl) == 1L,
                (namprvl) := 2L +
                  do.call(
                    fn,
                    c(
                      p = list(dqrunif(.N)),
                      mu = list(clamp(mu - 2, 0, Inf)),
                      sigma = list(sigma)
                    )
                  )
              ]
              sp$pop[get(namprvl) > age, (namprvl) := age]
              sp$pop[, (col_nam) := NULL]
            } else {
              stop(paste0("No duration defined for disease: ", self$name))
            }

            if (!is.null(self$meta$mortality$cure)) {
              sp$pop[
                get(namprvl) > self$meta$mortality$cure,
                (namprvl) := self$meta$mortality$cure
              ]
            }

            sp$pop[, (namprvl) := carry_backward_decr(get(namprvl), pid_mrk)] # necessary for c++
          } # End if incidence type not 0 or 1

          # TODO this only makes sense when probability of diagnosis is 1
          namdgns <- paste0(self$name, "_dgns")
          set(sp$pop, NULL, namdgns, 0L)
          sp$pop[
            get(namprvl) > 0 &
              year >= design_$sim_prm$init_year_long &
              dqrunif(.N) < self$meta$diagnosis$probability,
            (namdgns) := get(namprvl)
          ]
          sp$pop[, (namdgns) := carry_backward_decr(get(namdgns), pid_mrk)]
        }

        invisible(self)
      },

      # set_rr ----
      #' @description Set disease incidence probability in a new col in sp$pop.
      #' @param sp A synthetic population.
      #' @param design_ A design object with the simulation parameters.
      #' @param checkNAs If `TRUE`, prints the table of NAs before they get
      #'   overwritten with 1. Note that for some exposures, NAs are expected
      #'   for certain levels of exposure (i.e. for active days).
      #' @param forPARF Set TRUE when applied on the specialised forPARF
      #'   SynthPop
      #' @return The invisible self for chaining.

      set_rr = function(
        sp,
        design_ = design,
        checkNAs = design_$sim_prm$logs,
        forPARF = FALSE
      ) {
        # For incd type 1 forPARF = TRUE is meaningless but gen_parf() skips
        # this type so we are good here.

        if (!inherits(sp, "SynthPop")) {
          stop("Argument sp needs to be a SynthPop object.")
        }
        if (!inherits(design_, "Design")) {
          stop("Argument design_ needs to be a Design object.")
        }

        lapply(private$rr, function(x) {
          x$xps_to_rr(sp, design_, checkNAs = checkNAs, forPARF = forPARF)
        })

        if (!forPARF && length(private$rr) > 0) {
          sp$store_risks(self$name)
        }

        return(invisible(self))
      },

      #' @description Set disease incident probability in a new col in sp$pop.
      #' @param sp A synthetic population.
      #' @param design_ A design object with the simulation parameters.
      #' @return The invisible self for chaining.
      # set_incd_prb ----
      set_incd_prb = function(sp, design_ = design) {
        if (!inherits(design_, "Design")) {
          stop("Argument design_ needs to be a Design object.")
        }
        if (!inherits(sp, "SynthPop")) {
          stop("Argument sp needs to be a SynthPop object.")
        }

        if (
          is.numeric(self$meta$incidence$type) && self$meta$incidence$type > 0L
        ) {
          if (private$incd_colnam %in% names(sp$pop)) {
            stop(
              "A column named ",
              private$incd_colnam,
              " already exists in sp$pop. ",
              "Please delete it and run set_incd_prb() afterwards."
            )
          }

          # Get colnames in risk that end with _rr but exclude the influence by
          # diseases
          if (self$meta$incidence$type == 3) {
            # private$rr never NULL here but riskcolnam can be empty if disease
            # only influenced by other diseases but not exposures
            riskcolnam <- grep(
              paste0(
                "^((?!",
                paste(
                  self$meta$incidence$influenced_by_disease_name,
                  collapse = "|"
                ),
                ").)*_rr$"
              ),
              names(sp$get_risks(self$name)),
              value = TRUE,
              perl = TRUE
            )
          } else {
            # private$rr may be NULL here but I will cover this case below
            riskcolnam <- grep(
              "_rr$",
              names(sp$get_risks(self$name)),
              value = TRUE,
              perl = TRUE
            )
          }

          if (self$meta$incidence$type == 1L) {
            if (length(riskcolnam) == 1L) {
              thresh <- as.numeric(sp$get_risks(self$name)[[riskcolnam]])
            }
            if (
              length(riskcolnam) > 1L &&
                self$meta$incidence$aggregation == "any"
            ) {
              thresh <- as.numeric(sp$get_risks(self$name)[,
                do.call(pmax, .SD),
                .SDcols = riskcolnam
              ])
            }
            if (
              length(riskcolnam) > 1L &&
                self$meta$incidence$aggregation == "all"
            ) {
              thresh <- as.numeric(sp$get_risks(self$name)[,
                Reduce(`*`, .SD),
                .SDcols = riskcolnam
              ])
            }

            set(sp$pop, NULL, private$incd_colnam, thresh)
          } else if (length(private$rr) > 0L) {
            # if incidence$type not 1 and at least 1 associated RF
            if (length(riskcolnam) > 0) {
              risk_product <-
                sp$get_risks(self$name)[,
                  Reduce(`*`, .SD),
                  .SDcols = riskcolnam
                ]
            } else {
              risk_product <- 1
            }

            if (design_$sim_prm$init_year_incd_calibration) {
              # Calibrate estimated incidence prbl to init year incidence
              tbl <- self$get_incd(
                design_$sim_prm$init_year_long,
                mc_ = sp$mc_aggr,
                design_ = design_
              )[between(age, design_$sim_prm$ageL, design_$sim_prm$ageH)]
              #lookup_dt(sp$pop, tbl) #TODO: lookup_dt
              absorb_dt(sp$pop, tbl)
              sp$pop[,
                rp := (private$parf$p0 *
                  sp$get_risks(self$name)[,
                    Reduce(`*`, .SD),
                    .SDcols = patterns("_rr$")
                  ])
              ]
              setnafill(sp$pop, "c", 0, cols = c("rp", "mu"))
              # Above rp includes rr from diseases that risk_product doesn't have
              tbl <- sp$pop[
                year == design_$sim_prm$init_year &
                  get(paste0(self$name, "_prvl")) == 0L,
                .(clbfctr = sum(mu) / sum(rp)),
                keyby = .(sex)
              ] #STRATA?

              #lookup_dt(sp$pop, tbl) #TODO: lookup_dt
              absorb_dt(sp$pop, tbl)
              setnafill(sp$pop, "c", 1, cols = "clbfctr")

              # End of calibration

              set(
                sp$pop,
                NULL,
                private$incd_colnam,
                clamp(private$parf$p0 * risk_product * sp$pop$clbfctr)
              )
              # NOTE product above not expected to be equal to incidence because
              # p0 estimated using mean lags and RR, while each mc run samples
              # from their distribution.

              # setnames(sp$pop, "clbfctr", paste0(self$name, "_clbfctr"))
              # sp$pop[, (paste0(self$name, "_risk_product")) := risk_product]
              # sp$pop[, (paste0(self$name, "_p0")) := private$parf$p0]
              sp$pop[, c("mu", "rp", "clbfctr") := NULL]
            } else {
              set(
                sp$pop,
                NULL,
                private$incd_colnam,
                clamp(private$parf$p0 * risk_product)
              )
            }

            if (design_$sim_prm$export_PARF) {
              path <- file.path(design_$sim_prm$output_dir, "parf")
              filenam <- file.path(path, "parf.csv")
              if (!dir.exists(path)) {
                dir.create(path, showWarnings = FALSE, recursive = TRUE)
              }

              parf_dt <-
                cbind(
                  sp$pop[, .(wt_immrtl, age, sex, year)], #STRATA
                  "parf" = private$parf$parf
                )[year == design_$sim_prm$init_year]
              parf_dt <-
                parf_dt[
                  !is.na(parf),
                  .(parf = unique(parf), pop_size = sum(wt_immrtl)),
                  keyby = .(age, sex)
                ] #STRATA
              parf_dt[, `:=`(disease = self$name, mc = sp$mc)] # not sp$mc_aggr
              # EXAMPLE parf[, weighted.mean(parf, pop_size), keyby = sex]
              fwrite_safe(parf_dt, filenam)
            } # End export PARF
          } else {
            # End of incident$type not 1 and no associated RF
            tbl <- self$get_incd(
              seq(
                design_$sim_prm$init_year_long,
                design_$sim_prm$init_year_long +
                  design_$sim_prm$sim_horizon_max
              ),
              mc_ = sp$mc_aggr,
              design_ = design_
            )[between(age, design_$sim_prm$ageL, design_$sim_prm$ageH)]
            setnames(tbl, "mu", private$incd_colnam)
            #lookup_dt(sp$pop, tbl, check_lookup_tbl_validity = FALSE) #TODO: lookup_dt
            absorb_dt(sp$pop, tbl)
          } # End if no associated RF

          setnafill(sp$pop, "c", 0, cols = private$incd_colnam)
        } # End if incident$type numeric
        invisible(self)
      },

      #' @description Set diagnosis probability in a new col in sp$pop.
      #' @param sp A synthetic population.
      #' @param design_ A design object with the simulation parameters.
      #' @return The invisible self for chaining.

      # set_dgns_prb ----
      set_dgns_prb = function(sp, design_ = design) {
        if (
          is.numeric(self$meta$diagnosis$type) && self$meta$diagnosis$type > 0L
        ) {
          if (!inherits(sp, "SynthPop")) {
            stop("Argument sp needs to be a SynthPop object.")
          }
          if (!inherits(design_, "Design")) {
            stop("Argument design_ needs to be a Design object.")
          }

          if (private$dgns_colnam %in% names(sp$pop)) {
            stop(
              "A column named ",
              private$dgns_colnam,
              " already exists in sp$pop. ",
              "Please delete it and run set_dgns_prb() afterwards."
            )
          }

          set(
            sp$pop,
            NULL,
            private$dgns_colnam,
            self$meta$diagnosis$probability
          )
        }

        invisible(self)
      },

      #' @description Set disease case fatality when relevant, in a new col in
      #'   sp$pop.
      #' @param sp A synthetic population.
      #' @param design_ A design object with the simulation parameters.
      #' @return The invisible self for chaining.

      # set_mrtl_prb ----
      set_mrtl_prb = function(sp, design_ = design) {
        if (is.numeric(self$meta$mortality$type)) {
          if (!inherits(sp, "SynthPop")) {
            stop("Argument sp needs to be a SynthPop object.")
          }
          if (!inherits(design_, "Design")) {
            stop("Argument design_ needs to be a Design object.")
          }
          if (private$mrtl_colnam2 %in% names(sp$pop)) {
            stop("Column ", private$mrtl_colnam2, " exists already in sp$pop.")
          }

          # Note currently files ending with _ftlt are in practice mrtl. They
          # transforme to case ftlt with the multiplication of the calibration
          # factor
          ftlt <-
            self$get_ftlt(
              seq(
                design_$sim_prm$init_year_long,
                design_$sim_prm$init_year_long + design_$sim_prm$sim_horizon_max
              ),
              mc_ = sp$mc_aggr,
              design_ = design_
            )

          if (!"mu2" %in% names(ftlt)) {
            stop("mu2 need to be present in the ftlt file.")
          }

          # Deal with mrtl1 if present as it is unaffected by the logic below
          if ("mu1" %in% names(ftlt)) {
            private$mrtl2flag <- TRUE
            nam <- paste0("prb_", self$name, "_mrtl", 1)
            if (nam %in% names(sp$pop)) {
              stop("Column ", nam, " exists already in sp$pop.")
            }
            setnames(ftlt, "mu1", nam)
            #lookup_dt(sp$pop, ftlt[, .SD, .SDcols = !"mu2"],
            #          check_lookup_tbl_validity = FALSE) #TODO: lookup_dt
            absorb_dt(sp$pop, ftlt[, .SD, .SDcols = !"mu2"])
            setnafill(sp$pop, type = "const", fill = 0, cols = nam)
            ftlt[, (nam) := NULL]
          } else {
            private$mrtl2flag <- FALSE
          }

          # if (not apply_RR_to_mrtl2 and not depend on other diseases) or ----
          # length(private$rr) == 0L
          if (
            (!design_$sim_prm$apply_RR_to_mrtl2 &&
              !self$meta$mortality$type %in% 3:4) ||
              length(private$rr) == 0L
          ) {
            setnames(ftlt, "mu2", private$mrtl_colnam2)
            #lookup_dt(sp$pop, ftlt, check_lookup_tbl_validity = FALSE) #TODO: lookup_dt
            absorb_dt(sp$pop, ftlt)
          } else {
            if (self$meta$mortality$type %in% 3:4) {
              # private$rr never NULL here but riskcolnam can be empty if disease
              # only influenced by other diseases but not exposures
              riskcolnam <- grep(
                paste0(
                  "^((?!",
                  paste(
                    self$meta$mortality$influenced_by_disease_name,
                    collapse = "|"
                  ),
                  ").)*_rr$"
                ),
                names(sp$get_risks(self$name)),
                value = TRUE,
                perl = TRUE
              )
            } else {
              # private$rr may be NULL here but I will cover this case
              # below
              riskcolnam <- grep(
                "_rr$",
                names(sp$get_risks(self$name)),
                value = TRUE,
                perl = TRUE
              )
            }

            if (length(riskcolnam) > 0) {
              risk_product <-
                sp$get_risks(self$name)[,
                  Reduce(`*`, .SD),
                  .SDcols = riskcolnam
                ]
            } else {
              risk_product <- 1
            }

            set(
              sp$pop,
              NULL,
              private$mrtl_colnam2,
              clamp(private$parf$m0 * risk_product)
            )
          }

          setnafill(
            sp$pop,
            type = "const",
            fill = 0,
            cols = private$mrtl_colnam2
          )
        } # End numeric mortality type

        invisible(self)
      },

      #' @description Deletes the PARF file from disk.
      #' @param invert deletes all other disease relevant PARF file except those
      #'   that are associated to the current settings.
      #' @return The invisible self for chaining.

      del_parf_file = function(invert = FALSE) {
        stopifnot(is.logical(invert))

        if (invert) {
          parf_filenam2 <- list.files(
            private$parf_dir,
            pattern = paste0("^PARF_", self$name, ".*\\.fst$"),
            full.names = TRUE
          )

          parf_filenam <-
            setdiff(parf_filenam2, private$parf_filenam)

          if (any(file.exists(parf_filenam))) file.remove(parf_filenam)
        } else {
          # if not invert

          if (file.exists(private$parf_filenam)) {
            file.remove(private$parf_filenam)
          }
        }

        invisible(self)
      },

      #' @description Get disease incident probability.
      #' @param year_ A vector of years to return. All if missing.
      #' @param mc_ A scalar to realise the incidence probability. All if missing. The median for mc_ = 0
      #' @param design_ A design object.
      #' @return A data.table with disease incident probabilities unless
      #'   incidence type: Universal when it returns data.table(NULL).

      # get_incd ----
      get_incd = function(year_, mc_ = sp$mc_aggr, design_ = design) {
        if (!design_$sim_prm$incd_uncertainty_distr %in% c("beta", "uniform")) {
          stop("Argument distr need to be 'beta' or 'uniform'")
        }
        if (sum(dim(private$incd_indx)) > 0) {
          if (missing(year_) & missing(mc_)) {
            # Single year without MC
            ro <- list(from = 1, to = NULL)
          } else if (all(year_ %in% private$incd_indx[, year])) {
            ro <- private$incd_indx[
              year %in% sort(year_), # No sorting because only
              .("from" = min(from), "to" = max(to)) # single MC iteration at a time
            ]
          } else {
            stop(
              "Year for which incidence is to be estimated is not in index file!"
            )
          }
          out <- read_fst(
            private$filenams$incd,
            as.data.table = TRUE,
            from = ro$from,
            to = ro$to
          )[between(age, design_$sim_prm$ageL, design_$sim_prm$ageH)]
          out[mu > 1, mu := out[mu < 1, max(mu)]]
          out[mu_lower > 1, mu_lower := out[mu_lower < 1, max(mu_lower)]]
          out[mu_upper > 1, mu_upper := out[mu_upper < 1, max(mu_upper)]]

          if (!missing(mc_)) {
            if (design_$sim_prm$incd_uncertainty_distr == "uniform") {
              if (mc_ == 0L) {
                out[, `:=`(
                  mu = (mu_lower + mu_upper) / 2, # TODO update when use beta distr
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              } else {
                # if mc > 0 TODO stop if mc < 0
                set.seed(private$seed + mc_ * 10 + 4L)
                k <- runif(1)
                out[, `:=`(
                  mu = qunif(k, mu_lower, mu_upper),
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              }
            } else {
              # if distr == "beta"
              if (mc_ == 0L) {
                out[
                  mu != mu_upper,
                  `:=`(
                    mu = qbeta(0.5, shape1, shape2)
                  )
                ]
                out[, `:=`(
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              } else {
                # if mc > 0 TODO stop if mc < 0
                set.seed(private$seed + mc_ * 10 + 4L)
                k <- runif(1)
                out[
                  mu != mu_upper,
                  `:=`(
                    mu = qbeta(k, shape1, shape2)
                  )
                ]
                out[, `:=`(
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              }
            }
          } # end !missing(mc_)
          out[,
            c(
              "cause_name",
              "measure_name",
              "N",
              "N_lower",
              "N_upper",
              "shape1",
              "shape2"
            ) := NULL
          ]
        } else {
          message("Incidence type: ", self$meta$incidence$type)
          out <- data.table(NULL)
        }
        return(out)
      },

      #' @description Get disease duration distribution parameters.
      #' @param mc_ A scalar to realise the incidence probability. All if missing. The median for mc_ = 0
      #' @return A data.table with duration distribution parameters. unless
      #'   incidence type: Universal when it returns data.table(NULL).

      # get_dur ----
      get_dur = function(mc_ = sp$mc_aggr) {
        if (!is.null(private$filenams$dur)) {
          if (missing(mc_)) {
            ro <- list(from = 1, to = NULL)
          } else {
            ro <- private$dur_indx[
              mc %in% mc_, # No sorting because only single MC iteration at a time
              .("from" = min(from), "to" = max(to))
            ]
          }
          out <- read_fst(
            private$filenams$dur,
            as.data.table = TRUE,
            from = ro$from,
            to = ro$to
          )
          out[, mc := NULL]
        } else {
          message("Incidence type: ", self$meta$incidence$type)
          out <- data.table(NULL)
        }
        return(out)
      },

      #' @description Get disease prevalent probability.
      #' @param year_ A vector of years to return. All if missing.
      #' @param mc_ A scalar to realise the prevalence probability. All if missing. The median for mc_ = 0
      #' @param design_ A design object.
      #' @return A data.table with disease prevalent probabilities unless
      #'   incidence type: Universal when it returns data.table(NULL).

      # get_prvl ----
      get_prvl = function(year_, mc_ = sp$mc_aggr, design_ = design) {
        if (!design_$sim_prm$prvl_uncertainty_distr %in% c("beta", "uniform")) {
          stop("Argument distr need to be 'beta' or 'uniform'")
        }
        if (sum(dim(private$prvl_indx)) > 0) {
          if (missing(year_) & missing(mc_)) {
            ro <- list(from = 1, to = NULL)
          } else if (all(year_ %in% private$prvl_indx[, year])) {
            ro <- private$prvl_indx[
              year %in% sort(year_), # No sorting because only
              .("from" = min(from), "to" = max(to)) # single MC iteration at a time
            ]
          } else {
            stop(
              "Year for which prevalence is to be estimated is not in index file!"
            )
          }
          out <- read_fst(
            private$filenams$prvl,
            as.data.table = TRUE,
            from = ro$from,
            to = ro$to
          )[between(age, design_$sim_prm$ageL, design_$sim_prm$ageH)]
          out[mu > 1, mu := out[mu < 1, max(mu)]]
          out[mu_lower > 1, mu_lower := out[mu_lower < 1, max(mu_lower)]]
          out[mu_upper > 1, mu_upper := out[mu_upper < 1, max(mu_upper)]]

          if (!missing(mc_)) {
            if (design_$sim_prm$prvl_uncertainty_distr == "uniform") {
              if (mc_ == 0L) {
                out[, `:=`(
                  mu = prvl_mltp * (mu_lower + mu_upper) / 2,
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              } else {
                # if mc > 0 TODO stop if mc < 0
                set.seed(private$seed + mc_ * 10 + 5L)
                k <- runif(1)
                out[, `:=`(
                  mu = prvl_mltp * qunif(k, mu_lower, mu_upper),
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              }
            } else {
              # if distr == "beta"
              if (mc_ == 0L) {
                out[, `:=`(
                  mu = prvl_mltp * qbeta(0.5, shape1, shape2)
                )]
                out[, `:=`(
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              } else {
                # if mc > 0 TODO stop if mc < 0
                set.seed(private$seed + mc_ * 10 + 5L)
                k <- runif(1)
                out[, `:=`(
                  mu = prvl_mltp * qbeta(k, shape1, shape2)
                )]
                out[, `:=`(
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              }
            }
          } # end !missing(mc_)
          out[,
            c(
              "cause_name",
              "measure_name",
              "N",
              "N_lower",
              "N_upper",
              "shape1",
              "shape2",
              "prvl_mltp"
            ) := NULL
          ]
        } else {
          message("Incidence type: ", self$meta$incidence$type)
          out <- data.table(NULL)
        }
        return(out)
      },

      #' @description Get disease case fatality probability.
      #' @param year_ A vector of years to return. All if missing.
      #' @param mc_ A scalar to realise the incidence probability. All if missing. The median for mc_ = 0
      #' @param design_ A design object.
      #' @return A data.table with disease case fatality probabilities unless
      #' mortality type: Non-fatal when it returns data.table(NULL).

      # get_ftlt ----
      get_ftlt = function(year_, mc_ = sp$mc_aggr, design_ = design) {
        if (!design_$sim_prm$ftlt_uncertainty_distr %in% c("beta", "uniform")) {
          stop("Argument distr need to be 'beta' or 'uniform'")
        }
        if (sum(dim(private$ftlt_indx)) > 0) {
          if (missing(year_) & missing(mc_)) {
            ro <- list(from = 1, to = NULL)
          } else if (all(year_ %in% private$ftlt_indx[, year])) {
            ro <- private$ftlt_indx[
              year %in% sort(year_), # No sorting because only
              .("from" = min(from), "to" = max(to)) # single MC iteration at a time
            ]
          } else {
            stop(
              "Year for which case fatality is to be estimated is not in index file!"
            )
          }
          out <- read_fst(
            private$filenams$ftlt,
            as.data.table = TRUE,
            from = ro$from,
            to = ro$to
          )[between(age, design_$sim_prm$ageL, design_$sim_prm$ageH)]
          out[mu2 > 1, mu2 := out[mu2 < 1, max(mu)]]
          out[mu_lower > 1, mu_lower := out[mu_lower < 1, max(mu_lower)]]
          out[mu_upper > 1, mu_upper := out[mu_upper < 1, max(mu_upper)]]

          if (!missing(mc_)) {
            if (design_$sim_prm$ftlt_uncertainty_distr == "uniform") {
              if (mc_ == 0L) {
                out[, `:=`(
                  mu2 = (mu_lower + mu_upper) / 2,
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              } else {
                # if mc > 0 TODO stop if mc < 0
                set.seed(private$seed + mc_ * 10 + 6L)
                k <- runif(1)
                out[, `:=`(
                  mu2 = qunif(k, mu_lower, mu_upper),
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              }
            } else {
              # if distr == "beta"
              if (mc_ == 0L) {
                out[
                  mu2 != mu_upper,
                  `:=`(
                    mu2 = qbeta(0.5, shape1, shape2)
                  )
                ]
                out[, `:=`(
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              } else {
                # if mc > 0 TODO stop if mc < 0
                set.seed(private$seed + mc_ * 10 + 6L)
                k <- runif(1)
                out[
                  mu2 != mu_upper,
                  `:=`(
                    mu2 = qbeta(k, shape1, shape2)
                  )
                ]
                out[, `:=`(
                  mu_lower = NULL,
                  mu_upper = NULL
                )]
              }
            }
          } # end !missing(mc_)
          out[, c("cause_name", "measure_name", "shape1", "shape2") := NULL]
        } else {
          message("Mortality type: ", self$meta$mortality$type)
          out <- data.table(NULL)
        }
        return(out)
      },

      #' @description Get seed for RNG.
      #' @return A seed for the RNG that is produced by the digest of disease
      #'   name and outcome.

      # get_seed ----
      get_seed = function() {
        private$seed
      },

      #' @description Get the list of rr for all relevant exposures.
      #' @return A list of exposure objects.

      # get_rr ----
      get_rr = function() {
        private$rr
      },

      #' @description Deletes the stochastic effect files and indices from disk
      #'   for all relevant RR.
      #' @return The invisible self for chaining.

      # del_stochastic_effect ----
      del_stochastic_effect = function() {
        file.remove(private$filenam)
        file.remove(private$filenam_indx)
        lapply(private$rr, function(x) x$del_stochastic_effect)
        invisible(self)
      },

      #' @description Get the PARF by age/sex.
      #' @param what Columns to return (p0, m0, or parf)
      #' @return A data.table with PARF.

      # get_parf ----
      get_parf = function(what) {
        if (sum(dim(private$parf)) > 0L && !missing(what)) {
          private$parf[, ..what]
        } else {
          private$parf
        }
      },

      #' @description Get the PARF filename.
      #' @return A data.table with PARF.

      # get_parf_filename ----
      get_parf_filename = function() {
        private$parf_filenam
      },

      #' @description Harmonises classes and levels between the synthetic
      #'   population and the incidence/prevalence/fatality tables. It saves the
      #'   harmonised table to disk, overwriting the existing one.
      #' @param sp A synthetic population.
      #' @param verbose Logical. If TRUE, print the row number of the table that is processed to estimate shape1 and shape2.
      #' @return The invisible self for chaining.

      # harmonise_epi_tables ----
      harmonise_epi_tables = function(sp, verbose = FALSE) {
        if (!inherits(sp, "SynthPop")) {
          stop("Argument sp needs to be a SynthPop object.")
        }

        # TODO add logic to track file changes

        for (i in seq_along(private$filenams)) {
          if (grepl("_indx.fst$|_trend.fst|_dur.fst", private$filenams[[i]])) {
            next
          }
          print(private$filenams[[i]])
          cols <- metadata_fst(private$filenams[[i]])$columnNames
          # if ("mu_lower" %in% cols) {
          #   cols <- c("year", "age", "sex", "mu", "mu_lower", "mu_upper")
          # }
          tbl <- read_fst(
            private$filenams[[i]],
            as.data.table = TRUE,
            columns <- cols
          )[between(age, 30, 99)]
          val <- setdiff(names(tbl), names(sp$pop))
          com <- sort(intersect(names(tbl), names(sp$pop)))
          # Ensure year is always the first key if present
          com <- com[order(match(com, "year"))]

          # TODO add a property on yaml to recognise diseases apply to only one sex
          if (!"sex" %in% names(tbl) || uniqueN(tbl$sex) == 1L) {
            # NOTE not used for Japan and not updated
            tbl2 <- copy(tbl)
            if (self$name == "breast_ca") {
              tbl[, sex := factor("women", levels = c("men", "women"))]
              tbl2[, sex := factor("men", levels = c("men", "women"))]
            }

            if (self$name == "prostate_ca") {
              tbl[, sex := factor("men", levels = c("men", "women"))]
              tbl2[, sex := factor("women", levels = c("men", "women"))]
            }

            for (j in val) {
              set(tbl2, NULL, j, 0)
            }

            com <- c(com, "sex")
            tbl <- rbind(tbl, tbl2)
          }

          for (j in com) {
            if (
              inherits(sp$pop[[j]], "integer") &&
                !inherits(tbl[[j]], "integer")
            ) {
              tbl[, (j) := as.integer(get(j))]
            }
            if (
              inherits(sp$pop[[j]], "numeric") &&
                !inherits(tbl[[j]], "numeric")
            ) {
              tbl[, (j) := as.numeric(get(j))]
            }
            if (
              inherits(sp$pop[[j]], "character") &&
                !inherits(tbl[[j]], "character")
            ) {
              tbl[, (j) := as.character(get(j))]
            }
            if (inherits(sp$pop[[j]], "factor")) {
              # irrespective of class(j) to make sure that levels are the same
              # and in the right order.
              if (
                j == "dimd" &&
                  levels(tbl[[j]])[1] != "1 most deprived"
              ) {
                tbl[,
                  (j) := factor(
                    get(j),
                    levels = as.character(10:1),
                    labels = levels(sp$pop[[j]])
                  )
                ]
              } else {
                tbl[, (j) := factor(get(j), levels = levels(sp$pop[[j]]))]
              }
            }
          }

          if (
            grepl("_ftlt.fst$", private$filenams[[i]]) &&
              "mu" %in% names(tbl)
          ) {
            setnames(tbl, "mu", "mu2")
          }
          if (self$name %in% c("breast_ca", "prostate_ca") && anyNA(tbl)) {
            setnafill(tbl, "c", fill = 0, cols = val)
          }

          # If mc is in tbl then set "mc" as first key, else year is first key
          if ("mc" %in% names(tbl)) {
            setkeyv(tbl, c("mc", com))
          } else {
            setkeyv(tbl, com)
          }

          # Estimate the parameters of a beta distr
          if ("mu2" %in% names(tbl)) {
            setnames(tbl, "mu2", "mu")
            flag <- TRUE
          } else {
            flag <- FALSE
          }

          if (max(tbl$mu) > 1) {
            tbl[, `:=`(
              mu = mu / 1e5,
              mu_lower = mu_lower / 1e5,
              mu_upper = mu_upper / 1e5
            )]
          }
          if (!"shape1" %in% names(tbl)) {
            tbl[,
              c("shape1", "shape2") := private$fit_beta_vec(
                q = list(mu, mu_upper, mu_lower),
                p = c(0.5, 0.975, 0.025),
                tolerance = 0.01,
                verbose = verbose
              )
            ]
          } else {
            tbl[
              is.na(shape1) | is.na(shape2),
              c("shape1", "shape2") := private$fit_beta_vec(
                q = list(mu, mu_upper, mu_lower),
                p = c(0.5, 0.975, 0.025),
                tolerance = 0.01,
                verbose = verbose
              )
            ]
          }

          if (flag) {
            setnames(tbl, "mu", "mu2")
          }

          if (!is_valid_lookup_tbl(tbl, com)) {
            stop("Not a valid lookup table.")
          }
          # if (!self$name %in% c("breast_ca", "prostate_ca") &&  anyNA(tbl))
          if (anyNA(tbl)) {
            stop("NAs in ", private$filenams[[i]])
          }

          if (
            !identical(
              read_fst(private$filenams[[i]], as.data.table = TRUE),
              tbl
            )
          ) {
            write_fst(tbl, private$filenams[[i]])
          }
        } # End loop over relevant files
      },

      # to_cpp ----
      #' @description Returns a list to pass to the C++ side for Chris' parser.
      #' @param sp A synthetic population.
      #' @param design_ A design object with the simulation parameters.
      #' @param scenario_name A string with the scenario name. Currently is only
      #'   used when kismet == FALSE to generate new seeds for each scenario.
      #' @param scenario_suffix the suffix to identify columns from different
      #'   scenarios.
      #' @return A list.

      to_cpp = function(
        sp,
        design_ = design,
        scenario_name,
        scenario_suffix = ""
      ) {
        if (!inherits(sp, "SynthPop")) {
          stop("Argument sp needs to be a SynthPop object.")
        }
        if (!inherits(design_, "Design")) {
          stop("Argument design_ needs to be a Design object.")
        }

        out <- list()
        out <-
          list(
            "incidence" = NULL,
            "diagnosis" = NULL,
            "mortality" = NULL,
            "seed" = ifelse(
              design_$sim_prm$kismet, # if Kismet
              private$seed, # then seed the same for all scenarios
              abs(digest2int(
                # else new seed for each scenario
                paste0(self$name, scenario_name),
                seed = 230565490L
              ))
            )
          )

        out[["incidence"]] <- list(
          "type" = fifelse(
            is.numeric(self$meta$incidence$type),
            paste0("Type", self$meta$incidence$type),
            as.character(self$meta$incidence$type)
          ),
          "prevalence" = paste0(self$name, "_prvl", scenario_suffix),
          "probability" = paste0(private$incd_colnam, scenario_suffix),
          "can_recur" = self$meta$incidence$can_recur
        )
        if (is.null(out$incidence$can_recur)) {
          out$incidence <- within(out$incidence, rm("can_recur"))
        }
        if (out$incidence$type == "Universal") {
          out$incidence <- within(
            out$incidence,
            rm("prevalence", "probability")
          )
        }
        if (out$incidence$type == "Type0") {
          out$incidence <- within(out$incidence, rm("probability"))
        }

        # TODO resolve influenced by disease automatically from
        # paste0(names(design_$sim_prm$diseases), "_prvl") %in% private$rr

        if (self$meta$incidence$type == 0L) {
          influenced_by_incd <- list()

          for (i in self$meta$incidence$influenced_by_disease_name) {
            influenced_by_incd[[paste0(i, "_prvl")]] <- list("lag" = 0L)
          } # end for loop over influenced_by_disease_name
          out[["incidence"]][["influenced_by"]] <- influenced_by_incd
        } # end if incidence type 0

        if (self$meta$incidence$type == 3L) {
          influenced_by_incd <- list()

          for (i in self$meta$incidence$influenced_by_disease_name) {
            influenced_by_incd[[paste0(i, "_prvl")]] <-
              list(
                "multiplier" = paste0(self$name, "_incd_", i, "_prvl_mltp"),
                "lag" = private$rr[[paste0(
                  i,
                  "_prvl",
                  "~",
                  self$name
                )]]$get_lag(fifelse(design_$sim_prm$stochastic, sp$mc_aggr, 0L))
              )
          } # end for loop over influenced_by_disease_name
          out[["incidence"]][["influenced_by"]] <- influenced_by_incd
        } # end if incidence type 3

        if (!is.null(self$meta$diagnosis$type)) {
          out[["diagnosis"]] <- list(
            "type" = fifelse(
              is.numeric(self$meta$diagnosis$type),
              paste0("Type", self$meta$diagnosis$type),
              as.character(self$meta$diagnosis$type)
            ),
            "diagnosed" = paste0(self$name, "_dgns", scenario_suffix),
            "probability" = paste0(
              private$dgns_colnam,
              scenario_suffix
            ),
            "mm_wt" = self$meta$diagnosis$mm_wt
          )

          if (!is.null(self$meta$diagnosis$duration_distr_forwards)) {
            out[["diagnosis"]][["duration_distr_forwards"]] <-
              read_yaml(self$meta$diagnosis$duration_distr_forwards)
          }

          if (self$meta$diagnosis$type == 0L) {
            influenced_by_dgns <- list()

            for (i in self$meta$incidence$influenced_by_disease_name) {
              influenced_by_dgns[[paste0(i, "_dgns")]] <- list("lag" = 0L)
            } # end for loop over influenced_by_disease_name

            out[["diagnosis"]][["influenced_by"]] <- influenced_by_dgns

            out$diagnosis <- within(out$diagnosis, rm("probability"))
          }
        } else {
          out <- within(out, rm("diagnosis"))
        }

        if (!is.null(self$meta$mortality$type)) {
          out[["mortality"]] <- list(
            "type" = fifelse(
              is.numeric(self$meta$mortality$type),
              paste0("Type", self$meta$mortality$type),
              as.character(self$meta$mortality$type)
            ),
            "probability" = paste0(
              "prb_",
              self$name,
              "_mrtl2",
              scenario_suffix
            ),
            "code" = self$meta$mortality$code
          )

          if (!is.null(self$meta$mortality$cure)) {
            out[["mortality"]][["cure"]] <- self$meta$mortality$cure
          }

          if (private$mrtl2flag) {
            out[["mortality"]][["probability1styear"]] <-
              paste0("prb_", self$name, "_mrtl1", scenario_suffix)
          }

          if (self$meta$mortality$type %in% 3:4) {
            influenced_by_mrtl <- list()
            for (i in self$meta$mortality$influenced_by_disease_name) {
              influenced_by_mrtl[[paste0(i, "_prvl")]] <-
                list(
                  "multiplier" = paste0(self$name, "_mrtl_", i, "_prvl_mltp"),
                  "lag" = private$rr[[paste0(
                    i,
                    "_prvl",
                    "~",
                    self$name
                  )]]$get_lag(fifelse(
                    design_$sim_prm$stochastic,
                    sp$mc_aggr,
                    0L
                  ))
                )
            } # end for loop over influenced_by_disease_name
            out[["mortality"]][["influenced_by"]] <-
              influenced_by_mrtl
          } # end if mortality type 3
        } else {
          # end of not null mortality
          out <- within(out, rm("mortality"))
        } # end of null mortality
        out
      },

      #' @description Print the simulation parameters.
      #' @return The invisible self for chaining.
      print = function() {
        print(paste0("Disease name:       ", self$name))
        print(paste0("Meta incidence:     ", self$meta$incidence))
        print(paste0("Meta diagnosis:     ", self$meta$diagnosis))
        print(paste0("Meta mortality:     ", self$meta$mortality))
        print(paste0("Notes:              ", self$notes))

        invisible(self)
      }
    ), # end of public

    # private ------------------------------------------------------------------
    private = list(
      seed = NA_integer_,
      filenams = list(),
      mrtl2flag = FALSE, # TRUE if separate mortality for 1st year of disease is present
      incd_colnam = NA,
      dgns_colnam = NA,
      mrtl_colnam2 = NA,
      incd_indx = data.table(NULL),
      prvl_indx = data.table(NULL),
      ftlt_indx = data.table(NULL),
      p_zero_trend_indx = data.table(NULL),
      chksum = NA,
      parf_dir = NA,
      parf_filenam = NA,
      parf = data.table(NULL),
      sDiseaseBurdenDirPath = NA,
      rr = list(), # holds the list of relevant RR

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

      # gen_sp_forPARF ----
      gen_sp_forPARF = function(mc_, ff, design_, diseases_) {
        # recombine the chunks of large files
        # TODO logic to delete these files
        if (file.exists("./simulation/large_files_indx.csv")) {
          fl <- fread("./simulation/large_files_indx.csv")$pths
          for (i in 1:length(fl)) {
            if (file.exists(fl[i])) {
              next
            }
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

        dqRNGkind("pcg64")
        set.seed(private$seed + mc_)
        dqset.seed(private$seed, mc_)

        cm_mean <- as.matrix(
          read_fst(
            "./inputs/exposure_distributions/exposure_corr_mean.fst", # Change-for-IMPACT-NCD-Japan
            as.data.table = TRUE
          ),
          rownames = "rn"
        )
        r <-
          which(
            rownames(cm_mean) %in% c() # cols/rows to exclude i.e "Med_HT_r"
          )

        if (length(r) > 0 && r > 0) {
          rank_mtx <- generate_corr_unifs(nrow(ff), cm_mean[-r, -r])
        } else {
          rank_mtx <- generate_corr_unifs(nrow(ff), cm_mean)
        }

        # Restrict the range of some RNs to avoid unrealistic exposures
        # This scaling does not affect correlations
        # /0.999 because I multiplied all the columns below
        # rank_mtx <- rank_mtx * 0.999
        # rank_mtx[, "bmi_r"] <-
        #   rank_mtx[, "bmi_r"] * 0.90 / 0.999
        # rank_mtx[, "ssb_r"] <-
        #   rank_mtx[, "ssb_r"] * 0.95 / 0.999
        # rank_mtx[, "juice_r"] <-
        #   rank_mtx[, "juice_r"] * 0.95 / 0.999

        # sum((cor(rank_mtx) - cm_mean) ^ 2)

        rank_mtx <- data.table(rank_mtx)

        # NOTE rankstat_* is not needed here as we only alculate rank stats for 1 yr.
        ff[, paste0("rank_", gsub("_r$", "", colnames(rank_mtx))) := rank_mtx]

        rm(rank_mtx)

        rank_cols <- c("rank_Smoking_number")
        for (nam in rank_cols) {
          set(ff, NULL, nam, dqrunif(nrow(ff)))
        } # NOTE do not replace with generate_rns function.

        setkeyv(ff, c("year", "age", "sex")) #STRATA

        if (max(ff$age) > 90L) {
          ff[, age100 := age]
          ff[age > 90L, age := 90L]
        }

        xps_dep <- private$get_xps_dependency_tree(
          x = self$name,
          dssl = diseases_
        )
        xps_dep <- xps_dep[!grepl("_prvl$", xpscol)]
        setkey(xps_dep, xpscol)

        # below, for xps the first element is the exposure to be simulated and any other
        # entry are other exposures that require the first exposure to be simulated.
        # I.e., PA_days is the exposure to be simulated, and BMI depends directly to it
        # while HbA1c, LDLc and SBP depend indirectly through BMI.

        # Generate active days ----
        xps <- c("PA_days", "BMI", "HbA1c", "LDLc", "SBP")
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]
          ff[, trueyear := year]
          ff[age >= 70 & trueyear > 2025L, year := 2025L]
          ff[age >= 50 & sex == "men" & trueyear < 2010L, year := 2010L]
          ff[age >= 70 & sex == "women" & trueyear < 2015L, year := 2015L]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_PA_days.fst",
              as.data.table = TRUE
            )
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]
          nam <- intersect(names(ff), names(tbl))
          ff[
            tbl,
            on = nam,
            PA_days_curr_xps := factor(
              (rank_PA_days > pa0) +
                (rank_PA_days > pa1) +
                (rank_PA_days > pa2) +
                (rank_PA_days > pa3) +
                (rank_PA_days > pa4) +
                (rank_PA_days > pa5) +
                (rank_PA_days > pa6),
              levels = 0:7,
              labels = 0:7,
              ordered = TRUE
            )
          ]

          ff[, rank_PA_days := NULL]
          ff[, `:=`(year = trueyear, trueyear = NULL)]
          ff[, year := year + lag]
        }

        # Generate F&V ----
        xps <- "Fruit_vege"
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_Fruit_vege.fst",
              as.data.table = TRUE
            )
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))

          absorb_dt(ff, tbl)
          ff[, Fruit_vege_curr_xps := fqZINBI(rank_Fruit_vege, mu, sigma, nu)]
          ff[, c(col_nam, "rank_Fruit_vege") := NULL]
          ff[, year := year + lag]
        }

        xps <- c("Smoking", "Smoking_number", "SBP") #  "bmi", "sbp"
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps_dep[xps[1:2], ][!is.na(ds), any(duplicated(ds))]) {
            stop(
              "Smoking & Smoking_number cannot coexist as risk factors for a disease."
            )
          }

          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }

          ff[, year := year - lag] # need to be before tax_tabaco

          ff[,
            tax_tabaco := fcase(
              year < 2006L,
              0L,
              year >= 2006L & year < 2010L,
              1L,
              year >= 2010L & year < 2018L,
              2L,
              year >= 2018L,
              3L
            )
          ]
          ff[, tax_tabaco := factor(tax_tabaco, 0:3, 0:3)]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_Smoking_NevEx_vs_current.fst",
              as.data.table = TRUE
            )
          setnames(tbl, tolower(names(tbl)))
          tbl[, `:=`(sex = factor(sex, 0:1, c("men", "women")))]

          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))
          absorb_dt(ff, tbl)
          ff[, Smoking_curr_xps := as.integer(rank_Smoking_act < mu) * 2L] # 0 = never smoker or ex, 2 = current
          ff[, c(col_nam) := NULL]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_Smoking_never_vs_ex.fst",
              as.data.table = TRUE
            )
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]
          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))
          absorb_dt(ff, tbl)
          # range01 <- function(x) {
          #   if (length(x) > 1L) {
          #     (x - min(x)) / (max(x) - min(x))
          #   } else {
          #     x
          #   }
          # }
          ff[
            Smoking_curr_xps == 0L,
            Smoking_curr_xps := as.integer(rank_Smoking_ex < mu),
            by = .(year)
          ] # 0 = never smoker, 1=ex, 2=current

          ff[, Smoking_curr_xps := factor(Smoking_curr_xps + 1L)]

          ff[,
            c(
              col_nam,
              "tax_tabaco",
              "rank_Smoking_act",
              "rank_Smoking_ex"
            ) := NULL
          ]

          ff[, year := year + lag]
        }

        # Assign smok_cig_curr when pid_mrk == true (the first year an individual enters the simulation)
        xps <- c("Smoking_number")
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]
          set(ff, NULL, "Smoking_number", 0L)

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_Smoking_number.fst",
              as.data.table = TRUE
            )
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]
          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))

          absorb_dt(ff, tbl)

          ff[
            Smoking_curr_xps == 3L,
            Smoking_number_grp := (rank_Smoking_number > pa0) +
              (rank_Smoking_number > pa1) +
              (rank_Smoking_number > pa2) +
              (rank_Smoking_number > pa3) +
              (rank_Smoking_number > pa4) +
              (rank_Smoking_number > pa5) +
              (rank_Smoking_number > pa6) +
              (rank_Smoking_number > pa7)
          ]

          ff[Smoking_number_grp == 0L, Smoking_number_curr_xps := 5L]
          ff[Smoking_number_grp == 1L, Smoking_number_curr_xps := 10L]
          ff[Smoking_number_grp == 2L, Smoking_number_curr_xps := 15L]
          ff[Smoking_number_grp == 3L, Smoking_number_curr_xps := 20L]
          ff[Smoking_number_grp == 4L, Smoking_number_curr_xps := 25L]
          ff[Smoking_number_grp == 5L, Smoking_number_curr_xps := 30L]
          ff[Smoking_number_grp == 6L, Smoking_number_curr_xps := 35L]
          ff[Smoking_number_grp == 7L, Smoking_number_curr_xps := 40L]
          ff[
            Smoking_number_grp == 8L,
            Smoking_number_curr_xps := sample(
              c(50L, 60L, 80L),
              .N,
              TRUE,
              prob = c(0.4, 0.45, 0.15)
            )
          ]

          ff[, c(col_nam, "rank_Smoking_number", "Smoking_number_grp") := NULL]
          ff[, year := year + lag]
        }

        xps <- c("BMI", "HbA1c", "LDLc", "SBP")
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_BMI.fst",
              as.data.table = TRUE
            )
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

          ff[,
            PA_3cat := fifelse(
              PA_days_curr_xps %in% as.character(0:1),
              1L,
              fifelse(
                PA_days_curr_xps %in% as.character(2:4),
                2L,
                fifelse(
                  PA_days_curr_xps %in% as.character(5:7),
                  3L,
                  NA_integer_
                )
              )
            )
          ]
          ff[, PA_3cat := factor(PA_3cat)]

          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))
          absorb_dt(ff, tbl)
          ff[, BMI_curr_xps := qBCTo(rank_BMI, mu, sigma, nu, tau), ] #, n_cpu = design_$sim_prm$n_cpu)]
          ff[BMI_curr_xps < 10, BMI_curr_xps := 10] #Truncate BMI predictions to avoid unrealistic values.
          ff[BMI_curr_xps > 70, BMI_curr_xps := 70] #Truncate BMI predictions to avoid unrealistic values.

          ff[, c(col_nam, "rank_BMI", "PA_3cat") := NULL]
          ff[, year := year + lag]
        }

        xps <- c("Med_DM", "HbA1c")
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_Med_DM.fst",
              as.data.table = TRUE
            )
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))
          absorb_dt(ff, tbl)
          ff[, Med_DM_curr_xps := qbinom(rank_Med_DM, 1L, mu)] #, n_cpu = design_$sim_prm$n_cpu)]
          ff[, c(col_nam, "rank_Med_DM") := NULL]
          ff[, year := year + lag]
        }

        xps <- "HbA1c"
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_HbA1c.fst",
              as.data.table = TRUE
            )
          setnames(
            tbl,
            c("Age", "Sex", "Year", "BMI", "Med_DM"),
            c("age", "sex", "year", "BMI_round", "Med_DM_curr_xps")
          )
          tbl[, sex := factor(sex, 0:1, c("men", "women"))]
          tbl[, BMI_round := as.integer(10 * BMI_round)]
          ff[, BMI_round := as.integer(round(10 * BMI_curr_xps, 0))]
          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))

          absorb_dt(ff, tbl)
          ff[, HbA1c_curr_xps := fqBCT(rank_HbA1c, mu, sigma, nu, tau)] #, n_cpu = design_$sim_prm$n_cpu)]

          ff[, c(col_nam, "rank_HbA1c", "BMI_round") := NULL]
          ff[, year := year + lag]
        }

        xps <- c("Med_HL", "LDLc")
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_Med_HL.fst",
              as.data.table = TRUE
            )
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))
          absorb_dt(ff, tbl)
          ff[, Med_HL_curr_xps := qbinom(rank_Med_HL, 1L, mu)] #, n_cpu = design_$sim_prm$n_cpu)]
          ff[, c(col_nam, "rank_Med_HL") := NULL]
          ff[, year := year + lag]
        }

        xps <- "LDLc"
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_LDLc.fst",
              as.data.table = TRUE
            )
          setnames(
            tbl,
            c("Age", "Sex", "Year", "BMI", "Med_HL"),
            c("age", "sex", "year", "BMI_round", "Med_HL_curr_xps")
          )
          tbl[, sex := factor(sex, 0:1, c("men", "women"))]
          tbl[, BMI_round := as.integer(10 * BMI_round)]
          ff[, BMI_round := as.integer(round(10 * BMI_curr_xps, 0))]
          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))

          absorb_dt(ff, tbl)
          ff[, LDLc_curr_xps := fqBCT(rank_LDLc, mu, sigma, nu, tau)] #, n_cpu = design_$sim_prm$n_cpu)]

          ff[, c(col_nam, "rank_LDLc", "BMI_round") := NULL]
          ff[, year := year + lag]
        }

        xps <- c("Med_HT", "SBP")
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_Med_HT.fst",
              as.data.table = TRUE
            )
          setnames(tbl, tolower(names(tbl)))
          tbl[, sex := factor(sex, 0:1, c("men", "women")), ]

          col_nam <-
            setdiff(names(tbl), intersect(names(ff), names(tbl)))
          absorb_dt(ff, tbl)
          ff[, Med_HT_curr_xps := qbinom(rank_Med_HT, 1L, mu)] #, n_cpu = design_$sim_prm$n_cpu)]
          ff[, c(col_nam, "rank_Med_HT") := NULL]
          ff[, year := year + lag]
        }

        xps <- "SBP" #  depends on BMI Smoking Med_HT
        if (any(xps %in% xps_dep$xpscol)) {
          if (xps[[1]] %in% xps_dep$xpscol) {
            lag <- xps_dep[xps[[1]], max(lag)]
          } else {
            lag <- 0L
          }
          ff[, year := year - lag]

          tbl <-
            read_fst(
              "./inputs/exposure_distributions/Table_SBP.fst",
              as.data.table = TRUE
            )
          setnames(
            tbl,
            c("Age", "Sex", "Year", "BMI", "Smoking", "Med_HT"),
            c(
              "age",
              "sex",
              "year",
              "BMI_round",
              "smoking_tmp",
              "Med_HT_curr_xps"
            )
          )
          tbl[, `:=`(
            sex = factor(sex, 0:1, c("men", "women")),
            smoking_tmp = as.integer(smoking_tmp),
            BMI_round = as.integer(BMI_round)
          )] # TODO update the saved file so we don't have to do these slow conversions every time

          ff[, `:=`(
            BMI_round = as.integer(round(BMI_curr_xps)), # TODO consider Rfast::Round to speedup
            smoking_tmp = as.integer(Smoking_curr_xps == "3")
          )] # 1 = smoker

          col_nam <- setdiff(names(tbl), intersect(names(ff), names(tbl)))

          absorb_dt(ff, tbl)
          ff[, SBP_curr_xps := qBCPE(rank_SBP, mu, sigma, nu, tau)] #, n_cpu = design_$sim_prm$n_cpu)]

          ff[, c(col_nam, "rank_SBP", "BMI_round", "smoking_tmp") := NULL]
          ff[, year := year + lag]
        }

        nam <- grep("rank", names(ff), value = TRUE)
        if (length(nam) > 0) {
          ff[, (nam) := NULL]
        }

        if ("age100" %in% names(ff)) {
          ff[, age := NULL]
          setnames(ff, "age100", "age")
        }

        invisible(ff)
      },

      # fit_beta ----
      # initial idea from
      # https://stats.stackexchange.com/questions/112614/determining-beta-distribution-parameters-alpha-and-beta-from-two-arbitrary
      # attempts to fit a beta distribution to some percentiles. If it fails
      # progressively drop the percentiles from the tail to ease the fitting.
      # NOT VECTORISED
      fit_beta = function(
        x = c(0.01, 0.005, 0.5), # the values
        x_p = c(0.5, 0.025, 0.975), # the respective quantiles of the values
        tolerance = 0.01, # how close to get to the given values
        verbose = FALSE
      ) {
        if (length(x) != length(x_p)) {
          stop("x and x_p need to be of same length")
        }
        if (length(x) < 2L) {
          stop("x need to have at least length of 2")
        }
        if (length(unique(x)) == 1) {
          return(c(1, 1)) # early escape if all x the same
        }
        logit <- function(p) log(p / (1 - p))
        x_p_ <- logit(x_p)

        # Logistic transformation of the Beta CDF.
        f.beta <- function(alpha, beta, x, lower = 0, upper = 1) {
          p <- pbeta((x - lower) / (upper - lower), alpha, beta)
          log(p / (1 - p))
        }

        # Sums of squares.
        wts = c(1, rep(1, length(x) - 1L)) # start with equal importance for all values
        delta <- function(fit, actual, wts_) {
          sum((wts_ / sum(wts_)) * (fit - actual)^2)
        }

        # The objective function handles the transformed parameters `theta` and
        # uses `f.beta` and `delta` to fit the values and measure their discrepancies.
        objective <- function(theta, x, prob, wts_, ...) {
          ab <- exp(theta) # Parameters are the *logs* of alpha and beta
          fit <- f.beta(ab[1], ab[2], x, ...)
          return(delta(fit, prob, wts_))
        }
        # objective(start, x, x_p_)

        flag <- TRUE
        steptol_ <- 1e-6
        max_it <- 0L
        jump <- 2
        if (length(x) == 2) {
          start <- log(runif(2, c(1, 1), c(1e2, 1e6))) # A good guess is useful here
        } else {
          start <- log(private$fit_beta(x = x[c(1, 2)], x_p = x_p[c(1, 2)]))
        }

        while (flag && max_it < 1e4) {
          # sol <- optim(start, objective, x = x, prob = x_p_, method = "BFGS",
          #  lower = rep(-4, length(start)), upper = rep(4, length(start)),
          # control = list(trace = 5, fnscale = -1))

          sol <- tryCatch(
            {
              nlm(
                objective,
                start,
                x = x,
                prob = x_p_,
                wts_ = wts, # lower = 0, upper = 1,
                typsize = c(1, 1),
                fscale = 1e-12,
                gradtol = 1e-12,
                steptol = steptol_,
                iterlim = 5000
              )
            },
            error = function(e) list("estimate" = c(.5, .5), "code" = 5L)
          )

          # start <- start * runif(length(start), 1/jump, jump)
          start <- log(runif(2, c(0, 0), c(1e2, 1e6)))

          # summary(rbeta(1e6, runif(1e6, 0, 10), runif(1e6, 0, 1e6)))
          rel_error <- x /
            qbeta(x_p, exp(sol$estimate)[1], exp(sol$estimate)[2])

          if (verbose) {
            print(c(sol$code, rel_error, x))
          }
          flag <- (sol$code > 2L ||
            any(!between(rel_error, 1 - tolerance, 1 + tolerance)))
          if (is.na(flag)) {
            flag <- TRUE
          }
          max_it <- max_it + 1L
          if (max_it == 2000) {
            wts <- c(1, rep(0.9, length(x) - 1L))
          } # give even less importance to non 1st values
          if (max_it == 4000) {
            wts <- c(1, rep(0.8, length(x) - 1L))
          } # give even less importance to non 1st values
          if (max_it == 6000) {
            wts <- c(1, rep(0.7, length(x) - 1L))
          } # give even less importance to non 1st values
          if (max_it == 8000) {
            wts <- c(1, rep(0.6, length(x) - 1L))
          } # give even less importance to non 1st values
          # if (max_it == 450) steptol_ <- steptol_ * 10
          if (max_it == 9000) {
            #   print(max_it)
            wts <- c(1, rep(0.5, length(x) - 1L)) # give even less importance to non 1st values
            jump <- jump + 1
            if (length(x) == 2) {
              start <- log(runif(2, c(0, 0), c(1e3, 1e6))) # A good guess is useful here
            } else {
              start <- log(private$fit_beta(x = x[c(1, 3)], x_p = x_p[c(1, 3)]))
            }
          }
          if (max_it == 9000 && length(x) > 2) {
            if (verbose) {
              print("dropping last value")
            }
            start <- log(private$fit_beta(x = head(x, -1), x_p = head(x_p, -1)))
            x <- head(x, -1)
            x_p_ <- head(x_p_, -1)
            x_p <- head(x_p, -1)
            wts <- head(wts, -1)
            jump <- 2
            max_it <- 0
          }
        }
        if (sol$code < 3L && max_it < 1e4) {
          return(exp(sol$estimate)) # Estimates of alpha and beta
        } else {
          warning(c(
            sol$code,
            max_it,
            " Beta is not a good fit for these data!\n",
            x
          ))
          return(c(NA_real_, NA_real_))
        }
      },
      # fit_beta_vec ----
      # i.e use ttt[, c("shape1", "shape2") := fit_beta_vec(q = list(mu, mu_upper, mu_lower), p = c(0.5, 0.975, 0.025))]
      # VECTORISED
      fit_beta_vec = function(
        q = list(
          c(0.007248869, 0.0003693000),
          c(0.005198173, 0.0002744560),
          c(0.009516794, 0.0004751233)
        ),
        p = c(0.5, 0.025, 0.975),
        tolerance = 0.01,
        verbose = FALSE
      ) {
        if (length(unique(sapply(q, length))) != 1L) {
          stop("all elements in q need to be of same length")
        }
        out <- vector("list", length(q[[1]]))
        for (i in seq_len(length(q[[1]]))) {
          if (verbose) {
            print(i)
          }
          out[[i]] <- private$fit_beta(
            x = unlist(sapply(q, `[`, i)),
            x_p = p,
            tolerance = tolerance,
            verbose = verbose
          )
        }
        return(transpose(setDF(out)))
      },

      # helper function to get the tree of dependencies to exposures
      # x is a disease name string i.e. x = "other_ca"
      # diseases_ is a list of disease objects
      # TODO test that if (!i %in% out$ds)) allows interdependency (i.e. chd
      # causes t2dm and t2dm causes chd). Current approach will lead to an
      # infinite loop
      get_xps_dependency_tree = function(x = self$name, dssl = diseases_) {
        tr <- sapply(dssl[[x]]$get_rr(), `[[`, "name")
        if (length(tr) == 0L) {
          out <- data.table(
            xpscol = character(),
            lag = numeric(),
            ds = character()
          )
          return(out)
        }
        out <- data.table(
          xpscol = tr,
          lag = sapply(dssl[[x]]$get_rr(), `[[`, "lag"),
          ds = x
        )
        allds <-
          unique(
            c(
              dssl[[x]]$meta$incidence$influenced_by_disease_name,
              dssl[[x]]$meta$mortality$influenced_by_disease_name
            )
          )
        if (!is.null(allds)) {
          for (i in allds) {
            if (!i %in% out$ds) {
              out <- unique(rbind(
                out,
                private$get_xps_dependency_tree(i, dssl)
              ))
            }
          }
        }
        return(out)
      },

      # Fix given path so relative to current work directory.
      # @param sGivenPath string file path, perhaps from another computer with
      #   different root directories, e.g.
      #   /other/server/IMPACTncd_Japan/a/b/c/file.fst
      # @return string file path relevant for this PC's working dir, e.g.
      #   /this/PC/IMPACTncd_Japan/a/b/c/file.fst
      FixPathSoRelativeWorkDir = function(sGivenPath) {
        #return(sGivenPath)
        sProjectTopDir <- "IMPACTncd_Japan/"
        iPatternPos <- regexpr(pattern = sProjectTopDir, sGivenPath)[1]
        if (iPatternPos == -1) {
          return(sGivenPath)
        } else {
          #stop(paste0("Failed finding project top directory [",sProjectTopDir,"] in given file path: ",sGivenPath))
          sFileRelativePath <- substring(
            sGivenPath,
            iPatternPos + nchar(sProjectTopDir),
            nchar(sGivenPath)
          )
          return(file.path(getwd(), sFileRelativePath))
        }
      },

      # UpdateDiseaseSnapshotIfInvalid ----
      # Create or update snapshot of disease-related source files, as necessary.
      # Snapshot is updated on finding either no snapshot file, or added/deleted/changed source files.
      # Snapshot used subsequentially to detect source file changes, allowing dependent files to be updated.
      # @param bParfSnapshot bool, consider only PARF source files: '<diseaseName>_ftlt*.fst' and '<diseaseName>_incd*.fst'.
      # @param fnOnSnapshotChange function, action taken on snapshot update.
      # @return bool, a snapshot update occurred.
      UpdateDiseaseSnapshotIfInvalid = function(
        bParfSnapshot,
        fnOnSnapshotChange
      ) {
        sSnapshotFilePath <- file.path(
          private$sDiseaseBurdenDirPath,
          paste0(
            ".",
            self$name,
            "_file_snapshot",
            if (bParfSnapshot) "_parf" else "",
            ".qs"
          )
        )

        if (file.exists(sSnapshotFilePath)) {
          diseaseSnapShot <- qs_read(sSnapshotFilePath)
          diseaseSnapShot$path <- private$FixPathSoRelativeWorkDir(
            diseaseSnapShot$path
          )
          diseaseFileChanges <- changedFiles(diseaseSnapShot)
        } else {
          diseaseSnapShot <- NULL
        }

        if (
          is.null(diseaseSnapShot) ||
            any(
              nzchar(diseaseFileChanges$added),
              nzchar(diseaseFileChanges$deleted),
              nzchar(diseaseFileChanges$changed)
            )
        ) {
          fnOnSnapshotChange()
          if (!is.null(diseaseSnapShot)) {
            file.remove(sSnapshotFilePath)
          }
          sSourceFilesPattern <- if (bParfSnapshot) {
            paste0("^", self$name, "_incd|^", self$name, "_ftlt")
          } else {
            self$name
          }
          qs_save(
            fileSnapshot(
              private$sDiseaseBurdenDirPath,
              timestamp = NULL,
              md5sum = TRUE,
              recursive = FALSE,
              pattern = sSourceFilesPattern
            ),
            sSnapshotFilePath
          )
          return(TRUE)
        } else {
          return(FALSE)
        }
      },

      # from https://stackoverflow.com/questions/14324096/setting-seed-locally-not-globally-in-r
      # changes the seed temporarily
      with_random = function(expr, seed) {
        old <- .Random.seed # Assumes one exists. Make sure it does
        on.exit({
          .Random.seed <<- old
        })
        set.seed(seed)
        expr
      }
    ) # end of private
  )

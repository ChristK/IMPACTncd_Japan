# change L6 to L10 and remove "#" from L54 to L84 for IMPACT-NCD-JAPAN
source("./global.R")
design <- Design$new("./inputs/sim_design.yaml")
# RR ----
# Create a named list of Exposure objects for the files in ./inputs/RR
fl <- list.files(path = "./inputs/RR", pattern = ".csvy$", full.names = TRUE)
RR <- future_lapply(fl, Exposure$new, design, future.seed = 950480304L)
# RR <- vector("list", length(fl))
# for (i in seq_along(fl)) {
#     print(fl[i])
#     RR[[i]] <- Exposure$new(fl[i], design)
# }
names(RR) <- sapply(RR, function(x) x$get_name())
invisible(future_lapply(RR, function(x) {
    x$gen_stochastic_effect(design, overwrite = TRUE, smooth = FALSE)
},
future.seed = 627524136L))
# NOTE smooth cannot be exported to Design for now, because the first time
# this parameter changes we need logic to overwrite unsmoothed files
rm(fl)
#
# Generate diseases ----
diseases <- lapply(design$sim_prm$diseases, function(x) {
    x[["design_"]] <- design
    x[["RR"]] <- RR
    do.call(Disease$new, x)
})
names(diseases) <- sapply(design$sim_prm$diseases, `[[`, "name")

mk_scenario_init2 <- function(scenario_name, diseases_, sp, design_) {
    # scenario_suffix_for_pop <- paste0("_", scenario_name) # TODO get suffix from design
    scenario_suffix_for_pop <- scenario_name
    list(
        "exposures"          = design_$sim_prm$exposures,
        "scenarios"          = design_$sim_prm$scenarios, # to be generated programmatically
        "scenario"           = scenario_name,
        "kismet"             = design_$sim_prm$kismet, # If TRUE random numbers are the same for each scenario.
        "init_year"          = design_$sim_prm$init_year,
        "pids"               = "pid",
        "years"              = "year",
        "ages"               = "age",
        "ageL"               = design_$sim_prm$ageL,
        "all_cause_mrtl"     = paste0("all_cause_mrtl", scenario_suffix_for_pop),
        "cms_score"          = paste0("cms_score", scenario_suffix_for_pop),
        "cms_count"          = paste0("cms_count", scenario_suffix_for_pop),
        "strata_for_outputs" = c("pid", "year", "age", "sex"),
        "diseases"           = lapply(diseases_, function(x) x$to_cpp(sp, design_))
    )
}

# sim <- SynthPop$new(0L, design)
# sim$write_synthpop(1:500)
# sim$delete_synthpop(NULL)
# ll <- sim$gen_synthpop_demog(design)
sp  <- SynthPop$new(1L, design)

# lapply(diseases, function(x) x$harmonise_epi_tables(sp))

lapply(diseases, function(x) {
    print(x)
    x$gen_parf_files(design)
})
lapply(diseases, function(x) {
    print(x)
    x$gen_parf(sp, design, diseases)
})

lapply(diseases, function(x) {
    print(x)
    x$set_init_prvl(sp, design)
})

lapply(diseases, function(x) {
    print(x)
    x$set_rr(sp, design)
})

lapply(diseases, function(x) {
    print(x)
    x$set_incd_prb(sp, design)
})
lapply(diseases, function(x) {
    print(x)
    x$set_dgns_prb(sp, design)
})
lapply(diseases, function(x) {
    print(x)
    x$set_mrtl_prb(sp, design)
})


# old <- read_fst("/home/ckyprid/My_Models/IMPACTncd_Japan/backup_inputs_sep23/disease_burden/nonmodelled_ftlt.fst", as.data.table = T)
# new <- read_fst("/home/ckyprid/My_Models/IMPACTncd_Japan/inputs/disease_burden/nonmodelled_ftlt.fst", as.data.table = T)
# # new[, c("mu", "mu_lower", "mu_upper") := list(mu/1e5, mu_lower/1e5, mu_upper/1e5)]
# setnames(new, c("Rate", "Rate_lower", "Rate_upper"), c("mu2", "mu_lower", "mu_upper"))
# write_fst(new, "/home/ckyprid/My_Models/IMPACTncd_Japan/inputs/disease_burden/nonmodelled_ftlt.fst")

l <- mk_scenario_init2("", diseases, sp, design)
simcpp(sp$pop, l, sp$mc)

sp$update_pop_weights()
sp$pop[, mc := sp$mc_aggr]

# self <- IMPACTncd$.__enclos_env__$self
# private <- IMPACTncd$.__enclos_env__$private

self <- diseases$nonmodelled$.__enclos_env__$self
private <- diseases$nonmodelled$.__enclos_env__$private
design_ <- design
diseases_ <- diseases
popsize <- 100
check <- design_$sim_prm$logs
keep_intermediate_file <- TRUE
bUpdateExistingDiseaseSnapshot <- TRUE
mc_iter <- mc_ <- 1

ff <- self$get_ftlt(design_$sim_prm$init_year_long, design_ = design_)
ff <- CJ(
    age = seq(design_$sim_prm$ageL, design_$sim_prm$ageH),
    sex = ff$sex,
    year = design_$sim_prm$init_year_long,
    unique = TRUE
)

ff <- clone_dt(ff, 10, idcol = NULL)

# initial idea from https://stats.stackexchange.com/questions/112614/determining-beta-distribution-parameters-alpha-and-beta-from-two-arbitrary
fit_beta <- # NOT VECTORISED
    function(
     x = c(0.01, 0.005, 0.5), # the values
     x_p = c(0.5, 0.025, 0.975), # the respective quantiles of the values
     tolerance = 0.01, # how close to get to the given values
     verbose = FALSE
     ) {
        if (length(x) != length(x_p)) stop("x and x_p need to be of same length")
        if (length(x) < 2L) stop("x need to have at least length of 2")
        if (length(unique(x)) == 1) {
            return(c(1, 1)) # early escape ig all x the same
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
        delta <- function(fit, actual, wts_) sum((wts_/sum(wts_)) * (fit - actual)^2)

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
          start <- log(fit_beta(x = x[c(1, 2)], x_p = x_p[c(1, 2)]))
        }

        while (flag && max_it < 1e4) {
        # sol <- optim(start, objective, x = x, prob = x_p_, method = "BFGS",
        #  lower = rep(-4, length(start)), upper = rep(4, length(start)),
        # control = list(trace = 5, fnscale = -1))
         
        sol <- tryCatch({nlm(objective, start,
            x = x, prob = x_p_, wts_ = wts, # lower = 0, upper = 1,
            typsize = c(1, 1), fscale = 1e-12, gradtol = 1e-12, steptol = steptol_,
            iterlim = 5000
        )}, error = function(e) list("estimate" = c(.5, .5), "code" = 5L))


        # start <- start * runif(length(start), 1/jump, jump)
        start <- log(runif(2, c(0, 0), c(1e2, 1e6)))

        # summary(rbeta(1e6, runif(1e6, 0, 10), runif(1e6, 0, 1e6)))
        rel_error <- x / qbeta(x_p, exp(sol$estimate)[1], exp(sol$estimate)[2])

        # print(c(sol$code, rel_error, x))
        flag <- (sol$code > 2L || any(!between(rel_error, 1 - tolerance, 1 + tolerance)))
        if (is.na(flag)) flag <- TRUE
        max_it <- max_it + 1L
        if (max_it == 2000) wts <- c(1, rep(0.9, length(x) - 1L)) # give even less importance to non 1st values
        if (max_it == 4000) wts <- c(1, rep(0.8, length(x) - 1L)) # give even less importance to non 1st values
        if (max_it == 6000) wts <- c(1, rep(0.7, length(x) - 1L)) # give even less importance to non 1st values
        if (max_it == 8000) wts <- c(1, rep(0.6, length(x) - 1L)) # give even less importance to non 1st values
        # if (max_it == 450) steptol_ <- steptol_ * 10
        if (max_it == 9000) {
        #   print(max_it)
          wts <- c(1, rep(0.5, length(x) - 1L)) # give even less importance to non 1st values
          jump <- jump + 1
          if (length(x) == 2) {
            start <- log(runif(2, c(0, 0), c(1e3, 1e6))) # A good guess is useful here
          } else {
            start <- log(fit_beta(x = x[c(1, 3)], x_p = x_p[c(1, 3)]))
          }
        }
        if (max_it == 9000 && length(x) > 2) {
          if (verbose) print("dropping last value")
           start <- log(fit_beta(x = head(x, -1), x_p = head(x_p, -1)))
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
            warning(c(sol$code, max_it, " Beta is not a good fit for these data!\n", x))
            return(c(NA_real_, NA_real_))
        }
    }
parms <- fit_beta(x = c(0.01808766, 0.02387276, 0.01365250), x_p = c(0.5, 0.975, 0.025))
qbeta(c(0.5, 0.975, 0.025), parms[1], parms[2]) / c(0.01808766, 0.02387276, 0.01365250)

parms <- fit_beta(x = c(0.000153263819407615,0.00014409257851653), x_p = c(0.5, 0.025))
qbeta(c(0.5, 0.025), parms[1], parms[2]) / c(0.000153263819407615,0.00014409257851653)

fit_beta_vec <- # VECTORISED
    function(q = list(
                 c(0.007248869, 0.0003693000),
                 c(0.005198173, 0.0002744560),
                 c(0.009516794, 0.0004751233)
             ),
             p = c(0.5, 0.025, 0.975),
             tolerance = 0.01, 
             verbose = FALSE) {
        if (length(unique(sapply(q, length))) != 1L) stop("all elements in q need to be of same length")
        out <- vector("list", length(q[[1]]))
        for (i in seq_len(length(q[[1]]))) {
            if (verbose) print(i)
            out[[i]] <- fit_beta(x = unlist(sapply(q, `[`, i)), x_p = p, tolerance = tolerance, verbose = verbose)
        }
        return(transpose(setDF(out)))
    }

ttt <- read_fst("/home/ckyprid/My_Models/IMPACTncd_Japan/inputs/disease_burden/stroke_ftlt.fst",
    as.data.table = TRUE
)[between(age, 30, 99)]
anyNA(ttt)
ttt[, test := qbeta(0.5, shape1, shape2)]
ttt[mu2 != mu_upper, summary(mu2 / test)]
ttt[(mu2 / test) < 0.99, ]
ttt[, test := qbeta(0.975, shape1, shape2)]
ttt[mu2 != mu_upper, summary(mu_upper / test)]
ttt[, test := qbeta(0.025, shape1, shape2)]
ttt[mu2 != mu_upper, summary(mu_lower / test)]
summary(ttt$test)
summary(ttt$mu_lower)
ttt[is.na(shape1), ]
ttt[is.na(shape1), c("shape1", "shape2") := fit_beta_vec(q = list(mu2, mu_upper, mu_lower), p = c(0.5, 0.975, 0.025), tolerance = 0.01, verbose = T)]
ttt[, c("shape1", "shape2") := fit_beta_vec(q = list(mu2, mu_upper, mu_lower), p = c(0.5, 0.975, 0.025), tolerance = 0.01, verbose = T)]
ttt[, test := NULL]
# write_fst(ttt, "/home/ckyprid/My_Models/IMPACTncd_Japan/inputs/disease_burden/nonmodelled_ftlt.fst")



 lapply(diseases, function(x) {
     print(x)
     x$gen_parf_files(design)
 })
lapply(diseases, function(x) {
    print(x)
    x$gen_parf(sp, design, diseases)
})

lapply(diseases, function(x) {
    print(x)
    x$set_init_prvl(sp, design)
})

lapply(diseases, function(x) {
    print(x)
    x$set_rr(sp, design)
})

lapply(diseases, function(x) {
    print(x)
    x$set_incd_prb(sp, design)
})
lapply(diseases, function(x) {
    print(x)
    x$set_dgns_prb(sp, design)
})
lapply(diseases, function(x) {
    print(x)
    x$set_mrtl_prb(sp, design)
})
# # diseases$t2dm$harmonise_epi_tables(sp)
 # diseases$t2dm$gen_parf(sp, design)
 # diseases$t2dm$set_init_prvl(sp, design)
 # diseases$t2dm$set_rr(sp, design)
 # diseases$t2dm$set_incd_prb(sp, design)
 # diseases$t2dm$set_dgns_prb(sp, design)
 # diseases$t2dm$set_mrtl_prb(sp, design)
# #
# # # diseases$chd$harmonise_epi_tables(sp)
# diseases$chd$gen_parf_files(design, diseases)
# diseases$chd$set_init_prvl(sp, design)
# diseases$chd$set_rr(sp, design)
# diseases$chd$set_incd_prb(sp, design)
# diseases$chd$set_dgns_prb(sp, design)
# diseases$chd$set_mrtl_prb(sp, design)
#
# # diseases$stroke$harmonise_epi_tables(sp)
# diseases$stroke$gen_parf(sp, design)
# diseases$stroke$set_init_prvl(sp, design)
# diseases$stroke$set_rr(sp, design)
# diseases$stroke$set_incd_prb(sp, design)
# diseases$stroke$set_dgns_prb(sp, design)
# diseases$stroke$set_mrtl_prb(sp, design)
#
# diseases$obesity$gen_parf(sp, design)
# diseases$obesity$set_init_prvl(sp, design)
# diseases$obesity$set_rr(sp, design)
# diseases$obesity$set_incd_prb(sp, design)
# diseases$obesity$set_dgns_prb(sp, design)
# diseases$obesity$set_mrtl_prb(sp, design)
#
# #diseases$nonmodelled$harmonise_epi_tables(sp)
# diseases$nonmodelled$gen_parf(sp, design)
# diseases$nonmodelled$set_init_prvl(sp, design)
# diseases$nonmodelled$set_rr(sp, design)
# diseases$nonmodelled$set_incd_prb(sp, design)
# diseases$nonmodelled$set_dgns_prb(sp, design)
# diseases$nonmodelled$set_mrtl_prb(sp, design)

qsave(sp, "./simulation/tmp_s.qs")


lapply(diseases, function(x) {
    print(x$name)
    x$gen_parf(sp, design)$
    set_init_prvl(sp, design)$
    set_rr(sp, design)$
    set_incd_prb(sp, design)$
    set_dgns_prb(sp, design)$
    set_mrtl_prb(sp, design)
})

qsave(sp, "./simulation/tmp.qs")

transpose(sp$pop[, lapply(.SD, anyNA)], keep.names = "rn")[(V1)]


# qsave(sp, "./simulation/tmp.qs")
# sp <- qread("./simulation/tmp.qs")
l <- mk_scenario_init2("", diseases, sp, design)
simcpp(sp$pop, l, sp$mc)

sp$update_pop_weights()
sp$pop[, mc := sp$mc_aggr]

par(mfrow=c(2,2))
sp$pop[!is.na(all_cause_mrtl), sum(chd_prvl > 0)/.N, keyby = year][, plot(year, V1, type = "l", ylab = "CHD Prev.")]
sp$pop[!is.na(all_cause_mrtl), sum(stroke_prvl > 0)/.N, keyby = year][, plot(year, V1, type = "l", ylab = "Stroke Prev.")]
sp$pop[!is.na(all_cause_mrtl), sum(t2dm_prvl > 0)/.N, keyby = year][, plot(year, V1, type = "l", ylab = "T2DM Prev.")]
sp$pop[!is.na(all_cause_mrtl), sum(obesity_prvl > 0)/.N, keyby = year][, plot(year, V1, type = "l", ylab = "Obesity Prev.")]




# export xps
dt <- copy(sp$pop)
mc_ <- sp$mc_aggr
export_xps <- function(mc_,
                       dt,
                       write_to_disk = TRUE,
                       filenam = "val_xps_output.csv") {
    to_agegrp(dt, 20L, 99L, "age", "agegrp20", min_age = 30, to_factor = TRUE) # TODO link max age to design

    dt[, smok_never_curr_xps := fifelse(smok_status_curr_xps == "1", 1L, 0L)]
    dt[, smok_active_curr_xps := fifelse(smok_status_curr_xps == "4", 1L, 0L)]

    xps <- grep("_curr_xps$", names(dt), value = TRUE)
    xps <- xps[-which(xps %in% c("smok_status_curr_xps", "met_curr_xps",
                                 "bpmed_curr_xps", "t2dm_prvl_curr_xps",
                                 "af_prvl_curr_xps"))]
    out_xps <- groupingsets(
        dt[all_cause_mrtl >= 0L & year >= 13, ], # TODO link to design
        j = lapply(.SD, weighted.mean, wt),
        by = c("year", "sex", "agegrp20", "qimd", "ethnicity", "sha"),
        .SDcols = xps,
        sets = list(
            c("year", "sex", "agegrp20", "qimd"),
            c("year", "sex"),
            c("year", "agegrp20"),
            c("year", "qimd"),
            c("year", "ethnicity"),
            c("year", "sha")
        )
    )[, `:=` (year = year + 2000L, mc = mc_)]
    for (j in seq_len(ncol(out_xps)))
        set(out_xps, which(is.na(out_xps[[j]])), j, "All")
    dt[, c(
        "agegrp20",
        "smok_never_curr_xps",
        "smok_active_curr_xps"
    ) := NULL]

    setkey(out_xps, year)

    fwrite_safe(out_xps, output_dir(filenam))

    invisible(out_xps)
}






nam <- c("mc", "pid", "year", "sex", "dimd", "ethnicity", "sha", grep("_prvl$|_mrtl$", names(sp$pop), value = TRUE))
fwrite_safe(sp$pop[all_cause_mrtl >= 0L, ..nam],
            file.path(design$sim_prm$output_dir, "lifecourse", paste0(sp$mc_aggr, "_lifecourse.csv")))

parf <- fread("/mnt/storage_fast/output/hf_real/parf/parf.csv")

fl <- list.files("/mnt/storage_fast/output/hf_real/lifecourse/",
                 "_lifecourse.csv$", full.names = TRUE)

out <- rbindlist(lapply(fl, fread))

sp$pop[!is.na(all_cause_mrtl), median(bmi_curr_xps), keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), median(sbp_curr_xps), keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), weighted.mean(smok_status_curr_xps == "4", wt, na.rm), keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), mean(smok_cig_curr_xps), keyby = year][, plot(year, V1)]


sp$pop[!is.na(all_cause_mrtl), sum(chd_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(stroke_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(af_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(t2dm_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(obesity_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(htn_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(copd_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(lung_ca_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(breast_ca_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(colorect_ca_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(prostate_ca_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(hf_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(andep_prvl == 1)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(other_ca_prvl == 1)/.N, keyby = year][, plot(year, V1)]

sp$pop[, sum(all_cause_mrtl > 0, na.rm = T)/.N, keyby = year][, plot(year, V1)]
sp$pop[, sum(all_cause_mrtl == 12, na.rm = T)/.N, keyby = year][, plot(year, V1)]

sp$pop[, table(all_cause_mrtl, useNA = "a")]
sp$pop[year == 13, table(chd_prvl)]

sp$pop[!is.na(all_cause_mrtl), sum(chd_prvl > 0)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(stroke_prvl > 0)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(af_prvl > 0)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(t2dm_prvl > 0)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(obesity_prvl > 0)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(htn_prvl > 0)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(copd_prvl > 0)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(lung_ca_prvl > 0)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(hf_prvl > 0)/.N, keyby = year][, plot(year, V1)]
sp$pop[!is.na(all_cause_mrtl), sum(andep_prvl > 0)/.N, keyby = year][, plot(year, V1)]


sp$pop[between(age, 60, 64), sum(af_prvl > 0)/.N, keyby = year][, plot(year, V1)]

sp$pop[, sum(sbp_curr_xps > 140) / .N, keyby = year]



# Calibration ftlt ----
sp$pop[year == 2043, prop.table(table(Smoking_curr_xps))]
sp$pop[year == 2043 & age == 30, prop.table(table(Smoking_curr_xps))]
sp$pop[year == 2043 & age == 80, prop.table(table(Smoking_curr_xps))]

l <- mk_scenario_init2("", diseases, sp, design)
absorb_dt(sp$pop, ftlt)

for (year_ in 2013:2050) {
  de <- sp$pop[year == year_ & age >= 30, .(deaths = sum(wt_immrtl * mu2)), keyby = .(age, sex)] # deaths by age/sex
  ca <- sp$pop[year == year_ & age >= 30, .(cases = sum(wt_immrtl * (chd_prvl > 0))), keyby = .(age, sex)]
  absorb_dt(de, ca)
  de[, `:=` (prb_chd_mrtl2 = clamp(deaths/cases),
             year = year_,
             deaths = NULL,
             cases = NULL)]
  absorb_dt(sp$pop, de, exclude_col = "mu2")
  simcpp(sp$pop, l, sp$mc)
}
source("./global.R")

IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")

private <- IMPACTncd$.__enclos_env__$private
self <- IMPACTncd$.__enclos_env__$self  
mc = 1:2
mc_ = 1L
scenario_nam = ""
multicore = TRUE
scenario_fn <- function(sp) NULL

sp  <- SynthPop$new(1L, Design$new("./inputs/sim_design.yaml"))
 l <- IMPACTncd$.__enclos_env__$private$mk_scenario_init(sp, "")
        simcpp(sp$pop, l, sp$mc)

# g <- IMPACTncd$get_causal_structure(print_plot = TRUE)
# g <- IMPACTncd$get_causal_structure(processed = FALSE, print_plot = TRUE, focus = "chd")

# plot(igraph::make_ego_graph(g, order = 1, c("pain"), "in")[[1]])

source("./global.R")
IMPACTncd <- Simulation$new("./inputs/sim_design.yaml")
scenario_fn <- function(sp) NULL
IMPACTncd$
  del_logs()$
  del_outputs()$
  run(1:2, multicore = FALSE, "sc0")


x <- tryCatch(sqrt(5), error=function(e) {list(1, 2)})
x

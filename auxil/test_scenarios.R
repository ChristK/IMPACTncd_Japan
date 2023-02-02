
# p0 needs to be the same
# relative risks do not change after scenario?

sp <- qread("./simulation/tmp_s.qs")
sp2 <- qread("./simulation/tmp_s.qs")
setDT(sp$pop)
setDT(sp2$pop)

source("./auxil/scenarios.R")
scenario_fn <- scenario_3_fn
scenario_fn(sp)

#diseases_b <- diseases

lapply(diseases, function(x) {
  print(x$name)
  x$gen_parf(sp, design)$
    set_init_prvl(sp, design)$
    set_rr(sp, design)$
    set_incd_prb(sp, design)$
    set_dgns_prb(sp, design)$
    set_mrtl_prb(sp, design)
})

lapply(diseases, function(x) {
  print(x$name)
  x$gen_parf(sp2, design)$
    set_init_prvl(sp2, design)$
    set_rr(sp2, design)$
    set_incd_prb(sp2, design)$
    set_dgns_prb(sp2, design)$
    set_mrtl_prb(sp2, design)
})



all.equal(sp$pop$prb_t2dm_incd, sp2$pop$prb_t2dm_incd)
risk_sc = sp$get_risks()
risk_nosc = sp2$get_risks()

#c <- all.equal(risk_sc, risk_nosc)
#all.equal(a,b)
#all.equal(c,b)

test <- data.table(prb_sc = sp$pop$prb_t2dm_incd,
                   rp_sc = sp$pop$t2dm_risk_product,
                   clbf_sc = sp$pop$t2dm_clbfctr,
                   p0_sc = sp$pop$t2dm_p0,
                   sp$pop$pid,
                   sp$pop$bmi_curr_xps,
                   sp$pop$ssb_curr_xps,
                   prb_nosc = sp2$pop$prb_t2dm_incd,
                   rp_nosc = sp2$pop$t2dm_risk_product,
                   clbf_nosc = sp2$pop$t2dm_clbfctr,
                   p0_nonsc = sp2$pop$t2dm_p0,
                   sp2$pop$pid,
                   sp2$pop$bmi_curr_xps,
                   sp2$pop$ssb_curr_xps,
                   sp$pop$year,
                   bmi_rr_sc = risk_sc$t2dm$bmi_rr,
                   bmi_rr_nosc = risk_nosc$t2dm$bmi_rr,
                   ssb_rr_sc = risk_sc$t2dm$ssb_rr,
                   ssb_rr_nosc = risk_nosc$t2dm$ssb_rr)

test[, check := fifelse(prb_sc<prb_nosc, TRUE, FALSE)]
table(test$check)
test[, check := fifelse(bmi_rr_sc<bmi_rr_nosc, TRUE, fifelse(bmi_rr_sc==bmi_rr_nosc, TRUE, FALSE))]
table(test$check)
test[, check2 := ssb_rr_sc != ssb_rr_nosc]
table(test$check2)

test2 <- copy(test)

sum(sp2$pop$prb_t2dm_incd)
sum(sp$pop$prb_t2dm_incd)

sum(sp2$pop$prb_chd_incd)
sum(sp$pop$prb_chd_incd)

sum(sp2$pop$prb_stroke_incd)
sum(sp$pop$prb_stroke_incd)

l <- mk_scenario_init2("", diseases, sp, design)
l2 <- mk_scenario_init2("", diseases, sp2, design)

simcpp(sp$pop, l, sp$mc_aggr)
simcpp(sp2$pop, l2, sp2$mc_aggr)

sp$pop[t2dm_prvl == 1, .N]
sp2$pop[t2dm_prvl == 1, .N]

risks_sp <- sp$get_risks()
risks_sp2 <- sp2$get_risks()

all.equal(risks_sp$t2dm$bmi_rr, risks_sp2$t2dm$bmi_rr)
all.equal(risks_sp$t2dm$ssb_rr, risks_sp2$t2dm$ssb_rr)

head(test[check == TRUE], 100)




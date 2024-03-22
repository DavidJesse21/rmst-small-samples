# Setup ----

options(box.path = "R")

box::use(
  data.table[...],
  ggplot2[...],
  survival[Surv, survfit, survdiff, coxph, cox_zph = cox.zph],
  eventglm[rmeanglm],
  withr[with_seed],
  fs,
  ggsurvfit[survfit2, ggsurvfit, add_censor_mark]
)

box::use(
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  rmst/pseudo[pseudo_strat, pseudo_infjack, rmst_pseudo_test, rmst_pseudo_boot_test]
)


dt = survival::gbsg
setDT(dt)
dt

ggsurvfit(
  survfit2(Surv(rfstime, status) ~ hormon, data = dt),
  linewidth = 1.1
)

x = cox_zph(coxph(Surv(rfstime, status) ~ hormon, data = dt, x = TRUE))

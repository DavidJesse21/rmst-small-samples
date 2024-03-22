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


dt = survival::udca1
setDT(dt)

ggsurvfit(
  survfit2(Surv(futime, status) ~ trt, data = dt),
  linewidth = 1.1
)

x = cox_zph(coxph(Surv(futime, status) ~ trt, data = dt, x = TRUE))
pval_gt = x$table[1, "p"]
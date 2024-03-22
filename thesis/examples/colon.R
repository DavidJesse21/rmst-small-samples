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

dt = survival::colon
setDT(dt)
dt[, study := NULL]
help("colon", package = "survival")

dt[, table(rx)]


# EDA ----

## Original data + all treatments ----

ggsurvfit(
  survfit2(Surv(time, status) ~ rx, data = dt),
  linewidth = 1.1
)

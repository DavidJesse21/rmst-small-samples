options(box.path = "R")

box::use(
  survival[Surv],
  eventglm[rmeanglm],
  data.table[...]
)

box::use(
  rmst/km[rmst_diff_test],
  rmst/pseudo[rmst_pseudo_test]
)

dt = survival::veteran
setDT(dt)
dt[, trt := as.factor(trt)]

rmst_diff_test(
  Surv(time, status) ~ trt, data = dt,
  cutoff = 400, contrast = c("2", "1")
)

m = rmeanglm(
  Surv(time, status) ~ trt, data = dt, time = 400,
  model.censoring = "stratified", formula.censoring = ~ trt
)
rmst_pseudo_test(m)
# We can use any estimator for the covariance provided by `sandwich::vcovHC()`
rmst_pseudo_test(m, vcov_type = "const")

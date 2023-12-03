options(box.path = "R")

box::use(
  data.table[...],
  survival[Surv],
  survRM2[rmst2]
)
box::use(
  rmst/km[rmst, rmst_diff, rmst_diff_test]
)
box::help(rmst)

dt = survival::veteran
setDT(dt)

# Basic functionality: rmst()
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 400)
rmst(Surv(dt$time, dt$status) ~ 1, cutoff = 400)
rmst(Surv(time, status) ~ trt, data = dt, cutoff = 400)

# Basic functionality: rmst_diff()
rmst_diff(Surv(time, status) ~ trt, data = dt, cutoff = 400, contrast = c("1", "2"))
rmst_diff(Surv(time, status) ~ trt, data = dt, cutoff = 400, contrast = c("2", "1"))
rmst_diff(Surv(dt$time, dt$status) ~ dt$trt, cutoff = 400, contrast = c("2", "1"))

# Basic functionality: rmst_diff_test()
rmst_diff_test(Surv(time, status) ~ trt, data = dt, cutoff = 400, contrast = c("2", "1"))

# Compare with `rmst2()` to check if own implementation is correct
rmst2(dt$time, dt$status, dt$trt - 1, tau = 400)

# Different variance estimation method (should range from smallest to largest)
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 400, var_method = "nelson_aalen")
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 400, var_method = "greenwood")
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 400, var_method = "kaplan_meier")

# Inestimable cases
dt = dt[time < 600]
setorder(dt, time)
dt[.N, status := 0L]

rmst_diff_test(
  Surv(time, status) ~ trt, data = dt, cutoff = 600,
  contrast = c("2", "1"), inest_action = "error"
)
rmst_diff_test(
  Surv(time, status) ~ trt, data = dt, cutoff = 600,
  contrast = c("2", "1"), inest_action = "extend"
)
rmst_diff_test(
  Surv(time, status) ~ trt, data = dt, cutoff = 600,
  contrast = c("2", "1"), inest_action = "extend_warn"
)


options(box.path = "R")

box::use(
  data.table[...],
  survival[Surv],
  survRM2[rmst2]
)
box::use(
  rmst/km2[rmst, rmst_diff, rmst_diff_test]
)
box::help(rmst)

dt = survival::veteran
setDT(dt)

# Test basic functionality
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 400)
rmst(Surv(dt$time, dt$status) ~ 1, cutoff = 400)
rmst(Surv(time, status) ~ trt, data = dt, cutoff = 400)

rmst_diff(Surv(time, status) ~ trt, data = dt, cutoff = 400, contrast = c("1", "2"))
rmst_diff(Surv(time, status) ~ trt, data = dt, cutoff = 400, contrast = c("2", "1"))

# Compare with `rmst2()` to check if own implementation is correct
rmst2(dt$time, dt$status, dt$trt - 1, tau = 400)

# Different variance estimation method (should range from smallest to largest)
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 400, var_method = "nelson_aalen")
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 400, var_method = "greenwood")
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 400, var_method = "kaplan_meier")

# Testing function
rmst_diff_test(Surv(time, status) ~ trt, data = dt, cutoff = 400, contrast = c("2", "1"))
rmst2(dt$time, dt$status, dt$trt - 1, tau = 400)

# Test case in which RMST should be inestimable
# dt = dt[order(time)][time <= 600]
# dt[.N, status := 0]
# 
# rmst(Surv(time, status) ~ 1, data = dt, cutoff = 600)
# rmst(Surv(time, status) ~ trt, data = dt, cutoff = 600)
# rmst(Surv(time, status) ~ trt, data = dt, cutoff = 600, inest = "warning")
# rmst(Surv(time, status) ~ trt, data = dt, cutoff = 600, inest = "ignore")
# 
# rmst_diff(
#   Surv(time, status) ~ trt, data = dt, cutoff = 600, contrast = c("1", "2"),
#   inest = "warning"
# )
# rmst_diff_test(
#   Surv(time, status) ~ trt, data = dt, cutoff = 600, contrast = c("1", "2"),
#   inest = "warning"
# )

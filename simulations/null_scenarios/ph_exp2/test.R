options(box.path = "R")

box::use(
  data.table[...],
  simnph = SimNPH,
  survival[Surv, survfit],
  flexsurv[flexsurvspline]
)

design = data.table(
  lambda = simnph$m2r(12),
  lambda_cens = simnph$m2r(48),
  num_samples = 25
)

# Data generating function
gen_data = function(params) {
  dt = data.table(
    trt = rep(c("trt", "ctrl"), each = params$num_samples),
    time = round(rexp(params$num_samples * 2, params$lambda)),
    time_cens = round(rexp(params$num_samples * 2, params$lambda_cens))
  )
  
  dt[, status := fifelse(time <= time_cens, 1L, 0L)][
    , time := fifelse(status == 1L, time, time_cens)
  ][, time_cens := NULL]
  
  return(dt[])
}


# Analysisis functions ----

set.seed(42)
dt = gen_data(design)

fit_km = survfit(Surv(time, status) ~ trt, data = dt, se.fit = FALSE)
plot(fit_km, col = c("blue", "red"))

fit1_ctrl = flexsurvspline(Surv(time, status) ~ 1, data = dt[trt == "ctrl"], k = 1)
fit1_trt = flexsurvspline(Surv(time, status) ~ 1, data = dt[trt == "trt"], k = 1)
plot(fit_km, col = c("blue", "red"))
plot(fit1_ctrl, add = TRUE, col = "blue", B = 0)
plot(fit1_trt, add = TRUE, col = "red", B = 0)

#
data(Ex6crossing, package = "simtrial")
dt = Ex6crossing
setDT(dt)

fit_km = survfit(Surv(month, evntd) ~ trt, data = dt)
#plot(fit_km, col = c("blue", "red"))

fit1_0 = flexsurvspline(Surv(month, evntd) ~ 1, data = dt[trt == 0], k = 1)
fit1_1 = flexsurvspline(Surv(month, evntd) ~ 1, data = dt[trt == 1], k = 1)
plot(fit_km, col = c("blue", "red"))
plot(fit1_0, add = TRUE, col = "blue", B = 0)
plot(fit1_1, add = TRUE, col = "red", B = 0)

fit2_0 = flexsurvspline(Surv(month, evntd) ~ 1, data = dt[trt == 0], k = 2)
fit2_1 = flexsurvspline(Surv(month, evntd) ~ 1, data = dt[trt == 1], k = 2)
plot(fit_km, col = c("blue", "red"))
plot(fit2_0, add = TRUE, col = "blue", B = 0)
plot(fit2_1, add = TRUE, col = "red", B = 0)

fit3_0 = flexsurvspline(Surv(month, evntd) ~ 1, data = dt[trt == 0], k = 3)
fit3_1 = flexsurvspline(Surv(month, evntd) ~ 1, data = dt[trt == 1], k = 3)
plot(fit_km, col = c("blue", "red"))
plot(fit3_0, add = TRUE, col = "blue", B = 0)
plot(fit3_1, add = TRUE, col = "red", B = 0)


summary(fit2_0, type = "rmst", t = 20, se = TRUE)

box::use(flexsurv[standsurv])

standsurv(fit2_0, type = "rmst", t = 20, se = TRUE, boot = FALSE)
standsurv(fit2_0, type = "rmst", t = 20, se = TRUE, boot = TRUE, B = 1000)



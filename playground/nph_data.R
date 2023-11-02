# renv::install("Merck/simtrial")

box::use(
  data.table[...],
  survival[Surv, survfit],
  flexsurv[flexsurvreg]
)


fit_and_plot = function(data) {
  km = survfit(Surv(month, evntd) ~ trt, data = data)
  fit0 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 0], dist = "gengamma")
  fit1 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 1], dist = "gengamma")
  
  plot(km, col = c("blue", "red"))
  plot(fit0, ci = FALSE, col = "blue", add = TRUE)
  plot(fit1, ci = FALSE, col = "red", add = TRUE)
}


# Example 1 ----
data(Ex1delayedEffect, package = "simtrial")
dt = Ex1delayedEffect
setDT(dt)
fit_and_plot(dt)

# Example 2 ----
data(Ex2delayedEffect, package = "simtrial")
dt = Ex2delayedEffect
setDT(dt)
fit_and_plot(dt)

# Example 3 ----
data(Ex3curewithph, package = "simtrial")
dt = Ex3curewithph
setDT(dt)
fit_and_plot(dt)

# Example 4 ----
data(Ex4belly, package = "simtrial")
dt = Ex4belly
setDT(dt)
fit_and_plot(dt)

# Example 5 ----
data(Ex5widening, package = "simtrial")
dt = Ex5widening
setDT(dt)
fit_and_plot(dt)

# Example 6 ----
data(Ex6crossing, package = "simtrial")
dt = Ex6crossing
setDT(dt)
fit_and_plot(dt)

# Example 7 ----
data(MBdelayed, package = "simtrial")
dt = MBdelayed
setDT(dt)
# fit_and_plot(dt)
km = survfit(Surv(tte, event) ~ treatment, data = dt)
fit0 = flexsurvreg(Surv(tte, event) ~ 1, data = dt[treatment == "control"], dist = "gengamma")
fit1 = flexsurvreg(Surv(tte, event) ~ 1, data = dt[treatment == "experimental"], dist = "gengamma")
plot(km, col = c("blue", "red"))
plot(fit0, ci = FALSE, col = "blue", add = TRUE)
plot(fit1, ci = FALSE, col = "red", add = TRUE)

fit0$coefficients
do.call(flexsurv::mean_gengamma, as.list(fit1$coefficients))
fit0$cov

flexsurv::mean_gengamma()


mean(fit0)

fit = flexsurvreg(Surv(month, evntd) ~ trt, data = dt, dist = "gengamma")
fit
plot(fit)
fit$dlist
fit$coefficients
coef(fit)
class(fit)
?plot.flexsurvreg

plot(fit, ci = FALSE)

fit1 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 1], dist = "gengamma")
fit0 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 0], dist = "gengamma")

fit_km = survfit(Surv(month, evntd) ~ trt, data = dt)

?plot.survfit
plot(fit_km, col = c("blue", "red"))
plot(fit0, ci = FALSE, col = "blue", add = TRUE)
plot(fit1, ci = FALSE, col = "red", add = TRUE)

plot(fit_km, col = c("blue", "red"))

fit_km1 = survfit(Surv(month, evntd) ~ 1, data = dt[trt == 1])

fit1 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 1], dist = "gompertz")
plot(fit_km1)
plot(fit1, ci = FALSE, add = TRUE)

fit1 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 1], dist = "gamma")
plot(fit_km1)
plot(fit1, ci = FALSE, add = TRUE)

fit1 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 1], dist = "genf")
plot(fit_km1)
plot(fit1, ci = FALSE, add = TRUE)


fit1 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 1], dist = "weibull")
plot(fit_km1)
plot(fit1, ci = FALSE, add = TRUE)

fit1 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 1], dist = "lnorm")
plot(fit_km1)
plot(fit1, ci = FALSE, add = TRUE)

fit1 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 1], dist = "exp")
plot(fit_km1)
plot(fit1, ci = FALSE, add = TRUE)

fit1 = flexsurvreg(Surv(month, evntd) ~ 1, data = dt[trt == 1], dist = "llogis")
plot(fit_km1)
plot(fit1, ci = FALSE, add = TRUE)

box::use(
  flexsurv[flexsurvspline]
)

fit1 = flexsurvspline(Surv(month, evntd) ~ 1, data = dt[trt == 1], k = 7)
plot(fit_km1)
plot(fit1, ci = FALSE, add = TRUE)
fit1
names(fit1)

x = predict(fit1, type = "rmst", times = Inf, newdata = data.table(month = 0), se.fit = TRUE)
x


















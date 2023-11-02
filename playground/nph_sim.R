options(box.path = "R")

box::use(
  data.table[...],
  survival[Surv, survfit],
  flexsurv[flexsurvreg, flexsurvspline],
  simnph = SimNPH,
  survminer[ggsurvplot]
)

box::use(
  jointplot = plotting/joint,
  survplot = plotting/surv,
  ggplot2[...]
)

# Crossing hazards ----

dt = simnph$assumptions_crossing_hazards()


# Simulation setup
design = data.table(
  # Median survival control: 12 months
  hazard_ctrl = simnph$m2r(12),
  # Treatment arm, leading to same RMST at 1096 days
  hazard_trt_before = simnph$m2r(9),
  hazard_trt_after = simnph$m2r(24),
  crossing = 326.8855,
  # ~20% censoring in control arm
  random_withdrawal = simnph$m2r(48),
  # Different sample sizes per arm
  n_trt = 50,
  n_ctrl = 50
)

dt = simnph$generate_crossing_hazards(design) |>
  simnph$random_censoring_exp(design$random_withdrawal)
setDT(dt)
dt[, evt := as.integer(evt)]

# First plot real survival curves
survplot$plot_surv_crossing(
  design$hazard_ctrl, design$hazard_trt_before, design$hazard_trt_after, design$crossing
) +
  xlim(c(0, 2000))

fit = survfit(Surv(t, evt) ~ trt, data = dt)
plot(fit, col = c("blue", "red"))

fit0 = flexsurvreg(Surv(t, evt) ~ 1, data = dt[trt == 0], dist = "gengamma")
fit1 = flexsurvreg(Surv(t, evt) ~ 1, data = dt[trt == 1], dist = "gengamma")

plot(fit0, col = "blue", add = TRUE, ci = FALSE)
plot(fit1, col = "red", add = TRUE, ci = FALSE)

?predict.flexsurvreg
predict(fit0, newdata = data.table(t = 0), type = "mean", se.fit = TRUE)
predict(fit0, newdata = data.table(t = 0), type = "rmst", se.fit = TRUE)

fit3 = flexsurvreg(Surv(t, evt) ~ trt, data = dt, dist = "gengamma")
predict(fit3, type = "rmst", newdata = data.table(trt = c(0, 1)))
plot(fit3, newdata = data.table(trt = c(0, 1)))


box::use(
  jointplot = plotting/joint,
  survplot = plotting/surv,
  ggplot2[...]
)

p = jointplot$plot_joint_crossing(
  design$hazard_ctrl, design$hazard_trt_before, design$hazard_trt_after, design$crossing,
  align = "column"
) &
  xlim(c(0, 5000))

p = survplot$plot_surv_crossing(
  design$hazard_ctrl, design$hazard_trt_before, design$hazard_trt_after, design$crossing
) +
  xlim(c(0, 5000))
p

x0 = predict(fit0, type = "survival", times = seq(0, 5000, by = 10), newdata = data.table(t = 1))
x0 = x0[1, 1, drop = TRUE][[1]]

x1 = predict(fit1, type = "survival", times = seq(0, 5000, by = 10), newdata = data.table(t = 1))
x1 = x1[1, 1, drop = TRUE][[1]]

p +
  geom_line(
    aes(x = .time, y = .pred_survival),
    data = x0
  )

p +
  geom_line(
    aes(x = .time, y = .pred_survival),
    data = x1
  )

x[[1]]

p = ggsurvplot(fit)
p$plot +
  theme_bw()







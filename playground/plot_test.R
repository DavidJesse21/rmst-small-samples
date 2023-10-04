options(box.path = "R")
set.seed(123)

box::use(
  sim = SimNPH,
  survival[Surv, survfit],
  survminer[ggsurvplot],
  ggplot2[...],
  data.table[...]#,
  #miniPCH[...]
)

box::use(
  survplot = plotting/surv,
  hazardplot = plotting/hazard,
  hrplot = plotting/hr
)

legend_opts = theme(
  legend.background = element_blank(),
  legend.key = element_blank()
)
legend_opts_surv = legend_opts + theme(legend.position = c(0.85, 0.85))


# Delayed effect ----

params_surv = sim$assumptions_delayed_effect()
params_study = sim$design_fixed_followup()
params_all = merge(params_surv, params_study, by = NULL)
setDT(params_all)

# One sample scenario
x = params_all[5]

# Plot survival curves
survplot$plot_surv_delay(
  x$hazard_ctrl, x$hazard_trt, x$delay,
  linewidth = 0.9
) +
  xlim(c(0, 5000)) +
  legend_opts_surv

# Plot hazard rates
hazardplot$plot_hazard_delay(
  x$hazard_ctrl, x$hazard_trt, x$delay,
  linewidth = 0.9
) +
  xlim(c(0, 1000)) +
  legend_opts

# Plot hazard ratio
hrplot$plot_hr_delay(x$hazard_ctrl, x$hazard_trt, x$delay, linewidth = 0.9) +
  xlim(c(0, 1000))


# Crossing hazards ----

params_surv = sim$assumptions_crossing_hazards()
params_study = sim$design_fixed_followup()
params_all = merge(params_surv, params_study, by = NULL)
setDT(params_all)

# One sample scenario
x = params_all[5]

# Plot survival curves
survplot$plot_surv_crossing(
  x$hazard_ctrl, x$hazard_trt_before, x$hazard_trt_after, x$crossing,
  linewidth = 0.9
) +
  xlim(c(0, 5000)) +
  legend_opts_surv

# Plot hazard rates
hazardplot$plot_hazard_crossing(
  x$hazard_ctrl, x$hazard_trt_before, x$hazard_trt_after, x$crossing,
  linewidth = 0.9
) +
  xlim(c(0, 1000)) +
  legend_opts

# Plot hazard ratio
hrplot$plot_hr_crossing(
  x$hazard_ctrl, x$hazard_trt_before, x$hazard_trt_after, x$crossing,
  linewidth = 0.9
) +
  xlim(c(0, 1000))

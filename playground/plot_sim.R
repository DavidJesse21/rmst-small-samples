# Try out plotting functions I have written so far.

options(box.path = "R")
set.seed(123)

box::use(
  sim = SimNPH,
  data.table[...],
  ggplot2[...]
)

box::use(
  survplot = plotting/surv,
  hazardplot = plotting/hazard,
  hrplot = plotting/hr,
  jointplot = plotting/joint
)


# Delayed effect ----

params_surv = sim$assumptions_delayed_effect()
params_study = sim$design_fixed_followup()
params_all = merge(params_surv, params_study, by = NULL)
setDT(params_all)

# One sample scenario
x = params_all[5]

# Jointly plot survival, hazard and HR
jointplot$plot_joint_delay(
  x$hazard_ctrl, x$hazard_trt, x$delay,
  linewidth = 0.9 , align = "row"
) &
  xlim(c(0, 1000))

jointplot$plot_joint_delay(
  x$hazard_ctrl, x$hazard_trt, x$delay,
  linewidth = 0.9 , align = "column"
) &
  xlim(c(0, 1000))


# Crossing hazards ----

params_surv = sim$assumptions_crossing_hazards()
params_study = sim$design_fixed_followup()
params_all = merge(params_surv, params_study, by = NULL)
setDT(params_all)

# One sample scenario
x = params_all[5]

# Jointly plot survival, hazard and HR
jointplot$plot_joint_crossing(
  x$hazard_ctrl, x$hazard_trt_before, x$hazard_trt_after, x$crossing,
  linewidth = 0.9, align = "row"
) &
  xlim(c(0, 1000))

jointplot$plot_joint_crossing(
  x$hazard_ctrl, x$hazard_trt_before, x$hazard_trt_after, x$crossing,
  linewidth = 0.9, align = "column"
) &
  xlim(c(0, 1000))

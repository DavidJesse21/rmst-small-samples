#' Crossing hazards scenario
#' 
#' Goal:
#' Find a parametrization such that the RMSTs equal each other for a given 
#' cutoff time

options(box.path = "R")
set.seed(42)

box::use(
  # Data wrangling
  data.table[...],
  # Plotting
  ggplot2[...],
  ggsci[scale_color_jco, pal_jco],
  # Simulation functions
  simnph = SimNPH,
  # Survival R functions
  survival[Surv]
)

box::use(
  rmst/km[rmst, rmst_diff],
  true_rmst = rmst/true,
  jointplot = plotting/joint
)


params = simnph$assumptions_crossing_hazards()
cutoff = round(simnph$m2d(36))

params = data.table(
  # Median survival control: 12 months
  hazard_ctrl = simnph$m2r(12),
  # ~20% censoring in control arm
  random_withdrawal = simnph$m2r(48),
  # Try out
  hazard_trt_before = simnph$m2r(9),
  hazard_trt_after = simnph$m2r(24),
  crossing = 326.8855
)

with(
  params,
  jointplot$plot_joint_crossing(
    hazard_ctrl, hazard_trt_before, hazard_trt_after, crossing,
    align = "column"
  ) &
    xlim(c(0, 1500))
)


# Control
true_rmst$expo(simnph$m2r(12), cutoff)
# Treatment
true_rmst$pwexp(
  hazards = simnph$m2r(c(9, 24)), knots = 326.8855, cutoff = cutoff
)

params = data.table(
  # Median survival control: 12 months
  hazard_ctrl = simnph$m2r(12),
  # Treatment arm, leading to same RMST at 1096 days
  hazard_trt_before = simnph$m2r(9),
  hazard_trt_after = simnph$m2r(24),
  crossing = 326.8855,
  # ~20% censoring in control arm
  random_withdrawal = simnph$m2r(48)
)

p = with(
  params,
  jointplot$plot_joint_crossing(
    hazard_ctrl, hazard_trt_before, hazard_trt_after, crossing,
    align = "column"
  ) &
    xlim(c(0, 1500))
)
p[[1]] = p[[1]] + geom_vline(xintercept = 1096, linetype = "dashed")
p



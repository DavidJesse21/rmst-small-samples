options(box.path = "R")

box::use(
  ggplot2[...],
  data.table[...],
  patchwork[wrap_plots, plot_layout]
)

theme_set(theme_bw())

wb_haz = function(x, shape, scale) {
  scale * shape * (scale * x)^(shape - 1)
}

wb_surv = function(x, shape, scale) {
  exp(- (scale * x)^shape)
}

wb_hr = function(x, shape1, scale1, shape0, scale0) {
  wb_haz(x, shape1, scale1) / wb_haz(x, shape0, scale0)
}


# Proportional hazards ----

shape0_1 = 3
scale0_1 = 8

shape1_1 = 3
scale1_1 = 9


# Constant ggplot objects
spec_xaxis = scale_x_continuous(
  breaks = seq(0, 10, by = 2.5),
  limits = c(0, 10.5),
  expand = expansion(mult = c(0, 0), add = c(0.25, 0.25))
)

spec_xtitle = xlab("\nTime")

spec_legend = scale_color_manual(
  name = NULL,
  values = c("T0" = "#E69F00", "T1" = "#56B4E9"),
  labels = c(
    expression(paste(T[0], " ~ Weib(3, 8)")),
    expression(paste(T[1], " ~ Weib(3, 9)"))
  )
)


# Plots
p1_surv = ggplot() +
  geom_function(
    aes(color = "T1"), fun = \(x) wb_surv(x, shape1_1, 1/scale1_1),
    linewidth = 1.1
  ) +
  geom_function(
    aes(color = "T0"), fun = \(x) wb_surv(x, shape0_1, 1/scale0_1),
    linewidth = 1.1
  ) +
  spec_legend +
  spec_xaxis +
  spec_xtitle +
  ylab("Survival Probability\n") +
  scale_y_continuous(limits = 0:1)

p1_haz = ggplot() +
  geom_function(
    aes(color = "T1"), fun = \(x) wb_haz(x, shape1_1, 1/scale1_1),
    linewidth = 1.1
  ) +
  geom_function(
    aes(color = "T0"), fun = \(x) wb_haz(x, shape0_1, 1/scale0_1),
    linewidth = 1.1
  ) +
  spec_legend +
  spec_xaxis +
  spec_xtitle +
  ylab("Hazard Rate\n")

p1_hr = ggplot() +
  geom_function(
    fun = \(x) wb_hr(x, shape1_1, 1/scale1_1, shape0_1, 1/scale0_1),
    linewidth = 1.1, color = "#000000"
  ) +
  spec_xaxis +
  spec_xtitle +
  ylab(expression(paste("Hazard Ratio ", h[1](t)/h[0](t))))

wrap_plots(p1_surv, p1_haz, p1_hr) +
  plot_layout(ncol = 1, guides = "collect", axes = "collect_x") &
  theme(legend.position = "top")

wrap_plots(
  p1_surv + theme(legend.position = "top"),
  p1_haz + theme(legend.position = "none"),
  p1_hr + theme(legend.position = "none")
) +
  plot_layout(ncol = 1, axes = "collect_x")


# Nonproportional hazards ----

shape0_2 = 3
scale0_2 = 8

shape1_2 = 2
scale1_2 = 9

spec_legend = scale_color_manual(
  name = NULL,
  values = c("T0" = "#E69F00", "T1" = "#56B4E9"),
  labels = c(
    expression(paste(T[0], " ~ Weib(3, 8)")),
    expression(paste(T[1], " ~ Weib(2, 9)"))
  )
)

# Plots
p2_surv = ggplot() +
  geom_function(
    aes(color = "T1"), fun = \(x) wb_surv(x, shape1_2, 1/scale1_2),
    linewidth = 1.1
  ) +
  geom_function(
    aes(color = "T0"), fun = \(x) wb_surv(x, shape0_2, 1/scale0_2),
    linewidth = 1.1
  ) +
  spec_legend +
  spec_xaxis +
  spec_xtitle +
  #ylab("Survival Probability\n") +
  scale_y_continuous(limits = 0:1, name = NULL)

p2_haz = ggplot() +
  geom_function(
    aes(color = "T1"), fun = \(x) wb_haz(x, shape1_2, 1/scale1_2),
    linewidth = 1.1
  ) +
  geom_function(
    aes(color = "T0"), fun = \(x) wb_haz(x, shape0_2, 1/scale0_2),
    linewidth = 1.1
  ) +
  spec_legend +
  spec_xaxis +
  spec_xtitle +
  #ylab("Hazard Rate\n")
  scale_y_continuous(name = NULL)

p2_hr = ggplot() +
  geom_function(
    fun = \(x) wb_hr(x, shape1_2, 1/scale1_2, shape0_2, 1/scale0_2),
    linewidth = 1.1, color = "#000000"
  ) +
  spec_xaxis +
  spec_xtitle +
  #ylab(expression(paste("Hazard Ratio ", h[1](t)/h[0](t))))
  scale_y_continuous(name = NULL)

wrap_plots(p2_surv, p2_haz, p2_hr) +
  plot_layout(ncol = 1, guides = "collect", axes = "collect_x") &
  theme(legend.position = "top")

wrap_plots(
  p2_surv + theme(legend.position = "top"),
  p2_haz + theme(legend.position = "none"),
  p2_hr + theme(legend.position = "none")
) +
  plot_layout(ncol = 1, axes = "collect_x")


# Combine everything ----

wrap_plots(
  # Proportional hazards
  p1_surv + theme(legend.position = "top"),
  p1_haz + theme(legend.position = "none"),
  p1_hr + theme(legend.position = "none"),
  # Nonproportional hazards
  p2_surv + theme(legend.position = "top"),
  p2_haz + theme(legend.position = "none"),
  p2_hr + theme(legend.position = "none")
) +
  plot_layout(ncol = 2, byrow = FALSE, axes = "collect_x")


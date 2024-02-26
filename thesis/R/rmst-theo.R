options(box.path = "R")

box::use(
  ggplot2[...],
  data.table[...],
  patchwork[wrap_plots, plot_layout]
)

theme_set(theme_bw())


# One sample plot ----

lambda0 = 0.2

dt = data.table(
  time = seq(0, 10.5, length.out = 250),
  surv = exp(- (lambda0 * seq(0, 10.5, length.out = 250)))
)

p1 = ggplot(dt, aes(time, surv)) +
  geom_line(color = "#E69F00", linewidth = 1.1) +
  geom_area(color = "#E69F00", fill = "#E69F00", alpha = 0.4, data = dt[time <= 10]) +
  # Indicator line for RMST cutoff
  geom_vline(
    xintercept = 10, linewidth = 0.9,
    linetype = "longdash", color = "#222222"
  ) +
  # Fine-tuning of axes
  scale_x_continuous(
    breaks = seq(0, 10, by = 2.5),
    limits = c(0, 10.5),
    expand = expansion(mult = c(0, 0), add = c(0.5, 0))
  ) +
  scale_y_continuous(limits = 0:1) +
  # Axes labels
  labs(x = "\nTime", y = "Survival probability\n")


# Two-sample plot ----

lambda0 = 0.2
lambda1 = 0.1

dt = data.table(time = seq(0, 10.5, length.out = 250))
dt[, `:=`(surv0 = exp(- (lambda0 * time)), surv1 = exp(- (lambda1 * time)))]

p2 = ggplot(dt) +
  geom_ribbon(
    aes(x = time, ymin = surv0, ymax = surv1), data = dt[time <= 10],
    color = "#999999", fill = "#999999", alpha = 0.5
  ) +
  geom_line(aes(time, surv0, color = "T0"), linewidth = 1.1) +
  geom_line(aes(time, surv1, color = "T1"), linewidth = 1.1) +
  scale_color_manual(
    name = NULL,
    values = c("T0" = "#E69F00", "T1" = "#56B4E9"),
    labels = expression(T[0] ~ tilde("") ~ Exp(0.2), T[1] ~ tilde("") ~ Exp(0.1))
  ) +
  # Indicator line for RMST cutoff
  geom_vline(
    xintercept = 10, linewidth = 0.9,
    linetype = "longdash", color = "#222222"
  ) +
  # Fine-tuning of axes
  scale_x_continuous(
    breaks = seq(0, 10, by = 2.5),
    limits = c(0, 10.5),
    expand = expansion(mult = c(0, 0), add = c(0.5, 0))
  ) +
  scale_y_continuous(limits = 0:1) +
  # Axes labels
  labs(x = "\nTime", y = "Survival probability\n")


# Combine plots ----

p2 = p2 +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

wrap_plots(p1, p2, ncol = 2) +
  plot_layout(axis_titles = "collect", guides = "collect") &
  theme(legend.position = "top")

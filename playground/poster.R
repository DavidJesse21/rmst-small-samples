options(box.path = "R")

box::use(
  data.table[...],
  ggplot2[...],
  survival[Surv, survfit],
  fs
)

box::use(
  # simulation results
  simfuns2/analyze[calc_rejection_rates, calc_ci_metrics, setj_samples_alloc, setj_percent],
  simfuns2/get_funs[get_scenario_table],
)


# RMST visualization ----

# Function for visualising Kaplan-Meier curve and restricted mean survival time
ggrmst = function(fit, cutoff,
                  color = "#0072B2", linewidth = 1, alpha = 0.3,
                  ...) {
  # Extract survival data
  dt_surv = data.table(
    time = c(0, fit$time),
    surv = c(1, fit$surv)
  )
  
  # Plot of the survival curve
  p = ggplot(dt_surv, aes(time, surv)) +
    geom_step(linewidth = linewidth, color = color)
  
  # Create data.table for plotting RMST
  dt_rmst = dt_surv[time <= cutoff]
  # Area should be drawn until the specified cutoff and not end before
  if (dt_rmst[.N, time < cutoff]) {
    dt_rmst = rbindlist(list(
      dt_rmst, data.table(time = cutoff, surv = dt_rmst[.N, surv])
    ))
  }
  # For geom_ribbon()
  dt1 = copy(dt_rmst)[, id := "b"]
  dt2 = copy(dt_rmst)[, id := "a"]
  dt2[, surv := shift(surv)]
  dt_rmst = rbindlist(list(dt1, dt2))
  setorder(dt_rmst, time, id)
  
  # Plot RMST
  p = p +
    geom_ribbon(
      aes(x = time, ymin = 0, ymax = surv), data = dt_rmst,
      fill = color, alpha = alpha
    ) +
    geom_vline(
      xintercept = cutoff,
      linetype = "dashed", color = "#333333", linewidth = 0.75 * linewidth
    )
  
  return(p)
}

# RMST example
dt = survival::veteran
setDT(dt)
dt = dt[time <= 800]

fit = survfit(Surv(time, status) ~ 1, data = dt)

ggrmst(fit, cutoff = 350) +
  scale_x_continuous(
    name = "\nTime",
    breaks = seq(0, 800, by = 100),
    expand = expansion(mult = c(0, 0), add = c(20, 20))
  ) +
  scale_y_continuous(
    name = "Survival probability\n",
    breaks = seq(0, 1, by = 0.2)
  ) +
  theme_bw(base_size = 16)

ggsave(
  "rmst.png", path = fs$path("playground"),
  width = 10, height = 6, dpi = 300,
  type = "cairo"
)


# Simulation results ----

# Load simulation results
dtr = readRDS(fs$path("simulation", "results", "2024-02-08_results1.rds"))
setDT(dtr)
dts = get_scenario_table()

# Type-I error rates
dt1 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 0, scenario.id]],
  stats_NA = FALSE
)
dt1 = merge(
  dt1, dts[rmst_diff == 0, .(scenario.id, num_samples = n0 + n1)],
  by = "scenario.id"
)
setj_percent(dt1, "reject")

ggplot(dt1, aes(factor(algo.id), reject)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  # Binomial confidence interval
  geom_hline(yintercept = 4.4, linetype = "dashed") +
  geom_hline(yintercept = 5.6, linetype = "dashed") +
  # Labels etc.
  scale_y_continuous(
    name = "Type I error in %\n"
  ) +
  scale_x_discrete(
    name = "\nMethods",
    labels = c("Asy", "Stud. Perm.", "PO", "PO IJ", "PO IJ Boot")
  ) +
  # Differentiate between total sample sizes
  facet_wrap(
    ~ num_samples,
    labeller = labeller(num_samples = \(x) paste0("N = ", x))
  )

ggsave(
  "sim-type1-err.png", path = fs$path("playground"),
  width = 10, height = 6, dpi = 300,
  type = "cairo"
)


# Power
dt2 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 1.5, scenario.id]],
  stats_NA = FALSE
)
dt2 = merge(
  dt2, dts[rmst_diff == 1.5, .(scenario.id, num_samples = n0 + n1)],
  by = "scenario.id"
)
setj_percent(dt2, "reject")

ggplot(dt2, aes(factor(algo.id), reject)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  scale_y_continuous(
    name = "Power in %\n"
  ) +
  scale_x_discrete(
    name = "\nMethods",
    labels = c("Asy", "Stud. Perm.", "PO", "PO IJ", "PO IJ Boot")
  ) +
  facet_wrap(
    ~ num_samples,
    labeller = labeller(num_samples = \(x) paste0("N = ", x)),
    scales = "free_y"
  )

ggsave(
  "sim-power.png", path = fs$path("playground"),
  width = 10, height = 6, dpi = 300,
  type = "cairo"
)


# CI coverage
dt3 = calc_ci_metrics(
  merge(dtr, dts[, .(scenario.id, rmst_diff)], by = "scenario.id"),
  stats_NA = FALSE
)
dt3 = merge(
  dt3, dts[, .(scenario.id, num_samples = n0 + n1)],
  by = "scenario.id"
)
setj_percent(dt3, "coverage")

ggplot(dt3, aes(factor(algo.id), coverage)) +
  geom_boxplot() +
  theme_bw(base_size = 16) +
  scale_y_continuous(
    name = "Coverage in %\n"
  ) +
  scale_x_discrete(
    name = "\nMethods",
    labels = c("Asy", "Stud. Perm.", "PO", "PO IJ", "PO IJ Boot")
  ) +
  facet_wrap(
    ~ num_samples,
    labeller = labeller(num_samples = \(x) paste0("N = ", x))
  ) +
  # Binomial confidence interval
  geom_hline(yintercept = 94.4, linetype = "dashed") +
  geom_hline(yintercept = 95.6, linetype = "dashed")

ggsave(
  "sim-coverage.png", path = fs$path("playground"),
  width = 10, height = 6, dpi = 300,
  type = "cairo"
)

options(box.path = "R")

box::use(
  fs,
  data.table[...],
  ggplot2[...]
)

box::use(
  simfuns2/analyze[calc_rejection_rates, calc_ci_metrics, setj_samples_alloc, setj_percent],
  simfuns2/get_funs[get_scenario_table]
)

dtr = readRDS(fs$path("simulation", "results", "2024-02-08_results1", ext = "rds"))
setDT(dtr)
dts = get_scenario_table()

theme_set(theme_bw())


# Main Document ----

dt4 = calc_ci_metrics(
  merge(dtr[algo.id != 4], dts[, .(scenario.id, rmst_diff)], by = "scenario.id"),
  stats_NA = FALSE
)
dt4 = merge(dt4, dts, by = "scenario.id")
setj_percent(dt4, "coverage")
setj_samples_alloc(dt4)

ggplot(dt4, aes(factor(algo.id), coverage)) +
  geom_boxplot() +
  facet_grid(
    rows = vars(samples_k), cols = vars(samples_alloc),
    labeller = labeller(samples_k = \(x) sprintf("K = %s", x))
  ) +
  # Binomial confidence interval
  geom_hline(yintercept = 94.4, linetype = "dashed") +
  geom_hline(yintercept = 95.6, linetype = "dashed") +
  # y-axis
  scale_y_continuous(
    name = "Coverage in %\n"
  ) +
  # x-axis
  scale_x_discrete(
    name = "\nMethods",
    labels = c("Asy", "St Perm", "PO Asy", "PO Boot")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Another view at the data
dt4[, surv_model2 := factor(
  fcase(
    surv_model == "ph_exp", "S1: Exp. (proportional hazards)",
    surv_model == "crossing_pwexp", "S7: Exp. vs. piecewise Exp.",
    surv_model == "crossing_wb", "S8: Weibull (shape alternatives)"
  ),
  levels = c("S1: Exp. (proportional hazards)", "S7: Exp. vs. piecewise Exp.", "S8: Weibull (shape alternatives)")
)]

ggplot(dt4, aes(factor(samples_alloc), coverage)) +
  geom_boxplot() +
  facet_grid(
    rows = vars(algo.id), cols = vars(surv_model2),
    labeller = labeller(
      algo.id = as_labeller(
        setNames(c("Asy", "St Perm", "PO Asy", "PO Boot"), c(1:3, 5))
      )
    )
  ) +
  # Binomial confidence interval
  geom_hline(yintercept = 94.4, linetype = "dashed") +
  geom_hline(yintercept = 95.6, linetype = "dashed") +
  # y-axis
  scale_y_continuous(
    name = "Coverage in %\n"
  ) +
  # x-axis
  scale_x_discrete(
    name = "\nSample allocations"
  )


ggplot(dt4, aes(factor(algo.id), coverage)) +
  geom_boxplot() +
  facet_grid(
    rows = vars(samples_alloc), cols = vars(surv_model2)#,
    #labeller = labeller(samples_k = \(x) sprintf("K = %s", x))
  ) +
  # Binomial confidence interval
  geom_hline(yintercept = 94.4, linetype = "dashed") +
  geom_hline(yintercept = 95.6, linetype = "dashed") +
  # y-axis
  scale_y_continuous(
    name = "Coverage in %\n"
  ) +
  # x-axis
  scale_x_discrete(
    name = "\nMethods",
    labels = c("Asy", "St Perm", "PO Asy", "PO Boot")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(dt4[algo.id == 1], aes(samples_alloc, coverage)) +
  geom_boxplot() +
  # Binomial confidence interval
  geom_hline(yintercept = 94.4, linetype = "dashed") +
  geom_hline(yintercept = 95.6, linetype = "dashed") +
  # y-axis
  scale_y_continuous(
    name = "Coverage in %\n"
  ) +
  facet_wrap(~ surv_model2)


## Additional

dt4[, covered := between(coverage, 94.4, 95.6)]

dt4[, sum(covered), by = algo.id]
dt4[, mean(covered), by = algo.id]
dt4[, .N, by = algo.id]

dt4[, sum(covered), by = .(algo.id, surv_model)][order(algo.id)]
dt4[, mean(covered), by = .(algo.id, surv_model)][order(algo.id)]

dt4[, sum(covered), by = .(algo.id, cens_model)][order(algo.id)]
dt4[, mean(covered), by = .(algo.id, cens_model)][order(algo.id)]

dt4[, sum(covered), by = .(cens_model)]
dt4[, sum(covered), by = .(surv_model)]


# Appendix ----

dt5 = calc_ci_metrics(
  merge(dtr[algo.id %in% 3:4], dts[, .(scenario.id, rmst_diff)], by = "scenario.id"),
  stats_NA = FALSE
)
dt5 = merge(dt5, dts[, .(scenario.id, samples_k, n0, n1)], by = "scenario.id")
setj_percent(dt5, "coverage")
setj_samples_alloc(dt5)

ggplot(dt5, aes(factor(algo.id), coverage)) +
  geom_boxplot() +
  facet_grid(
    rows = vars(samples_k), cols = vars(samples_alloc),
    labeller = labeller(samples_k = \(x) sprintf("K = %s", x))
  ) +
  # Binomial confidence interval
  geom_hline(yintercept = 94.4, linetype = "dashed") +
  geom_hline(yintercept = 95.6, linetype = "dashed") +
  # y-axis
  scale_y_continuous(
    name = "Coverage in %\n"
  ) +
  # x-axis
  scale_x_discrete(
    name = "\nMethods",
    labels = c("PO", "PO IJ")
  ) #+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))

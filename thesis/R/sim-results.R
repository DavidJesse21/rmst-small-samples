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


# Plot type I error rate ----

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
  theme_bw() +
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


# Plot power ----

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
  theme_bw() +
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
    scales = "fixed"
  )

# Plot coverage ----

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
  theme_bw() +
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


# Same but distinguish between sample allocations

dt4 = calc_ci_metrics(
  merge(dtr, dts[, .(scenario.id, rmst_diff)], by = "scenario.id"),
  stats_NA = FALSE
)
dt4 = merge(dt4, dts[, .(scenario.id, samples_k, n0, n1)], by = "scenario.id")
setj_percent(dt4, "coverage")
setj_samples_alloc(dt4)

ggplot(dt4, aes(factor(algo.id), coverage)) +
  geom_boxplot() +
  facet_grid(
    rows = vars(samples_k), cols = vars(samples_alloc),
    labeller = labeller(samples_k = \(x) sprintf("K = %s", x))
  ) +
  theme_bw() +
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
    labels = c("Asy", "Stud. Perm.", "PO", "PO IJ", "PO IJ Boot")
  )

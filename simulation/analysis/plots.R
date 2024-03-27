options(box.path = "R")

box::use(
  fs,
  data.table[...],
  ggplot2[...]
)

box::use(
  simfuns2/analyze[calc_rejection_rates, calc_ci_metrics, setj_samples_alloc, setj_percent],
  simfuns2/get_funs[get_scenario_table, get_algo_table]
)


dtr = readRDS(fs$path("simulation", "results", "2024-02-08_results1", ext = "rds"))
setDT(dtr)
dts = get_scenario_table()


# Initial investigations ----

dtr[, anyNA(pval)]
dtr[, table(algo.id)]

for (i in 1:5) {
  finished = dtr[algo.id == i, sort(unique(scenario.id))]
  missing = setdiff(1:216, finished)
  
  cat("Algorithm ", i, "\n")
  cat("Missing simulations:\n")
  cat(paste(missing, collapse = ", "), "\n\n")
}


# Type I error rates ----

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


# Power ----

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
    labeller = labeller(num_samples = \(x) paste0("N = ", x))
  )


# Coverage ----

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


dt3[, sum(between(coverage, 94.4, 95.6)), by = algo.id]
dt3[, mean(between(coverage, 94.4, 95.6)), by = algo.id]

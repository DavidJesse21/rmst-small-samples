options(box.path = "R")

box::use(
  fs,
  data.table[...],
  ggplot2[...]
)

box::use(
  simfuns2/analyze[calc_ci_metrics, setj_samples_alloc, setj_percent],
  simfuns2/get_funs[get_scenario_table]
)


dt = readRDS(fs$path("simulation", "results", "2024-01-16_results1", ext = "rds"))
invisible(setalloccol(dt))
dts = get_scenario_table()

dt_ci = calc_ci_metrics(dt, stats_NA = FALSE)
dt_ci = merge(dt_ci, dts, by = "scenario.id")
setj_percent(dt_ci, "coverage")
setj_samples_alloc(dt_ci)

ggplot(dt_ci, aes(factor(algo.id), coverage)) +
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
    labels = c("Asy", "Perm", "Pseudo 1", "Pseudo 2")
  )

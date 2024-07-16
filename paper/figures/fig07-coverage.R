#' Figure 7 from my thesis showing the box-plots of the CI coverage from the 
#' simulation study aggregated by the sample size allocation (n0, n1) and its multiplier (K).


# Setup ----

options(box.path = "R")

box::use(
  # data wrangling
  data.table[...],
  # plotting
  ggplot2[...],
  # miscellaneous
  fs
)

box::use(
  # functions for analyzing simulation results
  simfuns2/analyze[calc_rejection_rates, calc_ci_metrics, setj_samples_alloc, setj_percent],
  simfuns2/get_funs[get_scenario_table]
)

# ggplot2 theme
theme_set(theme_bw(base_size = 16))

# simulation results
dtr = readRDS(fs$path("simulation", "results", "2024-02-08_results1", ext = "rds"))
dts = get_scenario_table()


# Plot ----

dt4 = calc_ci_metrics(
  merge(dtr[algo.id != 4], dts[, .(scenario.id, rmst_diff)], by = "scenario.id"),
  stats_NA = FALSE
)
dt4 = merge(dt4, dts[, .(scenario.id, samples_k, n0, n1, surv_model)], by = "scenario.id")
setj_percent(dt4, "coverage")
setj_samples_alloc(dt4)

ggplot(dt4, aes(factor(algo.id), coverage)) +
  geom_boxplot() +
  facet_grid(
    rows = vars(samples_k), cols = vars(samples_alloc),
    labeller = labeller(
      samples_k = \(x) sprintf("K = %s", x),
      samples_alloc = \(x) sprintf("n = %s", x)
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
    name = "\nMethods",
    labels = c("Asy", "St Perm", "PO Asy", "PO Boot")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "fig07-coverage.pdf",
  path = fs$path("paper", "figures"),
  width = 10, height = 11
)

# Clean environment for next figure
rm(list = ls())
box::purge_cache()

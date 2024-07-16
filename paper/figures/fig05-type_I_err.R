#' Figure 5 from my thesis showing the box-plots of type I error rates from the 
#' simulation study aggregated by the total sample size.


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

dt1 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 0, scenario.id] & algo.id != 4],
  stats_NA = FALSE
)
dt1 = merge(
  dt1, dts[rmst_diff == 0, .(scenario.id, num_samples = n0 + n1)],
  by = "scenario.id"
)
setj_percent(dt1, "reject")

ggplot(dt1, aes(factor(algo.id), reject)) +
  geom_boxplot() +
  # Binomial confidence interval
  geom_hline(yintercept = 4.4, linetype = "dashed") +
  geom_hline(yintercept = 5.6, linetype = "dashed") +
  # Labels etc.
  scale_y_continuous(
    name = "Type I error in %\n"
  ) +
  scale_x_discrete(
    name = "\nMethods",
    labels = c("Asy", "St Perm", "PO Asy", "PO Boot")
  ) +
  # Differentiate between total sample sizes
  facet_wrap(
    ~ num_samples,
    labeller = labeller(num_samples = \(x) paste0("N = ", x))
  )

ggsave(
  "fig05-type_I_err.pdf",
  path = fs$path("paper", "figures"),
  width = 10, height = 6
)

# Clean environment for next figure
rm(list = ls())
box::purge_cache()

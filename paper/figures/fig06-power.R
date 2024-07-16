#' Figure 6 from my thesis showing the box-plots of the rejection rates (power) from the 
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

dt2 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 1.5, scenario.id] & algo.id != 4],
  stats_NA = FALSE
)
dt2 = merge(
  dt2, dts[rmst_diff == 1.5, .(scenario.id, num_samples = n0 + n1)],
  by = "scenario.id"
)
setj_percent(dt2, "reject")

ggplot(dt2, aes(factor(algo.id), reject)) +
  geom_boxplot() +
  scale_y_continuous(
    name = "Power in %\n"
  ) +
  scale_x_discrete(
    name = "\nMethods",
    labels = c("Asy", "St Perm", "PO Asy", "PO Boot")
  ) +
  facet_wrap(
    ~ num_samples,
    labeller = labeller(num_samples = \(x) paste0("N = ", x)),
    scales = "fixed"
  )

ggsave(
  "fig06-power.pdf",
  path = fs$path("paper", "figures"),
  width = 10, height = 6
)

# Clean environment for next figure
rm(list = ls())
box::purge_cache()

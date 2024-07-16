#' Figure 4 from my thesis for displaying the simulation models under consideration.

options(box.path = "R")

box::use(
  ggplot2[...],
  plotting/surv[plot_surv_models],
  plotting/cens[plot_cens_models],
  fs
)

theme_set(theme_bw(base_size = 16))

wrap_plots(
  plot_surv_models(linewidth = 1.1, align = "h"),
  plot_cens_models(linewidth = 1.1, align = "h"),
  ncol = 1
)

ggsave(
  "fig04-sim_models.pdf",
  path = fs$path("paper", "figures"),
  width = 10, height = 8
)

# Clean environment for next figure
rm(list = ls())
box::purge_cache()

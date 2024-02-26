options(box.path = "R")

box::use(
  ggplot2[...],
  data.table[...],
  patchwork[wrap_plots, plot_layout]
)

box::use(
  plotting/surv[plot_surv_models],
  plotting/cens[plot_cens_models]
)

theme_set(theme_bw())

plot_surv_models(linewidth = 1.1)


plot_cens_models(linewidth = 1.1, align = "h")
plot_surv_models(linewidth = 1.1, align = "h")

wrap_plots(
  plot_surv_models(linewidth = 1.1, align = "h") + ggtitle("Test"),
  plot_cens_models(linewidth = 1.1, align = "h"),
  ncol = 1
)

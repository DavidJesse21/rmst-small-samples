options(box.path = "R")

box::use(
  plotting/surv[plot_surv_models],
  plotting/cens[plot_cens_models]
)

plot_surv_models(linewidth = 1.1)
plot_cens_models(linewidth = 1.1)

plot_surv_models(linewidth = 1.1, align = "h")
plot_cens_models(linewidth = 1.1, align = "h")

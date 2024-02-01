options(box.path = "R")

box::use(
  plotting2/surv[plot_survs_exp, plot_survs_pwexp, plot_survs_weibull]
)

box::use(
  ggplot2[...]
)

plot_survs_exp(linewidth = 1.1)
plot_survs_pwexp(linewidth = 1.1)
plot_survs_weibull(linewidth = 1.1)

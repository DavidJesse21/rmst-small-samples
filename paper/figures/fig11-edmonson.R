#' For the purpose of the paper, I will not plot the hazard functions but only the 
#' "empirical" survival function.


# Setup ----

options(box.path = "R")

box::use(
  # data wrangling
  data.table[...],
  # plotting
  ggplot2[...],
  ggsurvfit[survfit2, ggsurvfit, add_censor_mark, add_risktable, add_risktable_strata_symbol,
            theme_risktable_default, add_pvalue],
  # survival
  survival[Surv, survfit, survdiff, coxph, cox_zph = cox.zph],
  # miscellaneous
  fs
)

# ggplot2 theme
theme_set(theme_bw(base_size = 16))


# Data
dt = survival::ovarian
setDT(dt)
setnames(dt, old = c("futime", "fustat", "rx"), new = c("time", "event", "group"))
dt[, group := group - 1L]
dt[, ecog.ps := factor(ecog.ps, levels = as.character(2:1))]
# Use months instead of days
dt[, time := time / 30.417]




# Plot ----

p_km = ggsurvfit(
  survfit2(Surv(time, event) ~ group, data = dt),
  linewidth = 1.1
) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Treatment",
    values = c("#E69F00", "#56B4E9"),
    labels = c("Control", "Experimental")
  ) +
  xlab("\nTime (months)") +
  ylab("Survival probability\n") +
  scale_y_continuous(limit = c(0, 1)) +
  theme(legend.position = "top") +
  #scale_x_continuous(limits = c(0, 1250))
  scale_x_continuous(limits = c(0, 40))


p_km +
  add_risktable(
    risktable_stats = "n.risk",
    risktable_group = "risktable_stats",
    stats_label = list(n.risk = "Number at risk"),
    size = 4.8,
    theme = list(
      theme_risktable_default(),
      theme(plot.title = element_text(size = 14))
    )
  ) +
  add_risktable_strata_symbol(symbol = "\U25CF", size = 14) +
  add_pvalue(
    location = "annotation",
    caption = "P-value of log-rank test (in %): {p.value}",
    pvalue_fun = \(x) format(100 * x, digits = 1, nsmall = 1),
    prepend_p = FALSE,
    size = 4.6,
    x = 32
  )


ggsave(
  "fig11-edmonson.pdf",
  path = fs$path("paper", "figures"),
  width = 10, height = 7,
  device = cairo_pdf
)

# Clean environment for next figure
rm(list = ls())
box::purge_cache()

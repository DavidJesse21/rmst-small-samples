# Setup ----

options(box.path = "R")

box::use(
  # data wrangling
  data.table[...],
  # plotting
  ggplot2[...],
  patchwork[wrap_plots, plot_layout],
  # survival
  survival[Surv, survfit, survdiff, coxph, cox_zph = cox.zph],
  ggsurvfit[survfit2, ggsurvfit, add_censor_mark],
  flexsurv[flexsurvspline],
  # miscellaneous
  fs
)

# ggplot2 theme
theme_set(theme_bw(base_size = 16))


# FPM functions ----

fit_fpm = function(data, df1 = 3, df2 = 2) {
  li_anc = rep(list(~ group), df2)
  names(li_anc) = paste0("gamma", 1:df2)
  
  m = flexsurvspline(
    Surv(time, event) ~ group, data = data,
    k = df1 - 1, anc = li_anc
  )
  
  return(m)
}

plot_fpm = function(m, t_eval, type = c("survival", "hazard", "HR"), hr_order = 0:1, ...) {
  type = match.arg(type, c("survival", "hazard", "HR"))
  
  li_dt = summary(
    m,
    type = if (type == "survival") "survival" else "hazard",
    t = t_eval,
    ci = FALSE
  )
  
  invisible(lapply(li_dt, setDT))
  invisible(lapply(names(li_dt), function(x) {
    li_dt[[x]][, group := sub("group=(.*)", "\\1", x)]
  }))
  
  dt = rbindlist(li_dt)
  dt[, group := factor(group, levels = sub("group=(.*)", "\\1", names(li_dt)))]
  
  # Survival and hazards
  if (type %in% c("survival", "hazard")) {
    p = ggplot(dt, aes(time, est, color = group)) +
      geom_line(...)
  } else {
    # Hazard ratio
    dt = dcast(dt, time ~ group, value.var = "est")
    setnames(dt, old = 2:3, new = paste0("haz", hr_order))
    dt[, hr := haz0 / haz1]
    
    p = ggplot(dt, aes(time, hr)) +
      geom_line(...)
  }
  
  return(p)
}


# Plot ----

blank_x = theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)


# Read Hellmann data
load(fs$path("data", "Hellmann", ext = "Rdata"))
dt = data
rm(data)
setDT(dt)

# Cox HR
cox = coxph(Surv(time, event) ~ group, data = dt, x = TRUE)
cox_hr = unname(exp(coef(cox)))

# Kaplan-Meier plot
p_km = ggsurvfit(
  survfit2(Surv(time, event) ~ group, data = dt),
  linewidth = 1.1
) +
  theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Treatment",
    values = c("#E69F00", "#56B4E9"),
    labels = c(
      "Chemotherapy",
      "Nivolumab + Ipilimumab"
    )
  ) +
  xlab(NULL) +
  blank_x +
  ylab("Survival probability\n") +
  scale_y_continuous(limit = c(0, 1)) +
  theme(legend.position = "top") +
  scale_x_continuous(limits = c(0, 25))

# For FPMs
dt2 = copy(dt)
dt2[, group := fifelse(group == 0, "Chemotherapy", "Nivolumab + Ipilimumab")]

# FPM
m = fit_fpm(dt2, 3, 2)
t_eval = seq(0.1, 25, by = 0.1)

# Hazards
p_haz = plot_fpm(m , t_eval, "hazard", linewidth = 1.1)
p_haz = p_haz +
  xlab(NULL) +
  blank_x +
  ylab("Hazard Rate\n") +
  scale_color_manual(
    name = "Treatment",
    values = c("Chemotherapy" = "#E69F00", "Nivolumab + Ipilimumab" = "#56B4E9")
  ) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 25))

# HR
p_hr = plot_fpm(m, t_eval, "HR", linewidth = 1.1, hr_order = 0:1)
p_hr = p_hr +
  xlab(NULL) +
  ylab(expression(paste("Hazard Ratio ", h[1](t)/h[0](t), "\n"))) +
  scale_x_continuous(limits = c(0, 25)) +
  geom_hline(
    yintercept = cox_hr,
    linetype = "dashed",
    linewidth = 1
  )

# Plot
wrap_plots(p_km, p_haz, p_hr) +
  plot_layout(ncol = 1, axes = "collect_x") &
  xlab("\nTime (months)")


ggsave(
  "fig09-hellmann-data.pdf",
  path = fs$path("paper", "figures"),
  width = 10, height = 11
)

# Clean environment for next figure
rm(list = ls())
box::purge_cache()

#' Figure 2 from my thesis contrasting real-world data sets for which the proportional hazards
#' assumption might seem more or less plausible.
#' For the paper, this figure might be considered rather optional.


# Setup ----

options(box.path = "R")

# Required modules/functions
box::use(
  # data wrangling
  data.table[...],
  # plotting
  ggplot2[...],
  patchwork[wrap_plots, plot_layout],
  ggsurvfit[survfit2, ggsurvfit, add_censor_mark],
  # survival methods
  survival[Surv, survfit, survdiff, coxph],
  flexsurv[flexsurvspline],
  # miscellaneous
  fs
)


# Data sets

## `diabetic` (proportional hazards)
dt1 = survival::diabetic
setDT(dt1)
setnames(dt1, old = c("status", "trt"), new = c("event", "group"))
dt1[, group := fifelse(group == 1, "Laser", "Control")]

## Robert et. al. 2015 (non-proportional hazards)
li_dt = lapply(c("nivo", "daca"), function(x) {
  file = fs$path("data", paste0("robert_", x), ext = "txt")
  fread(file)
})
dt2 = rbindlist(li_dt)
setnames(dt2, new = c("time", "event", "group"))


# FPM functions

## Fit the FPM
fit_fpm = function(data, df1 = 3, df2 = 2) {
  li_anc = rep(list(~ group), df2)
  names(li_anc) = paste0("gamma", 1:df2)
  
  m = flexsurvspline(
    Surv(time, event) ~ group, data = data,
    k = df1 - 1, anc = li_anc
  )
  
  return(m)
}

## Plot the FPM
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


# ggplot2 theme
theme_set(theme_bw(base_size = 16))


# Figure ----

## diabetic data set ----

# Kaplan-Meier curves
p_km1 = ggsurvfit(
  survfit2(Surv(time, event) ~ group, data = dt1),
  linewidth = 1.1
) +
  theme_bw(base_size = 16)

# FPM
m1 = fit_fpm(dt1)

# Cox HR
hr1 = coxph(Surv(time, event) ~ group, data = dt1)
hr1 = unname(exp(coef(hr1)))

# time points for evaluating survival functions
t_eval1 = seq(0.5, 70, by = 0.25)

# Survival functions based on FPM
p_h1 = plot_fpm(m1, t_eval1, "hazard", linewidth = 1.1)
p_hr1 = plot_fpm(m1, t_eval1, "HR", linewidth = 1.1, hr_order = 1:0)


## Robert et. al. ----

# Kaplan-Meier curves
p_km2 = ggsurvfit(
  survfit2(Surv(time, event) ~ group, data = dt2),
  linewidth = 1.1
) +
  theme_bw(base_size = 16)

# FPM
m2 = fit_fpm(dt2)

# Cox HR
hr2 = coxph(Surv(time, event) ~ group, data = dt2)
hr2 = unname(exp(coef(hr2)))

# time points for evaluating survival functions
t_eval2 = seq(0.3, 15, by = 0.1)

# Survival functions based on FPM
p_h2 = plot_fpm(m2, t_eval2, "hazard", linewidth = 1.1)
p_hr2 = plot_fpm(m2, t_eval2, "HR", linewidth = 1.1)


## Joint plot ----

blank_x = theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)

p_km1 = p_km1 +
  xlab(NULL) +
  blank_x +
  ylab("Survival Probability\n") +
  scale_color_manual(
    name = NULL,
    labels = c("Control", "Laser"),
    values = c("Control" = "#E69F00", "Laser" = "#56B4E9")
  ) +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 70))

p_h1 = p_h1 +
  xlab(NULL) +
  blank_x +
  ylab("Hazard Rate\n") +
  scale_color_manual(
    name = NULL,
    labels = c("Control", "Laser"),
    values = c("Control" = "#E69F00", "Laser" = "#56B4E9")
  ) +
  scale_x_continuous(limits = c(0, 70)) +
  theme(legend.position = "none")

p_hr1 = p_hr1 +
  xlab(NULL) +
  #ylab(expression(paste("Hazard Ratio ", h[0](t)/h[1](t), "\n"))) +
  ylab(expression(paste("Hazard Ratio ", h[1](t)/h[0](t), "\n"))) +
  scale_x_continuous(limits = c(0, 70)) +
  #scale_y_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 1.2)) +
  geom_hline(
    yintercept = hr1,
    linetype = "dashed",
    linewidth = 1
  )


p_km2 = p_km2 +
  xlab(NULL) +
  blank_x +
  ylab(NULL) +
  scale_color_manual(
    name = NULL,
    labels = c("Dacarbazine", "Nivolumab"),
    values = c("Dacarbazine" = "#E69F00", "Nivolumab" = "#56B4E9")
  ) +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 15))

p_h2 = p_h2 +
  xlab(NULL) +
  blank_x +
  ylab(NULL) +
  scale_color_manual(
    name = NULL,
    labels = c("Dacarbazine", "Nivolumab"),
    values = c("Dacarbazine" = "#E69F00", "Nivolumab" = "#56B4E9")
  ) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 15))

p_hr2 = p_hr2 +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_continuous(limits = c(0, 15)) +
  #scale_y_continuous(limits = c(0, 6)) +
  scale_y_continuous(limits = c(0, 1.2)) +
  geom_hline(
    yintercept = hr2,
    linetype = "dashed",
    linewidth = 1
  )


wrap_plots(
  # Proportional hazards
  p_km1, p_h1, p_hr1,
  # Non-proportional hazards
  p_km2, p_h2, p_hr2
) +
  plot_layout(ncol = 2, byrow = FALSE, axes = "collect_x") &
  xlab("\nTime (months)")


ggsave(
  "fig02-ph_nph_data.pdf",
  path = fs$path("paper", "figures"),
  width = 10, height = 10
)

ggsave("fig02-ph_nph_data.pdf", path = fs$path("paper", "figures"))

# Clean environment for next figure
rm(list = ls())
box::purge_cache()

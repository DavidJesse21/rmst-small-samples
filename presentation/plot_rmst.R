# Do it all by myself ----

library(survival)
library(data.table)

ggrmst = function(fit, cutoff,
                  color = "#0072B2", linewidth = 1, alpha = 0.3,
                  ...) {
  # Extract survival data
  dt_surv = data.table(
    time = c(0, fit$time),
    surv = c(1, fit$surv)
  )
  
  # Plot of the survival curve
  p = ggplot(dt_surv, aes(time, surv)) +
    geom_step(linewidth = linewidth, color = color) +
    theme_bw()
  
  # Create data.table for plotting RMST
  dt_rmst = dt_surv[time <= cutoff]
  # Area should be drawn until the specified cutoff and not end before
  if (dt_rmst[.N, time < cutoff]) {
    dt_rmst = rbindlist(list(
      dt_rmst, data.table(time = cutoff, surv = dt_rmst[.N, surv])
    ))
  }
  # For geom_ribbon()
  dt1 = copy(dt_rmst)[, id := "b"]
  dt2 = copy(dt_rmst)[, id := "a"]
  dt2[, surv := shift(surv)]
  dt_rmst = rbindlist(list(dt1, dt2))
  setorder(dt_rmst, time, id)

  # Plot RMST
  p = p +
    geom_ribbon(
      aes(x = time, ymin = 0, ymax = surv), data = dt_rmst,
      fill = color, alpha = alpha
    ) +
    geom_vline(
      xintercept = cutoff,
      linetype = "dashed", color = "#333333", linewidth = 0.75 * linewidth
    )
  
  return(p)
}

ggrmst(fit, cutoff = 750) +
  labs(x = "\nTime", y = "Survival probability\n")

geom_ribbon(
  aes(x = time, ymin = 0, ymax = surv), data = dt_area[time <= 750],
  fill = "#0072B2", alpha = 0.3
)

fit = survfit(Surv(time, status - 1) ~ 1, data = survival::lung)



x = ggrmst(fit, cutoff = 750)
x = x[time <= 750]
if (!x[.N, time == 750]) {
  rbindlist(list(x, data.table(time = 750, surv = x[.N, surv])))
}
x[.N, time == 750]

dt = data.table(
  time = c(0, fit$time),
  surv = c(1, fit$surv),
  n.event = c(0, fit$n.event),
  n.censor = c(0, fit$n)
)

x = dt_area[time <= 750]
x[.N, time == 750]

dt1 = copy(dt)[, id := "b"]
dt2 = copy(dt)[, id := "a"]
dt2[, surv := lag(surv)]
dt_area = rbindlist(list(dt1, dt2))
setorder(dt_area, time, id)
dt_area

# predict_surv = function(time) {
#   summary(fit, times = time)$surv
# }

p = ggplot(dt, aes(time, surv)) +
  geom_step(linewidth = 1, color = "#0072B2") +
  theme_bw()

p +
  geom_ribbon(
    aes(x = time, ymin = 0, ymax = surv), data = dt_area[time <= 750],
    fill = "#0072B2", alpha = 0.3
  )


library(dplyr)

df <- tibble(x = seq(10), y = sample(10))

df_areaStep <- bind_rows(old = df, 
                         new = df %>% mutate(y = lag(y)),
                         .id = "source") %>%
  arrange(x, source)

ggplot(df, aes(x,y)) + 
  geom_ribbon(aes(x = x, ymin = 0, ymax = y), data = df_areaStep)

fit = survfit(Surv(time, status) ~ 1, data = survival::veteran)

d = data(package = "survival")


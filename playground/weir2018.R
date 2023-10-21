options(box.path = "R")

# Packages
box::use(
  data.table[...],
  survival[Surv, survfit, coxph, cox_zph = cox.zph],
  survminer[ggsurvplot],
  ggplot2[...]
)

# Own functions
box::use(
  rmst/km[rmst, rmst_diff]
)

# Data
url = "https://raw.githubusercontent.com/iweir/powerRMST/master/all_reconstructed_data.txt"
dt = fread(url)
setnames(dt, new = tolower)

# Just some summary statistics about size of trials
dt[, .N, by = id][, summary(N)]
dt[, unique(id)]

# Test trial data for NPH
test_ph = function(data) {
  m = coxph(Surv(time, event) ~ arm, data = data)
  test = cox_zph(m)
  test$table[1, "p"]
}

# Plot trial data
plot_surv = function(data) {
  m = survfit(Surv(time, event) ~ arm, data = data)
  ggsurvplot(m, data = data)
}

# Identify trials with evidence of NPH
vapply(dt[, unique(id)], \(x) test_ph(dt[id == x]), numeric(1)) |>
  setNames(dt[, unique(id)]) |>
  sort()

invisible(lapply(
  c(9, 19, 22, 24, 1),
  function(x) {
    p = plot_surv(dt[id == x]) + ggtitle(sprintf("Trial %d", x))
    print(p)
  }
))

# Investigate event/censoring rates
invisible(lapply(
  c(9, 19, 22, 24, 1),
  function(x) {
    cat("Trial ", x, "\n")
    with(dt[id == x], table(arm, event)) |>
      print()
    cat("\n")
  }
))

# Calculate RMST for the NPH trials
rmst_by_group = function(data, tau) {
  li_dt = split(data, by = "arm")
  lapply(li_dt, function(x) {
    rmst(Surv(time, event) ~ 1)
    rmst(time = time, status = event, data = x, tau = tau)
  })
}

rmst(Surv(time, event) ~ arm, data = dt[id == 9], cutoff = 9)
rmst(Surv(time, event) ~ arm, data = dt[id == 19], cutoff = 125)
rmst(Surv(time, event) ~ arm, data = dt[id == 22], cutoff = 55)
rmst(Surv(time, event) ~ arm, data = dt[id == 24], cutoff = 50)
rmst(Surv(time, event) ~ arm, data = dt[id == 1], cutoff = 300)

rmst_diff(
  Surv(time, event) ~ arm, data = dt[id == 9],
  cutoff = 9, contrast = c("1", "0")
)
rmst_diff(
  Surv(time, event) ~ arm, data = dt[id == 19],
  cutoff = 125, contrast = c("1", "0")
)
rmst_diff(
  Surv(time, event) ~ arm, data = dt[id == 22],
  cutoff = 55, contrast = c("1", "0")
)
rmst_diff(
  Surv(time, event) ~ arm, data = dt[id == 24],
  cutoff = 50, contrast = c("1", "0")
)
rmst_diff(
  Surv(time, event) ~ arm, data = dt[id == 1],
  cutoff = 300, contrast = c("1", "0")
)


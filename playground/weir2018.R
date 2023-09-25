box::use(
  data.table[...],
  survival[Surv, survfit, coxph, cox_zph = cox.zph],
  survminer[ggsurvplot],
  ggplot2[...]
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

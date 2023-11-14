options(box.path = "R")

box::use(
  rmst/km[rmst_diff_boot]
)

box::use(
  fs,
  data.table[...],
  survival[Surv]
)

load(fs$path("data", "Hellmann", ext = "Rdata"))
setDT(data)
data

set.seed(42)
x = rmst_diff_boot(
  Surv(time, event) ~ group, data = data,
  cutoff = 18, contrast = c("0", "1"), var_method = "nelson_aalen",
  num_samples = 10000L, light = FALSE
)
x$asymptotic
x$boot_ci
x$boot_rmstd[, 1] |>
  var()

# studentized bootstrap
boot_tstat = apply(
  x$boot_rmstd, MARGIN = 1L,
  function(y) {
    out = (y["diff"] - x$asymptotic["diff"]) / sqrt(y["var_diff"])
    unname(out)
  }
)
boot_tstat_quants = quantile(boot_tstat, probs = c(0.025, 0.975))
boot_var = unname(var(x$boot_rmstd[, "diff"]))

unname(c(
  x$asymptotic["diff"] + boot_tstat_quants[1] * sqrt(boot_var),
  x$asymptotic["diff"] + boot_tstat_quants[2] * sqrt(boot_var)
)) |>
  round(2)



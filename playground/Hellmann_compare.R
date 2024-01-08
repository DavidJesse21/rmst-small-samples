options(box.path = "R")

box::use(
  fs,
  data.table[...],
  survival[Surv, survfit, pseudo],
  eventglm[rmeanglm]
)

box::use(
  rmst/km[rmst_diff_studperm],
  rmst/pseudo[pseudo_strat, pseudo_infjack, rmst_pseudo_test, boot_pseudo],
)


load(fs$path("data", "Hellmann", ext = "Rdata"))
dt = data
rm(data)
setDT(dt)
dt


#' Studentized permutation
#' 
#' Expected results:
#' 
#' - point estimate: -4.02
#' - p-value: 1.1%
#' - confidence interval: [-7.09, -0.96]

profvis::profvis({
  set.seed(150918)
  x1 = rmst_diff_studperm(
    Surv(time, event) ~ group, data = dt,
    cutoff = 18, contrast = c("0", "1"), var_method = "nelson_aalen",
    num_samples = 5000L
  )
  x1$asymptotic["pval"] * 100
  x1$permutation$pval * 100
  round(x1$permutation$confint, 2)
})

set.seed(150918)
x1 = rmst_diff_studperm(
  Surv(time, event) ~ group, data = dt,
  cutoff = 18, contrast = c("0", "1"), var_method = "nelson_aalen",
  num_samples = 5000L
)
x1$asymptotic["pval"] * 100
x1$permutation$pval * 100
round(x1$permutation$confint, 2)


# Pseudo-observations stratified
profvis::profvis({
  m2 = rmeanglm(
    Surv(time, event) ~ group, data = dt, time = 18,
    model.censoring = pseudo_strat, formula.censoring = ~ group
  )
  x2 = rmst_pseudo_test(m2)
  round(x2, 3)
  round(x2[2, "pval"] * 100, 3)
})

m2 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 18,
  model.censoring = pseudo_strat, formula.censoring = ~ group
)
x2 = rmst_pseudo_test(m2)
round(x2, 3)
round(x2[2, "pval"] * 100, 3)


# Pseudo-observations infinitesimal jackknife
m3 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 18,
  model.censoring = pseudo_infjack, formula.censoring = ~ group
)
x31 = rmst_pseudo_test(m3)
round(x31, 3)
round(x31[2, "pval"] * 100, 3)

# Pseudo-observations infinitesimal jackknife using the bootstrap
profvis::profvis({
  set.seed(150918)
  time_boot = system.time({
    m3_boot = boot_pseudo(m3)
  })
  x32 = rmst_pseudo_test(m3_boot, vcov_type = "boot")
  round(x32, 3)
  round(x32[2, "pval"] * 100, 3)
})

set.seed(150918)
time_boot = system.time({
  m3_boot = boot_pseudo(m3)
})
x32 = rmst_pseudo_test(m3_boot, vcov_type = "boot")
round(x32, 3)
round(x32[2, "pval"] * 100, 3)

y = x32[2, ]
c(y["est"] - qnorm(0.975) * sqrt(y["var_est"]), y["est"] + qnorm(0.975) * sqrt(y["var_est"])) |>

quantile(m3_boot$boot[, 2], probs = c(0.025, 0.975))

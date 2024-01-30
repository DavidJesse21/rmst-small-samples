options(box.path = "R")

box::use(
  fs,
  data.table[...],
  survival[Surv, survfit, pseudo],
  eventglm[rmeanglm]
)

box::use(
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  rmst/pseudo[pseudo_strat, pseudo_infjack, rmst_pseudo_test, boot_pseudo, rmst_pseudo_boot_test],
)


dt = survival::ovarian
setDT(dt)
setnames(dt, old = c("futime", "fustat", "rx"), new = c("time", "event", "group"))
dt[, group := group - 1L]

plot(survfit(Surv(time, event) ~ group, data = dt), col = 19:20)
abline(v = 450)


# Asymptotic test ----

x1 = rmst_diff_test(
  Surv(time, event) ~ group, data = dt,
  cutoff = 450, contrast = c("1", "0"), var_method = "greenwood"
)
round(x1, 3)


# Studentized permutation test ----

set.seed(42)
x1 = rmst_diff_studperm(
  Surv(time, event) ~ group, data = dt,
  cutoff = 450, contrast = c("1", "0"), var_method = "nelson_aalen",
  num_samples = 5000L, light = TRUE
)
round(x1$asymptotic["pval"] * 100, 2)
round(x1$permutation$pval * 100, 2)
round(x1$permutation$confint, 2)


# Pseudo-observations, HC3 ----

m2 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 450,
  model.censoring = pseudo_strat, formula.censoring = ~ group
)
x2 = rmst_pseudo_test(m2)
round(x2, 2)
round(x2[2, "pval"] * 100, 2)
round(c(
  x2[2, "est"] - qnorm(0.975) * sqrt(x2[2, "var_est"]),
  x2[2, "est"] + qnorm(0.975) * sqrt(x2[2, "var_est"])
), 2)


# Pseudo-observations infjack, HC3 ----

m3 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 450,
  model.censoring = pseudo_infjack, formula.censoring = ~ group
)
x31 = rmst_pseudo_test(m3)
round(x31, 2)
round(x31[2, "pval"] * 100, 2)
round(c(
  x31[2, "est"] - qnorm(0.975) * sqrt(x31[2, "var_est"]),
  x31[2, "est"] + qnorm(0.975) * sqrt(x31[2, "var_est"])
), 2)


# Bootstrap hypothesis testing ----

box::help(rmst_pseudo_boot_test)

set.seed(42)
m3 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 450,
  model.censoring = pseudo_infjack, formula.censoring = ~ group
)
m3_boot_test = rmst_pseudo_boot_test(m3)

round(m3_boot_test[2, ], 2)
m3_boot_test[2, ]


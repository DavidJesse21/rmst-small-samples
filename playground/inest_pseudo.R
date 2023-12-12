options(box.path = "R")

box::use(
  data.table[...],
  eventglm[rmeanglm, pseudo_independent, pseudo_stratified,
           pseudo_aareg, pseudo_coxph],
  survival[Surv, survfit]
)

box::use(
  rmst/pseudo[
    pseudo_independent2, pseudo_stratified2,
    pseudo_fpm1_independent, pseudo_fpm1_stratified
  ]
)


# Data
dt = survival::veteran
setDT(dt)
for (j in setdiff(colnames(dt), c("trt", "time", "status"))) {
  set(dt, j = j, value = NULL)
}
dt = dt[time < 600]
setorder(dt, time)
dt

dt[, max(time), by = trt]
dt[time == 587]
dt[time == 553]


# Independent censoring ----

x1 = pseudo_independent(Surv(time, status) ~ 1, data = dt, time = 500, type = "rmean")
x2 = pseudo_independent2(Surv(time, status) ~ 1, data = dt, time = 500, type = "rmean")
x3 = pseudo_fpm1_independent(Surv(time, status) ~ 1, data = dt, time = 500, type = "rmean")
cbind(x1, x2, x3) |>
  View()
vapply(list(x1, x2, x3), mean, numeric(1))


x1 = pseudo_independent(Surv(time, status) ~ 1, data = dt, time = 600, type = "rmean")
x2 = pseudo_independent2(Surv(time, status) ~ 1, data = dt, time = 600, type = "rmean")
x3 = pseudo_fpm1_independent(Surv(time, status) ~ 1, data = dt, time = 600, type = "rmean")
cbind(x1, x2, x3) |>
  View()
vapply(list(x1, x2, x3), mean, numeric(1))


# Dependent censoring ----

x1 = pseudo_stratified(
  Surv(time, status) ~ 1, data = dt, time = 500, type = "rmean",
  formula.censoring = ~ factor(trt)
)
x2 = pseudo_stratified2(
  Surv(time, status) ~ 1, data = dt, time = 500, type = "rmean",
  formula.censoring = ~ factor(trt)
)
x3 = pseudo_fpm1_stratified(
  Surv(time, status) ~ 1, data = dt, time = 500, type = "rmean",
  formula.censoring = ~ factor(trt)
)
cbind(x1, x2, x3) |>
  View()
vapply(list(x1, x2, x3), mean, numeric(1))


x1 = pseudo_stratified(
  Surv(time, status) ~ 1, data = dt, time = 600, type = "rmean",
  formula.censoring = ~ factor(trt)
)
x2 = pseudo_stratified2(
  Surv(time, status) ~ 1, data = dt, time = 600, type = "rmean",
  formula.censoring = ~ factor(trt)
)
x3 = pseudo_fpm1_stratified(
  Surv(time, status) ~ 1, data = dt, time = 600, type = "rmean",
  formula.censoring = ~ factor(trt)
)
cbind(x1, x2, x3) |>
  View()
vapply(list(x1, x2, x3), mean, numeric(1))

all.equal(round(x1_500, 5), round(x2_500, 5))


# Models ----

m1 = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 500,
  formula.censoring = ~ factor(trt), model.censoring = pseudo_stratified2
)
summary(m1)
m2 = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 500,
  formula.censoring = ~ factor(trt), model.censoring = pseudo_fpm1_stratified
)
summary(m2)


m1 = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 600,
  formula.censoring = ~ factor(trt), model.censoring = pseudo_stratified2
)
summary(m1)
m2 = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 600,
  formula.censoring = ~ factor(trt), model.censoring = pseudo_fpm1_stratified
)
summary(m2)

# ~ 20 seconds
time_taken = system.time(
  rmeanglm(
    Surv(time, status) ~ factor(trt), data = dt, time = 600,
    formula.censoring = ~ factor(trt), model.censoring = pseudo_fpm1_stratified
  )
)

time_taken = system.time(
  rmeanglm(
    Surv(time, status) ~ factor(trt), data = dt[idx], time = 600,
    formula.censoring = ~ factor(trt), model.censoring = pseudo_fpm1_stratified
  )
)

idx = sample(1:nrow(dt), size = 60, replace = FALSE)

dt[]

options(box.path = "R")

box::use(
  data.table[...],
  eventglm[rmeanglm, pseudo_independent, pseudo_stratified,
           pseudo_aareg, pseudo_coxph],
  survival[Surv, survfit]
)

box::use(eventglm[get_pseudo_rmean])
eventglm:::get_pseudo_rmean


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


# Produces NaN for largest of all observations...
x1 = pseudo_independent(Surv(time, status) ~ 1, data = dt, time = 600, type = "rmean")

# What happens insdied of pseudo_independent() ? ----

f = Surv(time, status) ~ 1
margformula <- update.formula(f, . ~ 1)
mr <- model.response(model.frame(margformula, data = dt))
stopifnot(attr(mr, "type") %in% c("right", "mright"))
marginal.estimate <- survival::survfit(margformula, data = dt)
# Here the NaN is produced
POi <- eventglm:::get_pseudo_rmean(marginal.estimate, time = 600, cause = 1, mr)

# What happens inside of get_pseudo_rmean() ? ----

Smi = eventglm::leaveOneOut.survival(marginal.estimate, times = 600, mr)
Smi
dim(Smi) # number of observations x unique time points
which(is.nan(Smi), arr.ind = TRUE) 



# Error
rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 554,
  model.censoring = "stratified", formula.censoring = ~ factor(trt)
)
# Works
rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 553,
  model.censoring = "stratified", formula.censoring = ~ factor(trt)
)
# Works
rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 554,
  model.censoring = "coxph", formula.censoring = ~ factor(trt)
)
# 
rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 554,
  model.censoring = "aareg", formula.censoring = ~ factor(trt)
)

# Error
rmeanglm(Surv(time, status) ~ factor(trt), data = dt, time = 588)
# Works
rmeanglm(Surv(time, status) ~ factor(trt), data = dt, time = 587)


x = pseudo_independent(Surv(time, status) ~ 1, time = 500, data = dt, type = "rmean")
x
mean(x)

x = pseudo_stratified(
  Surv(time, status) ~ factor(trt), time = 500, data = dt, type = "rmean",
  formula.censoring = ~ factor(trt)
)
mean(x)

x = pseudo_aareg(
  Surv(time, status) ~ factor(trt), time = 500, data = dt, type = "rmean",
  formula.censoring = ~ factor(trt), ipcw.method = "hajek"
)
mean(x)

x = pseudo_coxph(
  Surv(time, status) ~ factor(trt), time = 500, data = dt, type = "rmean",
  formula.censoring = ~ factor(trt), ipcw.method = "hajek"
)
mean(x)

x = pseudo_independent(
  Surv(time, status) ~ factor(trt), time = 500, data = dt, type = "rmean"
)
mean(x)

dt[, pseudo := x]
dt[order(time)]


# Test ----

x1 = survfit(Surv(time, status) ~ 1, data = dt[-1])

pseudo_survfit1 = function(formula, data, i, cutoff = 400) {
  fit_marg = survfit(formula, data = data, se.fit = FALSE) |>
    summary(times = cutoff)
  fit_loo = survfit(formula, data = data[-i], se.fit = FALSE) |>
    summary(times = cutoff)
  
  n = nrow(data)
  
  pseudo = n * fit_marg$surv - (n - 1) * fit_loo$surv
  
  return(pseudo)
}

# agreement
pseudo_survfit1(Surv(time, status) ~ 1, data = dt, i = 100)
pseudo_independent(Surv(time, status) ~ 1, data = dt, time = 400, type = "survival")[100]

#
x = pseudo_stratified(
  Surv(time, status) ~ factor(trt), data = dt, time = 400, type = "survival",
  formula.censoring = ~ factor(trt)
)

x2 = lapply(
  1:2,
  function(group) {
    data = dt[trt == group]
    pseudos = vapply(
      1:nrow(data),
      function(i) {
        pseudo_survfit1(Surv(time, status) ~ 1, data = data, i = i)
      },
      numeric(1)
    )
  } 
)
x2 = do.call(c, x2)

cbind(x, x2) |> View()

x21 = vapply(
  
)

# Right direction but not the same

pseudo_survfit2 = function(formula, data, i, cutoff = 400) {
  fit_marg = survfit(formula, data = data, se.fit = FALSE) |>
    summary(times = cutoff)
  props_marg = fit_marg$n / sum(fit_marg$n)
  
  fit_loo = survfit(formula, data = data[-i], se.fit = FALSE) |>
    summary(times = cutoff)
  props_loo = fit_loo$n / sum(fit_loo$n)
  
  n = nrow(data)
  
  surv_marg = sum(props_marg * fit_marg$surv)
  surv_loo = sum(props_marg * fit_loo$surv)
  
  pseudo = n * surv_marg - (n - 1) * surv_loo
  
  return(pseudo)
}
pseudo_survfit2(Surv(time, status) ~ factor(trt), data = dt, i = 50)

pseudo_survfit3 = function(formula, data, i, cutoff = 400) {
  
}

x1 = survfit(Surv(time, status) ~ factor(trt), data = dt, se.fit = FALSE)
test = summary(x1, times = 400)
test$surv
sum(((test$n) / sum(test$n)) * test$surv)

test$surv

tau = 400

test = cbind(x1$time, x1$surv)

test = summary(x1, times = 400)

test$surv

test[test[, 1] == 400, ]

test = summary(x1, times = 400)

x1$surv
x1$n / sum(x1$n)


f = Surv(time, status) ~ factor(trt)
x = model.frame(f, data = dt)
y = model.response(x)
interaction(x)

options(box.path = "R")

box::use(
  data.table[...],
  flexsurv[flexsurvspline],
  survival[Surv, survfit],
  eventglm[pseudo_independent, pseudo_stratified, rmeanglm]
)

box::use(
  rmst/km[rmst, rmst_diff]
)


# Data
dt = survival::veteran
setDT(dt)
# for (j in setdiff(colnames(dt), c("trt", "time", "status"))) {
#   set(dt, j = j, value = NULL)
# }
dt = dt[time < 600]
setorder(dt, time)
dt


# Pseudo function one sample ----

pseudo_fpm1 = function(formula, time, cause = 1, data,
                       type = "rmean", #c("cuminc", "survival", "rmean"),
                       formula.censoring = NULL, ipcw.method = NULL) {
  margformula = update.formula(formula, . ~ 1)
  
  # Estimate using all data
  fit_marg = flexsurvspline(margformula, data = data, k = 1)
  
  theta = switch(type,
    "survial" = {
      x = predict(fit_marg, type = "survival", times = time, se.fit = FALSE, newdata = data.table(1))
      x$.pred_survival
    },
    "rmean" = {
      x = predict(fit_marg, type = "rmst", times = time, se.fit = FALSE, newdata = data.table(1))
      x$.pred_rmst
    }
  )
  
  # Jackknife estimates
  theta_jack = vapply(
    1:nrow(data),
    function(i) {
      fit_i = flexsurvspline(margformula, data = data[-i], k = 1, inits = coef(fit_marg))
      
      theta_i = switch(type,
        "survival" = {
          x = predict(fit_i, type = "survival", times = time, se.fit = FALSE, newdata = data.table(1))
          x$.pred_survival
        },
        "rmean" = {
          x = predict(fit_i, type = "rmst", times = time, se.fit = FALSE, newdata = data.table(1))
          x$.pred_rmst
        }
      )
      
      return(theta_i)
    },
    numeric(1)
  )
  
  # Calculate pseudo-observations
  pseudo_obs = theta  + (nrow(data) - 1) * (theta - theta_jack)
  return(pseudo_obs)
}

time_taken = system.time({
  po_fpm = pseudo_fpm2(Surv(time, status) ~ 1, data = dt, time = 600)
})
time_taken
mean(po_fpm)
# Comparison
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 600)

x1 = rmeanglm(
  Surv(time, status) ~ factor(trt), time = 500, data = dt,
  model.censoring = "independent"
)
summary(x1)

x2 = rmeanglm(
  Surv(time, status) ~ factor(trt), time = 500, data = dt,
  model.censoring = pseudo_fpm2
)
summary(x2)

rmst_diff(Surv(time, status) ~ trt, data = dt, cutoff = 500, contrast = c("2", "1"))


# Pseudo function stratified ----

pseudo_fpm1_stratified = function(formula, time, cause = 1, data,
                                  type = "rmean",
                                  formula.censoring = NULL, ipcw.method = NULL) {
  margformula = update.formula(formula, . ~ 1)
  mr = model.response(model.frame(margformula, data = data))
  
  # Get all strata levels
  mfout = model.frame(formula.censoring, data = data)
  strata = interaction(mfout)
  li_idx = lapply(levels(strata), \(j) which(strata == j))
  
  #
  out = numeric(nrow(data))
  pseudo_obs = lapply(li_idx, function(stratum) {
    pseudo_fpm3(formula, time, cause, data[stratum], type, formula.censoring = NULL, ipcw.method = NULL)
  })
  for (i in seq_along(pseudo_obs)) {
    out[li_idx[[i]]] = pseudo_obs[[i]]
  }
  
  return(out)
}


# Compare everything ----

m1 = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 500,
  model.censoring = "independent"
)
summary(m1)

m2 = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 500,
  model.censoring = "stratified", formula.censoring = ~ factor(trt)
)
summary(m2)

m3 = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 500,
  model.censoring = pseudo_fpm2, formula.censoring = ~ factor(trt)
)
summary(m3)

m4 = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 500,
  model.censoring = pseudo_fpm2_stratified, formula.censoring = ~ factor(trt)
)
summary(m4)

x = do.call(rbind, lapply(list(m1, m2, m3, m4), coef))
rownames(x) = c(
  paste0("KM ", c("ind.", "strat.")),
  paste0("FPM ", c("ind.", "strat."))
)
x


# Additional dataset ----

dt = eventglm::colon
setDT(dt)
dt[, status := fifelse(event == "death", 1, 0)]


survfit(Surv(time, status) ~ rx, data = dt) |>
  plot()

rmst_diff(Surv(time, status) ~ rx, data = dt, cutoff = 2250, contrast = c("Lev+5FU", "Obs"))

m1 = rmeanglm(
  Surv(time, status) ~ rx, data = dt, time = 2250,
  model.censoring = "independent"
)
summary(m1)

m2 = rmeanglm(
  Surv(time, status) ~ rx, data = dt, time = 2250,
  model.censoring = "stratified", formula.censoring = ~ rx
)
summary(m2)

m3 = rmeanglm(
  Surv(time, status) ~ rx, data = dt, time = 2250,
  model.censoring = pseudo_fpm2, formula.censoring = ~ rx
)
summary(m3)

m4 = rmeanglm(
  Surv(time, status) ~ rx, data = dt, time = 2250,
  model.censoring = pseudo_fpm2_stratified, formula.censoring = ~ rx
)
summary(m4)

x = do.call(rbind, lapply(list(m1, m2, m3, m4), coef))
rownames(x) = c(
  paste0("KM ", c("ind.", "strat.")),
  paste0("FPM ", c("ind.", "strat."))
)
x


# Another example ----

dt = simtrial::ex1_delayed_effect
setDT(dt)
dt

survfit(Surv(month, evntd) ~ trt, data = dt) |>
  plot()

rmst_diff(Surv(month, evntd) ~ trt, data = dt, cutoff = 12, contrast = c("1", "0"))

m1 = rmeanglm(Surv(month, evntd) ~ trt, data = dt, time = 12, model.censoring = "independent")
m2 = rmeanglm(
  Surv(month, evntd) ~ trt, data = dt, time = 12,
  model.censoring = "stratified", formula.censoring = ~ trt
)
m3 = rmeanglm(
  Surv(month, evntd) ~ trt, data = dt, time = 12,
  model.censoring = pseudo_fpm2, formula.censoring = ~ trt
)
m4 = rmeanglm(
  Surv(month, evntd) ~ trt, data = dt, time = 12,
  model.censoring = pseudo_fpm2_stratified, formula.censoring = ~ trt
)

x = do.call(rbind, lapply(list(m1, m2, m3, m4), coef))
rownames(x) = c(
  paste0("KM ", c("ind.", "strat.")),
  paste0("FPM ", c("ind.", "strat."))
)
x

for (m in list(m1, m2, m3, m4)) print(summary(m))

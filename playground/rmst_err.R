options(box.path = "R")

box::use(
  data.table[...],
  survival[Surv, survfit]
)

box::use(
  rmst/km[rmst]
)

rmst_surv = function(formula, data, cutoff) {
  fit = survfit(formula, data)
  smry = summary(fit, rmean = cutoff)
  smry$table[5:6]
}


# Example data ----

dt = survival::veteran
setDT(dt)
for (j in setdiff(colnames(dt), c("trt", "time", "status"))) {
  set(dt, j = j, value = NULL)
}

dt = dt[order(time)]
# remove 2 largest observations
dt = dt[time < 900]
# Now the largest event time is 600
dt[, max(time)]


# Last event time observed ----

# As long as the last event has been observed computing the RMST with a cutoff (restriction time) 
# that is greater than the last event time should be no problem

# Own function
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 600)
# survRM2
with(dt, survRM2:::rmst1(time, status, 600))$rmst
# survival
rmst_surv(Surv(time, status) ~ 1, dt, cutoff = 600)


# Last event time censored ----

# Assume last observation is censored
dt[.N, status := 0L]

# The RMST functions should now actually not work or at least produce a warning, since 
# the RMST is not uniquely defined beyond the latest event time, which is now censored.

# Own function
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 600)
# survRM2
with(dt, survRM2:::rmst1(time, status, 600))$rmst
# survival
rmst_surv(Surv(time, status) ~ 1, dt, cutoff = 600)

# All three functions still work like before and produce the same results.


# eventglm (pseudo-observations) ----

box::use(
  eventglm[rmeanglm]
)

m = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 400,
  model.censoring = "stratified", formula.censoring = ~ factor(trt)
)
summary(m)

m = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 600,
  model.censoring = "stratified", formula.censoring = ~ factor(trt)
)

# !!!
m = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 553,
  model.censoring = "stratified", formula.censoring = ~ factor(trt)
)
m = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 554,
  model.censoring = "stratified", formula.censoring = ~ factor(trt)
)
m = rmeanglm(
  Surv(time, status) ~ factor(trt), data = dt, time = 554#,
  #model.censoring = "stratified", formula.censoring = ~ factor(trt)
)



summary(m)

traceback()


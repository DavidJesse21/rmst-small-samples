options(box.path = "R")

box::use(
  data.table[...],
  survival[Surv]
)

box::use(
  rmst/km[rmst]
)

dt = survival::veteran
setDT(dt)

for (j in setdiff(colnames(dt), c("trt", "time", "status"))) {
  set(dt, j = j, value = NULL)
}

dt = dt[order(time)]
# remove 2 largest observations
dt = dt[time < 900]

# This should work with no problems
# rmst(Surv(time, status) ~ 1, data = dt, cutoff = 400)

dt[, max(time)]

# This should work although cutoff is larger than largest event time as long as 
# we have observed the last event time.
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 600)



# assume last observation is censored
# this should actually not work
dt[.N, status := 0L]
rmst(Surv(time, status) ~ 1, data = dt, cutoff = 600)
# but it does work

# How does rmst2 handle this?
with(
  dt,
  survRM2:::rmst1(time, status, 600)
)
# returns the same estimates
with(
  dt,
  survRM2perm:::rmst1_update(time, status, 600)
)

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
traceback()

survRM2perm:::rmst1_update()

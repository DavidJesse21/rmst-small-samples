options(box.path = "R")

box::use(
  fs,
  bt = batchtools,
  data.table[...]
)

box::use(
  bt2 = simfuns/analyze,
  simfuns/problems[gen_data_exp],
  rmst/km[rmst_diff_studperm],
  rmst/pseudo[pseudo_fpm1_independent, pseudo_fpm1_stratified]
)

box::use(
  eventglm[rmeanglm],
  flexsurv[flexsurvspline],
  survival[Surv]
)


reg = bt$loadRegistry(
  fs$path("simresults", "registry"), writeable = TRUE,
  work.dir = getwd()
)

set.seed(311)
params = bt$getJobPars(311)$prob.pars[[1]][-1]
dt = do.call(gen_data_exp, c(list(data = NULL, job = NULL), params))

pobs = pseudo_fpm1_stratified(Surv(time, event) ~ trt, data = dt, formula.censoring = ~ trt, time = 10)
m0 = flexsurvspline(Surv(time, event) ~ 1, data = dt[trt == 0], k = 1)
m1 = flexsurvspline(Surv(time, event) ~ 1, data = dt[trt == 1], k = 1)

dists = flexsurv::flexsurv.dists
names(dists)
dists$weibull$inits

dt[trt == 1, log(median(time))]


pobs0 = pseudo_fpm1_independent(Surv(time, event) ~ 1, data = dt[trt == 0], time = 10)
pobs1 = pseudo_fpm1_independent(Surv(time, event) ~ 1, data = dt[trt == 1], time = 10)

coef(m1)

for (i in 1:nrow(dt[trt == 1])) {
  cat("Leaving out observation no. ", i, "\n")
  flexsurvspline(Surv(time, event) ~ 1, data = dt[trt == 1][-i], k = 1, inits = coef(m1)) |>
    try()
}
# 9, 14, 19, 21, 22, 24, 29, 30
# 27

m1$knots

flexsurvspline(Surv(time, event) ~ 1, data = dt[trt == 1][-27], k = 1)
err = flexsurvspline(Surv(time, event) ~ 1, data = dt[trt == 1][-9], k = 1) |>
  try()
err[[1]]
err2 =attr(err, "condition")
err2$call

dt[trt == 1, median(log(time))]

dt[trt == 1, summary(log(time))]
dt[trt == 1][-9, summary(log(time))]



bt$testJob(290)

pobs = pseudo_fpm1_stratified(Surv(time, event) ~ trt, data = dt[], formula.censoring = ~ trt, time = 10)
pobs

box::use(
  flexsurv[flexsurvspline],
  survival[Surv]
)

box::use(
  rmst/pseudo[pseudo_fpm1_stratified]
)

idx = 1:100

bt$chunk(idx, chunk.size = 50, shuffle = FALSE)

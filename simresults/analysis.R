# Initial test / demonstration of how results from the simulation could be analyzed

options(box.path = "R")

box::use(
  fs,
  bt = batchtools,
  data.table[...]
)

box::use(
  bt2 = simfuns/analyze
)
names(bt2)

reg = bt$loadRegistry(fs$path("simresults", "registry"))

params = bt2$get_job_pars(unwrap = 1)


# ph_exp ----

# Null scenario
idx = params[problem == "ph_exp" & rmst_diff == 0]$job.id
res = bt2$get_results(idx, "value")

bt2$summarize_rejections(res)

res_ci = bt2$get_ci_metrics(res, true_vals = rep(0, nrow(res)))
bt2$summarize_cis(res_ci)

# Alt. scenario
idx = params[problem == "ph_exp" & rmst_diff == 1.5]$job.id
res = bt2$get_results(idx, "value")

bt2$summarize_rejections(res)

res_ci = bt2$get_ci_metrics(res, true_vals = rep(1.5, nrow(res)))
bt2$summarize_cis(res_ci)

excep = bt2$get_results(idx, c("error", "warning"))
bt2$summarize_exceptions(excep, what = "error", by = c("method", "message"))

# crossing_pwexp ----

# Null scenario
idx = params[problem == "crossing_pwexp" & rmst_diff == 0]$job.id
# idx = 5001:6070
res = bt2$get_results(idx, "value")

bt2$summarize_rejections(res)

res_ci = bt2$get_ci_metrics(res, true_vals = rep(0, nrow(res)))
bt2$summarize_cis(res_ci)

# Alt. scenario
idx = params[problem == "crossing_pwexp" & rmst_diff == 1.5]$job.id
res = bt2$get_results(idx, "value")

bt2$summarize_rejections(res)

res_ci = bt2$get_ci_metrics(res, true_vals = rep(1.5, nrow(res)))
bt2$summarize_cis(res_ci)


# crossing_wb ----

# Null scenario
idx = params[problem == "crossing_wb" & rmst_diff == 0]$job.id
res = bt2$get_results(idx, "value")

bt2$summarize_rejections(res)

res_ci = bt2$get_ci_metrics(res, true_vals = rep(0, nrow(res)))
bt2$summarize_cis(res_ci)

# Alt. scenario
idx = params[problem == "crossing_wb" & rmst_diff == 1.5]$job.id
res = bt2$get_results(idx, "value")

bt2$summarize_rejections(res)

res_ci = bt2$get_ci_metrics(res, true_vals = rep(1.5, nrow(res)))
bt2$summarize_cis(res_ci)


# Errors ----

# ph_exp null scenario
# Null scenario
idx = params[problem == "ph_exp" & rmst_diff == 0]$job.id
res = bt2$get_results(idx, "error")
err = res[!is.na(error)]
err

bt$unwrap(err)

box::use(
  simfuns/problems[gen_data_exp],
  rmst/km[rmst_diff_studperm]
)

box::use(
  eventglm[rmeanglm]
)

params = bt$getJobPars(521)$prob.pars[[1]][-1]
set.seed(reg$seed + 521)
dt = do.call(gen_data_exp, c(list(data = NULL, job = NULL), params))

# x = rmst_diff_studperm(
#   Surv(time, event) ~ trt, data = dt, cutoff = 10,
#   contrast = c("1", "0"), var_method = "nelson_aalen",
#   num_samples = 2000L, conf_level = (1 - 0.05),
#   light = TRUE
# )

split(dt, by = "trt") |>
  lapply(\(x) quantile(x$time))

m = rmeanglm(
  Surv(time, event) ~ trt, data = dt, time = 10,
  model.censoring = pseudo_fpm1_stratified, formula.censoring = ~ trt
)

summary(m)

pobs = pseudo_fpm1_stratified(Surv(time, event) ~ trt, data = dt, formula.censoring = ~ trt, time = 10)
pobs

box::use(
  flexsurv[flexsurvspline],
  survival[Surv]
)

box::use(
  rmst/pseudo[pseudo_fpm1_stratified]
)

m0 = flexsurvspline(Surv(time, event) ~ 1, data = dt[trt == 0], k = 1)
m1 = flexsurvspline(Surv(time, event) ~ 1, data = dt[trt == 1], k = 1)



m0
m1

bt$getJobTable(290)
bt$getJobPars(290)
bt$testJob(290)
bt$getJobTags(290)
bt$getJobStatus(290)
bt$getErrorMessages()
reg

reg$seed
reg$defs
names(reg)
reg$status
reg$version

bt2$summarize_rejections(res)


# Correlation? ----

# Null scenario
idx = params[problem == "ph_exp" & rmst_diff == 0]$job.id
res = bt2$get_results(idx, "value")

x = res[method == "pseudo"]
x = bt$unwrap(x)

tab = x[, table(pval)]
any(tab >= 2)

x[, summary(pval)]
x[, mean(pval <= 0.05)]

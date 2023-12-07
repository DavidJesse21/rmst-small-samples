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


# crossing_pwexp ----

# Null scenario
idx = params[problem == "crossing_pwexp" & rmst_diff == 0]$job.id
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



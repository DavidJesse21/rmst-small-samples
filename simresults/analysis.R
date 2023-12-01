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

reg = bt$loadRegistry(fs$path("simresults", "registry"))


# Analyze results ----

dt_val = bt2$get_results(what = "value")
dt_err = bt2$get_results(what = "error")
dt_warn = bt2$get_results(what = "warning")
rm(dt_warn)

dt_pars = bt2$get_job_pars(unwrap = 2)

dt = bt2$merge_res_params(dt_val, dt_pars)

bt2$summarize_rejections(dt)


dt_ci = bt2$get_ci_metrics(dt)
dt_ci
bt2$summarize_cis(dt_ci)

bt2$summarize_exceptions(dt_err, by = "method")
bt2$summarize_exceptions(dt_err, by = c("method", "message"))


options(box.path = "R")

box::use(
  fs,
  data.table[...]
)

box::use(
  simfuns2/get_funs[get_scenario_table, get_algo_table],
  simfuns2/analyze[calc_rejection_rates, calc_ci_metrics, setj_samples_alloc, setj_percent]
)

dtr = readRDS(fs$path("simulation", "results", "2024-02-08_results1.rds"))
setDT(dtr)
dts = get_scenario_table()


# Type I error ----

dt1 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 0, scenario.id]],
  stats_NA = FALSE
)
dt1[, reject := round(reject, 3)]
dt1[, controlled := between(reject, 0.044, 0.056)]
dt1

dt1[, mean(controlled), by = algo.id]
dt1[, sum(controlled), by = algo.id]
merge(dt1, dts, by = "scenario.id")[, sum(controlled), by = surv_model]


report_type1err = function(dt, algo_id) {
  dt_err = merge(dt[algo.id == algo_id], dts, by = "scenario.id")
  dt_err[, samples_alloc := sprintf("(%s, %s)", n0/samples_k, n1/samples_k)]
  dt_err[, num_samples := n0 + n1]
  
  cat(sprintf("Algorithm %s", algo_id), " ", paste0(rep("-", 10), collapse = ""), "\n\n")
  
  # By total number of samples
  print(dt_err[, .(n_controlled = sum(controlled)), by = num_samples])
  cat("\n")
  # By sample allocation
  print(dt_err[, .(n_controlled = sum(controlled)), by = samples_alloc])
  cat("\n")
  # By survival model
  print(dt_err[, .(n_controlled = sum(controlled)), by = surv_model])
  cat("\n")
  # By censoring model
  print(dt_err[, .(n_controlled = sum(controlled)), by = cens_model])
  cat("\n")
}

# Asymptotic test
report_type1err(dt1, 1)
# Studentized permutation test
report_type1err(dt1, 2)
# PO
report_type1err(dt1, 3)
# PO Boot
report_type1err(dt1, 5)

# Allocation + distribution
dt_err = merge(dt1[algo.id == 3], dts, by = "scenario.id")
dt_err[, samples_alloc := sprintf("(%s, %s)", n0/samples_k, n1/samples_k)]
dt_err[, num_samples := n0 + n1]
dt_err[, .(n_controlled = sum(controlled)), by = .(surv_model, samples_alloc)][order(surv_model)]


# Power ----

dt2 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 1.5, scenario.id] & algo.id != 4],
  stats_NA = FALSE
)

dt2[, rank := frank(-reject, ties.method = "first"), by = scenario.id]
dt2[, table(algo.id, rank)]


report_power = function(dt, algo_id) {
  dt_pow = merge(dt[algo.id == algo_id], dts, by = "scenario.id")
  dt_pow[, samples_alloc := sprintf("(%s, %s)", n0/samples_k, n1/samples_k)]
  dt_pow[, num_samples := n0 + n1]
  
  cat(sprintf("Algorithm %s", algo_id), " ", paste0(rep("-", 10), collapse = ""), "\n\n")
  
  # By total number of samples
  print(dt_pow[, .(mean_pow = mean(reject)), by = num_samples])
  cat("\n")
  # By sample allocation
  print(dt_pow[, .(mean_pow = mean(reject)), by = samples_alloc])
  cat("\n")
  # By survival model
  print(dt_pow[, .(mean_pow = mean(reject)), by = surv_model])
  cat("\n")
  # By censoring model
  print(dt_pow[, .(mean_pow = mean(reject)), by = cens_model])
  cat("\n")
}

report_power(dt2, 1)
report_power(dt2, 2)
report_power(dt2, 3)
report_power(dt2, 5)


report_power2 = function(dt) {
  dt_pow = merge(dt, dts, by = "scenario.id")
  dt_pow[, samples_alloc := sprintf("(%s, %s)", n0/samples_k, n1/samples_k)]
  dt_pow[, num_samples := n0 + n1]
  
  # By total number of samples
  print(dt_pow[, .(mean_pow = mean(reject)), by = .(algo.id, num_samples)])
  cat("\n")
  # By sample allocation
  print(dt_pow[, .(mean_pow = mean(reject)), by = .(algo.id, samples_alloc)])
  cat("\n")
  # By survival model
  print(dt_pow[, .(mean_pow = mean(reject)), by = .(algo.id, surv_model)])
  cat("\n")
  # By censoring model
  print(dt_pow[, .(mean_pow = mean(reject)), by = .(algo.id, cens_model)])
  cat("\n")
}

report_power2(dt2)

dt_pow = merge(dt2, dts, by = "scenario.id")
dt_pow[surv_model == "crossing_wb", min(reject)]

dt_pow[, min(reject), by = surv_model]
dt_pow[, max(reject), by = surv_model]

dt_pow[, mean(reject), by = cens_model]


# Coverage ----



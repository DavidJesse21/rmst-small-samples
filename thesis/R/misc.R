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

dt1[, uniqueN(scenario.id)]


# Power ----

dt2 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 1.5, scenario.id]],
  stats_NA = FALSE
)

dt2[, rank := frank(-reject), by = scenario.id]

dt2[, table(algo.id, rank)]

dt2

dt2[, frank(reject), by = scenario.id]

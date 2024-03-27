options(box.path = "R")

box::use(
  fs,
  data.table[...],
  ggplot2[...]
)

box::use(
  simfuns2/analyze[calc_rejection_rates, calc_ci_metrics, setj_samples_alloc, setj_percent],
  simfuns2/get_funs[get_scenario_table, get_algo_table]
)


dtr = readRDS(fs$path("simulation", "results", "2024-02-08_results1", ext = "rds"))
setDT(dtr)
dts = get_scenario_table()


# Type I error rates ----

dt1 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 0, scenario.id]],
  stats_NA = FALSE
)
dt1 = merge(
  dt1, dts[rmst_diff == 0, .(scenario.id, num_samples = n0 + n1)],
  by = "scenario.id"
)
setj_percent(dt1, "reject")

dt1
get_algo_table()


dt1[, sum(between(reject, 4.4, 5.6)), by = algo.id]
dt1[num_samples == 30, sum(between(reject, 4.4, 5.6)), by = algo.id]
dt1[num_samples == 60, sum(between(reject, 4.4, 5.6)), by = algo.id]

dt1[algo.id == 2, sum(between(reject, 4.4, 5.6)), by = num_samples]

dt1[num_samples == 180, sum(between(reject, 4.4, 5.6)), by = algo.id]

dt1[algo.id != 4, sum(between(reject, 4.4, 5.6)), by = .(num_samples, algo.id)]

dt1[algo.id == 5][reject == min(reject)]


dt2 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 0, scenario.id]],
  stats_NA = FALSE
)
setj_percent(dt2, "reject")

dt2 = merge(dt2, dts[rmst_diff == 0], by = "scenario.id")
dt2[, alloc := sprintf("(%d, %d)", n0/samples_k, n1/samples_k)]


dt2[, sum(between(reject, 4.4, 5.6)), by = .(alloc)][order(V1)]


# Power ----

dt2 = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 1.5, scenario.id]],
  stats_NA = FALSE
)
dt2 = merge(
  dt2, dts[rmst_diff == 1.5, .(scenario.id, num_samples = n0 + n1)],
  by = "scenario.id"
)
setj_percent(dt2, "reject")

dt2 = dt2[algo.id != 4]
dt2[, rank := frank(-reject), by = scenario.id]

dt2[, table(rank, algo.id)]

frank(5:10)


# Coverage ----

dt3 = calc_ci_metrics(
  merge(dtr, dts[, .(scenario.id, rmst_diff)], by = "scenario.id"),
  stats_NA = FALSE
)
setj_percent(dt3, "coverage")

dt3
dt3[algo.id != 4, sum(between(coverage, 94.4, 95.6)), by = algo.id]

dt3 = merge(dt3, dts[, .(scenario.id, num_samples = n0 + n1)], by = "scenario.id")

dt3[algo.id != 4 & num_samples == 180,
    sum(between(coverage, 94.4, 95.6)), by = algo.id]

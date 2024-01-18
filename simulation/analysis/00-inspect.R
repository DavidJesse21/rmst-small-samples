options(box.path = "R")

box::use(
  fs,
  data.table[...]
)

box::use(
  simfuns2/get_funs[get_scenario_table]
)

dt = readRDS(fs$path("simulation", "results", "2024-01-16_results1", ext = "rds"))
invisible(setalloccol(dt))
dts = get_scenario_table()

# All entries there?
nrow(dt)
anyDuplicated(dt)

# Any missing values?
dt[, sum(is.na(pval))]
dt[, sum(is.na(ci_lower))]
dt[, sum(is.na(ci_upper))]
# (yes)


# Investigation of NA patterns ----

dt[is.na(pval), table(algo.id)]
# By far the most NAs are there for studentized permutation

dt[is.na(pval), table(scenario.id)] |> sort(decreasing = TRUE)
dts[scenario.id %in% c(37, 181, 45, 29)]
# Mostly in very small and unbalanced sample settings

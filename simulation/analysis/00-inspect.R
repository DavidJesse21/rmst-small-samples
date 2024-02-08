options(box.path = "R")

box::use(
  fs,
  data.table[...]
)

box::use(
  simfuns2/get_funs[get_scenario_table]
)

dt = readRDS(fs$path("simulation", "results", "2024-01-16_results1", ext = "rds"))
setDT(dt)
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

dt2 = dt[is.na(pval)]

dt2[algo.id == 4, table(scenario.id)] |>
  sort(decreasing = TRUE)

dt2[scenario.id == 29, table(algo.id)]

dt2[, uniqueN(scenario.id)]

lapply(dt2[, unique(scenario.id)], function(id) {
  dt2[scenario.id == id, table(algo.id)]
})

dt2[, unique(scenario.id)][15]
dt2[, unique(scenario.id)][24]
dt2[, unique(scenario.id)][23]

# term3[, i]

# Investigate NAs for PO approaches ----

dt2 = dt[is.na(pval) & algo.id %in% 3:4]
split(dt2, by = "algo") |>
  lapply(function(x) x[, table(scenario.id)] |> sort(decreasing = TRUE))
# IJ boot: 29
# HC3: 181

dt2[algo.id == 3 & scenario.id == 181, 1:3]


## Go ----

options(box.path = "R")

box::use(
  fs,
  DBI[...],
  RSQLite[SQLite],
  withr[with_seed],
  data.table[...],
  parallel[mclapply]
)

box::use(
  simfuns2/scenarios[make_params],
  simfuns2/dgp[gen_surv_data],
  simfuns2/utils[trycatch_log]
)

# Get parameters for simulating the data
get_params = function(scenario.id, dir_sim = fs$path("simulation"), timeout = 60) {
  db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
  on.exit(dbDisconnect(db), add = TRUE)
  
  start_time = Sys.time()
  params = try(stop("init"), silent = TRUE)
  
  while (inherits(params, "try-error")) {
    # Stop if it takes too long
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      stop("Time limit elapsed.")
    }
    
    # Usual behaviour: get parameters
    params = try(
      dbGetQuery(
        db,
        sprintf("SELECT * FROM scenarios WHERE `scenario.id` = %d", scenario.id)
      ) |>
        make_params()
    )
  }
  
  return(params)
}


params = get_params(29)

li_data = with_seed(29, {
  lapply(seq_len(5000), function(i) {
    do.call(gen_surv_data, params)
  })
})

dt2[scenario.id == 29] |> View()

dt2[scenario.id == 29 & algo.id == 4]

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 1000L
)

box::use(simfuns2/algos[rmst_pseudo_hc3, rmst_pseudo_ij_boot])

for (i in dt2[scenario.id == 181, rep.id]) {
  tryCatch(
    rmst_pseudo_hc3(li_data[[i]], constants),
    error = \(e) cat(i, "\n")
  )
}

# Got it
x1 = li_data[[4480]]
rmst_pseudo_ij_boot(x1, constants)

#x1[, mean(event), by = trt]


#' This file is intended for containing R-code to be executed on the remote server.

#' Linux commands
#' 
#' ssh hpc-gwdg
#' # passphrase
#' 
#' cd rmst-sim
#' module load r/4.3.0
#' R
#' # for the first time maybe hit ctrl+c t
#' 


# Setting up project environment (only once usually)
# source("simulation/hpcenv/remote.R")

# Setting up simulation
# This might also be done locally
# source("simulation/01-setup.R")

# Double-check
cat(readLines("simulation/02-submit.R"), sep = "\n")
cat(readLines("R/simfuns2/submit_jobs.R"), sep = "\n")
# Submitting jobs
source("simulation/02-submit.R")


list.files("simulation")
cat(readLines("simulation/01-setup.R"), sep = "\n")
source("simulation/01-setup.R")

cat(readLines("simulation/02-submit.R"), sep = "\n")
cat(readLines("R/simfuns2/dgp.R"), sep = "\n")

rm(list = ls())
box::purge_cache()

list.files("simulation")
source("simulation/04-boot_new.R")


# Monitoring ----

options(box.path = "R")

box::use(
  simfuns2/get_funs[get_results_table, get_scenario_table, get_algo_table,
                    get_num_running, get_jobs_submitted, get_jobs_finished],
  simfuns2/cancel[cancel_jobs]
)

)

list.files("R/simfuns2")

box::use(simfuns2/post_submit[collect_results])
collect_results(join = NULL, .save = TRUE)

dtr = get_results_table()
dtr = dtr[scenario.id == 25]
dtr[, mean(pval <= 0.05, na.rm = TRUE), by = algo.id][order(algo.id)]

dts = get_scenario_table()

box::use(
  fs,
  data.table[...]
)

box::use(
  simfuns2/post_submit[check_sim_finished]
)

check = check_sim_finished()

# actual monitoring
get_num_running()
get_jobs_submitted()
get_results_table()


check = check_sim_finished()

all(1:216 %in% check$finished)
1:216 %in% check$finished

cat(readLines("simulation/registry/logs/job119.log"), sep = "\n")
cat(readLines("simulation/registry/logs/job162.log"), sep = "\n")


not_started = x$not_started

dtr = get_results_table()
dts = get_scenario_table()

# Only 1 (asymptotic) missing
dtr[scenario.id == 5, table(algo.id)]
# Only 3 (pseudo HC3)
dtr[scenario.id == 30, table(algo.id)]
# Only 3 (pseudo HC3)
dtr[scenario.id == 43, table(algo.id)]
# Only 2 (studperm)
dtr[scenario.id == 70, table(algo.id)]
# Only 2 (studperm)
dtr[scenario.id == 110, table(algo.id)]
# Only 3 (pseudo HC3)
dtr[scenario.id == 111, table(algo.id)]


dtr[scenario.id == 5 & algo.id == 2]


box::use(
  simfuns2/post_submit[read_logs]
)

read_more_logs = function(ids) {
  for (id in ids) {
    cat("ID: ", id, "\n")
    read_logs(id)
    cat("\n")
  }
}

read_more_logs(check$not_finished)

read_logs(5)
cat()
read_logs(30)
read_logs(43)
read_logs(70)
read_logs(110)
read_logs(111)



dtr[scenario.id %in% not_started]

# 208 submitted
# 123 still running
# 78 not started

dts[scenario.id %in% c(58, 62, 98, 99, 102, 103, 162, 166)]

dts[scenario.id == 58]

dtr[scenario.id == 58, mean(pval <= 0.05), by = algo.id][order(algo.id)]
dtr[scenario.id == 62, mean(pval <= 0.05), by = algo.id][order(algo.id)]

dtr[, unique(scenario.id)] |> sort()

dtr[scenario.id %in% c(58, 62), mean(pval <= 0.05), by = .(scenario.id, algo.id)][
  order(scenario.id, algo.id)
]

dtr[, table(scenario.id)]

dts[scenario.id %in% c(98, 99, 102, 103, 162, 166)]

dts[surv_model == "ph_exp" & cens_model == "eq_wb" & samples_k == 2][1:2]
# 58 (null) and 62 (alternative)

dt[scenario.id == 99, mean(pval <= 0.05), by = algo.id]
dt[scenario.id == 103, mean(pval <= 0.05), by = algo.id]

get_algo_table()

dt[, mean(pval <= 0.05), by = .(scenario.id, algo.id)][
  order(scenario.id, algo.id)
]
dts = get_scenario_table()

set_slurm_resources(
  walltime = "10:00:00"
)



set_sim_resources(
  scenario.ids = dts[surv_model == "crossing_wb" & cens_model == "eq_unif" & n0 == 30 & n1 == 30]$scenario.id
)

get_slurm_resources()
set_slurm_resources(walltime = "10:00:00")


dts[surv_model == "crossing_wb" & cens_model == "eq_unif" & n0 == 30 & n1 == 30]$scenario.id

dt[, table(scenario.id)]
dts[scenario.id == 162]

dts[scenario.id == 6]

dt[scenario.id == 162, mean(pval <= 0.05), by = algo.id]
dt[scenario.id == 166, mean(pval <= 0.05), by = algo.id]

dt[, sum(is.na(pval))]
dt[, mean(pval <= 0.05), by = .(scenario.id, algo.id)][order(scenario.id, algo.id)]

dt[scenario.id == 162, mean(pval <= 0.05), by = algo.id][order(algo.id)]
dt[scenario.id == 166, mean(pval <= 0.05), by = algo.id][order(algo.id)]

dt = get_scenario_table()
dt[scenario.id == 162]
dt[scenario.id == 166]

dt[scenario.id == 2]
dt[scenario.id == 6]

# Something went wrong?
list.files("simulation/registry/logs")
cat(readLines("simulation/registry/logs/job162.log"), sep = "\n")
cat(readLines("simulation/registry/logs/job166.log"), sep = "\n")

# Clean the environment
rm(list = ls())
box::purge_cache()




# 98, 102
dt = get_results_table()
# Null scenario
dt[scenario.id == 98, mean(pval <= 0.05), by = algo.id][order(algo.id)]
# Alt. scenario
dt[scenario.id == 102, mean(pval <= 0.05), by = algo.id][order(algo.id)]

dts[scenario.id == 98]
dts[scenario.id == 102]

dts[scenario.id == 99]
dts[scenario.id == 103]

dt[scenario.id == 99, mean(pval <= 0.05), by = algo.id][order(algo.id)]
dt[scenario.id == 103, mean(pval <= 0.05), by = algo.id][order(algo.id)]


f = function(n = 200, wait = 1) {
  for (i in seq_len(n)) {
    if (i > 1) Sys.sleep(wait)
    cat(i, "\n")
  }
}

system.time(f())

fs$file_show(fs$path("simulation", "registry", "logs", "job162.log"))
cat(readLines(fs$path("simulation", "registry", "logs", "job30.log")), sep = "\n")


options(box.path = "R")
box::use(simfuns2/post_submit[collect_results])
collect_results(.save = TRUE)

dtr = readRDS("simulation/results/2024-01-12_results.rds")






# 15.01.2024 ----

cat(readLines("simulation/templates/run_r.tmpl"), sep = "\n")
cat(readLines("simulation/02-submit.R"), sep = "\n")
cat(readLines("R/simfuns2/submit_jobs.R"), sep = "\n")

source("simulation/02-submit.R")

system('squeue --format="%.12i %.12j %.8u %.3P %.3q %.10l %.10M %.5D %.4C %.7m %16R %.8T %.12p"')


get_all_jobs = function(data_table = TRUE) {
  cmd = "squeue"
  args = '--format="%.12i %.12j %.8u %.3P %.3q %.10l %.10M %.5D %.4C %.7m %16R %.8T %.12p"'
  
  if (!data_table) {
    system2(cmd, args, stdout = "")
  } else {
    x = system2(cmd, args, stdout = TRUE)
    # tryCatch(
    #   fread(text = x),
    #   error = \(e) cat(x, sep = "\n"),
    #   warning = \(e) cat(x, sep = "\n")
    # )
    length(x)
  }
}



# 16.01.2024 ----

options(box.path = "R")

box::use(simfuns2/run_sim[run_sim])

sim_resources = list(
  num_sims = 5000,
  algos = "asy",
  ncpus = 1,
  data_subset = NULL
)

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 1000L
)

run_sim(5, sim_resources, constants)



box::use(
  simfuns2/get_funs[get_results_table, get_scenario_table, get_algo_table,
                    get_num_running, get_jobs_submitted, get_jobs_finished],
  simfuns2/cancel[cancel_jobs]
)


check = check_sim_finished()

cat(readLines("simulation/registry/logs/job119.log"), sep = "\n")

not_started = x$not_started

dtr = get_results_table()
dtr[scenario.id == 5, table(algo.id)]
dtr[scenario.id == 5][order(algo.id)]



sim_resources = list(
  scenario.ids = c(70, 110),
  num_sims = 5000,
  algos = "studperm"
)
algos = sim_resources$algos[match(sim_resources$algos, c("asy", "pseudo_hc3", "studperm", "pseudo_ij_boot"))]


cat(readLines("R/simfuns2/run_sim.R"), sep = "\n")

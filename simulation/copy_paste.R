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


# 00 Set up environment ----

# This (usually) needs to be done once only
source("simulation/hpcenv/remote.R")


# 01 Set up registry/infrastructure for simulation study ----

source("simulation/01-setup.R")

# This might also be done locally.
# Moreover, make sure that there exists a directory `simulation/templates` with the files
# * run_r.tmpl
# * slurm-gwdg.tmpl


# 02 Submit jobs ----

# Each time new/other jobs shall be submitted, edit the file `simulation/02-submit.R` 
# and re-upload it to the remote server.
source("simulation/02-submit.R")

cat(readLines("simulation/03-temp-submit.R"), sep = "\n")
source("simulation/03-temp-submit.R")

# 03 Monitor/control submitted jobs ----

# We can check the status of submitted jobs, cancel them and/or 
# obtain some preliminary results.

options(box.path = "R")

box::use(
  simfuns2/get_funs[get_results_table, get_scenario_table, get_algo_table,
                    get_num_running, get_jobs_submitted, get_jobs_finished],
  simfuns2/cancel[cancel_jobs],
  simfuns2/post_submit[read_logs]
)

# Check how many jobs are currently running
get_num_running()

# Get a list/table of all submitted job
get_jobs_submitted()

# Get/look at some preliminary results
get_results_table()

# Cancel jobs (you need to supply the job IDs assigned by Slurm)
cancel_jobs()

# Something went wrong?
# Read the log file (scenario.id needs to be supplied)
read_logs()

# Preliminary analysis
get_algo_table()

dtr = get_results_table()
dtr[, anyNA(pval)]
dtr[, table(algo.id)]

dts = get_scenario_table()
ids = dts[rmst_diff == 0, scenario.id]

dt1 = dtr[scenario.id %in% ids, 
    mean(pval <= 0.05, na.rm = TRUE),
    by = .(scenario.id, algo.id)][
      order(scenario.id, algo.id)
    ]

dt1[scenario.id == 1]

dtr[, sum(is.na(pval)), by = algo.id]
dtr[, mean(pval <= 0.05, na.rm = TRUE), by = .(scenario.id, algo.id)][order(scenario.id, algo.id)]
dtr[, sum(is.na(pval)), by = .(algo.id, scenario.id)]
dtr[is.na(pval)]

dt1[, control := between(V1, 0.044, 0.056)]
dt1[, mean(control), by = algo.id]

list.files("simulation/registry/backup")


# 04 After simulation ----

box::use(
  simfuns2/post_submit[collect_results, rm_temp_files, check_sim_finished]
)

# Remove temporary files created for job submission
rm_temp_files()

# Collect/save results from the simulation study
collect_results(.save = TRUE)

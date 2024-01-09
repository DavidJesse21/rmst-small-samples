#' This file is intended for containing R-code to be executed on the remote server.

# Setting up and submitting simulation
source("simulation/01-setup.R")
source("simulation/02-submit.R")

# Monitoring
options(box.path = "R")

box::use(
  simfuns2/get_funs[get_results_table, get_scenario_table, get_algo_table,
                    get_num_running, get_jobs_submitted, get_jobs_finished],
  simfuns2/cancel[cancel_jobs]
)

box::use(
  data.table[...]
)


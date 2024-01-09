options(box.path = "R")

box::use(
  DBI[...],
  RSQLite[SQLite],
  data.table[...],
  fs
)

box::use(
  simfuns2/setup[setup_sim, add_algorithm, remove_algorithm],
  simfuns2/scenarios[make_scenarios],
  simfuns2/algos[rmst_asy, rmst_studperm, rmst_pseudo_hc3, rmst_pseudo_ij_boot]
)

# box::use(
#   simfuns2/get_infos[get_results_table]
# )


# Setup ----

dt_scenarios = make_scenarios()
setup_sim(dt_scenarios = dt_scenarios)

con = dbConnect(SQLite(), fs$path("simulation", "registry", "simdb", ext = "db"))

dbListTables(con)
dbReadTable(con, "algorithms")
dbReadTable(con, "results")
dbReadTable(con, "scenarios")

# Add algorithms
add_algorithm("asy", rmst_asy)
add_algorithm("studperm", rmst_studperm)
add_algorithm("pseudo_hc3", rmst_pseudo_hc3)
add_algorithm("pseudo_ij_boot", rmst_pseudo_ij_boot)
dbReadTable(con, "algorithms")

# get_results_table()

# Test simulations ----

box::use(
  simfuns2/run_sim[run_sim]
)

sim_resources = list(
  num_sims = 3L,
  algos = c("asy", "studperm", "pseudo_hc3", "pseudo_ij_boot"),
  data_subset = NULL,
  ncpus = 1L
)

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 1000L
)

dt = dbReadTable(con, "scenarios")
setDT(dt)
dt[scenario.id == 2]
dt[scenario.id == 6]

x = run_sim(2, sim_resources, constants)
dbReadTable(con, "results")
#get_results_table()
#get_results_table()[, table(algo.id)]

x = run_sim(6, sim_resources, constants)
dbReadTable(con, "results")
#get_results_table()
#get_results_table()[, table(algo.id)]

# dbExecute(con, "DELETE FROM results WHERE `scenario.id` = 2")
# dbExecute(con, "DELETE FROM results WHERE `scenario.id` = 6")

dbDisconnect(con)


# Parallel ----

# con = dbConnect(SQLite(), fs$path("simulation", "simdb", ext = "db"))
# dbListTables(con)
# dbRemoveTable(con, "test")
# 
# dbWriteTable(con, "test", data.table(id = numeric(), x1 = numeric(), x2 = numeric()))
# dbReadTable(con, "test")
# dbDisconnect(con)
# 
# parallel::stopCluster(cl)
# 
# cl = parallel::makeCluster(4)
# parallel::clusterEvalQ(cl, {
#   library(DBI)
# })
# x = parallel::parLapply(cl, 1:4, function(i) {
#   con = dbConnect(RSQLite::SQLite(), fs::path("simulation", "simdb", ext = "db"))
#   on.exit(dbDisconnect(con))
#   
#   df = data.frame(
#     id = i,
#     x1 = runif(10),
#     x2 = runif(10)
#   )
#   Sys.sleep(2)
#   
#   dbAppendTable(con, "test", df)
# })
# 
# dbDisconnect(con)

# Test submitting ----

box::use(
  simfuns2/submit_jobs[submit_jobs]
)

# Copy templates files
fs$file_copy(
  fs$path("hpc", "run_r", ext = "tmpl"),
  fs$path("simulation", "registry", "templates", "run_r", ext = "tmpl")
)
fs$file_copy(
  fs$path("hpc", "slurm-gwdg", ext = "tmpl"),
  fs$path("simulation", "registry", "templates", "slurm-gwdg", ext = "tmpl")
)

slurm_resources = list(
  mail.type = "NONE",
  mail.user = "david.jesse@stud.uni-goettingen.de",
  walltime = "03:00:00",
  partition = "medium",
  ncpus = 4,
  mem = "4GB",
  mem_per_cpu = "1GB"
)

sim_resources$scenario.ids = 1:3
sim_resources

submit_jobs(sim_resources, slurm_resources, constants, submit = FALSE)

dbDisconnect(con)
rm(list = ls())
box::purge_cache()


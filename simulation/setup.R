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


# Setup ----

dt_scenarios = make_scenarios()
setup_sim(dt_scenarios = dt_scenarios)

con = dbConnect(SQLite(), fs$path("simulation", "simdb", ext = "db"))

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

x = run_sim(1, sim_resources, constants)

dbReadTable(con, "results")

dbReadTable(con, "results")

# Test submitting ----

box::use(
  simfuns2/submit_jobs[submit_jobs]
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


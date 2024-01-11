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
  simfuns2/algos[rmst_asy, rmst_studperm, rmst_pseudo_hc3, rmst_pseudo_ij_boot],
  simfuns2/resources[set_slurm_resources, set_sim_resources]
)

dt_scenarios = make_scenarios()
setup_sim(dt_scenarios = dt_scenarios)

con = dbConnect(SQLite(), fs$path("simulation", "registry", "simdb", ext = "db"))

# Add algorithms
add_algorithm("asy", rmst_asy)
add_algorithm("studperm", rmst_studperm)
add_algorithm("pseudo_hc3", rmst_pseudo_hc3)
add_algorithm("pseudo_ij_boot", rmst_pseudo_ij_boot)

# Set resources
set_sim_resources(
  num_sims = 5000L,
  algos = c("asy", "studperm", "pseudo_hc3", "pseudo_ij_boot")
)
set_slurm_resources(
  mail.type = "END",
  mail.user = "david.jesse@stud.uni-goettingen.de",
  walltime = "06:00:00",
  partition = "medium",
  ncpus = 4,
  memory = "3G"
)

# Disconnect and clean
dbDisconnect(con)
rm(list = ls())
box::purge_cache()

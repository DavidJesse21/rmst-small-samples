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
  simfuns2/algos[rmst_asy, rmst_studperm, rmst_pseudo, rmst_pseudo_ij, rmst_pseudo_ij_boot],
  simfuns2/resources[set_slurm_resources, set_sim_resources]
)

dt_scenarios = make_scenarios()
setup_sim(dt_scenarios = dt_scenarios)

db = dbConnect(SQLite(), fs$path("simulation", "registry", "simdb", ext = "db"))

# Add algorithms
add_algorithm("asy", rmst_asy)
add_algorithm("studperm", rmst_studperm)
add_algorithm("pseudo", rmst_pseudo)
add_algorithm("pseudo_ij", rmst_pseudo_ij)
add_algorithm("pseudo_ij_boot", rmst_pseudo_ij_boot)

# Set resources
set_sim_resources(
  num_sims = 5000L,
  algos = c("asy", "studperm", "pseudo", "pseudo_ij", "pseudo_ij_boot")
)
set_slurm_resources(
  mail.type = "END",
  mail.user = "david.jesse@stud.uni-goettingen.de",
  walltime = "18:00:00",
  partition = "medium",
  ncpus = 4,
  memory = "3G"
)

# Disconnect and clean
dbDisconnect(db)
rm(list = ls())
box::purge_cache()

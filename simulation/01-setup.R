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

dt_scenarios = make_scenarios()
setup_sim(dt_scenarios = dt_scenarios)

con = dbConnect(SQLite(), fs$path("simulation", "simdb", ext = "db"))

# Add algorithms
add_algorithm("asy", rmst_asy)
add_algorithm("studperm", rmst_studperm)
add_algorithm("pseudo_hc3", rmst_pseudo_hc3)
add_algorithm("pseudo_ij_boot", rmst_pseudo_ij_boot)

# Copy templates files
fs$file_copy(
  fs$path("hpc", "run_r", ext = "tmpl"),
  fs$path("simulation", "templates", "run_r", ext = "tmpl")
)
fs$file_copy(
  fs$path("hpc", "slurm-gwdg", ext = "tmpl"),
  fs$path("simulation", "templates", "slurm-gwdg", ext = "tmpl")
)

# Quick info message
message("Specify your resources!")

# Disconnect and clean
dbDisconnect(con)
rm(list = ls())
box::purge_cache()

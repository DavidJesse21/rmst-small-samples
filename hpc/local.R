# Identify packages that need to be installed on the remote system for 
# running the simulations.
# (This should be executed on the local system.)

box::use(
  fs,
  data.table[...]
)

paths = list(
  funs = fs$path("R", "rmst"),
  sim1 = fs$path("R", "simfuns"),
  sim2 = fs$path("simsetup")
)

deps = lapply(paths, \(path) {
  x = renv::dependencies(path)
  setDT(x)
  return(x)
}) |>
  rbindlist()

unique(deps$Package)
saveRDS(unique(deps$Package), fs$path("hpc", "packages", ext = "rds"))

# readRDS(fs$path("hpc", "packages.rds"))

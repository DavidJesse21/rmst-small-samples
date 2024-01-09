# Identify packages that need to be installed on the remote system for 
# running the simulations.
# (This should be executed on the local system.)

box::use(
  fs,
  data.table[...]
)

paths = list(
  algos = fs$path("R", "rmst"),
  sim = fs$path("R", "simfuns2")
)

deps = lapply(paths, \(path) {
  x = renv::dependencies(path)
  setDT(x)
  return(x)
}) |>
  rbindlist()
setDT(deps)
deps = deps[Source != fs$path_wd("R", "rmst", "pseudo_extra", ext = "R")]
deps = unique(deps$Package)

saveRDS(deps, fs$path("simulation", "hpcenv", "packages", ext = "rds"))

# readRDS(fs$path("simulation", "hpcenv", "packages.rds"))

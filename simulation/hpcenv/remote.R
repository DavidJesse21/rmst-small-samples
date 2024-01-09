# Prepare/restore setup on the remote system

if (!nzchar(system.file(package = "renv"))) {
  install.packages("renv")
}
to_install = readRDS("simulation/hpcenv/packages.rds")
renv::restore(packages = to_install)

# Update lockfile
renv::snapshot()

# Disable automatic snapshots
options(renv.config.auto.snapshot = FALSE)
write(
  "options(renv.config.auto.snapshot = FALSE)",
  file = ".Rprofile", append = TRUE
)


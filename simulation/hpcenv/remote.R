# Prepare/restore setup on the remote system

if (!nzchar(system.file(package = "renv"))) {
  install.packages("renv")
}
to_install = readRDS("simulation/hpcenv/packages.rds")
renv::restore(packages = to_install)

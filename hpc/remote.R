# Prepare/restore setup on the remote system

install.packages("renv")
to_install = readRDS("hpc/packages.rds")
renv::restore(packages = to_install)

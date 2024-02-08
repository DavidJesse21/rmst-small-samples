# Remaining asymptotic test ----

options(box.path = "R")

box::use(
  simfuns2/submit_jobs[submit_jobs],
  simfuns2/resources[get_sim_resources, get_slurm_resources, set_slurm_resources, set_sim_resources]
)

ids = c(21, 49, 81, 84, 86, 141, 142, 145, 199, 209, 214)
set_sim_resources(
  scenario.ids = ids,
  algos = "asy",
  num_sims = 5000
)
sim_resources = get_sim_resources()

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 2000L
)

set_slurm_resources(
  walltime = "00:30:00",
  ncpus = 1
)
slurm_resources = get_slurm_resources()

## Double-check and submit ----

# Simulation resources
cat("The following simulation resources are set:\n\n")
for (r in names(sim_resources)) {
  cat(r, ":\n", "  ", paste0(sim_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_sim_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Slurm resources
cat("The following slurm resources are set:\n\n")
for (r in names(slurm_resources)) {
  cat(r, ":\n", "  ", paste0(slurm_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_slurm_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Submit jobs
submit_jobs(sim_resources, slurm_resources, constants)

# Clean environment
rm(list = ls())
box::purge_cache()


# Remaining studentized permutation test ----

options(box.path = "R")

box::use(
  simfuns2/submit_jobs[submit_jobs],
  simfuns2/resources[get_sim_resources, get_slurm_resources, set_slurm_resources, set_sim_resources]
)

ids = 149
set_sim_resources(
  scenario.ids = ids,
  algos = "studperm",
  num_sims = 5000
)
sim_resources = get_sim_resources()

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 2000L
)

set_slurm_resources(
  walltime = "02:30:00",
  ncpus = 4
)
slurm_resources = get_slurm_resources()

## Double-check and submit ----

# Simulation resources
cat("The following simulation resources are set:\n\n")
for (r in names(sim_resources)) {
  cat(r, ":\n", "  ", paste0(sim_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_sim_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Slurm resources
cat("The following slurm resources are set:\n\n")
for (r in names(slurm_resources)) {
  cat(r, ":\n", "  ", paste0(slurm_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_slurm_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Submit jobs
submit_jobs(sim_resources, slurm_resources, constants)

# Clean environment
rm(list = ls())
box::purge_cache()


# Remaining pseudo ----

options(box.path = "R")

box::use(
  simfuns2/submit_jobs[submit_jobs],
  simfuns2/resources[get_sim_resources, get_slurm_resources, set_slurm_resources, set_sim_resources]
)

ids = 46
set_sim_resources(
  scenario.ids = ids,
  algos = "pseudo",
  num_sims = 5000
)
sim_resources = get_sim_resources()

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 2000L
)

set_slurm_resources(
  walltime = "00:30:00",
  ncpus = 4
)
slurm_resources = get_slurm_resources()

## Double-check and submit ----

# Simulation resources
cat("The following simulation resources are set:\n\n")
for (r in names(sim_resources)) {
  cat(r, ":\n", "  ", paste0(sim_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_sim_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Slurm resources
cat("The following slurm resources are set:\n\n")
for (r in names(slurm_resources)) {
  cat(r, ":\n", "  ", paste0(slurm_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_slurm_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Submit jobs
submit_jobs(sim_resources, slurm_resources, constants)

# Clean environment
rm(list = ls())
box::purge_cache()


# Remaining pseudo_ij ----

options(box.path = "R")

box::use(
  simfuns2/submit_jobs[submit_jobs],
  simfuns2/resources[get_sim_resources, get_slurm_resources, set_slurm_resources, set_sim_resources]
)

ids = c(60, 105, 125)
set_sim_resources(
  scenario.ids = ids,
  algos = "pseudo_ij",
  num_sims = 5000
)
sim_resources = get_sim_resources()

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 2000L
)

set_slurm_resources(
  walltime = "00:30:00",
  ncpus = 4
)
slurm_resources = get_slurm_resources()

## Double-check and submit ----

# Simulation resources
cat("The following simulation resources are set:\n\n")
for (r in names(sim_resources)) {
  cat(r, ":\n", "  ", paste0(sim_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_sim_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Slurm resources
cat("The following slurm resources are set:\n\n")
for (r in names(slurm_resources)) {
  cat(r, ":\n", "  ", paste0(slurm_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_slurm_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Submit jobs
submit_jobs(sim_resources, slurm_resources, constants)

# Clean environment
rm(list = ls())
box::purge_cache()



# Remaining pseudo_ij_boot ----

options(box.path = "R")

box::use(
  simfuns2/submit_jobs[submit_jobs],
  simfuns2/resources[get_sim_resources, get_slurm_resources, set_slurm_resources, set_sim_resources]
)

ids = c(80, 100, 116, 170)
set_sim_resources(
  scenario.ids = ids,
  algos = "pseudo_ij_boot",
  num_sims = 5000
)
sim_resources = get_sim_resources()

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 2000L
)

set_slurm_resources(
  walltime = "10:00:00",
  ncpus = 4
)
slurm_resources = get_slurm_resources()

## Double-check and submit ----

# Simulation resources
cat("The following simulation resources are set:\n\n")
for (r in names(sim_resources)) {
  cat(r, ":\n", "  ", paste0(sim_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_sim_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Slurm resources
cat("The following slurm resources are set:\n\n")
for (r in names(slurm_resources)) {
  cat(r, ":\n", "  ", paste0(slurm_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_slurm_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Submit jobs
submit_jobs(sim_resources, slurm_resources, constants)

# Clean environment
rm(list = ls())
box::purge_cache()

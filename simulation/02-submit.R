options(box.path = "R")

box::use(
  simfuns2/submit_jobs[submit_jobs],
  simfuns2/resources[get_sim_resources, get_slurm_resources, set_slurm_resources, set_sim_resources],
  simfuns2/post_submit[check_sim_finished]
)


# 03.02.2024 - Submit all jobs except for 25 and 162 ----

ids = setdiff(1:216, c(25, 162))


# Simulation resources
set_sim_resources(
  scenario.ids = ids,
  algos = c("asy", "studperm", "pseudo", "pseudo_ij", "pseudo_ij_boot")
)
sim_resources = get_sim_resources()

# Constant objects used for algorithms
constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 2000L
)

# Slurm resources
slurm_resources = get_slurm_resources()

# Double-check and submit ----

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

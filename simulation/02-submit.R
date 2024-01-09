options(box.path = "R")

box::use(
  simfuns2/submit_jobs[submit_jobs],
  simfuns2/resources[get_sim_resources, get_slurm_resources, set_slurm_resources, set_sim_resources],
  simfuns2/get_funs[get_scenario_table]
)

# For initial experiments only evaluate 2 scenarios (one under H0 and one under H1)
dt = get_scenario_table()
dt = dt[surv_model == "crossing_pwexp" & cens_model == "uneq_wb" & n0 == 36 & n1 == 24]
scenario.ids = c(
  dt[rmst_diff == 0][1, scenario.id],
  dt[rmst_diff == 1.5][1, scenario.id]
)

sim_resources = get_sim_resources()
sim_resources$scenario.ids = scenario.ids

# Constant objects used for algorithms
constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 1000L
)

# Slurm
slurm_resources = get_slurm_resources()

# Double check simulation resources specification
cat("The following simulation resources are set:\n\n")
for (r in names(sim_resources)) {
  cat(r, ":\n", "  ", paste0(sim_resources[[r]], collapse = ", "), "\n", sep = "")
}
cat("\nYou can alter the resources using `set_sim_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Double check slurm resources specifications
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

# 16.01.2024
# Submit some remaining "jobs"/simulations
# Use this with "copy + paste"

options(box.path = "R")

box::use(
  simfuns2/submit_jobs[submit_jobs],
  simfuns2/resources[get_sim_resources, get_slurm_resources, set_slurm_resources, set_sim_resources]
)

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 1000L
)


# Studentized permutation ----

slurm_resources = get_slurm_resources()
slurm_resources$walltime = "02:30:00"

sim_resources = list(
  scenario.ids = c(70, 110),
  num_sims = 5000,
  algos = "studperm"
)

submit_jobs(sim_resources, slurm_resources, constants)


# Pseudo HC3 ----

slurm_resources = get_slurm_resources()
slurm_resources$ncpus = 1
slurm_resources$walltime = "00:20:00"

sim_resources = list(
  scenario.ids = c(30, 43, 111),
  num_sims = 5000,
  algos = "pseudo_hc3"
)

submit_jobs(sim_resources, slurm_resources, constants)

options(box.path = "R")

box::use(
  simfuns2/submit_jobs[submit_jobs],
  simfuns2/resources[get_sim_resources, get_slurm_resources]
)

constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 1000L
)

sim_resources = get_sim_resources()
if (length(sim_resources) == 0) {
  sim_resources = list(
    num_sims = 1000L,
    algos = c("asy", "studperm", "pseudo_hc3", "pseudo_ij_boot"),
    data_subset = NULL
  )
}
# Only for now
sim_resources$scenario.ids = c(2L, 6L)


slurm_resources = get_slurm_resources()
if (length(slurm_resources) == 0) {
  slurm_resources = list(
    mail.type = "END",
    mail.user = "david.jesse@stud.uni-goettingen.de",
    walltime = "18:00:00",
    partition = "medium",
    ncpus = 4,
    memory = "3G"
  )
}

submit_jobs(sim_resources, slurm_resources, constants)

rm(list = ls())
box::purge_cache()

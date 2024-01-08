# Set up the simulation plan using batchtools here

options(box.path = "R")

# batchtools and other utilities
box::use(
  fs,
  bt = batchtools,
  data.table[...]
)

# Problems and algorithms
box::use(
  simfuns/designs[make_prob_design],
  simfuns/problems[gen_data_exp, gen_data_pwexp, gen_data_weibull],
  simfuns/algos[rmst_asy, rmst_studperm, rmst_pseudo_hc3, rmst_pseudo_ij_boot]
)


# Registry creation ----

# Create the registry
reg = bt$makeExperimentRegistry(
  fs$path("simsetup", "registry"),
  seed = 42L
)

# `data` argument for each problem (actually used for algorithms)
constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 1000L
)

# Add problems
bt$addProblem("ph_exp", fun = gen_data_exp, data = constants, seed = 1L)
bt$addProblem("crossing_pwexp", fun = gen_data_pwexp, data = constants, seed = 1L)
bt$addProblem("crossing_wb", fun = gen_data_weibull, data = constants, seed = 1L)

# Add the algorithms
bt$addAlgorithm("asy", fun = rmst_asy)
bt$addAlgorithm("studperm", fun = rmst_studperm)
bt$addAlgorithm("pseudo_hc3", fun = rmst_pseudo_hc3)
bt$addAlgorithm("pseudo_ij_boot", fun = rmst_pseudo_ij_boot)

# Add experiments
des = make_prob_design()
bt$addExperiments(prob.designs = des, repls = 5000)

# Quick overview
ids = bt$getJobPars()
ids[, algo.pars := NULL]
nrow(ids)
head(ids)


# Test jobs ----

bt$testJob(
  bt$findExperiments(
    prob.name = "ph_exp",
    prob.pars = (samples_k == 2 & rmst_diff == 0),
    algo.name = "asy"
  )[1]
)

bt$testJob(
  bt$findExperiments(
    prob.name = "ph_exp",
    prob.pars = (samples_k == 2 & rmst_diff == 0),
    algo.name = "studperm"
  )[1]
)

bt$testJob(
  bt$findExperiments(
    prob.name = "ph_exp",
    prob.pars = (samples_k == 2 & rmst_diff == 0),
    algo.name = "pseudo_hc3"
  )[1]
)

bt$testJob(
  bt$findExperiments(
    prob.name = "ph_exp",
    prob.pars = (samples_k == 2 & rmst_diff == 0),
    algo.name = "pseudo_ij_boot"
  )[1]
)

bt$findExperiments(
  prob.name = "ph_exp",
  prob.pars = (samples_k == 2 & rmst_diff == 0),
  algo.name = "pseudo_hc3"
)[1]
bt$testJob(id = 10001)

bt$testJob(id = 2)

x = bt$testJob(id = 1)
x = bt$testJob(id = 1, external = TRUE)

bt$findExperiments(
  prob.name = "ph_exp",
  prob.pars = (samples_k == 2 & rmst_diff == 1.5)
)[1]
x = bt$testJob(id = 5001)
x = bt$testJob(id = 5001, external = TRUE)


# crossing_pwexp
bt$findExperiments(
  prob.name = "crossing_pwexp",
  prob.pars = (samples_k == 2 & rmst_diff == 0)
)[1]
x = bt$testJob(id = 10001)
x = bt$testJob(id = 10001, external = TRUE)

bt$findExperiments(
  prob.name = "crossing_pwexp",
  prob.pars = (samples_k == 2 & rmst_diff == 1.5)
)[1]
x = bt$testJob(id = 15001)
x = bt$testJob(id = 15001, external = TRUE)


# crossing_wb
bt$findExperiments(
  prob.name = "crossing_wb",
  prob.pars = (samples_k == 2 & rmst_diff == 0)
)[1]
x = bt$testJob(id = 20001)
x = bt$testJob(id = 20001, external = TRUE)

bt$findExperiments(
  prob.name = "crossing_wb",
  prob.pars = (samples_k == 2 & rmst_diff == 1.5)
)[1]
x = bt$testJob(id = 25001)
x = bt$testJob(id = 25001, external = TRUE)


bt$getJobPars()

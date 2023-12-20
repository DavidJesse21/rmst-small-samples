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
  simfuns/algos[rmst_funs]
)

des = make_prob_design()
# Subset for testing
des = lapply(des, \(dt) dt[(samples_k == 2)])
des = lapply(des, function(dt) {
  idx = vapply(dt$samples_alloc, \(x) identical(unname(x), c(15, 15)), logical(1))
  dt[idx]
})
des = lapply(des, function(dt) {
  idx = vapply(
    dt$params_cens,
    function(x) {
      isTRUE(all.equal(
        unname(x), c(0.01335314, 0.04700036), tolerance = 0.0001
      ))
    },
    logical(1)
  )
  dt[idx]
})



# `data` argument for each problem
constants = list(
  cutoff = 10,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  studperm_samples = 2000L
)


# Registry creation ----

# Create the registry
reg = bt$makeExperimentRegistry(
  fs$path("simsetup", "registry"),
  seed = 42L
)
# reg = bt$loadRegistry(fs$path("simsetup", "registry"), writeable = TRUE)
# bt$removeRegistry()

# Add problems
bt$addProblem("ph_exp", fun = gen_data_exp, data = constants, seed = 1L)
bt$addProblem("crossing_pwexp", fun = gen_data_pwexp, data = constants, seed = 1L)
bt$addProblem("crossing_wb", fun = gen_data_weibull, data = constants, seed = 1L)

# Add the algorithms (they are all wrapped in one function here)
bt$addAlgorithm("all", fun = rmst_funs)

# Add experiments
bt$addExperiments(prob.designs = des, repls = 5000)

# Quick overview
bt$getJobPars()[1:5, .(job.id, prob.pars)] |>
  bt$unwrap()


# Test jobs ----

# ph_exp
bt$findExperiments(
  prob.name = "ph_exp",
  prob.pars = (samples_k == 2 & rmst_diff == 0)
)[1]
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

# Set up the simulation plan using batchtools here

options(box.path = "R")

box::use(
  fs,
  bt = batchtools,
  data.table[...]
)

# Problems and algorithms
box::use(
  ./problems[gen_data],
  ./algos[do_one]
)

# Some helper functions
med2rate = \(x) (12 * log(2)) / (x * 365.25)
months2days = \(x) 365.25 * x / 12
minus_plus = \(x, add) c(x - add, x + add)

# Create the registry
reg = bt$makeExperimentRegistry(
  fs$path("simulations", "registry"),
  seed = 42L
)
# reg = bt$loadRegistry(fs$path("simulations", "registry"), writeable = TRUE)

# Add the problem(s)
bt$addProblem("ph", fun = gen_data, seed = 1L)

# Add the algorithms (they are all wrapped in one function here)
bt$addAlgorithm("all", fun = do_one)

# Add experiments
bt$addExperiments(
  prob.designs = list(
    ph = data.table(
      num_samples = list(c(12, 18)),
      lambda_ctrl = med2rate(12),
      lambda_trt = med2rate(24),
      cens_ctrl =  med2rate(36),
      cens_trt =  med2rate(36)
    )
  ),
  algo.designs = list(
    all = data.table(
      alpha = 0.05,
      cutoff = months2days(36)
    )
  ),
  repls = 10
)

# Quick overview
bt$getJobPars()
bt$unwrap(bt$getJobPars())

# Test job
bt$testJob(id = 1, external = TRUE)
x = bt$testJob(id = 2, external = TRUE)

# Check if working with list columns works
dt = x$data
dt[, .N, by = trt]
# Yes, it does

x$results

x[3, error]

x[1, error]

months2days(36)

options(box.path = "R")

box::use(
  fs,
  bt = batchtools,
  data.table[...],
  parallelly[availableCores]
)

# Load registry
reg = bt$loadRegistry(fs$path("simulations", "registry"), writeable = TRUE)

# Create cluster function
reg$cluster.functions = bt$makeClusterFunctionsSocket(
  ncpus = availableCores(omit = 1), fs.latency = 0
)

# Quick overview
# bt$getJobPars()

# Test a job
# bt$testJob(id = 6, external = TRUE)

# Submit jobs
bt$submitJobs()
# bt$getStatus()

# Close connections
reg$cluster.functions = bt$makeClusterFunctionsInteractive()

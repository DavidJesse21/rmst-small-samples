options(box.path = "R")

box::use(
  fs,
  bt = batchtools,
  data.table[...],
  parallelly[availableCores]
)

box::use(
  simfuns/resources[get_resources, set_resources],
  simfuns/utils[get_job_pars]
)

# Load registry
reg = bt$loadRegistry(fs$path("simsetup", "registry"), writeable = TRUE)

# Get resources
resources = get_resources()

# Create cluster function (parallel)
reg$cluster.functions = bt$makeClusterFunctionsSocket(
  ncpus = availableCores(omit = 1), fs.latency = 0
)

# Create cluster function (sequential)
reg$cluster.functions = bt$makeClusterFunctionsInteractive()

# Submit jobs as chunks
ids = get_job_pars(1:100, unwrap = 1)
for (j in setdiff(colnames(ids), c("job.id", "chunk"))) {
  set(ids, j = j, value = NULL)
}

# Submit jobs
time_taken = system.time({
  bt$submitJobs(
    ids = ids,
    resources = resources
  )
})

# Close connections (if parallel backend used)
reg$cluster.functions = bt$makeClusterFunctionsInteractive()

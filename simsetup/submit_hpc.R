options(box.path = "R")

box::use(
  fs,
  bt = batchtools,
  data.table[...]
)

# Load registry
reg = bt$loadRegistry(
  fs$path("simsetup", "registry"), work.dir = getwd(), writeable = TRUE
)

# Create cluster function
reg$cluster.functions = bt$makeClusterFunctionsSlurm(
  fs$path("hpc/slurm-gwdg.tmpl")
)

# Chunks / job arrays
ids = bt$getJobPars()
setdiff(colnames(ids), "job.id")
for (j in setdiff(colnames(ids), "job.id")) set(ids, j = j, value = NULL)
ids[, chunk := bt$chunk(ids$job.id, chunk.size = 1000L, shuffle = FALSE)]

  
# Chunks / job arrays
# ids = bt$getJobPars()
# for (j in setdiff(colnames(ids), c("job.id", "prob.pars"))) {
#   set(ids, j = j, value = NULL)
# }
# ids = bt$unwrap(ids)
# for (j in setdiff(colnames(ids), c("job.id", "chunk"))) {
#   set(ids, j = j, value = NULL)
# }
# ids[, chunk := rleid(chunk)]

# Submit jobs
bt$submitJobs(
  ids = ids,
  resources = list(
    mail.type = "NONE",
    mail.user = "david.jesse@stud.uni-goettingen.de",
    walltime = "02:00:00",
    ncpus = 1,
    memory = "3GB",
    chunks.as.arrayjobs = TRUE
  ),
  reg = reg
)

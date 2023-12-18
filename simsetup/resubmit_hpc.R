options(box.path = "R")

box::use(
  bt = batchtools,
  fs,
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

# Find jobs that have not been submitted
not_submitted = bt$findNotSubmitted(reg = reg)

# Resubmit jobs
bt$submitJobs(
  ids = not_submitted$job.id,
  resources = list(
    mail.type = "NONE",
    mail.user = "david.jesse@stud.uni-goettingen.de",
    walltime = "00:10:00",
    ncpus = 1,
    memory = "3GB"
  ),
  reg = reg
)

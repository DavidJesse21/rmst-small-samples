options(box.path = "R")

box::use(
  fs,
  bt = batchtools,
  data.table[...]
)

box::use(
  simfuns/resources[get_resources, set_resources],
  simfuns/utils[get_job_pars]
)


# Load registry
reg = bt$loadRegistry(
  fs$path("simsetup", "registry"), work.dir = getwd(), writeable = TRUE
)

# Get resources
resources = get_resources()

# Create cluster function
reg$cluster.functions = bt$makeClusterFunctionsSlurm(fs$path("hpc/slurm-gwdg.tmpl"))

# Submit jobs as chunks
ids = bt$findNotSubmitted(reg = reg)
ids[, chunk := bt$chunk(job.id, shuffle = FALSE, chunk.size = 1000L)]

# Double check (number of) jobs and `resources` object
cat("About to submit", format(nrow(ids), big.mark = ","), "jobs in", ids[, uniqueN(chunk)], "chunks\n")
cat("(Average chunk size of ", ids[, format(.N / uniqueN(chunk), big.mark = ",")], ")\n\n", sep = "")
if (!exists("resources")) stop("`resources` must be defined.")
cat("The following resources are set:\n\n")
for (r in names(resources)) {
  cat(r, ":\n", "  ", resources[[r]], "\n", sep = "")
}
cat("\nYou can alter the resources using `set_resources()`.\n")
cont = readline("Continue? [y/n] ")
stopifnot(cont %in% c("y", "n"))
if (cont == "n") stop("Execution halted.", .call = FALSE)

# Submit jobs
bt$submitJobs(
  ids = ids,
  resources = resources,
  reg = reg
)

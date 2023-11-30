options(box.path = "R")

box::use(
  fs,
  bt = batchtools,
  data.table[...],
  parallelly[availableCores]
)

# Some helper functions
med2rate = \(x) (12 * log(2)) / (x * 365.25)
months2days = \(x) 365.25 * x / 12
minus_plus = \(x, add) c(x - add, x + add)

# Load registry
reg = bt$loadRegistry(fs$path("simulations", "registry"), writeable = TRUE)

# Create cluster function
reg$cluster.functions = bt$makeClusterFunctionsSocket(
  ncpus = availableCores(omit = 1), fs.latency = 0
)

# Example: one problem with 10 replications
bt$addExperiments(
  prob.designs = list(
    ph = data.table(
      num_samples = list(c(15, 15)),
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

# Test the job (arbitrary replication number)
bt$testJob(id = 4)
bt$testJob(id = 6, external = TRUE)

# Submit all jobs
bt$submitJobs()
bt$waitForJobs()
bt$getStatus()

# Close connections
reg$cluster.functions = bt$makeClusterFunctionsInteractive()

x = vapply(
  fs$dir_ls(fs$path("simulations", "registry", "results")),
  fs$file_size,
  numeric(1)
) |>
  sum()

x * 1000 * 500

fs$dir_ls(fs$path("simulations", "registry", "results"))

test = fs$file_size(fs$path("simulations", "registry", "results", "6", ext = "rds"))


# Retrieve and analyze results
res = bt$reduceResultsDataTable(fun = \(res) res)

# This will unnest the results and annotate the job ID
f1 = function(dt) {
  out = lapply(1:nrow(dt), function(i) {
    res = dt[i, result][[1]]
    res[, job.id := dt[i, job.id]]
    res
  }) |> 
    rbindlist()
  
  setcolorder(out, neworder = "job.id")
  
  return(out)
}

test1 = f1(res)

# This gives us a somewhat complete data.table with the results
test2 = bt$unwrap(test1[, .(job.id, method, value)])[, value.1 := NULL][]

# For a proper analysis we need the associated parameters for each job (and algorithm)
test3 = bt$getJobPars()[, `:=`(algorithm = NULL, algo.pars = NULL)] |>
  bt$unwrap()

test2[1:10]
test3[1:10]

test4 = bt$ijoin(test2, test3)

test4[1:5]

test4[, .(rejection_rate = mean(pval <= 0.05, na.rm = TRUE)),
      by = .(method)]

test2
test3

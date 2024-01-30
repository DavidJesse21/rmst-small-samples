options(box.path = "R")

box::use(
  fs,
  data.table[...]
)

box::use(
  simfuns2/algos[rmst_asy, rmst_studperm, rmst_pseudo, rmst_pseudo_ij, rmst_pseudo_ij_boot]
)


load(fs$path("data", "Hellmann", ext = "Rdata"))
dt = data
rm(data)
setDT(dt)
setnames(dt, old = "group", new = "trt")

constants = list(
  cutoff = 18,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 2000L
)

# Asymptotic test
rmst_asy(dt, constants)

# Pseudo-Observations
rmst_pseudo(dt, constants)

# IJ pseudo-observations
rmst_pseudo_ij(dt, constants)

# Studentized permutation test
set.seed(42)
rmst_studperm(dt, constants)

# IJ pseudo-observations + bootstrap hypothesis testing
set.seed(42)
rmst_pseudo_ij_boot(dt, constants)



# Estimate runtimes ----

box::use(
  simfuns/utils[estimate_runtime],
  microbenchmark[microbenchmark]
)

set.seed(42)
bench = microbenchmark(
  asy = rmst_asy(dt, constants),
  studperm = rmst_studperm(dt, constants),
  po1 = rmst_pseudo(dt, constants),
  po2 = rmst_pseudo_ij(dt, constants),
  po_boot = rmst_pseudo_ij_boot(dt, constants),
  times = 3
)

t_milisec = summary(bench)$mean
names(t_milisec) = c("asy", "studperm", "po1", "po2", "po_boot")
t_sec = 0.001 * t_milisec

# Sequential runtime
estimate_runtime(sum(t_sec))

# Parallel runtime
estimate_runtime(sum(t_sec), parallel = list(num_cores = 4, prop = 0.9))
estimate_runtime(sum(t_sec), parallel = list(num_cores = 4, prop = 0.9), mult = 2)
# With 20:00:00 we should be on the safe side

options(box.path = "R")

box::use(
  survival[Surv, survfit],
  data.table[...],
  microbenchmark[microbenchmark],
  fs,
  eventglm[rmeanglm]
)

box::use(
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  rmst/pseudo[pseudo_strat, pseudo_infjack, rmst_pseudo_test, boot_pseudo],
)

load(fs$path("data", "Hellmann", ext = "Rdata"))
dt = data
rm(data)
setDT(dt)
dt

set.seed(42)

# This is for estimating the required runtimes ("walltime")
bench = microbenchmark(
  asy = rmst_diff_test(Surv(time, event) ~ group, dt, cutoff = 18, contrast = c("1", "0")),
  studperm = rmst_diff_studperm(Surv(time, event) ~ group, dt, cutoff = 18, contrast = c("1", "0"), num_samples = 2000L),
  pseudo_hc3 = {
    rmeanglm(Surv(time, event) ~ group, data = dt, time = 18, model.censoring = pseudo_strat, formula.censoring = ~ group) |>
      rmst_pseudo_test(vcov_type = "HC3")
  },
  pseudo_ij_boot = {
    rmeanglm(Surv(time, event) ~ group, data = dt, time = 18, model.censoring = pseudo_infjack, formula.censoring = ~ group) |>
      boot_pseudo(num_samples = 1000L) |>
      rmst_pseudo_test(vcov_type = "boot")
  },
  times = 10
)

# Medians in miliseconds
# asy: 4.63
# studperm: 2055.37
# pseudo_hc3: 64.52
# pseudo_ij_boot: 6302.69

bench$time
time_milisec = summary(bench)$median |>
  round(2)
names(time_milisec) = c("asy", "studperm", "pseudo_hc3", "pseudo_ij_boot")
time_sec = 0.001 * time_milisec
time_min = time_sec / 60
time_hours = time_min / 60

time_sec * 5000
time_min * 5000
time_hours * 5000
sum(time_hours) * 5000

box::use(simfuns/utils[estimate_runtime])

x = sum(time_sec)
#x = 2.1
total = x * 5000 * 1
hours = floor(total / 3600)
minutes = floor((total %% 3600) / 60)
seconds = total %% 60

total %% 2

estimate_runtime(sum(time_sec))
estimate_runtime(
  sum(time_sec),
  parallel = list(num_cores = 4, prop = 0.9)
)

estimate_sequential_runtime(8.86593)

# Runtime of one simulation scenario per function
# Asymptotic: ~ 24 second
# Studentized permutation: ~ 3 hours
# Pseudo HC3: ~ 5.5 minutes
# Pseudo IJ Boot: ~ 9.5 hours


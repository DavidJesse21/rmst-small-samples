# Setup ----

options(box.path = "R")
set.seed(42)

box::use(
  # Data wrangling
  data.table[...],
  # Data generation and analysis
  stats[rexp],
  survival[Surv],
  simnph = SimNPH,
  # Parallellization
  future[plan, multisession, sequential],
  future.apply[future_lapply],
  parallelly[availableCores],
  # Store results
  qs,
  fs
)

# RMST difference and test statistic
box::use(
  rmst/km[rmst_diff]
)


# Simulation setup ----

# RMST cutoff
cutoff = round(simnph$m2d(36))

# Simulation setup
design = CJ(
  # Median survival control: 12 months
  hazard_ctrl = simnph$m2r(12),
  # Treatment arm, leading to same RMST at 1096 days
  hazard_trt_before = simnph$m2r(9),
  hazard_trt_after = simnph$m2r(24),
  crossing = 326.8855,
  # ~20% censoring in control arm
  random_withdrawal = simnph$m2r(48),
  # Different sample sizes per arm
  num_samples = c(15, 25, 50, 100, 250, 500, 1000, 2500, 5000)
)

# Needs to be compatible with simnph$generate_crossing_hazards()
design[, `:=`(n_trt = num_samples, n_ctrl = num_samples)][, num_samples := NULL]


# Data generating function
gen_data = function(params) {
  dt = simnph$generate_crossing_hazards(params) |>
    simnph$random_censoring_exp(params$random_withdrawal)
  setDT(dt)
  setnames(dt, old = "t", new = "time")
  dt[, `:=`(
    evt = as.integer(evt),
    trt = fifelse(trt == 1, "trt", "ctrl")
  )]
  
  return(dt[])
}


# Analysis function: Calculate RMST test statistic
rmst_tstat = function(data) {
  x = rmst_diff(
    Surv(time, evt) ~ trt, data = data,
    cutoff = round(simnph$m2d(36)), contrast = c("trt", "ctrl")
  )
  
  unname(x[1] / x[2])
}


# Simulation ----

plan(multisession, workers = availableCores(omit = 1))

# Number of simulation replications
num_sims = 2500

# Run simulation
sim_results = future_lapply(
  1:nrow(design),
  function(i) {
    x = replicate(num_sims, {
      dt = gen_data(design[i])
      rmst_tstat(dt)
    })
    
    data.table(
      num_samples = design[i, n_trt],
      results = list(x)
    )
  },
  future.seed = 42
)
sim_results = rbindlist(sim_results)

# Parallellization not needed anymore
plan(sequential)

# Store results
qs$qsave(
  sim_results,
  fs$path("simulations", "null_scenarios", "nph_crossing", "results", ext = "qs")
)


# Setup ----

options(box.path = "R")
set.seed(42)

box::use(
  # Data wrangling
  data.table[...],
  # Data generation and analysis
  stats[rexp],
  survival[Surv],
  # Parallellization
  future[plan, multisession, sequential],
  future.apply[future_lapply],
  parallelly[availableCores],
  # Simulation helpers
  simnph = SimNPH,
  # Store results
  qs,
  fs
)

# RMST difference and test statistic
box::use(
  rmst/km[rmst_diff]
)


# Simulation setup ----

# # Simulation setup
# design = CJ(
#   # Event rate
#   lambda = simnph$m2r(12),
#   # ~ 33%, 25%, 20%, 11% censoring
#   lambda_cens = simnph$m2r(c(24, 36, 48, 96)),
#   # Sample size per arm
#   num_samples = c(15, 25, 50, 100)
# )

# Simulation setup (start off simpley)
design = CJ(
  # Event rate
  lambda = simnph$m2r(12),
  # 20% censoring
  lambda_cens = simnph$m2r(48),
  # Sample size per arm
  num_samples = c(15, 25, 50, 100, 250, 500, 1000, 2500, 5000)
)


# Data generating function
gen_data = function(params) {
  dt = data.table(
    trt = rep(c("trt", "ctrl"), each = params$num_samples),
    time = round(rexp(params$num_samples * 2, params$lambda)),
    time_cens = round(rexp(params$num_samples * 2, params$lambda_cens))
  )
  dt[, status := fifelse(time <= time_cens, 1L, 0L)][, time_cens := NULL]
  
  return(dt[])
}


# Analysis function: Calculate RMST test statistic
rmst_tstat = function(data) {
  x = rmst_diff(
    Surv(time, status) ~ trt, data = data,
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
      num_samples = design[i, num_samples],
      lambda_cens = design[i, lambda_cens],
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
  fs$path("simulations", "null_scenarios", "ph_exp", "results", ext = "qs")
)

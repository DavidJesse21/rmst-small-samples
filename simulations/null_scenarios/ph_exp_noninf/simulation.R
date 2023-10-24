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

box::use(
  rmst/km[rmst_diff],
  niconvert = nimargin/convert
)


# Simulation setup -----

# Cutoff time point for RMST
cutoff = round(simnph$m2d(36))

design = CJ(
  # Control arm: median survival 12 months
  hazard_ctrl = simnph$m2r(12),
  # Varying hazard rates for treatment arm
  hazard_trt = simnph$m2r(seq(12, 6, by = -1)),
  # Sample size per arm (just one setting for now)
  num_samples = 25,
  # (Common) censoring rate (~ 25% for control arm)
  cens_rate = simnph$m2r(36)
)

# Compute non-inferiority margins
design[, margin_hr := hazard_trt / hazard_ctrl][
  , margin_drmst := niconvert$hr_to_drmst(margin_hr, hazard_ctrl, cutoff)
]

# Data generating function
gen_data = function(params) {
  dt = data.table(
    trt = rep(c("trt", "ctrl"), each = params$num_samples),
    time = round(c(
      rexp(params$num_samples, params$hazard_trt),
      rexp(params$num_samples, params$hazard_ctrl)
    )),
    time_cens = round(rexp(params$num_samples * 2, params$cens_rate))
  )
  
  dt[, status := fifelse(time <= time_cens, 1L, 0L)][
    , time := fifelse(status == 1L, time, time_cens)
  ][, time_cens := NULL]
  
  return(dt[])
}


# Analysis function: Calculate RMST test statistic
rmst_tstat = function(data, margin) {
  x = rmst_diff(
    Surv(time, status) ~ trt, data = data,
    cutoff = round(simnph$m2d(36)), contrast = c("trt", "ctrl")
  )
  
  unname((x[1] - margin) / x[2])
}


# Simulation ----

plan(multisession, workers = availableCores(omit = 1))

# Number of simulation replications
num_sims = 7300

# Run simulation
sim_results = future_lapply(
  1:nrow(design),
  function(i) {
    x = replicate(num_sims, {
      dt = gen_data(design[i])
      rmst_tstat(dt, design[i, margin_drmst])
    })
    
    data.table(
      margin_hr = design[i, margin_hr],
      margin_drmst = design[i, margin_drmst],
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
  fs$path("simulations", "null_scenarios", "ph_exp_noninf", "results", ext = "qs")
)

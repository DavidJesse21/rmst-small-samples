options(box.path = "R")

box::use(
  # Data wrangling
  data.table[...],
  # Data generation and analysis
  stats[rexp],
  survival[Surv],
  # Parallellization
  future[plan, multisession, sequential],
  parallelly[availableCores],
  # Manage/store results
  fs,
  qs
)

box::use(
  simfuns/run_sim[run_sim_all],
  ./`firstsim-functions`[f_asy, f_studperm, f_pseudo],
  rmst/true
)

sim_dir = fs$path("simulations", "first_sim")


# Helper functions copied from SimNPH package
med2rate = \(x) (12 * log(2)) / (x * 365.25)
months2days = \(x) 365.25 * x / 12
minus_plus = \(x, add) c(x - add, x + add)


# Data generating mechanism ----

# For nor only try out "one" setting, in which either H0 or H1 is true
design = CJ(
  lambda_ctrl = med2rate(12),
  lambda_trt = med2rate(c(12, 24)),
  lambda_cens = med2rate(36),
  num_samples = 25,
  cutoff = months2days(36),
  true_diff = NA_real_
)
for (i in 1:nrow(design)) {
  set(
    design, i = i, j = "true_diff",
    value = diff(true$expo(
      design[i, c(lambda_ctrl, lambda_trt)],
      design[i, cutoff]
    ))
  )
}


# Data generating function
gen_data = function(params) {
  dt = data.table(
    trt = rep(c(1L, 0L), each = params$num_samples),
    time = round(c(
      rexp(params$num_samples, params$lambda_trt),
      rexp(params$num_samples, params$lambda_ctrl)
    )),
    time_cens = round(rexp(params$num_samples * 2, params$lambda_cens))
  )
  
  dt[, status := fifelse(time <= time_cens, 1L, 0L)][
    , time := fifelse(status == 1L, time, time_cens)
  ][, time_cens := NULL]
  
  return(dt[])
}

# Analysis functions
li_funs = list(
  asy = f_asy,
  studperm = f_studperm,
  pseudo = f_pseudo
)


# Run the simulation ----

plan(multisession, workers = availableCores(omit = 1))

run_sim_all(
  design,
  num_sims = 500,
  fun_generate = gen_data,
  funs_analyze = li_funs,
  dir_out = fs$path(sim_dir, "results")
)

plan(sequential)

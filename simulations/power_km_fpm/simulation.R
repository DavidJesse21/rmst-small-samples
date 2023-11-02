# Setup ----

options(box.path = "R")
set.seed(42)

box::use(
  # Data wrangling
  data.table[...],
  # Data generation and analysis
  stats[rexp],
  survival[Surv],
  flexsurv[flexsurvspline, standsurv],
  # Parallellization
  future[plan, multisession, sequential],
  future.apply[future_lapply, future_replicate],
  parallelly[availableCores],
  # Simulation helpers
  simnph = SimNPH,
  # Store results
  qs,
  fs
)

# RMST difference and test statistic
box::use(
  rmst/km[rmst_diff],
  nimargin/convert[hr_to_drmst]
)


# Simulation setup ----

# One setting only for now
design = data.table(
  lambda_ctrl = simnph$m2r(12),
  lambda_trt = simnph$m2r(12),
  lambda_cens = simnph$m2r(48),
  num_samples = 100
)

# Effect sizes
effect_hr = design[, lambda_trt / lambda_ctrl]
effect_drmst = hr_to_drmst(effect_hr, design$lambda_ctrl, cutoff = simnph$m2d(36))

# Data generating function
gen_data = function(params) {
  dt = data.table(
    trt = rep(c("trt", "ctrl"), each = params$num_samples),
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

# Calculate RMST difference test statistic based on Kaplan-Meier
tstat_km = function(data) {
  x = rmst_diff(
    Surv(time, status) ~ trt, data = data,
    cutoff = round(simnph$m2d(36)), contrast = c("trt", "ctrl")
  )
  
  unname(x[1] / x[2])
}

# Calculate RMST difference test statistic based on FPMs
tstat_fpm = function(data) {
  li_rmst = lapply(c("trt", "ctrl"), function(group) {
    fit = flexsurvspline(Surv(time, status) ~ 1, data = data[trt == group], k = 2)
    df_rmst = standsurv(fit, type = "rmst", t = round(simnph$m2d(36)), se = TRUE, boot = FALSE)
    df_rmst$trt = group
    return(df_rmst)
  }) |>
    suppressMessages()
  
  x = rbindlist(li_rmst)
  diff = x[trt == "trt", at1] - x[trt == "ctrl", at1]
  se_diff = x[, sqrt(sum(at1_se^2))]
  out = diff / se_diff
  
  return(out)
}


# Simulation ----

plan(multisession, workers = availableCores(omit = 1))

# Number of simulation replications
num_sims = 2500

dt_sims = future_replicate(
  num_sims,
  gen_data(design),
  future.seed = 42,
  simplify = FALSE
)

res_km = future_vapply(
  dt_sims,
  \(dt) tryCatch(tstat_km(dt), error = \(e) NA_real_),
  numeric(1)
)

res_fpm = future_vapply(
  dt_sims,
  \(dt) tryCatch(tstat_fpm(dt), error = \(e) NA_real_),
  numeric(1),
  future.seed = 42
)

emp_power = function(x, alpha = 0.05) {
  x = x[!is.na(x)]
  
  reject_h0 = abs(x) > qnorm(1 - alpha/2)
  mean(reject_h0)
}

emp_power(res_km) * 100
emp_power(res_fpm) * 100

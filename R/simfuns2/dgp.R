box::use(
  stats[rexp, rweibull, runif],
  data.table[data.table, fifelse, setcolorder],
  miniPCH[rpwexp = rpch]
)


#' Generate possibly censored survival data
#' 
#' @param n0,n1 (`numeric(1)`)\cr
#'   The sample sizes for the control and treatment group, respectively.
#' @param surv_model (`character(1)`)\cr
#'   The name of the model for generating the uncensored survival times.
#' @param surv_params (`numeric()`)\cr
#'   A named vector with the parameters required by the survival model.
#' @param cens_model (`character(1)`)\cr
#'   The name of the model for generating the censoring times.
#' 
#' @export
gen_surv_data = function(n0, n1,
                         surv_model, surv_params,
                         cens_model,
                         cutoff) {
  estimable = FALSE
  
  while (!estimable) {
    surv_times = li_surv_models[[surv_model]](n0, n1, surv_params)
    cens_times = li_cens_models[[cens_model]](n0, n1)
    
    # Complete data set
    dt = do.call(combine_and_censor, c(surv_times, cens_times))
    
    # Check estimability
    estimable = is_estimable(dt, cutoff)
  }
  
  return(dt)
}


# Survival times ----

# Proportional hazards / exponential distributions
surv_ph_exp = function(n0, n1, surv_params) {
  t0 = rexp(n0, surv_params["lambda0"])
  t1 = rexp(n1, surv_params["lambda1"])
  
  list(t0, t1)
}

# Crossing hazards / piecewise exponential distributions
surv_cross_pwexp = function(n0, n1, surv_params) {
  t0 = rexp(n0, surv_params["lambda0"])
  t1 = rpwexp(n1, c(0, surv_params["crosstime"]), surv_params[c("lambda11", "lambda12")])
  
  list(t0, t1)
}

# Crossing hazards / Weibull distributions
surv_cross_wb = function(n0, n1, surv_params) {
  t0 = rweibull(n0, surv_params["shape0"], surv_params["scale0"])
  t1 = rweibull(n1, surv_params["shape1"], surv_params["scale1"])
  
  list(t0, t1)
}

# List of survival models for lookup
li_surv_models = list(
  ph_exp = surv_ph_exp,
  crossing_wb = surv_cross_wb,
  crossing_pwexp = surv_cross_pwexp
)


# Censoring times ----

# Unequal censoring with Weibull distributions
cens_uneq_wb = function(n0, n1,
                        shape0 = 3, scale0 = 18,
                        shape1 = 0.5, scale1 = 40) {
  c0 = rweibull(n0, shape0, scale0)
  c1 = rweibull(n1, shape1, shape1)
  
  list(c0, c1)
}

# Equal censoring with uniform distributions
cens_eq_unif = function(n0, n1, a = 0, b = 25) {
  c0 = runif(n0, a, b)
  c1 = runif(n1, a, b)
  
  list(c0, c1)
}

# Equal censoring with Weibull distributions
cens_eq_wb = function(n0, n1, shape = 3, scale = 15) {
  c0 = rweibull(n0, shape, scale)
  c1 = rweibull(n1, shape, scale)
  
  list(c0, c1)
}

# List of censoring models for lookup
li_cens_models = list(
  uneq_wb = cens_uneq_wb,
  eq_unif = cens_eq_unif,
  eq_wb = cens_eq_wb
)


# Miscellaneous ----

# Combine survival and censoring times and apply censoring
combine_and_censor = function(t0, t1, c0, c1) {
  dt = data.table(
    time = c(t0, t1),
    time_cens = c(c0, c1),
    trt = rep(0:1, c(length(t0), length(t1)))
  )
  
  dt[, event := fifelse(time <= time_cens, 1L, 0L)][
    , `:=`(time = pmin(time, time_cens),
           time_cens = NULL)
  ]
  
  setcolorder(dt, c("time", "event", "trt"))
  
  return(dt[])
}

# Check if RMST contrast is estimable
is_estimable = function(dt, cutoff) {
  idx = dt[, .I[which.max(time)], by = trt]$V1
  check = !dt[idx, (time < cutoff) & (event == 0)]
  all(check)
}

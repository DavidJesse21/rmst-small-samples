box::use(
  stats[rexp, rweibull, runif],
  data.table[data.table, fifelse, setcolorder],
  miniPCH[rpwexp = rpch]
)



#' Generate two-sample survival data
#' 
#' @param data,job Internal parameters for `batchtools`.
#' @param samples_alloc (`numeric(2)`)\cr
#'   The base sample sizes for the control and treatment group, respectively.
#' @param samples_k (`numeric(1)`)\cr
#'   A positive integerish value with which the base sample sizes get multiplied.
#' @param surv_params (`numeric()`)\cr
#'   A named vector of parameters for the time to event distributions.
#' @param cens_model (`character(1)`)\cr
#'   The name of the censoring model used for looking up the data generating function for the 
#'   censoring times.
#'   Must be one of `"wb_uneq"`, `"unif_eq"` or `"wb_eq"`.
#' @param ... Ignored.
#' 
#' @name gen_data
NULL


#' Proportional hazards: exponential distributions
#' 
#' @rdname gen_data
#' 
#' @description
#' The survival times from both the control and treatment group follow an exponential 
#' distribution, thus fulfilling the proportional hazards assumption.
#' 
#' @section Survival parameters:
#' `params_surv` must contain the following named elements:
#' * `lambda0`: The rate parameter for the control group
#' * `lambda1`: The rate parameter for the treatment group
#' 
#' @export
gen_data_exp = function(data, job, 
                        samples_alloc, samples_k,
                        surv_params,
                        cens_model,
                        ...) {
  estimable = FALSE
  
  while (!estimable) {
    # Sample sizes
    n0 = samples_alloc[1] * samples_k
    n1 = samples_alloc[2] * samples_k
    
    # Survival times
    t0 = rexp(n0, surv_params["lambda0"])
    t1 = rexp(n1, surv_params["lambda1"])
    
    # Censoring times
    cens_times = li_cens_models[[cens_model]](n0, n1)
    
    # Complete data set
    dt = combine_and_censor(t0, t1, cens_times[[1]], cens_times[[2]])
    
    # Check estimability
    estimable = is_estimable(dt, data$cutoff)
  }
  
  return(dt)
}



#' Crossing hazards: exponential and piecewise exponential distribution
#' 
#' @rdname gen_data
#' 
#' @description
#' The survival times of the control group follow an exponential distribution while the ones 
#' from the treatment group come from a piecewise exponential distribution.
#' The distributions are parametrized such that the hazards cross at some time point.
#' 
#' @section Survival parameters: 
#' `params_surv` must contain the following named elements:
#' * `lambda0`: The constant hazard rate of the control group
#' * `lambda11`: The hazard rate of the treatment group in the first time interval
#' * `lambda12`: The hazard rate of the treatment group in the second time interval
#' * `crosstime`: The time point at which the hazard rate of the treatment group changes
#' 
#' @export
gen_data_pwexp = function(data, job, 
                          samples_alloc, samples_k,
                          surv_params,
                          cens_model,
                          ...) {
  estimable = FALSE
  
  while (!estimable) {
    # Sample sizes
    n0 = samples_alloc[1] * samples_k
    n1 = samples_alloc[2] * samples_k
    
    # Survival times
    t0 = rexp(n0, surv_params["lambda0"])
    t1 = rpwexp(n1, c(0, surv_params["crosstime"]), surv_params[c("lambda11", "lambda12")])
    
    # Censoring times
    cens_times = li_cens_models[[cens_model]](n0, n1)
    
    # Complete data set
    dt = combine_and_censor(t0, t1, cens_times[[1]], cens_times[[2]])
    
    # Check estimability
    estimable = is_estimable(dt, data$cutoff)
  }
  
  return(dt)
}


#' Crossing hazards: Weibull distribution
#' 
#' @rdname gen_data
#' 
#' @description
#' The survival times of both groups follow Weibull distributions, which are parametrized 
#' such that the hazards cross at some time point.
#' 
#' @section Survival parameters: 
#' `params_surv` must contain the following named elements:
#' * `shape0`, `scale0`: Shape and scale parameters for the control group
#' * `shape1`, `scale1`: Shape and scale parameters for the treatment group
#' 
#' @export
gen_data_weibull = function(data, job, 
                            samples_alloc, samples_k,
                            surv_params,
                            cens_model,
                            ...) {
  estimable = FALSE
  
  while (!estimable) {
    # Sample sizes
    n0 = samples_alloc[1] * samples_k
    n1 = samples_alloc[2] * samples_k
    
    # Survival times
    t0 = rweibull(n0, surv_params["shape0"], surv_params["scale0"])
    t1 = rweibull(n1, surv_params["shape1"], surv_params["scale1"])
    
    # Censoring times
    cens_times = li_cens_models[[cens_model]](n0, n1)
    
    # Complete data set
    dt = combine_and_censor(t0, t1, cens_times[[1]], cens_times[[2]])
    
    # Check estimability
    estimable = is_estimable(dt, data$cutoff)
  }
  
  return(dt)
}


# Censoring functions ----

# Unequal censoring with Weibull distributions
cens_wb_uneq = function(n0, n1,
                        shape0 = 3, scale0 = 18,
                        shape1 = 0.5, scale1 = 40) {
  c0 = rweibull(n0, shape0, scale0)
  c1 = rweibull(n1, shape1, shape1)
  
  list(c0, c1)
}

# Equal censoring with uniform distributions
cens_unif_eq = function(n0, n1, a = 0, b = 25) {
  c0 = runif(n0, a, b)
  c1 = runif(n1, a, b)
  
  list(c0, c1)
}

# Equal censoring with Weibull distributions
cens_wb_eq = function(n0, n1, shape = 3, scale = 15) {
  c0 = rweibull(n0, shape, scale)
  c1 = rweibull(n1, shape, scale)
  
  list(c0, c1)
}

# List of censoring models for lookup
li_cens_models = list(
  wb_uneq = cens_wb_uneq,
  unif_eq = cens_unif_eq,
  wb_eq = cens_wb_eq
)

# Combine generated survival and censoring times
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


# Miscellaneous ----

# Check if RMST contrast is estimable
is_estimable = function(dt, cutoff) {
  idx = dt[, .I[which.max(time)], by = trt]$V1
  check = !dt[idx, (time < cutoff) & (event == 0)]
  all(check)
}

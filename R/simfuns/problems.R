box::use(
  stats[rexp, rweibull],
  data.table[data.table, fifelse, setcolorder],
  miniPCH[rpwexp = rpch]
)



#' Generate two-sample survival data
#' 
#' @param samples_alloc (`numeric(2)`)\cr
#'   The base sample sizes for the control and treatment group, respectively.
#' @param samples_k (`numeric(1)`)\cr
#'   A positive integerish value with which the base sample sizes get multiplied.
#' @param params_surv (`numeric()`)\cr
#'   A named vector of parameters for the time to event distributions.
#' @param params_cens (`numeric(2)`)\cr
#'   A named vector of parameters for the censoring distributions.
#' 
#' @note
#' The vector `params_cens` must always contain the values `lambda0` and `lambda1` 
#' referring to the rate parameters of the exponential censoring distributions for 
#' the control and treatment group, respectively.
#' These distributional assumptions do not differ between the different simulation problems 
#' (though the parameters are varied within each problem).
#' With regards to `params_surv` consult the documentation of the respective function.
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
gen_data_exp = function(samples_alloc, samples_k,
                        params_surv, params_cens) {
  # Sample sizes
  n0 = samples_alloc[1] * samples_k
  n1 = samples_alloc[2] * samples_k
  
  # Survival times
  t0 = rexp(n0, params_surv["lambda0"])
  t1 = rexp(n1, params_surv["lambda1"])
  
  # Censoring times
  c0 = rexp(n0, params_cens["lambda0"])
  c1 = rexp(n1, params_cens["lambda1"])
  
  # Complete data set
  dt = combine_and_censor(n0, n1, t0, t1, c0, c1)
  
  return(dt[])
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
gen_data_pwexp = function(samples_alloc, samples_k,
                          params_surv, params_cens) {
  # Sample sizes
  n0 = samples_alloc[1] * samples_k
  n1 = samples_alloc[2] * samples_k
  
  # Survival times
  t0 = rexp(n0, params_surv["lambda0"])
  t1 = rpwexp(n1, c(0, params_surv["crosstime"]), params_surv[c("lambda11", "lambda12")])
  
  # Censoring times
  c0 = rexp(n0, params_cens["lambda0"])
  c1 = rexp(n1, params_cens["lambda1"])
  
  # Complete data set
  dt = combine_and_censor(n0, n1, t0, t1, c0, c1)
  
  return(dt[])
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
gen_data_weibull = function(samples_alloc, samples_k,
                            params_surv, params_cens) {
  # Sample sizes
  n0 = samples_alloc[1] * samples_k
  n1 = samples_alloc[2] * samples_k
  
  # Survival times
  t0 = rweibull(n0, params_surv["shape0"], params_surv["scale0"])
  t1 = rweibull(n1, params_surv["shape1"], params_surv["scale1"])
  
  # Censoring times
  c0 = rexp(n0, params_cens["lambda0"])
  c1 = rexp(n1, params_cens["lambda1"])
  
  # Complete data set
  dt = combine_and_censor(n0, n1, t0, t1, c0, c1)
  
  return(dt[])
}


# Internal helper
combine_and_censor = function(n0, n1, t0, t1, c0, c1) {
  dt = data.table(
    time = c(t0, t1),
    time_cens = c(c0, c1),
    trt = rep(0:1, c(n0, n1))
  )
  
  dt[, event := fifelse(time <= time_cens, 1L, 0L)][, time_cens := NULL]
  setcolorder(dt, c("time", "event", "trt"))
  
  return(dt[])
}

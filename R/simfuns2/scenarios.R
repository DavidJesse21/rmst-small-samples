options(box.path = "R")

box::use(
  data.table[...],
  stats[setNames, uniroot, pweibull]
)

box::use(
  rmean = rmst/true
)


#' Create a data.table with different simulation scenarios
#' @export
make_scenarios = function(surv_model = c("ph_exp", "crossing_pwexp", "crossing_wb"),
                          cens_model = c("eq_wb", "uneq_wb", "eq_unif"),
                          rmst_diff = c(0, 1.5),
                          samples_alloc = list(c(12, 18), c(15, 15), c(18, 12)),
                          samples_k = c(1L, 2L, 4L, 6L)) {
  dt = CJ(
    surv_model = surv_model,
    cens_model = cens_model,
    rmst_diff = rmst_diff,
    samples_alloc.id = seq_along(samples_alloc),
    samples_k = c(1L, 2L, 4L, 6L)
  )
  
  dt_samples_alloc = as.data.table(transpose(samples_alloc))
  setnames(dt_samples_alloc, new = c("n0", "n1"))
  dt_samples_alloc[, samples_alloc.id := 1:.N]
  
  dt = merge(dt, dt_samples_alloc, by = "samples_alloc.id")
  
  dt[, `:=`(n0 = n0 * samples_k,
            n1 = n1 * samples_k,
            samples_alloc.id = NULL)]
  
  dt[, scenario.id := 1:.N]
  setcolorder(dt, new = "scenario.id")
  
  return(dt[])
}


#' Make parameters for a data generating function
#' 
#' @param row (`data.table()`)\cr
#'   A single row of a scenario data.table as it is returned by `make_scenarios()`.
#' @param ... (`ANY`)\cr
#'   Optional arguments passed to one of the `get_params_` functions.
#' 
#' @returns (`list()`)\cr
#'   A named list with arguments that can be passed to `gen_surv_data()`.
#'   This is especially useful in combination with `do.call()`.
#' 
#' @export
make_params = function(row, cutoff = 10, ...) {
  params = switch(row$surv_model,
    "ph_exp" = get_params_exp(row$rmst_diff, cutoff = cutoff, ...),
    "crossing_pwexp" = get_params_pwexp(row$rmst_diff, cutoff = cutoff, ...),
    "crossing_wb" = get_params_weibull(row$rmst_diff, cutoff = cutoff, ...)
  )
  
  params = merge(row, params, by = "surv_model")
  args = c("n0", "n1", "surv_model", "surv_params", "cens_model")
  setcolorder(params, neworder = args)
  for (j in setdiff(colnames(params), args)) {
    set(params, j = j, value = NULL)
  }
  
  params = as.list(params)
  params$surv_params = params$surv_params[[1L]]
  params$cutoff = cutoff
  
  return(params)
}


# Distribution calibration ----


#' Get parameters for survival model "ph_exp"
#' 
#' @description
#' Given a fixed rate parameter for the control group, this function finds the rate 
#' parameter for the treatment group for different magnitudes of the difference in 
#' restricted mean survival time.
#' 
#' @param rmst_diff (`numeric()`)\cr
#'   Vector of RMST differences for which the design table should be created.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param lamda0 (`numeric(1)`)\cr
#'   Rate parameter of the control group.
#'   
#' @export
get_params_exp = function(rmst_diff = c(0, 1.5), lambda0 = 0.2, cutoff = 10) {
  params = data.table(surv_model = "ph_exp", rmst_diff = rmst_diff)
  
  set(
    params, j = "surv_params",
    value = lapply(rmst_diff, function(delta) {
      lambda1 = find_lambda1(delta, lambda0, cutoff)
      c(lambda0 = lambda0, lambda1 = lambda1)
    })
  )
  
  return(params)
}

# Helper function
find_lambda1 = function(delta, lambda0 = 0.2, cutoff = 10) {
  rmst0 = rmean$expo(lambda0, cutoff)
  f = \(lambda1) rmean$expo(lambda1, cutoff) - rmst0 - delta
  opt = uniroot(f, interval = c(0.0001, lambda0), tol = sqrt(.Machine$double.eps))
  opt$root
}


#' Get parameters for problem "crossing_pwexp"
#' 
#' @description
#' Given a fixed rate parameter for the exponential distribution of the control group and 
#' two fixed rate parameters for the piecewise exponential distribution of the treatment group, 
#' this function finds the crossing time at which the hazards of the treatment group change 
#' for different magnitudes of the difference in restricted mean survival time.
#' 
#' @param rmst_diff (`numeric()`)\cr
#'   Vector of RMST differences for which the design table should be created.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param lamda0 (`numeric(1)`)\cr
#'   Rate parameter of the control group.
#' @param lambdas1 (`numeric(2)`)\cr
#'   Two rate parameters of the treatment group for the piecewise exponential distribution.
#'   
#' @export
get_params_pwexp = function(rmst_diff = c(0, 1.5),
                            lambda0 = 0.2, lambdas1 = c(0.5, 0.05),
                            cutoff = 10) {
  params = data.table(surv_model = "crossing_pwexp", rmst_diff = rmst_diff)
  
  set(
    params, j = "surv_params",
    value = lapply(rmst_diff, function(delta) {
      crosstime = find_crosstime(delta, lambda0, lambdas1, cutoff)
      out = c(lambda0, lambdas1, crosstime)
      names(out) = c(paste0("lambda", c("0", "11", "12")), "crosstime")
      return(out)
    })
  )
  
  return(params)
}

# Helper function
find_crosstime = function(delta, lambda0 = 0.2, lambdas1 = c(0.5, 0.05), cutoff = 10) {
  rmst0 = rmean$expo(lambda0, cutoff)
  f = \(tstar) rmean$pwexp(hazards = lambdas1, tstar, cutoff) - rmst0 - delta
  opt = uniroot(f, interval = c(0, 10), tol = sqrt(.Machine$double.eps))
  opt$root
}


#' Get parameters for problem "crossing_weibull"
#' 
#' @description
#' Given fixed parameters for the Weibull distribution of the control group and a fixed 
#' scale parameter for the Weibull distribution of the treatment group, this function finds 
#' the shape parameter for the treatment group.
#' 
#' @param rmst_diff (`numeric()`)\cr
#'   Vector of RMST differences for which the design table should be created.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param shape0,scale0 (`numeric(1)`)\cr
#'   Shape and scale parameters of the control group for the Weibull distribution.
#' @param scale1 (`numeric(1)`)\cr
#'   Scale parameter of the treatment group for the Weibull distribution
#' 
#' @export
get_params_weibull = function(rmst_diff = c(0, 1.5),
                              shape0 = 3, scale0 = 8, scale1 = 14, 
                              cutoff = 10) {
  params = data.table(surv_model = "crossing_wb", rmst_diff = rmst_diff)
  
  set(
    params, j = "surv_params",
    value = lapply(rmst_diff, function(delta) {
      shape1 = find_shape1(delta, shape0, scale0, scale1, cutoff)
      out = c(shape0, scale0, shape1, scale1)
      names(out) = paste0(rep(c("shape", "scale"), 2), rep(0:1, each = 2))
      return(out)
    })
  )
  
  return(params)
}

# Helper function
find_shape1 = function(delta, shape0 = 3, scale0 = 8, scale1 = 14, cutoff = 10) {
  rmst0 = rmean$numint(\(x) 1 - pweibull(x, shape = 3, scale = 8), cutoff = 10)
  
  f = function(shape) {
    rmst1 = rmean$numint(\(x) 1 - pweibull(x, shape = shape, scale = scale1), cutoff = cutoff)
    rmst1 - rmst0 - delta
  }
  
  opt = uniroot(f, lower = 0.000001, upper = 100, tol = sqrt(.Machine$double.eps))
  opt$root
}


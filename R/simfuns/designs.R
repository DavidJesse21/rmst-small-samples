options(box.path = "R")

box::use(
  data.table[...],
  stats[setNames, uniroot, pweibull]
)

box::use(
  rmean = rmst/true
)



#' Create problem design data.table
#' 
#' @param rmst_diff (`numeric()`)\cr
#'   Vector of RMST differences for which the design table should be created.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param params_exp,params_pwexp,params_weibull (named `list()`)\cr
#'   Fixed parameters of the respective problem configuration.
#'   See the documentation of the `get_params_()` functions for more details.
#' @param cens_surv (`numeric(3)`)\cr
#'   Survival probabilities of the censoring processes at `cutoff`.
#'   See `get_params_cens()` for more details.
#' @param split (`logical(1)`)\cr
#'   Whether to split the complete design data.table by its `problem` column.
#'   This can be useful when using `batchtools` as we need a named list of design 
#'   parameters anyway.
#' 
#' @export
make_prob_design = function(rmst_diff = c(0, 1, 1.5), cutoff = 10,
                            params_exp = list(lambda0 = 0.2),
                            params_pwexp = list(lambda0 = 0.2, lambdas1 = c(0.5, 0.05)),
                            params_weibull = list(shape0 = 3, scale0 = 8, scale1 = 14),
                            cens_surv = c(0.625, 0.75, 0.875),
                            split = TRUE) {
  # Fixed parameters
  samples_alloc = list(c(12, 18), c(15, 15), c(18, 12))
  samples_alloc = lapply(samples_alloc, setNames, nm = c("ctrl", "trt"))
  
  des_base = CJ(
    problem = c("ph_exp", "crossing_pwexp", "crossing_wb"),
    rmst_diff = rmst_diff,
    cens.id = 1:3,
    samples_alloc = samples_alloc,
    samples_k = c(1, 2, 4, 6),
    # `CJ()` argument
    sorted = FALSE
  )
  
  # Distributional parameters calibrated according to effect size
  get_params = function(params, fun) {
    do.call(
      fun,
      c(list(rmst_diff = rmst_diff), params, list(cutoff = cutoff))
    )
  }
  params_exp = get_params(params_exp, get_params_exp)
  params_pwexp = get_params(params_pwexp, get_params_pwexp)
  params_weibull = get_params(params_weibull, get_params_weibull)
  params_all = rbindlist(list(params_exp, params_pwexp, params_weibull))
  
  # Merge the calibrated parameters
  des = merge(des_base, params_all, by = c("problem", "rmst_diff"))
  
  # Censoring parameters
  params_cens = get_params_cens(cens_surv, cutoff)
  des = merge(des, params_cens, by = "cens.id")
  set(des, j = "cens.id", value = NULL)
  
  # Order by `problem`
  des[, problem := factor(problem, levels = c("ph_exp", "crossing_pwexp", "crossing_wb"))]
  setorder(des, problem)
  
  # Split by problem?
  if (split) {
    des = split(des, by = "problem")
    for (dt in des) {
      set(dt, j = "problem", value = NULL)
    }
    return(des)
  } else {
    return(des[])
  }
}



# Distribution calibration ---

#' Get parameters for problem "ph_exp"
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
get_params_exp = function(rmst_diff = c(0, 1, 1.5), lambda0 = 0.2, cutoff = 10) {
  params = data.table(problem = "ph_exp", rmst_diff = rmst_diff)
  
  set(
    params, j = "params_surv",
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
get_params_pwexp = function(rmst_diff = c(0, 1, 1.5),
                            lambda0 = 0.2, lambdas1 = c(0.5, 0.05),
                            cutoff = 10) {
  params = data.table(problem = "crossing_pwexp", rmst_diff = rmst_diff)
  
  set(
    params, j = "params_surv",
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
get_params_weibull = function(rmst_diff = c(0, 1, 1.5),
                              shape0 = 3, scale0 = 8, scale1 = 14, 
                              cutoff = 10) {
  params = data.table(problem = "crossing_wb", rmst_diff = rmst_diff)
  
  set(
    params, j = "params_surv",
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



# Censoring parameters ----

#' Get parameters for the censoring distributions
#' 
#' @param prob (`numeric(3)`)\cr
#'   A vector of survival probabilities for the censoring process at the specified 
#'   `cutoff` (restriction time) value.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' 
#' @note
#' This function is opinionated and not very flexible.
#' `probs` is expected to be of length 3 and has an ascending order.
#' From this argument only 3 combinations will be created:
#' 1. Both groups have `probs[2]`.
#' 2. Control group has `probs[1]` and treatment group has `probs[3]`
#' 3. Control group has `probs[3]` and treatment group has `probs[1]`
#'   
#' @export
get_params_cens = function(probs = c(0.625, 0.75, 0.875), cutoff = 10) {
  stopifnot(length(probs) == 3)
  
  rates = - log(probs) / cutoff
  # Just a subset for some convenience
  rates2 = rates[c(1, length(rates))]
  
  x = CJ(lambda0 = rates2, lambda1 = rates2)
  x = x[lambda0 != lambda1]
  x = rbindlist(list(x, data.table(lambda0 = rates[2], lambda1 = rates[2])))
  setorder(x, lambda0)
  #set(x, j = "cens.id", value = 1:3)
  
  # Make list columns and delete the original ones
  for (i in 1:nrow(x)) {
    set(
      x, i = i, j = "params_cens",
      value = list(unlist(x[i]))
    )
  }
  for (j in paste0("lambda", 0:1)) {
    set(x, j = j, value = NULL)
  }
  
  # Assign ID for merging with design data.table
  set(x, j = "cens.id", value = 1:nrow(x))
  setcolorder(x, neworder = "cens.id")
  
  return(x[])
}

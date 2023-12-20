options(box.path = "R")

box::use(
  stats[qnorm],
  data.table[data.table],
  survival[Surv],
  # Asymptotic and studentized permutation test
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  # Pseudo-observation approaches
  eventglm[rmeanglm],
  rmst/pseudo[rmst_pseudo_test, pseudo_strat]
)

box::use(
  simfuns/utils[trycatch2]
)


#' Evaluate all RMST methods
#' 
#' @param data,job,instance Internal parameters for `batchtools`.
#' @param ... Ignored.
#' 
#' @section `batchtools` parameters:
#' `data` contains static/constant objects that stay the same for all experiments/simulations.
#' For this simulation these objects are (as of now):
#' * `cutoff`: The restriction time
#' * `alpha`: The significance level used for testing and constructing confidence intervals
#' * `var_method_asy`: Variance estimation method used for the asymptotic test
#' * `var_method_studperm`: Variance estimation method used for the studentized permutation test
#' * `studperm_samples`: Number of permutation samples
#' 
#' @returns (`data.table()`)\cr
#'   A data.table with 4 rows and 4 columns.
#'   The rows refer to the different methods: asymptotic, studentized permutation, 
#'   pseudo-observations (constant variance), pseudo-observations (HC3).
#'   The columns identify the method (`method`), list the results (`value`), 
#'   errors (`error`) and warnings (`warning`).
#'   Except for `method` all columns are list columns.
#' 
#' @details
#' This function bundles all algorithms / estimation methods we want to evaluate into 
#' one single function, which is in principle inconsistent with the design of the 
#' `batchtools` package, where one job would be defined as a combination of a single simulation 
#' setting and a single algorithm/method (and a single replication).
#' However, as all methods included right now are relatively fast to compute.
#' Therefore, I think that it does not make sense to split all methods into distinct jobs 
#' and regenerate the data (`instance`) for a given setting/replication every time.
#' 
#' A single entry in the column `value` (if available) of the output contains a 
#' `numeric(3)` vector with the following elements:
#' 1. p-value
#' 2. Lower bound of the confidence interval
#' 3. Upper bound of the confidence interval
#' 
#' @export
rmst_funs = function(data, job, instance, ...) {
  # Standard normal quantile for testing and construction of CIs
  quant = qnorm(1 - (data$alpha / 2))
  
  # Asymptotic
  res_asy = trycatch2({
    x = rmst_diff_test(
      Surv(time, event) ~ trt, data = instance, cutoff = data$cutoff,
      contrast = c("1", "0"), var_method = data$var_method_asy,
      inest_action = "error"
    )
    
    ci = minus_plus(x[["diff"]], quant * sqrt(x[["var_diff"]]))
    
    out = c(x[["pval"]], ci)
    names(out) = c("pval", "ci_lower", "ci_upper")
    
    out
  })
  
  # Studentized permutation
  res_studperm = trycatch2({
    x = rmst_diff_studperm(
      Surv(time, event) ~ trt, data = instance, cutoff = data$cutoff,
      contrast = c("1", "0"), var_method = data$var_method_studperm,
      num_samples = data$studperm_samples, conf_level = (1 - data$alpha),
      light = TRUE
    )
    
    out = c(x$permutation$pval, x$permutation$confint)
    names(out) = c("pval", "ci_lower", "ci_upper")
    
    out
  })
  
  # Pseudo-observations using Kaplan-Meier
  res_pseudo = trycatch2({
    m = rmeanglm(
      Surv(time, event) ~ trt, data = instance, time = data$cutoff,
      model.censoring = pseudo_strat, formula.censoring = ~ trt
    )
    
    x = rmst_pseudo_test(m, vcov_type = "HC3")[2, ]
    
    ci = minus_plus(x[["est"]], quant * sqrt(x[["var_est"]]))
    
    out = c(x[["pval"]], ci)
    names(out) = c("pval", "ci_lower", "ci_upper")
    
    out
  })
  
  # Final output
  res = list(
    asy = res_asy,
    studperm = res_studperm,
    pseudo = res_pseudo
  )
  
  dt = data.table(
    method = names(res),
    value = lapply(res, \(x) x$value),
    error = lapply(res, \(x) x$error),
    warning = lapply(res, \(x) x$warning)
  )
  
  return(dt)
}


#' Used for calculating confidence intervals
minus_plus = \(x, add) c(x - add, x + add)

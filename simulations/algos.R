# Define functions ("algorithms") to be evaluated here

options(box.path = "R")

box::use(
  survival[Surv],
  stats[qnorm],
  data.table[data.table],
  # Asymptotic and studentized permutation test
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  # Pseudo-observations approach
  eventglm[rmeanglm],
  rmst/pseudo[rmst_pseudo_test]
)


#' Do one simulation iteration
#' 
#' @description
#' Evaluates all functions ("algorithms") for one instance of a simulated data set.
#' 
#' @param data,job,instance Internal arguments for `batchtools`.
#' @param alpha (`numeric(1)`)\cr
#'   The significane level for (two-sided) testing.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param ... Further arguments, e.g. passed to `rmst_diff()`.
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
#' `batchtools` package, where one job would be defined as a combination of a single 
#' setting and a single algorithm/method (and a single replication).
#' However, only the permutation method takes a larger amount of time to be evaluated and 
#' all other methods are relatively fast.
#' Therefore, I think that it does not make sense to split all methods into distinct jobs 
#' and regenerate the data (`instance`) for a given setting/replication every time.
#' 
#' A single entry in the column `value` (if available) of the output contains a 
#' `numeric(3)` vector with the following elements:
#' 1. Test decision: equals 1 if null hypothesis is rejected and 0 otherwise.
#' 2. Lower bound of the confidence interval
#' 3. Upper bound of the confidence interval
#' 
#' @export
do_one = function(data, job, instance, alpha, cutoff, ...) {
  
  quant = qnorm(1 - (alpha / 2))
  
  # Asymptotic
  res_asy = trycatch2({
    x = rmst_diff_test(
      Surv(time, status) ~ trt, data = instance, cutoff = cutoff,
      contrast = c("1", "0"),
      # var_method
      ...
    )
    
    ci = minus_plus(x[["diff"]], quant * sqrt(x[["var_diff"]]))
    
    out = c(x[["pval"]], ci)
    names(out) = c("pval", "ci_lower", "ci_upper")
    
    out
  })
  
  # Studentized permutation
  res_studperm = trycatch2({
    x = rmst_diff_studperm(
      Surv(time, status) ~ trt, data = instance, cutoff = cutoff,
      contrast = c("1", "0"), num_samples = 2000L, conf_level = 1 - alpha,
      # var_method
      ...
    )
    
    out = c(x$permutation$pval, x$permutation$confint)
    names(out) = c("pval", "ci_lower", "ci_upper")
    
    out
  })
  
  # Pseudo-observations (constant variance)
  res_pseudo_const = trycatch2({
    m = rmeanglm(
      Surv(time, status) ~ factor(trt), data = instance, time = cutoff,
      model.censoring = "stratified", formula.censoring = ~ factor(trt)
    )
    
    x = rmst_pseudo_test(m, vcov_type = "const")[2, ]
    
    ci = minus_plus(x[["est"]], quant * sqrt(x[["var_est"]]))
    
    out = c(x[["pval"]], ci)
    names(out) = c("pval", "ci_lower", "ci_upper")
    
    out
  })
  
  # Pseudo-observations (HC3)
  res_pseudo_hc3 = trycatch2({
    m = rmeanglm(
      Surv(time, status) ~ factor(trt), data = instance, time = cutoff,
      model.censoring = "stratified", formula.censoring = ~ factor(trt)
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
    pseudo_const = res_pseudo_const,
    pseudo_hc3 = res_pseudo_hc3
  )
  
  dt = data.table(
    method = names(res),
    value = lapply(res, \(x) x$value),
    error = lapply(res, \(x) x$error),
    warning = lapply(res, \(x) x$warning)
  )
  
  return(dt)
}





# Helper functions ----


#' Used for calculating confidence intervals
minus_plus = \(x, add) c(x - add, x + add)


#' Capture output (if any), warnings and errors all at once
#' 
#' @param expr (`expression()`)\cr
#'   Code/expression to be evaluated.
#'   
#' @return (`vector("list", 3)`)\cr
#'   A list containing the output (`"value"`), the last warning (`"warning"`) and 
#'   the error (`"error"`) if any of these is present.
#'   
#' @note
#' If multiple warnings occur during the evaluation of the expression, only the last one 
#' will be returned.
#' 
#' @export
trycatch2 = function(expr) {
  warn = NA
  err = NA
  
  value = withCallingHandlers(
    tryCatch(expr, error = \(e) {
      err <<- e
      NA
    }),
    warning = \(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    }
  )
  
  list(value = value, warning = warn, error = err)
}

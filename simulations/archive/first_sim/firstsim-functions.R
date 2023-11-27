options(box.path = "R")

box::use(
  survival[Surv],
  stats[qnorm],
  # Asymptotic and studentized permutation test
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  # Pseudo-observations approach
  eventglm[rmeanglm],
  rmst/pseudo[rmst_pseudo_test]
)

# Helper functions
months2days = \(x) 365.25 * x / 12
minus_plus = \(x, add) c(x - add, x + add)


#' @export
f_asy = function(data) {
  x = rmst_diff_test(
    Surv(time, status) ~ trt, data = data,
    cutoff = months2days(36), contrast = c("1", "0")
  )
  
  ci = minus_plus(
    x[["diff"]],
    qnorm(0.975) * sqrt(x[["var_diff"]])
  )
  
  out = c(
    decision = if (x[["pval"]] <= 0.05) 1 else 0,
    ci_lower = ci[1],
    ci_upper = ci[2]
  )
  
  return(out)
}

#' @export
f_studperm = function(data) {
  x = rmst_diff_studperm(
    Surv(time, status) ~ trt, data = data,
    cutoff = months2days(36), contrast = c("1", "0"),
    var_method = "greenwood", num_samples = 2000L, conf_level = 0.95
  )
  
  ci = x$permutation$confint
  
  out = c(
    decision = if (x$permutation$pval <= 0.05) 1 else 0,
    ci_lower = ci[1],
    ci_upper = ci[2]
  )
  
  return(out)
}


#' @export
f_pseudo = function(data) {
  m = rmeanglm(
    Surv(time, status) ~ factor(trt), data = data, time = months2days(36),
    model.censoring = "stratified", formula.censoring = ~ factor(trt)
  )
  
  x = rmst_pseudo_test(m, vcov_type = "HC3")[2, ]
  
  ci = minus_plus(
    x[["est"]],
    qnorm(0.975) * sqrt(x[["var_est"]])
  )
  
  out = c(
    decision = if (x[["pval"]] <= 0.05) 1 else 0,
    ci_lower = ci[1],
    ci_upper = ci[2]
  )
  
  return(out)
}


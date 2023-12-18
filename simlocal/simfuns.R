options(box.path = "R")

box::use(
  stats[qnorm, rexp],
  data.table[...],
  survival[Surv],
  # Asymptotic and studentized permutation test
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  # Pseudo-observation approaches
  eventglm[rmeanglm],
  rmst/pseudo[rmst_pseudo_test, pseudo_stratified2, pseudo_fpm1_stratified]
)

box::use(
  simfuns/trycatch2[trycatch2]
)


#' @export
gen_data = function(lambda0 = 0.2, lambda1 = 0.2,
                    num_samples = c(30, 30),
                    cens0 = 0.01335314, cens1 = 0.04700036,
                    cutoff = 10,
                    regenerate = TRUE) {
  estimable = FALSE
  
  while (!estimable) {
    # Survival times
    t0 = rexp(num_samples[1], lambda0)
    t1 = rexp(num_samples[2], lambda1)
    
    # Censoring times
    c0 = rexp(num_samples[1], cens0)
    c1 = rexp(num_samples[2], cens1)
    
    # Complete data set
    dt = combine_and_censor(t0, t1, c0, c1)
    
    # Check estimability
    if (!regenerate) {
      estimable = TRUE
    } else {
      estimable = is_estimable(dt, cutoff)
    }
  }
  
  return(dt)
}


#' @export
rmst_all_methods = function(instance) {
  
  # Standard normal quantile for testing and construction of CIs
  quant = qnorm(0.975)
  
  # Asymptotic
  res_asy = trycatch2({
    x = rmst_diff_test(
      Surv(time, event) ~ trt, data = instance, cutoff = 10,
      contrast = c("1", "0"), var_method = "greenwood",
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
      Surv(time, event) ~ trt, data = instance, cutoff = 10,
      contrast = c("1", "0"), var_method = "nelson_aalen",
      num_samples = 2000L, conf_level = 0.95,
      light = TRUE
    )
    
    out = c(x$permutation$pval, x$permutation$confint)
    names(out) = c("pval", "ci_lower", "ci_upper")
    
    out
  })
  
  # Pseudo-observations using Kaplan-Meier
  res_pseudo = trycatch2({
    m = rmeanglm(
      Surv(time, event) ~ trt, data = instance, time = 10,
      model.censoring = pseudo_stratified2, formula.censoring = ~ trt
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


#' Combine generated survival and censoring times
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


#' Check if RMST contrast is estimable
is_estimable = function(dt, cutoff) {
  idx = dt[, .I[which.max(time)], by = trt]$V1
  check = !dt[idx, (time < cutoff) & (event == 0)]
  all(check)
}

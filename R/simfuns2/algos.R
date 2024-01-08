options(box.path = "R")

box::use(
  stats[qnorm],
  data.table[data.table],
  survival[Surv],
  eventglm[rmeanglm]
)

box::use(
  # Asymptotic and studentized permutation test
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  # Pseudo-observation approaches
  rmst/pseudo[pseudo_strat, pseudo_infjack, rmst_pseudo_test, boot_pseudo]
)


#' Used for calculating confidence intervals
minus_plus = \(x, add) c(x - add, x + add)


#' RMST algorithms
#' 
#' @param .data (`data.table()`)\cr
#'   Randomly generated survival data.
#' @param constants (`list()`)\cr
#'   A list of objects that stay constant across all simulations:
#'   * `cutoff`: the restriction time
#'   * `alpha`: the significance level
#'   * `var_method_asy`:
#'   * `var_method_studperm`:
#'   * `num_samples_studperm `:
#'   * `num_samples_boot`:
#' 
#' @returns (`numeric(3)`)
#'   A named vector containing the p-value of the test (1) and 
#'   the lower and upper bounds of the confidence intervals (2 and 3).
#'   
#' @name algorithms
NULL


#' @rdname algorithms
#' @export
rmst_asy = function(.data, constants, ...) {
  quant = qnorm(1 - (constants$alpha / 2))
  
  x = rmst_diff_test(
    Surv(time, event) ~ trt, data = .data, cutoff = constants$cutoff,
    contrast = c("1", "0"), var_method = constants$var_method_asy,
    inest_action = "error"
  )
  
  ci = minus_plus(x[["diff"]], quant * sqrt(x[["var_diff"]]))
  
  out = c(x[["pval"]], ci)
  names(out) = c("pval", "ci_lower", "ci_upper")
  
  return(out)
}


#' @rdname algorithms
#' @export
rmst_studperm = function(.data, constants, ...) {
  quant = qnorm(1 - (constants$alpha / 2))
  
  x = rmst_diff_studperm(
    Surv(time, event) ~ trt, data = .data, cutoff = constants$cutoff,
    contrast = c("1", "0"), var_method = constants$var_method_studperm,
    num_samples = constants$num_samples_studperm, conf_level = (1 - constants$alpha),
    light = TRUE
  )
  
  out = c(x$permutation$pval, x$permutation$confint)
  names(out) = c("pval", "ci_lower", "ci_upper")
  
  return(out)
}


#' @rdname algorithms
#' @export
rmst_pseudo_hc3 = function(.data, constants, ...) {
  quant = qnorm(1 - (constants$alpha / 2))
  
  m = rmeanglm(
    Surv(time, event) ~ trt, data = .data, time = constants$cutoff,
    model.censoring = pseudo_strat, formula.censoring = ~ trt
  )
  
  x = rmst_pseudo_test(m, vcov_type = "HC3")[2, ]
  
  ci = minus_plus(x[["est"]], quant * sqrt(x[["var_est"]]))
  
  out = c(x[["pval"]], ci)
  names(out) = c("pval", "ci_lower", "ci_upper")
  
  return(out)
}


#' @rdname algorithms
#' @export
rmst_pseudo_ij_boot = function(.data, constants, ...) {
  quant = qnorm(1 - (constants$alpha / 2))
  
  m = rmeanglm(
    Surv(time, event) ~ trt, data = .data, time = constants$cutoff,
    model.censoring = pseudo_infjack, formula.censoring = ~ trt
  )
  m = boot_pseudo(m, num_samples = constants$num_samples_boot)
  
  x = rmst_pseudo_test(m, vcov_type = "boot")[2, ]
  
  ci = minus_plus(x[["est"]], quant * sqrt(x[["var_est"]]))
  
  out = c(x[["pval"]], ci)
  names(out) = c("pval", "ci_lower", "ci_upper")
  
  return(out)
}

box::use(
  survival[Surv],
  data.table[...],
  eventglm[rmeanglm]
)

box::use(
  chk = checkmate,
  stats[coef, pchisq],
  sandwich[vcovHC]
)

#' Inference on `pseudoglm` objects
#' 
#' @param m (`eventglm::rmeanglm()`)\cr
#'   Object of class `pseudoglm`.
#' @param vcov_type (`character(1)`)\cr
#'   Passed to `type` argument of `sandwich::vcovHC()`.
#'   
#' @return (`matrix()`)\cr
#'   A 4-column matrix containing the parameter estimates as well as their associated 
#'   variances, test statistics and p-values.
#'   
#' @examples
#' dt = survival::veteran
#' dt$trt = factor(dt$trt)
#' m = eventglm::rmeanglm(survival::Surv(time, status) ~ trt, data = dt, time = 400)
#' rmst_test_pseudo(m)
#' 
#' 
#' @export
rmst_pseudo_test = function(m, vcov_type = "HC3") {
  chk$assert_class(m, "pseudoglm")
  chk$assert_choice(
    vcov_type,
    choices = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5")
  )
  
  # Make `vcovHC()` function work
  class(m) = c("glm", "lm")
  
  # Extract objects required for inference
  beta = coef(m)
  sigma = vcovHC(m, type = vcov_type)
  
  # Output object
  out = matrix(
    NA_real_, nrow = length(beta), ncol = 4L,
    dimnames = list(names(beta), c("est", "var_est", "tstat", "pval"))
  )
  out[, 1] = beta
  out[, 2] = diag(sigma)
  out[, 3] = out[, 1] / sqrt(out[, 2])
  out[, 4] = 1 - pchisq(out[, 3]^2, df = 1)
  
  return(out)
}


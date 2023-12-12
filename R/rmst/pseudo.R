options(box.path = "R")

box::use(
  chk = checkmate,
  stats[coef, pchisq, update.formula, model.frame, predict],
  sandwich[vcovHC],
  flexsurv[flexsurvspline],
  data.table[data.table]
)

box::use(
  rmst/utils[get_surv_data],
  rmst/km[fsurvfit, .rmst]
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



#' Second version of non-parametric estimator of pseudo-observations with independent censoring
#' 
#' @inheritParams eventglm::pseudo_independent
#' 
#' @export
pseudo_independent2 = function(formula, time, cause = 1, data,
                               type = "rmean",
                               formula.censoring = NULL, ipcw.method = NULL) {
  
  margformula = update.formula(formula, . ~ 1)
  x = get_surv_data(margformula, data = data)
  
  fit_marg = fsurvfit(x[[1]], x[[2]])
  theta = .rmst(fit_marg, cutoff = time, var_method = "none")[1]
  
  theta_jack = vapply(
    1:nrow(data),
    function(i) {
      x_i = x[-i]
      fit_i = fsurvfit(x_i[[1]], x_i[[2]])
      theta_i = .rmst(fit_i, cutoff = time, var_method = "none")[1]
      return(theta_i)
    },
    numeric(1)
  )
  
  # Calculate pseudo-observations
  pseudo_obs = theta + (nrow(data) - 1) * (theta - theta_jack)
  
  return(pseudo_obs)
}


#' Second version of non-parametric estimator of pseudo-observations with dependent censoring
#' 
#' @inheritParams eventglm::pseudo_stratified
#' 
#' @export
pseudo_stratified2 = function(formula, time, cause = 1, data,
                              type = "rmean",
                              formula.censoring = NULL, ipcw.method = NULL) {
  margformula = update.formula(formula, . ~ 1)
  
  # Get all strata levels
  mfout = model.frame(formula.censoring, data = data)
  strata = interaction(mfout)
  li_idx = lapply(levels(strata), \(j) which(strata == j))
  
  # Output vector of pseudo-observations
  out = numeric(nrow(data))
  
  # Fill output vector
  pseudo_obs = lapply(li_idx, function(stratum) {
    pseudo_independent2(margformula, time, cause, data[stratum], type, formula.censoring, ipcw.method)
  })
  for (i in seq_along(pseudo_obs)) {
    out[li_idx[[i]]] = pseudo_obs[[i]]
  }
  
  return(out)
}


#' Pseudo-observations based on flexible parametric models with independent censoring
#' 
#' @inheritParams eventglm::pseudo_independent
#' 
#' @note
#' "1" denotes that one internal knot is used for the flexible parametric model.
#' 
#' @export
pseudo_fpm1_independent = function(formula, time, cause = 1, data,
                                   type = "rmean",
                                   formula.censoring = NULL, ipcw.method = NULL) {
  margformula = update.formula(formula, . ~ 1)
  
  # Estimate using all data
  fit_marg = flexsurvspline(margformula, data = data, k = 1)
  theta = predict(fit_marg, type = "rmst", times = time, se.fit = FALSE, newdata = data.table(1))
  theta = theta$.pred_rmst
  
  # Jackknife estimates
  theta_jack = vapply(
    1:nrow(data),
    function(i) {
      fit_i = flexsurvspline(margformula, data = data[-i], k = 1, inits = coef(fit_marg))
      theta_i = predict(fit_i, type = "rmst", times = time, se.fit = FALSE, newdata = data.table(1))
      theta_i = theta_i$.pred_rmst
      return(theta_i)
    },
    numeric(1)
  )
  
  # Calculate pseudo-observations
  pseudo_obs = theta  + (nrow(data) - 1) * (theta - theta_jack)
  return(pseudo_obs)
}


#' Pseudo-observations based on flexible parametric models with dependent censoring
#' 
#' @inheritParams eventglm::pseudo_stratified
#' 
#' @note
#' "1" denotes that one internal knot is used for the flexible parametric model.
#' 
#' @export
pseudo_fpm1_stratified = function(formula, time, cause = 1, data,
                                  type = "rmean",
                                  formula.censoring = NULL, ipcw.method = NULL) {
  margformula = update.formula(formula, . ~ 1)
  
  # Get all strata levels
  mfout = model.frame(formula.censoring, data = data)
  strata = interaction(mfout)
  li_idx = lapply(levels(strata), \(j) which(strata == j))
  
  # Output vector of pseudo-observations
  out = numeric(nrow(data))
  
  # Fill output vector
  pseudo_obs = lapply(li_idx, function(stratum) {
    pseudo_fpm1_independent(margformula, time, cause, data[stratum], type, formula.censoring, ipcw.method)
  })
  for (i in seq_along(pseudo_obs)) {
    out[li_idx[[i]]] = pseudo_obs[[i]]
  }
  
  return(out)
}


box::use(
  stats[update.formula, model.frame, predict, coef],
  flexsurv[flexsurvspline]
)


#' Pseudo-observations based on flexible parametric models with independent censoring
#' 
#' @inheritParams eventglm::pseudo_independent
#' 
#' @note
#' "1" denotes that one internal knot is used for the flexible parametric model.
#' 
#' @export
pseudo_fpm1_indep = function(formula, time, cause = 1, data,
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
pseudo_fpm1_strat = function(formula, time, cause = 1, data,
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


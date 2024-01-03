options(box.path = "R")

box::use(
  chk = checkmate,
  stats[coef, pchisq, update, update.formula, model.frame, model.matrix, glm.fit, var],
  sandwich[vcovHC],
  data.table[data.table, setnames],
  survival[survfit, Surv, pseudo]
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
#'   Either `"boot"` (if possible/available) or one of the options available for `sandwich::vcovHC()`.
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
    choices = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5", "boot")
  )
  
  # Point estimates / coefficients
  beta = coef(m)
  
  # Covariance matrix
  if (vcov_type == "boot") {
    if (is.null(m$boot)) {
      stop("No bootstrap samples available.")
    } else {
      sigma = var(m$boot)
    }
  } else {
    # Make `vcovHC()` function work
    class(m) = c("glm", "lm")
    sigma = vcovHC(m, type = vcov_type)
  }
  
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
pseudo_indep = function(formula, time, cause = 1, data,
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
pseudo_strat = function(formula, time, cause = 1, data,
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
    pseudo_indep(margformula, time, cause, data[stratum], type, formula.censoring, ipcw.method)
  })
  for (i in seq_along(pseudo_obs)) {
    out[li_idx[[i]]] = pseudo_obs[[i]]
  }
  
  return(out)
}


#' Calculate pseudo-observations based on infinitesimal jackknife
#'
#' @inheritParams eventglm::pseudo_stratified
#' 
#' @export
pseudo_infjack = function(formula, time, cause = 1, data = environment(formula),
                          type = "rmean",
                          formula.censoring = NULL, ipcw.method = NULL) {
  # Get strata
  mfout = model.frame(formula.censoring, data = data)
  strata = if (ncol(mfout) == 0L) NULL else interaction(mfout)
  
  # Fit Kaplan-Meier for pseudo-observations
  formula.censoring = update.formula(
    formula,
    sprintf(". ~ %s", as.character(formula.censoring[[2]]))
  )
  # `model = TRUE` and the formula hack are needed for `survival::pseudo()` to work properly
  fit = survfit(formula.censoring, data = data, se.fit = FALSE, model = TRUE)
  fit$formula = formula.censoring
  
  # Errors (and warnings) will happen if there are 0 events in at least one stratum
  pseudo_obs = suppressWarnings(
    tryCatch(
      drop(pseudo(fit, times = time, type = "rmst")),
      error = \(e) NULL
    )
  )
  
  # Strategy for handling errors due to 0 events
  if (is.null(pseudo_obs)) {
    if (is.null(strata)) {
      # Case 1: 0 events and no strata
      # All pseudo-observations equal the specified restriction time
      # Should usually never happen
      pseudo_obs = rep(time, nrow(mfout))
    } else {
      # Case 2: strata are present and in at least one of them we have 0 events
      # We fit Kaplan-Meier separately and compute the pseudo-observations if possible
      # For strata with 0 events we set the pseudo-observations to the specified restriction time
      pseudo_obs = numeric(nrow(mfout))
      li_idx = lapply(levels(strata), \(j) which(strata == j))
      
      pseudo_fill = lapply(li_idx, function(stratum) {
        newfit = update(fit, . ~ 1, data = data[stratum])
        if (sum(newfit$n.event) == 0L) {
          rep(time, newfit$n)
        } else {
          drop(pseudo(newfit, times = time, type = "rmst"))
        }
      })
      
      for (i in seq_along(pseudo_fill)) {
        pseudo_obs[li_idx[[i]]] = pseudo_fill[[i]]
      }
    }
  }
  
  return(pseudo_obs)
}



#' Bootstrap pseudo-observations
#' 
#' @param m (`pseudoglm`)\cr
#'   A `pseudoglm` model object.
#' @param num_samples (`numeric(1)`)\cr
#'   The number of bootstrap replications.
#' 
#' @return (`pseudoglm`)\cr
#'   Returns `m` with the slot `boot` added to it, containing the bootstrap samples 
#'   of the regression coefficients.
#' 
#' @export
boot_pseudo = function(m, num_samples = 1000L) {
  # Retrieve required objects from model object
  df_orig = m$data
  pseudo_fun = eval(m$call$model.censoring)
  formula = m$formula
  formula.censoring = eval(m$call$formula.censoring)
  type = m$type
  time = m$time
  weights = unname(m$weights)
  if (!all(weights == 1L)) stop("`boot()` not available for weighted regression yet.")
  control = m$control
  family = m$family
  
  # Bootstrap
  boot = t(vapply(
    seq_len(num_samples),
    function(i) {
      boot_idx = sample(1:nrow(df_orig), replace = TRUE)
      boot_df = df_orig[boot_idx]
      boot_y = pseudo_fun(formula, time, data = boot_df, type = type, formula.censoring = formula.censoring)
      boot_x = model.matrix(formula, data = boot_df)
      boot_fit = glm.fit(boot_x, boot_y,
                         family = family,
                         mustart = rep(mean(boot_y), length(boot_y)),
                         intercept = TRUE, singular.ok = TRUE,
                         control = control)
      boot_fit$coefficients
    },
    numeric(2L)
  ))
  
  m$boot = boot
  
  return(m)
}



options(box.path = "R")

box::use(
  chk = checkmate,
  stats[coef, pchisq, update, update.formula, model.frame, model.matrix, glm.fit, var, terms, quantile],
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



#' Bootstrap pseudo-observation models
#' 
#' @description
#' This function performs a non-parametric bootstrap on the "raw" survival data of the supplied model.
#' Thus, it resamples the data, calculates the pseudo-observations on the resample and thereafter estimates 
#' the model/coefficients.
#' Only the resampled coefficients are returned and stored inside the model object.
#' Therefore, the bootstrap samples can be used for estimating the covariance matrix and/or constructing 
#' percentile confidence intervals.
#' 
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



#' Bootstrap hypothesis testing for models based on pseudo-observations
#' 
#' @description
#' This function differs from `boot_pseudo()` in that it does not only bootstrap the coefficients 
#' but applies the bootstrap for hypothesis testing.
#' I.e. it is used for estimating the distribution of the studentized test statistic under the null hypothesis.
#' The rest of the inference procedure is the same as with the asymptotic test based on pseudo-observations, 
#' only that we do not use the quantiles of the standard normal distribution for testing and 
#' constructing confidence intervals but the ones from the resampled test statistics.
#' Here, we assume that the null distribution is symmetric (i.e. we estimate one quantile only).
#' The reasons for this is that I believe this will in general be more robust, especially with 
#' moderate numbers of bootstrap replications.
#' 
#' @param m (`pseudoglm`)\cr
#'   A `pseudoglm` model object.
#' @param num_samples (`numeric(1)`)\cr
#'   The number of bootstrap replications.
#' @param vcov_type (`character(1)`)\cr
#'   One of the options available for `type` argument in `sandwich::vcovHC()`.
#' @param conf_level (`numeric(1)`)\cr
#'   The nominal confidence level, primarily used for constructing the confidence interval here.
#' @param light (`logical(1)`)\cr
#'   Whether to discard (`TRUE`) or return (`FALSE`) the bootstrap resamples.
#'   
#' @export
rmst_pseudo_boot_test = function(m,
                                 num_samples = 2000L,
                                 vcov_type = "HC3",
                                 conf_level = 0.95,
                                 light = TRUE) {
  chk$assert_class(m, "pseudoglm")
  chk$assert_choice(
    vcov_type,
    choices = c("HC3", "const", "HC", "HC0", "HC1", "HC2", "HC4", "HC4m", "HC5")
  )
  
  # Results / test statistics based on original/full data set
  beta = coef(m)
  res_orig = get_coef_mat(m, vcov_type)
  
  # Collect information/specifications from original model for bootstrapping
  dt = m$data
  pseudo_fun = eval(m$call$model.censoring)
  formula = m$formula
  formula.censoring = eval(m$call$formula.censoring)
  type = m$type
  time = m$time
  weights = unname(m$weights)
  if (!all(weights == 1L)) stop("`boot()` not available for weighted regression yet.")
  control = m$control
  family = m$family
  
  # Do the bootstrapping
  boot = lapply(seq_len(num_samples), function(i) {
    tryCatch(
      {
        new_dt = dt[sample(1:.N, replace = TRUE)]
        new_y = pseudo_fun(formula, time, data = new_dt, type = type, formula.censoring = formula.censoring)
        new_x = model.matrix(formula, data = new_dt)
        new_m = glm.fit(new_x, new_y,
                        family = family,
                        mustart = rep(mean(new_y), length(new_y)),
                        intercept = TRUE, singular.ok = TRUE,
                        control = control)
        class(new_m) = c("glm", "lm")
        new_m$terms = terms(m)
        new_m$x = new_x
        
        # Get the coefficient matrix
        coefmat = get_coef_mat(new_m, vcov_type, delta = beta)
        
        # For the bootstrap samples we only need the test statistics in the end
        return(coefmat[, "tstat", drop = TRUE])
      },
      error = \(e) NULL
    )
  })
  
  # For non-obvious or very edge case scenarios some bootstrap iterations might fail.
  # E.g. for one resampled data set there might be 0 variance for a covariate.
  # This leads to (try-catched) errors or NaNs but it does not make everything else useless.
  # Therefore, discard "errors" and give a warning.
  boot = Filter(\(x) !(is.null(x) | anyNA(x)), boot)
  if (length(boot) < num_samples) {
    warning(sprintf(
      "Using less bootstrap samples than specified (%d out of %d missing).",
      (num_samples - length(boot)), num_samples
    ))
  }
  
  # Combine results and calculate some statistics
  boot = do.call(rbind, boot)
  
  # Get the "empirical" quantile of the null distribution
  # We assume that the null distribution is symmetric, which does not need to be the case
  # However, for "lower" numbers of bootstrap samples I think this might lead to more
  # robust results.
  z_boot = apply(abs(boot), 2L, quantile, probs = conf_level)
  res = cbind(res_orig, z_boot)
  
  # Calculate p-values following same argumentation as above
  pval_boot = numeric(nrow(res))
  names(pval_boot) = rownames(res)
  for (j in rownames(res)) {
    pval_boot[[j]] = mean(abs(res[j, "tstat"]) <= abs(boot[, j]), na.rm = TRUE)
  }
  res = cbind(res, pval_boot)
  
  # Bootstrap t-intervals
  ci_lower = res[, "est"] - res[, "z_boot"] * sqrt(res[, "var_est"])
  ci_upper = res[, "est"] + res[, "z_boot"] * sqrt(res[, "var_est"])
  res = cbind(res, ci_lower, ci_upper)
  
  # Return bootstrap resamples?
  if (!light) {
    res = list(result = res, boot = boot)
  }
  
  return(res)
}


#' @param m (`glm`)\cr
#'   The model under consideration.
#' @param vcov_type (`character(1)`)\cr
#'   Type of covariance matrix estimator to use (passed to `sandwich::vcovHC()`).
#' @param delta (`numeric()`)\cr
#'   Values to subtract from the point estimates for calculating the test statistics.
#'   Defaults to 0 but e.g. for bootstrap hypothesis testing it would be suitable to set it 
#'   to the original estimates from the full data set.
#'   
#' @noRd
get_coef_mat = function(m, vcov_type = "HC3", delta = 0) {
  if (inherits(m, "pseudoglm")) {
    class(m) = c("glm", "lm")
  }
  
  beta = coef(m)
  sigma = vcovHC(m, type = vcov_type)
  
  out = matrix(
    NA_real_, nrow = length(beta), ncol = 3L,
    dimnames = list(names(beta), c("est", "var_est", "tstat"))
  )
  out[, 1] = beta
  out[, 2] = diag(sigma)
  out[, 3] = (out[, 1] - delta) / sqrt(out[, 2])
  
  return(out)
}

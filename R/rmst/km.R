box::use(
  chk = checkmate,
  survival[Surv, survfit],
  data.table[...],
  stats[pchisq, get_all_vars, quantile]
)

box::use(
  ./utils[strata_labels]
)


#' Kaplan-Meier based estimation of the restricted mean survival time (RMST) and the 
#' difference thereof
#' 
#' @param formula (`formula()`)\cr
#'   A formula object with a `Surv` object on the left-hand side and an intercept (`1`) or 
#'   a single grouping (e.g. treatment) variable on the right hand side.
#' @param data (`data.frame()` or `environment()`)\cr
#'   The data or environment containing the objects that appear in `formula`.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param var_method (`character(1)`)\cr
#'   One of  `c("greenwood", "kaplan_meier", "nelson_aalen", "none")` to choose between the variance estimation method.
#' @param contrast (`character(2)`)\cr
#'   A vector indicating which difference in RMST should be estimated.
#'   E.g. `contrast = c("treatment", "control")` would imply to estimate \eqn{RMST_{trt} - RMST_{ctrl}}.
#'
#' @return (`numeric(2)` or `matrix(ncol = 2)`)\cr
#'   `rmst()`\cr
#'   Depending on the term on the right-hand side of `formula` either a numeric vector with 
#'   the estimate of the RMST and its standard error or a matrix with the number of rows 
#'   depending on the number of unique groups (e.g. treatments).\cr
#'   `rmst_diff()`\cr
#'   Numeric vector with estimate of the difference and associated standard error.
#'  
#' @references 
#' Hasegawa, Takahiro, Saori Misawa, Shintaro Nakagawa, Shinichi Tanaka, Takanori Tanase, Hiroyuki Ugai, Akira Wakana, et. al
#' „Restricted Mean Survival Time as a Summary Measure of Time-to-Event Outcome“.
#' Pharmaceutical Statistics 19, Nr. 4 (2020): 436–53.
#' https://doi.org/10.1002/pst.2004.
#' 
#' Kaplan EL, Meier P. Nonparametric estimation from incomplete observations.
#' J Am Stat Assoc. 1958;53:457-481.
#'   
#' @name rmst
NULL


#' @rdname rmst
#' @export
rmst = function(formula, data = environment(formula),
                cutoff,
                var_method = "greenwood") {
  # Safety checks
  chk$assert_formula(formula)
  if (!(chk$test_data_frame(data) || chk$test_environment(data))) {
    stop("`data` must be a data.frame or an environment containing the ",
         "vectors appearing in the `formula` object.")
  }
  chk$assert_number(cutoff, lower = 0)
  chk$assert_choice(var_method, choices = c("greenwood", "kaplan_meier", "nelson_aalen", "none"))
  
  # Kaplan-Meier estimate of survival curve
  fit = survfit(formula, data = data, se.fit = FALSE)
  
  # Organize Kaplan-Meier data in a data.table
  dt = data.table(
    time = fit$time,
    n_event = fit$n.event,
    n_risk = fit$n.risk,
    surv = fit$surv,
    group = rep(strata_labels(fit), times = fit$strata)
  )
  
  # Either groups are present or not
  if (!is.null(dt$group)) {
    li_dt = split(dt, by = "group")
    li_rmst = lapply(li_dt, \(dt) .rmst(dt, cutoff, var_method))
    out = do.call(rbind, li_rmst)
  } else {
    out = .rmst(dt, cutoff, var_method)
  }
  
  return(out)
}


#' Internal function for calculating RMST for a single group
#' 
#' @param dt_km (`data.table()`)\cr
#'   A data.table with organized results from Kaplan-Meier estimation for a single stratum.
.rmst = function(dt_km, cutoff, var_method) {
  # Apply restriction time
  dt_km = dt_km[time <= cutoff]
  
  # Point estimation
  time_diffs = diff(c(0, dt_km$time, cutoff))
  surv_probs = c(1, dt_km$surv)
  areas = time_diffs * surv_probs
  rmst_mu = sum(areas)
  
  # Variance estimation
  rmst_var = switch(var_method,
    "greenwood" = {
      t1 = cumsum(rev(areas[-1]))^2
      t2 = dt_km[, fifelse(
        (n_risk - n_event) == 0, 0,
        n_event / (n_risk * (n_risk - n_event))
      )]
      sum(t1 * rev(t2))
    },
    "kaplan_meier" = {
      t1 = cumsum(rev(areas[-1]))^2
      t2 = dt_km[, fifelse(
        (n_risk - n_event) == 0, 0,
        n_event / (n_risk * (n_risk - n_event))
      )]
      var_greenwood = sum(t1 * rev(t2))
      num_events = dt_km[, sum(n_event)]
      (num_events / (num_events - 1)) * var_greenwood
    },
    "nelson_aalen" = {
      t1 = cumsum(rev(areas[-1]))^2
      t2 = dt_km[, n_event / n_risk^2]
      sum(t1 * rev(t2))
    },
    "none" = NA_real_
  )
  
  # Output
  out = c(rmst_mu, rmst_var)
  names(out) =  c("rmst", "var_rmst")
  
  return(out)
}



#' @rdname rmst
#' @export
rmst_diff = function(formula, data = environment(formula),
                     cutoff,
                     contrast,
                     var_method = "greenwood") {
  # Kaplan-Meier RMST estimation
  mat_rmst = rmst(formula, data, cutoff, var_method)
  
  # Error if no grouping/treatment variable has been provided in `formula`
  if (!is.matrix(mat_rmst)) {
    stop("`formula` must contain a group variable on the right-hand side for which ",
         "the RMST difference can be calculated.")
  }
  
  # `contrast` must be specified and valid
  chk$assert_set_equal(contrast, rownames(mat_rmst))
  chk$assert_character(contrast, len = 2L)
  
  # Sort and calculate/estimate RMST difference
  mat_rmst = mat_rmst[contrast, ]
  out = c(diff = NA_real_, var_diff = NA_real_)
  out[1] = mat_rmst[1, 1] - mat_rmst[2, 1]
  out[2] = sum(mat_rmst[, 2])
  
  # Output
  return(out)
}


#' @rdname rmst
#' @export
rmst_diff_test = function(formula, data = environment(formula),
                          cutoff,
                          contrast,
                          var_method = "greenwood") {
  x = rmst_diff(formula, data, cutoff, contrast, var_method)
  y = c(tstat = NA_real_, pval = NA_real_)
  y[1] = unname(x[1] / sqrt(x[2]))
  y[2] = unname(1 - pchisq(y[1]^2, df = 1))
  
  c(x, y)
}



#' Inference on the difference in restricted mean survival time based on studentized permutation
#' 
#' @inheritParams rmst
#' @param num_samples (`numeric(1)`)\cr
#'   The number of permutations.
#' @param conf_level (`numeric(1)`)\cr
#'   The nominal confidence level for the permutation based confidence interval.
#' @param light (`logical(1)`)\cr
#'   Whether to discard or return the permutation samples.
#'
#' @references
#' Ditzhaus, Marc, Menggang Yu, and Jin Xu.
#' „Studentized Permutation Method for Comparing Two Restricted Mean Survival Times with Small Sample from Randomized Trials“.
#' Statistics in Medicine 42, Nr. 13 (2023): 2226–40. https://doi.org/10.1002/sim.9720.   
#' 
#' @note
#' Code for this function has been provided by Marc Ditzhaus.
#' 
#' @export
rmst_diff_studperm = function(formula, data = environment(formula),
                              cutoff,
                              contrast,
                              var_method = "nelson_aalen",
                              num_samples = 1000L,
                              conf_level = 0.95,
                              light = TRUE) {
  # Some safety checks
  chk$assert_number(num_samples, lower = 1L)
  chk$assert_number(conf_level, lower = 0L, upper = 1L)
  chk$assert_flag(light)
  
  # Asymptotic results
  res_asy = rmst_diff_test(formula, data, cutoff, contrast, var_method)
  
  # Obtain original survival data ordered by survival times
  dt = get_all_vars(formula, data = data)
  setDT(dt)
  dt = dt[order(dt[[1L]])]
  
  # Permutation samples and results
  shuffled_group = vapply(1:num_samples, \(i) sample(dt[[3L]]), numeric(nrow(dt)))
  
  perm = t(apply(
    shuffled_group, MARGIN = 2L,
    function(shuffled) {
      dt[[3L]] = shuffled
      rmst_diff(formula, dt, cutoff, contrast, var_method)
    }
  ))
  
  # Analyse data based on permutations
  perm_suared_tstat = perm[, 1]^2 / perm[, 2]
  pval = mean(res_asy["tstat"]^2 <= perm_suared_tstat, na.rm = TRUE)
  null_quant = quantile(sqrt(perm_suared_tstat), probs = conf_level)
  
  res_perm = list(
    pval = pval,
    confint = unname(c(
      res_asy["diff"] - null_quant * sqrt(res_asy["var_diff"]),
      res_asy["diff"] + null_quant * sqrt(res_asy["var_diff"])
    )),
    permutations = if (light) NULL else perm
  )
  
  # Final output 
  out = list(
    asymptotic = res_asy,
    permutation = res_perm
  )
  
  return(out)
}


#' (Non-parametric) bootstrap of the difference in restricted mean survival time
#' 
#' @inheritParams rmst
#' @param num_samples (`numeric(1)`)\cr
#'   The number of bootstrap replications.
#' @param probs (`numeric(2)`)\cr
#'   Vector of probabilities for the bootstrap quantiles.
#' @param light (`numeric(1)`)\cr
#'   Whether to discard or return the bootstrap estimates of the difference in 
#'   restricted mean survival time.
#' 
#' @details
#' This function computes a bootstrap percentile interval.
#' However, other bootstrap confidence intervals can also be computed by setting `light = FALSE`, 
#' i.e. returning the bootstrap estimates.
#' E.g. one could then compute a bootstrap t-interval.
#' 
#' 
#' @export
rmst_diff_boot = function(formula, data = environment(formula),
                          cutoff,
                          contrast,
                          var_method = "nelson_aalen",
                          num_samples = 1000L,
                          probs = c(0.025, 0.975),
                          light = TRUE) {
  # Some safety checks
  chk$assert_number(num_samples, lower = 1L)
  chk$assert_numeric(probs, lower = 0, upper = 1, len = 2, sorted = TRUE)
  chk$assert_flag(light)
  
  # Asymptotic results
  res_asy = rmst_diff(formula, data, cutoff, contrast, var_method)
  
  # Obtain and organize original survival data
  dt = get_all_vars(formula, data = data)
  setDT(dt)
  setnames(dt, new = c("time", "status", "group"))
  li_dt = split(dt, by = "group")
  
  # Bootstrap samples
  boot_samples = lapply(1:num_samples, function(i) {
    dt_new = lapply(li_dt, function(.dt) {
      idx = sample(1:nrow(.dt), replace = TRUE)
      .dt[idx]
    })
    rbindlist(dt_new)
  })
  
  # RMSTD bootstrap estimates
  boot_rmstd = t(vapply(
    boot_samples,
    function(.dt) {
      rmst_diff(
        Surv(time, status) ~ group, data = .dt,
        cutoff, contrast, var_method
      )
    },
    numeric(2)
  ))
  
  # Output
  out = list(
    asymptotic = res_asy,
    boot_ci = quantile(boot_rmstd[, 1], probs = probs),
    boot_rmstd = if (light) NULL else boot_rmstd
  )
  
  return(out)
}

box::use(
  chk = checkmate,
  survival[Surv, survfit],
  data.table[...]
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
#'   One of `"greenwood"` and `"km"` to choose between the variance estimation method.
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


#' @rdname rmst
#' @export
rmst = function(formula, data = environment(formula),
                cutoff,
                var_method = "greenwood") {
  # Safety checks
  chk$assert_formula(formula)
  if (!(chk$test_data_frame(data) || chk$test_environment(data))) {
    stop("`data` must be a data.frame or an environment containing the ",
         "vectors appearing in the `fm` formula object.")
  }
  chk$assert_number(cutoff, lower = 0)
  chk$assert_choice(var_method, choices = c("greenwood", "km"))
  
  # Kaplan-Meier estimate of survival curve
  fit = survfit(formula, data = data)
  
  # Organize Kaplan-Meier data in a data.table
  dt = data.table(
    time = fit$time,
    n_event = fit$n.event,
    n_risk = fit$n.risk,
    surv = fit$surv,
    trt = rep(strata_labels(fit), times = fit$strata)
  )
  
  # Either groups are present or not
  if (!is.null(dt$trt)) {
    li_dt = split(dt, by = "trt")
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
  
  # Variance estimation (Greenwood's formula)
  t1 = cumsum(rev(areas[-1]))^2
  t2 = dt_km[, fifelse(
    (n_risk - n_event) == 0, 0,
    n_event / (n_risk * (n_risk - n_event))
  )]
  rmst_var = sum(t1 * rev(t2))
  
  # Kaplan-Meier variance estimator (if requested)
  if (var_method == "km") {
    num_events = dt_km[, sum(n_event)]
    rmst_var = (num_events / (num_events - 1)) * rmst_var
  }
  
  # Output
  out = c(rmst_mu, sqrt(rmst_var))
  names(out) =  c("rmst", "se(rmst)")
  
  return(out)
}



#' @rdname rmst
#' @export
rmst_diff = function(formula, data = environment(formula),
                     cutoff,
                     var_method = "greenwood",
                     contrast) {
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
  out = numeric(2)
  names(out) = c("diff", "se(diff)")
  out[1] = mat_rmst[1, 1] - mat_rmst[2, 1]
  out[2] = sum(mat_rmst[, 2])
  
  # Output
  return(out)
}


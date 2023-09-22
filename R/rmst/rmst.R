box::use(
  survival[Surv, survfit],
  data.table[data.table, fifelse],
  chk = checkmate
)


#' Kaplan-Meier based estimation of the restricted mean survival time (RMST)
#' 
#' @param time (`numeric()`)\cr
#'   A numeric vector of the event times.
#' @param status (`numeric()`)\cr
#'   A numeric vector of the event indicators.
#'   Entries must be either 1 (event) or 0 (no event, censored).
#'   Must be the of the same length as `status`.
#' @param data (`data.frame()`)\cr
#'   A data.frame containing the `time` and `status` vectors.
#'   This argument is optional and can be set to `NULL` (default) if `time` and 
#'   `status` are supplied as vectors directly.
#' @param tau (`numeric(1)`)\cr
#'   A single number representing the restriction time.
#' @param var_bias_correct (`logical(1)`)\cr
#'   Whether to apply a bias correction to the estimator of the variance as proposed 
#'   by Kaplan & Meier.
#' 
#' @return (`numeric(2)`)\cr
#'   A numeric vector containing the estimate of the RMST (1st entry) 
#'   and its associated variance estimate (2nd entry).
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
#' @export
rmst = function(time, status, data = NULL,
                tau, var_bias_correct = FALSE) {
  # Safety checks
  chk$assert_data_frame(data, null.ok = TRUE)
  chk$assert_number(tau, lower = 0)
  
  # Kaplan-Meier estimate of the survival curve
  if (is.null(data)) {
    fit = survfit(Surv(time, status) ~ 1, se.fit = FALSE)
  } else {
    fm = substitute(Surv(time, status) ~ 1)
    fit = survfit(eval(fm), data = data, se.fit = FALSE)
  }
  
  # Organize and subset Kaplan-Meier estimate
  dt_fit = data.table(
    time = fit$time,
    n_event = fit$n.event,
    n_risk = fit$n.risk,
    surv = fit$surv
  )
  dt_fit = dt_fit[time <= tau]
  
  # Point estimation
  time_diffs = diff(c(0, dt_fit$time, tau))
  surv_probs = c(1, dt_fit$surv)
  areas = time_diffs * surv_probs
  rmst_mu = sum(areas)
  
  # Variance estimation (Greenwood's formula)
  t1 = cumsum(rev(areas[-1]))^2
  t2 = dt_fit[, fifelse(
    (n_risk - n_event) == 0, 0,
    n_event / (n_risk * (n_risk - n_event))
  )]
  rmst_var = sum(t1 * rev(t2))
  
  if (var_bias_correct) {
    num_events = dt_fit[, sum(n_event)]
    rmst_var = (num_events / (num_events - 1)) * rmst_var
  }
  
  # Output
  out = c(rmst_mu, rmst_var)
  names(out) =  c("rmst", "Var(rmst)")
  
  return(out)
}

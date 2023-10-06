box::use(
  stats[integrate]
)


#' Functions for computing the true restricted mean survival time (RMST) and 
#' restricted standard deviation of survival time (RSDST).

#' Compute the RMST using numerical integration
#' 
#' @description
#' This function computes the restricted mean survival time (RMST) using numerical 
#' integration via `stats::integrate()`.
#' 
#' @param surv_fun (`function()`)\cr
#'   A fully parametrised survival function, i.e. it only requires a vector of 
#'   time points as input.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#'
#' @export
numint = function(surv_fun, cutoff) {
  res = integrate(surv_fun, lower = 0, upper = cutoff)
  return(res$value)
}


#' Compute the RMST for a piecewise exponential distribution
#' 
#' @description
#' The restricted mean survival time (RMST) is analytically tractable for the 
#' piecewise exponential distribution and is computed by this function.
#'  
#' @param hazards (`numeric()`)\cr
#'   Vector of piecewise constant hazards.
#' @param knots (`numeric()`)\cr
#'   Internal boundary knots at which the hazards change.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' 
#' @references 
#' Royston, Patrick, und Mahesh Kb Parmar.
#' „Restricted Mean Survival Time: An Alternative to the Hazard Ratio for the Design and Analysis of Randomized Trials with a Time-to-Event Outcome“.
#' BMC Medical Research Methodology 13, Nr. 1 (Dezember 2013): 152.
#' https://doi.org/10.1186/1471-2288-13-152.
#'   
#' @export
pwexp = function(hazards, knots, cutoff) {
  durations = diff(c(0, knots, cutoff))
  cum_hazards = c(0, cumsum(hazards * durations))
  bterms = (1 - exp(- hazards * durations)) / hazards
  mu = sum(exp(- cum_hazards[-length(cum_hazards)]) * bterms)
  
  return(mu)
}

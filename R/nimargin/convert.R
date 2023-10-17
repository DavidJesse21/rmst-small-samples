box::use(
  stats[uniroot]
)


#' Convert non-inferiority margin from hazard ratio (HR) to difference in restricted mean 
#' survival time (DRMST) and the other way around
#' 
#' @description
#' These functions assume an exponential model in both the treatment and control arm 
#' for converting the non-inferiority margins from one scale to the other.
#' 
#' @param margin_hr,margin_drmst (`numeric(1)`)\cr
#'   Non-inferiority margin on the HR and DRMST scale, respectively.
#' @param lambda_ctrl (`numeric(1)`)\cr
#'   Rate parameter of the exponential model for the control arm.
#' @param cutoff (`numeric(1)`)\cr
#'   Cutoff value for the restricted mean survival time.
#' @param interval (`numeric(2)`)\cr
#'   Interval in which the HR margin is searched for for a given DRMST margin 
#'   (passed to `stats::uniroot()`).
#' @param tol,maxiter (`numeric(1)`)\cr
#'   Passed to `stats::uniroot()`.
#'   
#' @details
#' The conversion from the HR to the DRMST scale is conducted via an analytical expression.
#' For the conversion from DRMST to HR no analytical solution is available and therefore 
#' the solution is found numerically via `stats::uniroot()`.
#' 
#' @name nim_convert


#' @rdname nim_convert
#' @export
hr_to_drmst = function(margin_hr, lambda_ctrl, cutoff) {
  rmst_trt = (1 - exp(- margin_hr * lambda_ctrl * cutoff)) / (margin_hr * lambda_ctrl)
  rmst_ctrl = (1 - exp(- lambda_ctrl * cutoff)) / lambda_ctrl
  rmst_trt - rmst_ctrl
}


#' @rdname nim_convert
#' @export
drmst_to_hr = function(margin_drmst, lambda_ctrl, cutoff,
                       interval = c(1, 5),
                       tol = sqrt(.Machine$double.eps),
                       maxiter = 1000) {
  f = function(margin_hr) {
    rmst_trt = (1 - exp(- margin_hr * lambda_ctrl * cutoff)) / (margin_hr * lambda_ctrl)
    rmst_ctrl = (1 - exp(- lambda_ctrl * cutoff)) / lambda_ctrl
    rmst_trt - rmst_ctrl - margin_drmst
  }
  
  uniroot(f, interval = interval, tol = tol, maxiter = maxiter)$root
}

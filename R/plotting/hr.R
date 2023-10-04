box::use(
  miniPCH[hpch_fun],
  ggplot2[ggplot, geom_function, aes, theme_bw, labs]
)


#' Generic function for plotting the true hazard ratio of treatments vs. control arm
#' 
#' @param hazard_ctrl,hazard_trt (`function()`)\cr
#'   Hazard functions of the control and treatment arm, respectively.
#'  Their parameters need to be fixed, e.g. they could be objects as returned by 
#'  `miniPCH::hpch_fun()`.
#' @param xlab,ylab (`character(1)`)\cr
#'   x- and y-axis titles.
#' @param ... (any)\cr
#'   Additional arguments passed to *both* (control and treatment) ggplot layers 
#'   created with `ggplot2::geom_function()`, e.g. `linewidth`.
#'   
#' @note
#' The ratio, which is calculated and plotted, is \eqn{trt / control}.
#' 
#' @export
plot_hr_generic = function(hazard_ctrl, hazard_trt,
                           xlab = "Time", ylab = "Hazard ratio",
                           ...) {
  ggplot() +
    geom_function(fun = \(x) hazard_trt(x) / hazard_ctrl(x), ...) +
    theme_bw() +
    labs(x = xlab, y = ylab)
}


#' Plot true hazard ratio in a delayed effect scenario
#' 
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   True hazard rate of the control group throughout the study and of the treatment 
#'   group before onset of the effect.
#' @param hazard_trt (`numeric(1)`)\cr
#'   True hazard rate of the treatment group after onset of the effect.
#' @param xlab,ylab,... See `plot_hr_generic()`.
#' 
#' @export
plot_hr_delay = function(hazard_ctrl, hazard_trt, delay,
                         xlab = "Time", ylab = "Hazard rate",
                         ...) {
  haz_ctrl = hpch_fun(0, hazard_ctrl)
  haz_trt = hpch_fun(c(0, delay), c(hazard_ctrl, hazard_trt))
  
  plot_hr_generic(haz_ctrl, haz_trt, xlab, ylab, ...)
}


#' Plot true hazard ratio in a crossing hazard scenario
#' 
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   The (constant) hazards rate of the control arm.
#' @param hazard_trt_before,hazard_trt_after (`numeric(1)`)\cr
#'   The hazard rates of the treatment group before and after crossing.
#' @param crosstime (`numeric(1)`)\cr
#'   The time at which the hazard rate of the treatment arm changes or at which the 
#'   two arms' hazards cross.
#' @param xlab,ylab,... See `plot_hr_generic()`.
#' 
#' @export
plot_hr_crossing = function(hazard_ctrl, hazard_trt_before, hazard_trt_after, crosstime,
                            xlab = "Time", ylab = "Hazard rate",
                            ...) {
  haz_ctrl = hpch_fun(0, hazard_ctrl)
  haz_trt = hpch_fun(c(0, crosstime), c(hazard_trt_before, hazard_trt_after))
  
  plot_hr_generic(haz_ctrl, haz_trt, xlab, ylab, ...)
}

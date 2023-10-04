box::use(
  miniPCH[spch_fun],
  ggplot2[ggplot, geom_function, aes, theme_bw, labs],
  ggsci[scale_color_jco]
)


#' Generic function for plotting the true survival curves of a control and treatment arm
#' 
#' @param surv_ctrl,surv_trt (`function()`)\cr
#'  Survival functions of the control and treatment arm, respectively.
#'  Their parameters need to be fixed, e.g. they could be objects as returned by 
#'  `miniPCH::spch_fun()`.
#' @param xlab,ylab (`character(1)`)\cr
#'   x- and y-axis titles.
#' @param ... (any)\cr
#'   Additional arguments passed to *both* (control and treatment) ggplot layers 
#'   created with `ggplot2::geom_function()`, e.g. `linewidth`.
#'   
#' @note
#' This function produces a ggplot of the true survival curves of two treatment arms.
#' However, further refinement of the returned plot is almost surely required.
#' For example, the default x-axis limits are 0 and 1.
#' 
#' @export
plot_surv_generic = function(surv_ctrl, surv_trt,
                             xlab = "Time", ylab = "Survival probability",
                             ...) {
  ggplot() +
    geom_function(fun = surv_ctrl, aes(color = "Control"), ...) +
    geom_function(fun = surv_trt, aes(color = "Treatment"), ...) +
    theme_bw() +
    scale_color_jco(name = "Arm") +
    labs(x = xlab, y = ylab)
}


#' Plot true delayed survival curves
#' 
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   True hazard rate of the control group throughout the study and of the treatment 
#'   group before onset of the effect.
#' @param hazard_trt (`numeric(1)`)\cr
#'   True hazard rate of the treatment group after onset of the effect.
#' @param xlab,ylab,... See `plot_surv_generic()`.
#' 
#' @export
plot_surv_delay = function(hazard_ctrl, hazard_trt, delay,
                           xlab = "Time", ylab = "Survival probability",
                           ...) {
  surv_ctrl = spch_fun(0, hazard_ctrl)
  surv_trt = spch_fun(c(0, delay), c(hazard_ctrl, hazard_trt))
  
  plot_surv_generic(surv_ctrl, surv_trt, xlab, ylab, ...)
}


#' Plot true crossing survival curves
#' 
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   The (constant) hazards rate of the control arm.
#' @param hazard_trt_before,hazard_trt_after (`numeric(1)`)\cr
#'   The hazard rates of the treatment group before and after crossing.
#' @param crosstime (`numeric(1)`)\cr
#'   The time at which the hazard rate of the treatment arm changes or at which the 
#'   two arms' hazards cross.
#' @param xlab,ylab,... See `plot_surv_generic()`.
#' 
#' @note
#' Notice that "crossing" and `crosstime` refer to the crossing of the *hazard* functions 
#' and not of the survival curves.
#' 
#' @export
plot_surv_crossing = function(hazard_ctrl, hazard_trt_before, hazard_trt_after, crosstime,
                              xlab = "Time", ylab = "Survival probability",
                              ...) {
  surv_ctrl = spch_fun(0, hazard_ctrl)
  surv_trt = spch_fun(c(0, crosstime), c(hazard_trt_before, hazard_trt_after))
  
  plot_surv_generic(surv_ctrl, surv_trt, xlab, ylab, ...)
}


# TBD
# - progression
# - subgroup
# do not understand the data generating mechanisms yet

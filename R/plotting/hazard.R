box::use(
  miniPCH[hpch_fun],
  ggplot2[ggplot, geom_function, aes, theme_bw, labs],
  ggsci[scale_color_jco]
)

box::use(
  ./utils_mixture[mixture_hazard]
)


#' Generic function for plotting the true hazards of a control and treatment arm
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
#' This function produces a ggplot of the true hazard rates of two treatment arms.
#' However, further refinement of the returned plot is almost surely required.
#' For example, the default x-axis limits are 0 and 1.
#' 
#' @export
plot_hazard_generic = function(hazard_ctrl, hazard_trt,
                               xlab = "Time", ylab = "Hazard rate",
                               ...) {
  ggplot() +
    geom_function(fun = hazard_ctrl, aes(color = "Control"), ...) +
    geom_function(fun = hazard_trt, aes(color = "Treatment"), ...) +
    theme_bw() +
    scale_color_jco(name = "Arm") +
    labs(x = xlab, y = ylab)
}


#' Plot true hazards with a delayed effect in the treatment arm
#' 
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   True hazard rate of the control group throughout the study and of the treatment 
#'   group before onset of the effect.
#' @param hazard_trt (`numeric(1)`)\cr
#'   True hazard rate of the treatment group after onset of the effect.
#' @param delay (`numeric(1)`)\cr
#'   The delay time until the treatment effect enters.
#' @param xlab,ylab,... See `plot_hazard_generic()`.
#' 
#' @export
plot_hazard_delay = function(hazard_ctrl, hazard_trt, delay,
                             xlab = "Time", ylab = "Hazard",
                             ...) {
  haz_ctrl = hpch_fun(0, hazard_ctrl)
  haz_trt = hpch_fun(c(0, delay), c(hazard_ctrl, hazard_trt))
  
  plot_hazard_generic(haz_ctrl, haz_trt, xlab, ylab, ...)
}


#' Plot true crossing hazards
#' 
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   The (constant) hazards rate of the control arm.
#' @param hazard_trt_before,hazard_trt_after (`numeric(1)`)\cr
#'   The hazard rates of the treatment group before and after crossing.
#' @param crosstime (`numeric(1)`)\cr
#'   The time at which the hazard rate of the treatment arm changes or at which the 
#'   two arms' hazards cross.
#' @param xlab,ylab,... See `plot_hazard_generic()`.
#' 
#' @export
plot_hazard_crossing = function(hazard_ctrl, hazard_trt_before, hazard_trt_after, crosstime,
                                xlab = "Time", ylab = "Hazard",
                                ...) {
  haz_ctrl = hpch_fun(0, hazard_ctrl)
  haz_trt = hpch_fun(c(0, crosstime), c(hazard_trt_before, hazard_trt_after))
  
  plot_hazard_generic(haz_ctrl, haz_trt, xlab, ylab, ...)
}


#' Plot true hazards with a mixture of populations in the treatment arm
#' 
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   The (constant) hazards rate of the control arm.
#' @param hazard_trt,hazard_subgroup (`numeric(1)`)\cr
#'   Hazard rates in the treatment arm for the two subgroups.
#' @param prevalence (`numeric(1)`)\cr
#'   Prevalence of the subgroup as a relative fraction (between 0 and 1).
#' @param xlab,ylab,... See `plot_hazard_generic()`.
#' 
#' @references 
#' Ristl, Robin, Nicolás M Ballarini, Heiko Götte, Armin Schüler, Martin Posch, and Franz König. 
#' „Delayed treatment effects, treatment switching and heterogeneous patient populations: How to design and analyze RCTs in oncology“.
#' Pharmaceutical Statistics 20, Nr. 1 (2021): 129–45. https://doi.org/10.1002/pst.2062.
#' 
#' @export
plot_hazard_subgroup = function(hazard_ctrl, hazard_trt, hazard_subgroup, prevalence,
                                xlab = "Time", ylab = "Hazard",
                                ...) {
  # Control arm: simple exponential model
  haz_ctrl = hpch_fun(0, hazard_ctrl)
  
  # Treatment arm: mixture of subpopulations
  haz_trt = \(x) mixture_hazard(
    time = x,
    hazards = c(hazard_trt, hazard_subgroup),
    probs = c(1 - prevalence, prevalence)
  )
  
  # Plot
  plot_hazard_generic(haz_ctrl, haz_trt, xlab, ylab, ...)
}

# TBD
# - progression
# do not understand the data generating mechanisms yet

box::use(
  survplot = ./surv,
  hazardplot = ./hazard,
  hrplot = ./hr
)

box::use(
  patchwork[wrap_plots],
  ggplot2[theme, element_blank, ylim]
)

#' Jointly plot survival probabilities, hazards and hazard ratio in a delayed effect scenario
#'
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   True hazard rate of the control group throughout the study and of the treatment 
#'   group before onset of the effect.
#' @param hazard_trt (`numeric(1)`)\cr
#'   True hazard rate of the treatment group after onset of the effect.
#' @param delay (`numeric(1)`)\cr
#'   The delay time until the treatment effect enters.
#' @param xlab (`character(1)`)\cr
#'   x-axis title for *all* subplots.
#' @param ylabs (`character(3)`)\cr
#'   y-axis title for plot of survival probabilities, hazards and hazard ratio in that order.
#' @param align (`character(1)`)\cr
#'   One of `"row"` or `"column"` specifying, whether the subplots should be 
#'   horizontally or vertically aligned.
#' @param ... (any)\cr
#'   Additional parameters passed to *all* functions `plot_surv_delay()`, 
#'   `plot_hazard_delay()` and `plot_hr_delay()`.
#' 
#' @return (`patchwork`)
#'   An object of class `patchwork`, which is essentially a list of `ggplot`s.
#'   
#' @note
#' In order to modify the plot output of this function you cannot use the `+` operator 
#' as we are dealing with a composition (list) of plots instead of a single on.
#' Instead you have to use the `&` operator with which you can e.g. change the 
#' axes limits for all plots.
#'
#' @export
plot_joint_delay = function(hazard_ctrl, hazard_trt, delay,
                            xlab = "Time",
                            ylabs = c("Survival probability", "Hazard", "Hazard ratio"),
                            align = c("row", "column"),
                            ...) {
  p_surv = survplot$plot_surv_delay(
    hazard_ctrl, hazard_trt, delay,
    xlab = xlab, ylab = ylabs[1], ...
  )
  p_hazard = hazardplot$plot_hazard_delay(
    hazard_ctrl, hazard_trt, delay,
    xlab = xlab, ylab = ylabs[2], ...
  )
  p_hr = hrplot$plot_hr_delay(
    hazard_ctrl, hazard_trt, delay,
    xlab = xlab, ylab = ylabs[3], ...
  )
  
  out = organize_plots(p_surv, p_hazard, p_hr, align = align)
  return(out)
}


#' Jointly plot survival probabilities, hazards and hazard ratio in a crossing hazards scenario
#' 
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   The (constant) hazards rate of the control arm.
#' @param hazard_trt_before,hazard_trt_after (`numeric(1)`)\cr
#'   The hazard rates of the treatment group before and after crossing.
#' @param crosstime (`numeric(1)`)\cr
#'   The time at which the hazard rate of the treatment arm changes or at which the 
#'   two arms' hazards cross.
#' @param xlab (`character(1)`)\cr
#'   x-axis title for *all* subplots.
#' @param ylabs (`character(3)`)\cr
#'   y-axis title for plot of survival probabilities, hazards and hazard ratio in that order.
#' @param align (`character(1)`)\cr
#'   One of `"row"` or `"column"` specifying, whether the subplots should be 
#'   horizontally or vertically aligned.
#' @param ... (any)\cr
#'   Additional parameters passed to *all* functions `plot_surv_crossing()`, 
#'   `plot_hazard_crossing()` and `plot_hr_crossing()`.
#'   
#' @note
#' In order to modify the plot output of this function you cannot use the `+` operator 
#' as we are dealing with a composition (list) of plots instead of a single on.
#' Instead you have to use the `&` operator with which you can e.g. change the 
#' axes limits for all plots.
#' 
#' @export
plot_joint_crossing = function(hazard_ctrl, hazard_trt_before, hazard_trt_after, crosstime,
                               xlab = "Time",
                               ylabs = c("Survival probability", "Hazard", "Hazard ratio"),
                               align = c("row", "column"),
                               ...) {
  p_surv = survplot$plot_surv_crossing(
    hazard_ctrl, hazard_trt_before, hazard_trt_after, crosstime,
    xlab = xlab, ylab = ylabs[1], ...
  )
  p_hazard = hazardplot$plot_hazard_crossing(
    hazard_ctrl, hazard_trt_before, hazard_trt_after, crosstime,
    xlab = xlab, ylab = ylabs[2], ...
  )
  p_hr = hrplot$plot_hr_crossing(
    hazard_ctrl, hazard_trt_before, hazard_trt_after, crosstime,
    xlab = xlab, ylab = ylabs[3], ...
  )
  
  out = organize_plots(p_surv, p_hazard, p_hr, align = align)
  return(out)
}


#' Jointly plot survival probabilities, hazards and hazard ratio in a subpopulation scenario
#'
#' @param hazard_ctrl (`numeric(1)`)\cr
#'   The (constant) hazards rate of the control arm.
#' @param hazard_trt,hazard_subgroup (`numeric(1)`)\cr
#'   Hazard rates in the treatment arm for the two subgroups.
#' @param prevalence (`numeric(1)`)\cr
#'   Prevalence of the subgroup as a relative fraction (between 0 and 1).
#' @param xlab (`character(1)`)\cr
#'   x-axis title for *all* subplots.
#' @param ylabs (`character(3)`)\cr
#'   y-axis title for plot of survival probabilities, hazards and hazard ratio in that order.
#' @param align (`character(1)`)\cr
#'   One of `"row"` or `"column"` specifying, whether the subplots should be 
#'   horizontally or vertically aligned.
#' @param ... (any)\cr
#'   Additional parameters passed to *all* functions `plot_surv_crossing()`, 
#'   `plot_hazard_crossing()` and `plot_hr_crossing()`.
#'   
#' @note
#' In order to modify the plot output of this function you cannot use the `+` operator 
#' as we are dealing with a composition (list) of plots instead of a single on.
#' Instead you have to use the `&` operator with which you can e.g. change the 
#' axes limits for all plots.
#' 
#' @export
plot_joint_subgroup = function(hazard_ctrl, hazard_trt, hazard_subgroup, prevalence,
                               xlab = "Time",
                               ylabs = c("Survival probability", "Hazard", "Hazard ratio"),
                               align = c("row", "column"),
                               ...) {
  p_surv = survplot$plot_surv_subgroup(
    hazard_ctrl, hazard_trt, hazard_subgroup, prevalence,
    xlab = xlab, ylab = ylabs[1], ...
  )
  p_hazard = hazardplot$plot_hazard_subgroup(
    hazard_ctrl, hazard_trt, hazard_subgroup, prevalence,
    xlab = xlab, ylab = ylabs[2], ...
  )
  p_hr = hrplot$plot_hr_subgroup(
    hazard_ctrl, hazard_trt, hazard_subgroup, prevalence,
    xlab = xlab, ylab = ylabs[3], ...
  )
  
  out = organize_plots(p_surv, p_hazard, p_hr, align = align)
  return(out)
}


# Helper functions ----

# Wrap and organize subplots (invisible)
organize_plots = function(p_surv, p_hazard, p_hr,
                          align = c("row", "column")) {
  align = match.arg(align, choices = c("row", "column"))
  
  # If plots are vertically aligned, we want/need information from x-axis only
  # from the bottom plot (hazard ratio plot).
  if (align == "column") {
    theme_x_blank = theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
    p_surv = p_surv + theme_x_blank
    p_hazard = p_hazard + theme_x_blank
  }
  
  # If plots are horizontally aligned, we need to keep all information but can remove 
  # the x-axis label from the two outer plots (survival and hazard ratio)
  if (align == "row") {
    rm_x_label = theme(axis.title.x = element_blank())
    p_surv = p_surv + rm_x_label
    p_hr = p_hr + rm_x_label
  }
  
  # Return composition of plots
  wrap_plots(
    p_surv, p_hazard, p_hr,
    ncol = if (align == "row") 3L else 1L,
    nrow = if (align == "row") 1L else 3L,
    guides = "collect"
  )
}


#' Set the y-axis limits for joint plots
#' 
#' @param plot (`patchwork`)\cr
#'   A `patchwork` plot object returned by any of the joint plot functions.
#' @param surv,hazard,hr (`numeric(2)`)\cr
#'   Vectors for the respective y-axis limits (lower, upper).
#'   
#' @export
set_ylims = function(plot, surv = NULL, hazard = NULL, hr = NULL) {
  li_ylims = list(surv, hazard, hr)
  
  for (i in seq_along(plot)) {
    if (!is.null(li_ylims[[i]])) {
      plot[[i]] = plot[[i]] + ylim(li_ylims[[i]])
    }
  }
  
  return(plot)
}


# TBD
# - progression
# do not understand the data generating mechanisms yet

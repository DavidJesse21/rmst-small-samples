options(box.path = "R")

box::use(
  ggplot2[...]
)

box::use(
  simfuns2/scenarios[get_params_exp, get_params_pwexp, get_params_weibull],
  ./utils[pal_oi, fun_surv_exp, fun_surv_pwexp, fun_surv_wb]
)



#' Plot survival functions
#' 
#' @param surv0,surv1_h0,surv1_h1 (`function`)\cr
#'   Survival functions to be plotted.
#'   `surv0` is the survival function of the control group.
#'   `surv1_h0` and `surv1_h1` are the survival functions of the treatment group under the 
#'   null and alternative hypothesis, respectively.
#' @param cutoff (`numeric(1)`)\cr
#'   Cutoff value for the restricted mean survival time.
#'   It is used here for displaying a vertical dashed line that indicates this cutoff.
#' @param cutoff_linewidth (`numeric(1)`)\cr
#'   Line width of the vertical line for the cutoff
#' @param xlab,ylab (`character(1)`)\cr
#'   x- and y-axis title.
#' @param xlim,ylim (`numeric(2)`)\cr
#'   x- and y-axis limits.
#' @param ... (`any`)\cr
#'   Arguments passed to `ggplot2::geom_function()`.
#'   
#' @export
plot_survs_generic = function(surv0, surv1_h0, surv1_h1,
                              cutoff = 10, cutoff_linewidth = 0.9, 
                              xlab = "\nTime", ylab = "Survival probability\n",
                              xlim = c(0, 10.5), ylim = c(0, 1),
                              ...) {
  # Expression vector for legends (needed later)
  .labels = c(
    "Control",
    expression(Treatment ~ (Delta == 0)),
    expression(Treatment ~ (Delta == 1.5))
  )
  
  
  ggplot() +
    # Survival functions
    geom_function(fun = surv0, aes(color = "c", linetype = "c"), ...) +
    geom_function(fun = surv1_h0, aes(color = "t0", linetype = "t0"), ...) +
    geom_function(fun = surv1_h1, aes(color = "t1.5", linetype = "t1.5"), ...) +
    # RMST cutoff
    geom_vline(
      xintercept = cutoff,
      linewidth = cutoff_linewidth, linetype = "dashed", color = "#222222"
    ) +
    # Theme
    theme_bw() +
    # Legend
    scale_color_manual(
      name = "Group",
      values = pal_oi(c("orange", "blue", "blue")),
      labels = .labels
    ) +
    scale_linetype_manual(
      name = "Group",
      values = c("solid", "dotdash", "solid"),
      labels = .labels
    ) +
    theme(legend.text.align = 0) +
    # Axes and labels
    labs(x = xlab, y = ylab) +
    scale_x_continuous(
      breaks = seq(0, 10, by = 2.5),
      limits = xlim,
      expand = expansion(mult = c(0, 0), add = c(0.5, 0))
    ) +
    scale_y_continuous(
      limits = ylim
    )
}



#' S1: proportional hazards / exponential distributions
#'
#' @inheritParams get_params_exp
#' @inheritParams plot_survs_generic
#' 
#' @note
#' `rmst_diff` must be of length 2 here and should contain 0 as the first entry and 
#' the value for the alternative scenario as the second one.
#' 
#' @export
plot_survs_exp = function(rmst_diff = c(0, 1.5), lambda0 = 0.2, cutoff = 10,
                          cutoff_linewidth = 0.9,
                          xlab = "\nTime", ylab = "Survival probability\n",
                          xlim = c(0, 10.5), ylim = c(0, 1),
                          ...) {
  # Obtain parameters and survival functions
  params = get_params_exp(rmst_diff, lambda0, cutoff)
  
  params_h0 = params[1, surv_params][[1]]
  surv0 = fun_surv_exp(unname(params_h0["lambda0"]))
  surv1_h0 = fun_surv_exp(unname(params_h0["lambda1"]))
  
  params_h1 = params[2, surv_params][[1]]
  surv1_h1 = fun_surv_exp(unname(params_h1["lambda1"]))
  
  # Plot
  plot_survs_generic(
    surv0, surv1_h0, surv1_h1,
    cutoff = cutoff, cutoff_linewidth = cutoff_linewidth,
    xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim,
    ...
  )
}


#' S7: crossing curves / piecewise exponential distribution
#' 
#' @inheritParams get_params_pwexp
#' @inheritParams plot_survs_generic
#' 
#' @note
#' `rmst_diff` must be of length 2 here and should contain 0 as the first entry and 
#' the value for the alternative scenario as the second one.
#' 
#' @export
plot_survs_pwexp = function(rmst_diff = c(0, 1.5), lambda0 = 0.2, lambdas1 = c(0.5, 0.05), cutoff = 10,
                           cutoff_linewidth = 0.9,
                           xlab = "\nTime", ylab = "Survival probability\n",
                           xlim = c(0, 10.5), ylim = c(0, 1),
                           ...) {
  # Obtain parameters and survival functions
  params = get_params_pwexp(rmst_diff, lambda0, lambdas1, cutoff)
  
  params_h0 = params[1, surv_params][[1]]
  surv0 = fun_surv_exp(unname(params_h0["lambda0"]))
  surv1_h0 = fun_surv_pwexp(unname(params_h0[c("lambda11", "lambda12")]), c(0, unname(params_h0["crosstime"])))
  
  params_h1 = params[2, surv_params][[1]]
  surv1_h1 = fun_surv_pwexp(unname(params_h1[c("lambda11", "lambda12")]), c(0, unname(params_h1["crosstime"])))
  
  # Plot
  plot_survs_generic(
    surv0, surv1_h0, surv1_h1,
    cutoff = cutoff, cutoff_linewidth = cutoff_linewidth,
    xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim,
    ...
  )
}



#' S8: crossing curves / Weibull distributions with shape alternatives
#' 
#' @inheritParams get_params_weibull
#' @inheritParams plot_survs_generic
#' 
#' @note
#' `rmst_diff` must be of length 2 here and should contain 0 as the first entry and 
#' the value for the alternative scenario as the second one.
#' 
#' @export
plot_survs_weibull = function(rmst_diff = c(0, 1.5), shape0 = 3, scale0 = 8, scale1 = 14, cutoff = 10,
                              cutoff_linewidth = 0.9,
                              xlab = "\nTime", ylab = "Survival probability\n",
                              xlim = c(0, 10.5), ylim = c(0, 1),
                              ...) {
  # Obtain parameters and survival functions
  params = get_params_weibull(rmst_diff, shape0, scale0, scale1, cutoff)
  
  params_h0 = params[1, surv_params][[1]]
  surv0 = fun_surv_wb(params_h0[["shape0"]], params_h0[["scale0"]])
  surv1_h0 = fun_surv_wb(params_h0[["shape1"]], params_h0[["scale1"]])
  
  params_h1 = params[2, surv_params][[1]]
  surv1_h1 = fun_surv_wb(params_h1[["shape1"]], params_h1[["scale1"]])
  
  # Plot
  plot_survs_generic(
    surv0, surv1_h0, surv1_h1,
    cutoff = cutoff, cutoff_linewidth = cutoff_linewidth,
    xlab = xlab, ylab = ylab,
    xlim = xlim, ylim = ylim,
    ...
  )
}


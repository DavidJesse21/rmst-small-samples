options(box.path = "R")

box::use(
  ggplot2[...],
  data.table[...]
)

box::use(
  ./utils[pal_oi, surv_exp, surv_pwexp, surv_weibull],
  simfuns2/scenarios[get_params_exp, get_params_pwexp, get_params_weibull]
)


#' Plot survival models used for simulation study
#'
#' @param rmst_diff (`numeric(2)`)\cr
#'   Vector of RMST differences.
#'   The first entry should always be 0 and therefore reflect a null hypothesis scenario.
#' @param cutoff (`numeric(1)`)\cr
#'   Cutoff value for the restricted mean survival time.
#'   It is used here for displaying a vertical dashed line that indicates this cutoff.
#' @param cutoff_linewidth (`numeric(1)`)\cr
#'   Line width of the vertical line for the cutoff.
#' @param xlim,ylim (`numeric(2)`)\cr
#'   x- and y-axis limits.
#' @param xlab,ylab (`character(1)`)\cr
#'   x- and y-axis title.
#' @param n (`numeric(1)`)\cr
#'   A number determining how many data points to use for constructing the curves.
#' @param align (`character(1)`)\cr
#'   Either `"v"` for a vertical alignment of the different plots or `"h"` for a 
#'   horizontal one.
#' @param ... (`any`)\cr
#'   Any arguments that can be passed to `ggplot2::geom_line()`, which are used for 
#'   styling the curves.\cr
#'   Note however, that `color` and `linetype` are already used to differentiate between 
#'   treatment groups and effect sizes.
#'   
#' @export
plot_surv_models = function(rmst_diff = c(0, 1.5), cutoff = 10, cutoff_linewidth = 0.9,
                            xlim = c(0, 10.5), ylim = c(0, 1),
                            xlab = "\nTime", ylab = "Survival probability\n",
                            n = 250, align = c("v", "h"),
                            ...) {
  
  # Create data for plotting ----
  
  tseq = seq(xlim[1], xlim[2], length.out = n)
  
  # S1: Exponential (proportional hazards)
  params_s1 = get_params_exp(rmst_diff, lambda0 = 0.2, cutoff = cutoff)
  
  dt0 = data.table(
    time = tseq,
    surv = surv_exp(tseq, params_s1[1, surv_params][[1]]["lambda0"]),
    group = "control",
    hypo = "ref"
  )
  dt10 = data.table(
    time = tseq,
    surv = surv_exp(tseq, params_s1[1, surv_params][[1]]["lambda1"]),
    group = "trt",
    hypo = "null"
  )
  dt11 = data.table(
    time = tseq,
    surv = surv_exp(tseq, params_s1[2, surv_params][[1]]["lambda1"]),
    group = "trt",
    hypo = "alt"
  )
  
  dt_s1 = rbindlist(list(dt0, dt10, dt11))
  dt_s1[, model := "S1: Exp. (proportional hazards)"]
  
  
  # S7: Exponential vs. piecewise exponential
  params_s7 = get_params_pwexp(rmst_diff, lambda0 = 0.2, lambdas1 = c(0.5, 0.05), cutoff = cutoff)
  
  dt0 = data.table(
    time = tseq,
    surv = surv_exp(tseq, params_s7[1, surv_params][[1]]["lambda0"]),
    group = "control",
    hypo = "ref"
  )
  dt10 = data.table(
    time = tseq,
    surv = surv_pwexp(tseq, lambda = params_s7[1, surv_params][[1]][2:3], c(0, params_s7[1, surv_params][[1]]["crosstime"])),
    group = "trt",
    hypo = "null"
  )
  dt11 = data.table(
    time = tseq,
    surv = surv_pwexp(tseq, lambda = params_s7[2, surv_params][[1]][2:3], c(0, params_s7[2, surv_params][[1]]["crosstime"])),
    group = "trt",
    hypo = "alt"
  )
  
  dt_s7 = rbindlist(list(dt0, dt10, dt11))
  dt_s7[, model := "S7: Exp. vs. piecewise Exp."]
  
  
  # S8: Weibull (shape alternatives)
  params_s8 = get_params_weibull(rmst_diff, shape0 = 3, scale0 = 8, scale1 = 14, cutoff = cutoff)
  
  dt0 = data.table(
    time = tseq,
    surv = surv_weibull(tseq, params_s8[1, surv_params][[1]]["shape0"], params_s8[1, surv_params][[1]]["scale0"]),
    group = "control",
    hypo = "ref"
  )
  dt10 = data.table(
    time = tseq,
    surv = surv_weibull(tseq, params_s8[1, surv_params][[1]]["shape1"], params_s8[1, surv_params][[1]]["scale1"]),
    group = "trt",
    hypo = "null"
  )
  dt11 = data.table(
    time = tseq,
    surv = surv_weibull(tseq, params_s8[2, surv_params][[1]]["shape1"], params_s8[2, surv_params][[1]]["scale1"]),
    group = "trt",
    hypo = "alt"
  )
  
  dt_s8 = rbindlist(list(dt0, dt10, dt11))
  dt_s8[, model := "S8: Weibull (shape alternatives)"]
  
  
  # Combine all data
  dt = rbindlist(list(dt_s1, dt_s7, dt_s8))
  dt[, group := paste0(group, "_", hypo)]
  dt[, group := factor(group, levels = unique(group))]
  
  
  # Create the plot ----
  
  # Labels used for legend later on
  .labels = c(
    "Control",
    expression(Treatment ~ (Delta == 0)),
    expression(Treatment ~ (Delta == 1.5))
  )
  
  # Display plots in a column/vertically or in a row/horizontally
  align = match.arg(align, c("v", "h"))
  
  # Plot
  ggplot(dt, aes(time, surv, color = group, linetype = group)) +
    geom_line(...) +
    facet_wrap(~ model, nrow = if (align == "v") 3L else 1L) +
    #theme_bw() +
    # Indicator line for RMST cutoff
    geom_vline(
      xintercept = cutoff, linewidth = cutoff_linewidth,
      linetype = "longdash", color = "#222222"
    ) +
    # Control colors and linetypes
    scale_color_manual(
      name = "Group",
      values = pal_oi(c("orange", "blue", "blue")),
      labels = .labels
    ) +
    scale_linetype_manual(
      name = "Group",
      values = c("solid", "dashed", "solid"),
      labels = .labels
    ) +
    # Axes labels
    labs(x = xlab, y = ylab) +
    # Left align legend labels
    theme(legend.text.align = 0) +
    # Fine-tuning of axes
    scale_x_continuous(
      breaks = seq(0, 10, by = 2.5),
      limits = xlim,
      expand = expansion(mult = c(0, 0), add = c(0.5, 0))
    ) +
    scale_y_continuous(
      limits = ylim
    ) +
    # Better differentiate between different line types
    theme(legend.key.size = unit(2.5, "line")) +
    # Place legend at top
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
}

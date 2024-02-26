options(box.path = "R")

box::use(
  ggplot2[...],
  data.table[...]
)

box::use(
  ./utils[surv_weibull, surv_unif]
)


#' Plot survival functions of censoring models used for simulation study
#' 
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
#'   Note however, that `linetype` argument is already used.
#'   
#' @export   
plot_cens_models = function(cutoff = 10, cutoff_linewidth = 0.9,
                            xlim = c(0, 10.5), ylim = c(0, 1),
                            xlab = "\nTime", ylab = "Survival probability\n",
                            n = 250, align = c("v", "h"),
                            ...) {
  
  # Generate data for plotting ----
  
  tseq = seq(xlim[1], xlim[2], length.out = n)
  
  # C1: Weibull (unequal)
  dt0 = data.table(
    time = tseq,
    surv = surv_weibull(tseq, shape = 3, scale = 18),
    group = "control"
  )
  dt1 = data.table(
    time = tseq,
    surv = surv_weibull(tseq, shape = 0.5, scale = 40),
    group = "trt"
  )
  dt_c1 = rbindlist(list(dt0, dt1))
  dt_c1[, model := "C1: Weibull (unequal)"]
  
  # C2: Uniform (equal)
  dt_c2 = data.table(
    time = rep(tseq, 2),
    surv = rep(surv_unif(tseq, 0, 25), 2),
    group = rep(c("control", "trt"), each = n),
    model = "C2: Uniform (equal)"
  )
  
  # C3: Weibull (equal)
  dt_c3 = data.table(
    time = rep(tseq, 2),
    surv = rep(surv_weibull(tseq, shape = 3, scale = 15)),
    group = rep(c("control", "trt"), each = n),
    model = "C3: Weibull (equal)"
  )
  
  # Combine all data
  dt = rbindlist(list(dt_c1, dt_c2, dt_c3))
  
  
  # Create the plot ----
  
  # Display plots in a column/vertically or in a row/horizontally
  align = match.arg(align, c("v", "h"))
  
  # Plot
  ggplot(dt, aes(time, surv, linetype = group)) +
    geom_line(...) +
    facet_wrap(~ model, nrow = if (align == "v") 3L else 1L) +
    #theme_bw() +
    # Indicator line for RMST cutoff
    geom_vline(
      xintercept = cutoff, linewidth = cutoff_linewidth,
      linetype = "longdash", color = "#222222"
    ) +
    # Control linetype
    scale_linetype_manual(
      name = "Group",
      values = c("solid", "dashed"),
      labels = c("Control", "Treatment")
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

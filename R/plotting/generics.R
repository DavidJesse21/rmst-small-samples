box::use(
  ggplot2[ggplot, geom_function, aes, theme_bw, labs, scale_color_manual,
          scale_x_continuous, scale_y_continuous]
)

box::use(
  ./utils[pal_oi]
)


#' Generic plotting of the hazard and survival functions as well as of the hazard ratio
#' 
#' @param hazard0,hazard1 (`function`)\cr
#'   The hazard functions of the control and treatment group, respectively.
#' @param surv0,surv1 (`function`)\cr
#'   The survival functions of the control and treatment group, respectively.
#' @param xlab,ylab (`character(1)`)\cr
#'   Axis labels.
#' @param xlim,ylim (`numeric(2)`)\cr
#'   Axis limits.
#' @param ... (`any`)\cr
#'   Arguments passed to `ggplot2::geom_function()`.
#' 
#' @name plot-generic
NULL


#' @rdname plot-generic
#' @export
plot_hazards_generic = function(hazard0, hazard1,
                                xlab = "Time", ylab = "Hazard rate",
                                xlim = c(0, 11), ylim = NULL,
                                ...) {
  ggplot() +
    geom_function(fun = hazard0, aes(color = "Control"), ...) +
    geom_function(fun = hazard1, aes(color = "Treatment"), ...) +
    theme_bw() +
    scale_color_manual(name = "Arm", values = pal_oi(1:2)) +
    labs(x = xlab, y = ylab) +
    scale_x_continuous(
      breaks = seq(0, 10, by = 2.5),
      limits = xlim
    ) +
    scale_y_continuous(
      limits = ylim
    )
}


#' @rdname plot-generic
#' @export
plot_survs_generic = function(surv0, surv1,
                              xlab = "Time", ylab = "Survival probability",
                              xlim = c(0, 11), ylim = c(0, 1),
                              ...) {
  ggplot() +
    geom_function(fun = surv0, aes(color = "Control"), ...) +
    geom_function(fun = surv1, aes(color = "Treatment"), ...) +
    theme_bw() +
    scale_color_manual(name = "Arm", values = pal_oi(1:2)) +
    labs(x = xlab, y = ylab) +
    scale_x_continuous(
      breaks = seq(0, 10, by = 2.5),
      limits = xlim
    ) +
    scale_y_continuous(
      limits = ylim
    )
}


#' @rdname plot-generic
#' @export
plot_hr_generic = function(hazard0, hazard1,
                           xlab = "Time", ylab = "Hazard ratio",
                           xlim = c(0, 11), ylim = NULL,
                           ...) {
  ggplot() +
    geom_function(fun = \(x) hazard1(x) / hazard0(x), color = pal_oi("black"), ...) +
    theme_bw() +
    labs(x = xlab, y = ylab) +
    scale_x_continuous(
      breaks = seq(0, 10, by = 2.5),
      limits = xlim
    ) +
    scale_y_continuous(
      limits = ylim
    )
}


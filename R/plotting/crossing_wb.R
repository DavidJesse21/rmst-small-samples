box::use(
  ./generics[plot_hazards_generic, plot_survs_generic, plot_hr_generic],
  ./utils[fun_hazard_wb, fun_surv_wb, organize_plots]
)


#' @export
plot_hazards = function(shape0, scale0, shape1, scale1,
                        xlab = "Time", ylab = "Hazard rate",
                        xlim = c(0, 11), ylim = NULL,
                        ...) {
  plot_hazards_generic(
    fun_hazard_wb(shape0, scale0), fun_hazard_wb(shape1, scale1),
    xlab, ylab, xlim, ylim, ...
  )
}


#' @export
plot_survs = function(shape0, scale0, shape1, scale1,
                      xlab = "Time", ylab = "Survival probability",
                      xlim = c(0, 11), ylim = c(0, 1),
                      ...) {
  plot_survs_generic(
    fun_surv_wb(shape0, scale0), fun_surv_wb(shape1, scale1),
    xlab, ylab, xlim, ylim, ...
  )
}


#' @export
plot_hr = function(shape0, scale0, shape1, scale1,
                   xlab = "Time", ylab = "Hazard rate",
                   xlim = c(0, 11), ylim = NULL,
                   ...) {
  plot_hr_generic(
    fun_hazard_wb(shape0, scale0), fun_hazard_wb(shape1, scale1),
    xlab, ylab, xlim, ylim, ...
  )
}


#' @export
plot_joint = function(shape0, scale0, shape1, scale1,
                      xlab = "Time",
                      ylabs = c("Survival probability", "Hazard", "Hazard ratio"),
                      xlim = c(0, 11),
                      ylims = list(surv = c(0, 1), hazard = NULL, hr = NULL),
                      align = c("row", "column"),
                      ...) {
  
  p_surv = plot_survs(shape0, scale0, shape1, scale1, xlab, ylabs[1], xlim, ylims$surv, ...)
  p_hazard = plot_hazards(shape0, scale0, shape1, scale1, xlab, ylabs[2], xlim, ylims$hazard, ...)
  p_hr = plot_hr(shape0, scale0, shape1, scale1, xlab, ylabs[3], xlim, ylims$hr, ...)
  
  p_all = organize_plots(p_surv, p_hazard, p_hr, align = align)
  return(p_all)
}


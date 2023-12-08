box::use(
  ./generics[plot_hazards_generic, plot_survs_generic, plot_hr_generic],
  ./utils[fun_hazard_exp, fun_surv_exp, organize_plots]
)


#' @export
plot_hazards = function(lambda0, lambda1,
                        xlab = "Time", ylab = "Hazard rate",
                        xlim = c(0, 11), ylim = NULL,
                        ...) {
  plot_hazards_generic(
    fun_hazard_exp(lambda0), fun_hazard_exp(lambda1),
    xlab, ylab, xlim, ylim, ...
  )
}


#' @export
plot_survs = function(lambda0, lambda1,
                      xlab = "Time", ylab = "Survival probability",
                      xlim = c(0, 11), ylim = c(0, 1),
                      ...) {
  plot_survs_generic(
    fun_surv_exp(lambda0), fun_surv_exp(lambda1),
    xlab, ylab, xlim, ylim, ...
  )
}


#' @export
plot_hr = function(lambda0, lambda1,
                   xlab = "Time", ylab = "Hazard ratio",
                   xlim = c(0, 11), ylim = NULL,
                   ...) {
  plot_hr_generic(
    fun_hazard_exp(lambda0), fun_hazard_exp(lambda1),
    xlab, ylab, xlim, ylim, ...
  )
}


#' @export
plot_joint = function(lambda0, lambda1,
                      xlab = "Time",
                      ylabs = c("Survival probability", "Hazard", "Hazard ratio"),
                      xlim = c(0, 11),
                      ylims = list(surv = c(0, 1), hazard = NULL, hr = NULL),
                      align = c("row", "column"),
                      ...) {
  
  p_surv = plot_survs(lambda0, lambda1, xlab, ylabs[1], xlim, ylims$surv, ...)
  p_hazard = plot_hazards(lambda0, lambda1, xlab, ylabs[2], xlim, ylims$hazard, ...)
  p_hr = plot_hr(lambda0, lambda1, xlab, ylabs[3], xlim, ylims$hr, ...)
  
  p_all = organize_plots(p_surv, p_hazard, p_hr, align = align)
  return(p_all)
}

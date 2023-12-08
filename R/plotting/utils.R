li_pal_oi = c(
  orange = "#E69F00",
  light_blue = "#56B4E9",
  green = "#009E73",
  yellow = "#F0E442",
  blue = "#0072B2",
  red = "#D55E00",
  purple = "#CC79A7",
  grey = "#999999",
  black = "#000000",
  sky_blue = "#56B4E9",
  dark_yellow = "#F5C710"
)


#' Okabe & Ito color palette
#' 
#' @param which (`numeric()` or `character()`)\cr
#'   A vector of integer IDs or of names for subsetting the color palette.
#' @param unname (`logical(1)`)\cr
#'   Whether to unname the resulting vector.
#' 
#' @export
pal_oi = function(which = NULL, unname = TRUE) {
  out = if (is.null(which)) li_pal_oi else li_pal_oi[which]
  
  if (unname) {
    return(unname(out))
  } else{
    return(out)
  }
}


# Make survival functions ----

box::use(
  miniPCH[hpwexp = hpch, spwexp = spch]
)

#' @export
fun_hazard_exp = function(lambda) {
  \(x) lambda
}

#' @export
fun_surv_exp = function(lambda) {
  \(x) exp(- lambda * x)
}

#' @export
fun_hazard_pwexp = function(lambda, knots = 0) {
  \(x) hpwexp(x, t = knots, lambda = lambda)
} 

#' @export
fun_surv_pwexp = function(lambda, knots = 0) {
  \(x) spwexp(x, t = knots, lambda = lambda)
}

#' @export
fun_hazard_wb = function(shape, scale) {
  \(x) (shape / scale) * (x / scale)^(shape - 1)
}

#' @export
fun_surv_wb = function(shape, scale) {
  \(x) exp(- ((x / scale)^shape))
}


# Joint plotting ----

box::use(
  patchwork[wrap_plots],
  ggplot2[theme, element_blank]
)


#' @export
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


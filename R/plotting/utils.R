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


# Survival functions ----

box::use(
  miniPCH[spwexp = spch],
  data.table[fcase, between]
)


#' @param x (`numeric(x)`)\cr
#'   Vector of quantiles.
#' @param lambda (`numeric(1)`)\cr
#'   Rate parameter.
#' @export
surv_exp = function(x, lambda) {
  exp(- lambda * x)
}


#' @param x (`numeric(x)`)\cr
#'   Vector of quantiles.
#' @param lambda (`numeric()`)\cr
#'   Vector of piecewise hazards/rate parameters.
#' @param knots (`numeric()`)\cr
#'   Vector of knots/left interval borders for the piecewise hazards.
#'   Must include 0.   
#' @export
surv_pwexp = function(x, lambda, knots = 0) {
  spwexp(x, t = knots, lambda = lambda)
}


#' @param x (`numeric(x)`)\cr
#'   Vector of quantiles.
#' @param shape,scale (`numeric(1)`)\cr
#'   Shape and scale parameters of the Weibull distribution.
#'   See `?stats::dweibull` for more details.
#' @export
surv_weibull = function(x, shape, scale) {
  exp(- ((x / scale)^shape))
}


#' @param x (`numeric(x)`)\cr
#'   Vector of quantiles.
#' @param a,b (`numeric(1)`)\cr
#'   Values for `min` and `max`, respectively.
#' @export
surv_unif = function(x, a = 0, b) {
  fcase(
    x < a, 1,
    x > b, 0,
    between(x, a, b), (b - x) / (b - a)
  )
}


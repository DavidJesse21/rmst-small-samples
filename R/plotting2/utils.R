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

box::use(miniPCH[spwexp = spch])

#' @param lambda (`numeric(1)`)\cr
#'   Hazard / rate parameter of the exponential distribution.
#' @export
fun_surv_exp = function(lambda) {
  \(x) exp(- lambda * x)
}

#' @param lambda (`numeric()`)\cr
#'   Vector of piecewise constant hazards / rate parameters.
#' @param knots (`numeric()`)\cr
#'   Vector of left interval borders where the hazards change.
#'   Must include 0 as first element.
#' @export
fun_surv_pwexp = function(lambda, knots = 0) {
  \(x) spwexp(x, t = knots, lambda = lambda)
}

#' @param shape,scale (`numeric(1)`)\cr
#'   Shape and scale parameters of the Weibull distribution.
#'   See `?stats::dweibull` for more details.
#' @export
fun_surv_wb = function(shape, scale) {
  \(x) exp(- ((x / scale)^shape))
}

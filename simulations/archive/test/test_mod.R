#' @export
f1 = \(x) x

#' @export
f2 = function(x) {
  if (x > 5) warning("x greater than 5")
  x
}

#' @export
f3 = function(x) {
  if (x > 5) stop("x greater than 5") else x
}

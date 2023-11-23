options(box.path = "R")

box::use(
  rmst/km[rmst]
)

#' @export
rmst_wrapper = function(formula, data, cutoff) {
  rmst(formula, data, cutoff)
}

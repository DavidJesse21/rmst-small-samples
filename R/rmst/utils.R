#' Check if more than two vectors are of the same length
#' @export
same_length = function(...) {
  x = vapply(list(...), length, integer(1))
  length(unique(x)) == 1
}

#' Check if more than two vectors are of the same length
#' @export
same_length = function(...) {
  x = vapply(list(...), length, integer(1))
  length(unique(x)) == 1
}


#' Get strata labels
#' 
#' @description
#' When using `survfit(Surv(time, event) ~ trt)`, the treatment strata are saved/labelled 
#' with the pattern "trt=value".
#' This function extracts the "values" from these patterns
#' 
#' @param x (`survfit()`)\cr
#'   A `survfit` object.
#' 
#' @export
strata_labels = function(x) {
  if (is.null(x$strata)) {
    return(NULL)
  } else {
    out = strsplit(names(x$strata), split = "=") |>
      vapply(\(x) x[[2]], character(1))
    return(out)
  }
}

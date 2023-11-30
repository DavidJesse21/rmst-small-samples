# Define data-generating mechanisms ("problems") here

box::use(
  data.table[...],
  stats[rexp]
)

#' Test functions
#' 
#' @param data,job Internal parameters for `batchtools`.
#' @param num_samples (`numeric(2)`)\cr
#'   Sample size of control and treatment group, respectively.
#' @param lambda_ctrl,lambda_trt (`numeric(1)`)\cr
#'   "Rate" parameter of the exponential distribution.
#' @param cens_ctrl,cens_trt (`numeric(1)`)\cr
#'   "Rate" parameter for the censoring distribution.
#' 
#' @export
gen_data = function(data, job,
                    num_samples,
                    lambda_ctrl, lambda_trt,
                    cens_ctrl, cens_trt,
                    ...) {
  dt = data.table(
    trt = c(rep(0L, num_samples[[1]]), rep(1L, num_samples[[2]])),
    time = round(c(
      rexp(num_samples[[1]], lambda_ctrl),
      rexp(num_samples[[2]], lambda_trt)
    )),
    time_cens = round(c(
      rexp(num_samples[[1]], cens_ctrl),
      rexp(num_samples[[2]], cens_trt)
    ))
  )
  
  dt[, status := fifelse(time <= time_cens, 1L, 0L)][
    , time := fifelse(status == 1L, time, time_cens)
  ][, time_cens := NULL]
  
  return(dt[])
}

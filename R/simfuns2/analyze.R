options(box.path = "R")

box::use(
  data.table[...],
  stats[median],
)



# Main calculations ----


#' Calculate operating characteristics/metrics from the empirical simulation data
#' 
#' @param res (`data.table`)\cr
#'   Results of the simulation study.
#' @param by (`character()`)\cr
#'   Character vector of (factor like) columns in the results data table by which
#'   to group the calculations.
#' @param alpha (`numeric(1)`)\cr
#'   Significance level to use for making rejections.
#' @param stats_NA (`logical(1)`)\cr
#'   If `TRUE` relative and absolute counts of missing values are included.
#'   
#' @name calc_metrics
NULL


#' @rdname calc_metrics
#' @export
calc_rejection_rates = function(res, by = c("scenario.id", "algo.id"),
                                alpha = 0.05, stats_NA = TRUE) {
  dt = res[, .(
    reject = mean(pval <= alpha, na.rm = TRUE),
    num_NA = sum(is.na(pval)),
    prop_NA = mean(is.na(pval))
  ), by = by]
  
  if (!stats_NA) {
    dt[, `:=`(num_NA = NULL, prop_NA = NULL)]
  }
  
  return(dt[])
}


#' @rdname calc_metrics
#' @export
calc_ci_metrics = function(res, by = c("scenario.id", "algo.id"),
                           stats_NA = TRUE) {
  dt = res[, .(
    coverage = mean(between(rmst_diff, ci_lower, ci_upper), na.rm = TRUE),
    mean_width = mean(ci_upper - ci_lower, na.rm = TRUE),
    median_width = median(ci_upper - ci_lower, na.rm = TRUE),
    num_NA = sum(is.na(pval)),
    prop_NA = mean(is.na(pval))
  ), by = by]
  
  if (!stats_NA) {
    dt[, `:=`(num_NA = NULL, prop_NA = NULL)]
  }
  
  return(dt[])
}



# Helpers / utilities ----

#' Create a sample allocation column
#' 
#' @description
#' The intended purpose/usage of this function is for preparing the data for creating 
#' graphics and tables.
#' 
#' @export
setj_samples_alloc = function(res) {
  set(
    res, j = "samples_alloc", value = factor(
      sprintf("(%d, %d)", res[, n0 / samples_k], res[, n1 / samples_k]),
      levels = sprintf("(%d, %d)", c(12, 15, 18), c(18, 15, 12))
    )
  )
  
  return(invisible(res[]))
}


#' Transform columns to percentage
#' 
#' @param dt (`data.table`)\cr
#'   Data table with some summary statistics / operating characteristics obtained from 
#'   the simulation results.
#' @param cols (`character(1)`)\cr
#'   Character vector of columns for which to apply the transformation.
#'
#' @export
setj_percent = function(dt, cols) {
  for (j in cols) {
    set(dt, j = j, value = dt[[j]] * 100)
  }
  
  return(invisible(dt[]))
}

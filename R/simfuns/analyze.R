box::use(
  bt = batchtools,
  data.table[...],
  chk = checkmate,
  stats[median]
)


# Obtain results ----


#' Obtain results from the simulation study
#' 
#' @description
#' Wrapper around `batchtools::reduceResultsDataTable()` tailored to the output of the jobs 
#' for my simulation study.
#' 
#' @param ids,reg Passed to `batchtools::reduceResultsDataTable()`.
#' @param what (`character()`)\cr
#'   Specify which subset of values, errors and warnings should be returned in the table.
#' 
#' @export
get_results = function(ids = NULL,
                       what = c("value", "error", "warning"),
                       reg = bt$getDefaultRegistry()) {
  
  # Control if values, errors and/or warnings should be returned
  what = unique(what)
  chk$assert_subset(what, c("value", "error", "warning"))
  to_drop = setdiff(c("value", "error", "warning"), what)
  reduce = if (length(to_drop) == 0) {
    NULL
  } else {
    function(res) {
      for (j in to_drop) {
        set(res, j = j, value = NULL)
      }
      return(res)
    }
  }
  
  # Obtain the results
  dt = bt$reduceResultsDataTable(ids = ids, fun = reduce, reg = reg)
  
  # Expand the results (nested data.tables)
  dt = lapply(1:nrow(dt), function(i) {
    res = dt[i, result][[1]]
    res[, job.id := dt[i, job.id]]
    res
  }) |>
    rbindlist()
  
  # job.id should be first column
  setcolorder(dt, neworder = "job.id")
  
  return(dt)
}


#' Obtain job parameters
#' 
#' @description
#' Wrapper around `batchtools::getJobPars()` tailored to the job table for my simulation study.
#' 
#' @param drop_algo (`logical(1)`)\cr
#'   Whether to drop columns related to "algorithms".
#'   They should be redundant as one job executes all algorithms and the parameters (significance level, cutoff, ...) 
#'   should all remain fixed.
#' @param unwrap (`numeric(1)`)\cr
#'   A number indicating up to which level `batchtools::unwrap()` should be applied to the table 
#'   with the job parameters.
#'   
#' @export
get_job_pars = function(ids = NULL,
                        reg = bt$getDefaultRegistry(),
                        drop_algo = TRUE,
                        unwrap = 0) {
  chk$assert_int(unwrap, lower = 0)
  
  dt = bt$getJobPars(ids = ids, reg = reg)
  
  if (drop_algo) {
    for (j in c("algorithm", "algo.pars")) {
      set(dt, j = j, value = NULL)
    }
  }
  
  if (unwrap != 0) {
    for (i in 1:unwrap) {
      dt = bt$unwrap(dt)
      if (!any(vapply(dt, \(x) is.list(x), logical(1)))) break
    }
  }
  
  return(dt)
}


#' Merge simulation results and parameters
#' 
#' @param res,params (`data.table`)\cr
#'   Corresponds to `x` and `y` in `data.table::merge()`.
#' @param by (`character()`)\cr
#'   Shared columns to merge on. This should usually not be altered.
#'   
#' @export
merge_res_params = function(res, params, by = "job.id") {
  merge(res, params, by = "job.id")
}



# Operating characteristics ----


#' Calculate rejection rates
#' 
#' @param res (`data.table`)\cr
#'   A data.table containing simulation results.
#' @param by (`character()`)\cr
#'   A character vector of columns used for grouping during the calculation.
#'   Defaults to `"method"` but later other columns related to the simulation settings 
#'   should be included.
#' @param alpha (`numeric(1)`)\cr
#'   The significance level used for making test decisions.
#' @param na_stats (`logical(1)`)\cr
#'   Whether to return summary statistics (counts and proportions) about the occurrence 
#'   of `NA`s.
#'   
#' @export
summarize_rejections = function(res, by = "method", alpha = 0.05, na_stats = TRUE) {
  dt = copy(res)
  
  dt[, reject := vapply(value, \(x) if (any(is.na(x))) NA else x[["pval"]] <= alpha, logical(1))][
    , value := NULL
  ]
  
  dt = dt[, .(
    rejection_rate = mean(reject, na.rm = TRUE),
    num_NA = sum(is.na(reject)),
    prop_NA = mean(is.na(reject))
  ), by = by]
  
  if (!na_stats) {
    dt[, `:=`(num_NA = NULL, prop_NA = NULL)]
  }
  
  return(dt[])
}


# TODO: true values available via problem design?
# Then how to best write this function?

#' Obtain/calculate confidence bounds, width and coverage indicator
#' 
#' @param res (`data.table`)\cr
#'   A data.table containing simulation results.
#' @param true_vals (`numeric()`)\cr
#'   A vector of the true effect sizes.
#'
#' @export
get_ci_metrics = function(res, true_vals = NA_real_) {
  dt = bt$unwrap(res, cols = "value")
  
  # Drop redundant columns
  for (j in setdiff(colnames(dt), c("job.id", "method", "ci_lower", "ci_upper"))) {
    set(dt, j = j, value = NULL)
  }
  
  # Calculate metrics
  dt[, `:=`(
    ci_width = ci_upper - ci_lower,
    ci_covered = between(true_vals, ci_lower, ci_upper)
  )]
  
  return(dt[])
}


#' Calculate operating characteristics of confidence intervals
#' 
#' @param res (`data.table`)\cr
#'   A data.table containing simulation results, including the columns `ci_covered` and 
#'   `ci_width` as they are returned by `get_ci_metrics()`.
#' @param by (`character()`)\cr
#'   Character vector of columns used for grouping.
#'
#' @export
summarize_cis = function(res, by = "method") {
  dt = res[, .(
    coverage = mean(ci_covered, na.rm = TRUE),
    mean_width = mean(ci_width, na.rm = TRUE),
    median_width = median(ci_width, na.rm = TRUE)
  ), by = by]
  
  return(dt)
}



# Exceptions ----


#' Summarize occurrences of errors and warnings
#' 
#' @param res (`data.table`)\cr
#'   A data.table containing simulation results.
#' @param what (`character(1)`)\cr
#'   One of `c("error", "warning")`
#' @param by (`character()`)\cr
#'   Character vector of columns used for grouping.
#'   The columns `message` and `call`, which are not directly contained in `res` but are created 
#'   using `batchtools::unwrap()` are also valid options.
#' @param ge1 (`logical(1)`)\cr
#'   If `TRUE` only return rows with at least one occurrence.
#' 
#' @note
#' This function only returns absolute counts but no proportions, since the latter might not be 
#' meaningful in the way that `data.table` computes them using `by`.
#' 
#' @export
summarize_exceptions = function(res, what = "error", by = "method", ge1 = TRUE) {
  chk$assert_choice(what, c("error", "warning"))
  
  dt = copy(res)
  to_drop = setdiff(c("error", "warning"), what)
  if (to_drop %chin% colnames(dt)) set(dt, j = to_drop, value = NULL)
  
  no_exceptions = tryCatch(
    all(vapply(dt[[what]], is.na, logical(1))),
    error = \(e) FALSE
  )
  
  # Special case: no errors/warnings at all
  if (no_exceptions) {
    dt[, c(what, "message", "call") := list(NULL, NA_character_, NA)]
  } else {
    # "Normal" case, i.e. at least one error/warning
    dt = bt$unwrap(dt, cols = what)
  }
  
  # Indicator column for summary statistics
  dt[, ind := fifelse(!is.na(message), 1L, 0L)]
  
  # Make `call` column available for `by` argument if needed
  if ("call" %chin% by) {
    dt[, call := vapply(call, \(x) if (is.call(x)) toString(x) else NA_character_, character(1))]
  }
  
  # Calculate summary statistics
  dt = dt[, .(.count = sum(ind)), by = by]
  
  #
  if (ge1) {
    dt = dt[.count >= 1]
  }
  
  return(dt[])
}


box::use(
  bt = batchtools,
  data.table[...],
  chk = checkmate,
  stats[median]
)


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


#' Capture output (if any), warnings and errors all at once
#' 
#' @param expr (`expression()`)\cr
#'   Code/expression to be evaluated.
#'   
#' @return (`vector("list", 3)`)\cr
#'   A list containing the output (`"value"`), the last warning (`"warning"`) and 
#'   the error (`"error"`) if any of these is present.
#'   
#' @note
#' If multiple warnings occur during the evaluation of the expression, only the last one 
#' will be returned.
#' 
#' @export
trycatch2 = function(expr) {
  warn = NA_character_
  err = NA_character_
  
  value = withCallingHandlers(
    tryCatch(expr, error = \(e) {
      #err <<- e$message
      err <<- e
      NA
    }),
    warning = \(w) {
      #warn <<- w$message
      warn <<- w
      invokeRestart("muffleWarning")
    }
  )
  
  list(value = value, warning = warn, error = err)
}

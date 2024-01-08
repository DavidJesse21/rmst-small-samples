#' Modified `tryCatch()` functions
#' 
#' @param code (`expression()`)\cr
#'   Code to be evaluated.
#' @param err_val (`ANY`)\cr
#'   Return value in case of an error.
#' 
#' @section `trycatch_log()`:
#' `trycatch_log()` works just like `tryCatch()` but will print any errors and warnings 
#' to the console (using `cat()`).
#' This can be useful e.g. for slurm. 
#' 
#' @section `trycatch_wrap()`: 
#' `trycatch_wrap()` evaluates `code` and always returns a list with three elements:
#' 1. `value`: What is returned by `code` or `err_val` in case of an error
#' 2. `error`: An error object (if one occurs).
#' 3. `warning`: A warning object (if one occurs).
#' 
#' @name trycatch2

#' @rdname trycatch2
#' @export
trycatch_log = function(code, err_val = NA_real_) {
  warn = NULL
  err = NULL
  
  value = withCallingHandlers(
    tryCatch(code, error = function(e) {
      err <<- e
      err_val
    }),
    warning = function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    }
  )
  
  if (!is.null(warn)) cat_warning(warn)
  if (!is.null(err)) cat_error(err)
  
  return(value)
}

cat_error = function(err) {
  .call = deparse(err$call)
  .message = err$message
  txt = sprintf("Error in %s: %s\n", .call, .message)
  cat(txt)
}

cat_warning = function(warn) {
  .call = deparse(warn$call)
  .message = warn$message
  txt = sprintf("Warning in %s: %s\n", .call, .message)
  cat(txt)
}


#' @rdname trycatch2
#' @export
trycatch_wrap = function(code, err_val = NA_real_) {
  warn = NA
  err = NA
  
  value = withCallingHandlers(
    tryCatch(expr, error = \(e) {
      err <<- e
      err_val
    }),
    warning = \(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    }
  )
  
  list(value = value, warning = warn, error = err)
}


#' Estimate and format the runtime of a job
#' 
#' @param secs_per_job (`numeric(1)`)\cr
#'   The (assumed/estimated) runtime for a single job in seconds
#' @param num_jobs (`numeric(1)`)\cr
#'   The number of replications of the job.
#' @param mult (`numeric(1)`)\cr
#'   A number that the runtime will be multiplied with for more conservative estimates.
#' @param parallel (`list()` or `NULL`)\cr
#'   Either `NULL` to estimate the sequential runtime or a named list with the elements:\cr
#'   * `num_cores` (`numeric(1)`): The number of available cores
#'   * `prop` (`numeric(1)`): The proportion of parallelizable jobs
#' 
#' @note
#' For the estimation of the runtime using parallel processes Amdahl's law is used.
#' (https://en.wikipedia.org/wiki/Amdahl%27s_law)
#'   
#' @export
estimate_runtime = function(secs_per_job,
                            num_jobs = 5000L,
                            mult = 1,
                            parallel = NULL) {
  total = secs_per_job * num_jobs * mult
  
  if (is.null(parallel)) {
    hours = floor(total / 3600)
    minutes = floor((total %% 3600) / 60)
    seconds = ceiling(total %% 60)
    fmt = sprintf("%02d:%02d:%02d", hours, minutes, seconds)
    return(fmt)
  } else {
    speedup = 1 / ((1 - parallel$prop) + (parallel$prop / parallel$num_cores))
    total = total / speedup
    hours = floor(total / 3600)
    minutes = floor((total %% 3600) / 60)
    seconds = ceiling(total %% 60)
    fmt = sprintf("%02d:%02d:%02d", hours, minutes, seconds)
    return(fmt)
  }
}


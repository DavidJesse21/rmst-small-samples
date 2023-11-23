box::use(
  ./trycatch2[trycatch2]
)

box::use(
  future.apply[future_lapply],
  withr[with_preserve_seed],
  fs,
  qs
)


#' Run simulations
#' 
#' @param params (`data.table::data.table()`)\cr
#'   A data.table containing the simulation parameters, which are passed to `fun_generate()`.
#'   For `run_sim_one()` `params` should consist of a single row/observation only, 
#'   for `run_sim_all()` the number of parameter configurations is (in principle) arbitrary.
#' @param num_sims (`numeric(1)`)\cr
#'   The number of simulation runs.
#' @param fun_generate (`function()`)\cr
#'   The data-generating function, which takes (only) `params` as its input.
#' @param funs_analyze (`list()`)\cr
#'   A named list containing the functions to be applied to the simulated data 
#'   produced by `fun_generate`.
#' @param future_args (`list()`)\cr
#'   A named list with additional arguments passed to `future_lapply()`.
#' @param dir_out (`character(1)`)\cr
#'   The path to the directory, in which the simulation results should be stored 
#'   (using the `qs` package).
#'   Defaults to the current working directory, but it is recommended to change this 
#'   to a directory dedicated to the simulation results only.
#' @param filenames (`character(1)`)\cr
#'   The "main" name of the output files.
#'   Note that each file is prefixed by the row number/ID of the simulation/parameter setting, 
#'   e.g. for two simulation settings you would obtain the files `01-simresults.qs` and `02-simresults.qs`.
#' @param verbose (`logical(1)`)\cr
#'   Whether to print (meta-)information about the simulations to the console during the simulations.
#'   
#' @name sim
NULL


#' @rdname sim
#' @export
run_sim_one = function(params,
                       num_sims,
                       fun_generate,
                       funs_analyze,
                       future_args = list(future.seed = 42L)) {
  # Record warnings and errors
  # Preserve seed (e.g. when using a permutation function)
  funs_analyze = lapply(
    funs_analyze,
    \(f) \(...) trycatch2(with_preserve_seed(f(...)))
  )
  
  all_args = c(
    # Standard first two arguments for *apply() functions 
    list(
      X = seq_len(num_sims),
      FUN = function(i) {
        x = fun_generate(params)
        out = lapply(funs_analyze, \(f) f(x))
        out = matrix(
          out, nrow = 1L, ncol = length(funs_analyze),
          dimnames = list(NULL, names(funs_analyze))
        )
      }
    ),
    # future_lapply() arguments
    future_args
  )
  
  # Run simulation and return results
  results = do.call(future_lapply, all_args)
  results = do.call(rbind, results)
  
  return(results)
}


#' @rdname sim
#' @export
run_sim_all = function(params,
                       num_sims,
                       fun_generate,
                       funs_analyze,
                       future_args = list(future.seed = 42L),
                       dir_out = getwd(),
                       file_names = "simresults",
                       verbose = TRUE) {
  # Create output directory if it does not exist yet
  if (!fs$dir_exists(dir_out)) fs$dir_create(dir_out)
  
  # Number of different simulation scenarios
  num_params = nrow(params)
  
  # Output files, in which simulation results will be stored
  fstring_num = paste0("%0", max(2, nchar(num_params)), "d")
  fstring_file = paste0(fstring_num, "-", file_names)
  file_names = sprintf(fstring_file, seq_len(num_params))
  file_paths = fs$path(dir_out, file_names, ext = "qs")
  # Maybe include a check for existing files/results
  
  if (verbose) {
    cat2(
      "Number of simulation scenarios: ", num_params, "\n",
      "Number of simulation runs: ", num_sims, "\n\n"
    )
  }
  
  # Print following information to the console
  #   -> include `verbose` argument
  # - current row/scenario
  # - start time, end time
  # - path to produced file
  for (i in seq_len(num_params)) {
    time_start = Sys.time()
    
    if (verbose) {
      cat2(
        paste0(rep("-", getOption("width")/2),  collapse = ""), "\n",
        "Running simulation ", sprintf(fstring_num, i), "\n\n",
        "Time started: ", format(time_start), "\n",
        "...", "\n"
      )
    }
    
    result = run_sim_one(
      params[i],
      num_sims,
      fun_generate,
      funs_analyze,
      future_args = list(future.seed = 42L)
    )
    
    time_end = Sys.time()
    time_taken = time_end - time_start
    
    out = list(
      simulator = list(params = params[i], fun = fun_generate),
      results = result,
      duration = time_taken
    )
    
    qs$qsave(out, file_paths[i])
    
    if (verbose) {
      cat2(
        "Time finished: ", format(time_end), "\n",
        sprintf("(Total duration: %s)", format(round(time_taken, 2))), "\n\n",
        "Results saved to ", file_paths[i], "\n\n"
      )
    }
  }
  
  
  return(invisible(file_paths))
}


# Simple wrapper around `cat()`
cat2 = \(...) cat(..., sep = "")


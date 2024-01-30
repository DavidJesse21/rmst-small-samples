options(box.path = "R")

box::use(
  fs,
  DBI[...],
  RSQLite[SQLite],
  data.table[...]
)

box::use(
  simfuns2/get_funs[get_results_table, get_scenario_table, get_algo_table]
)


#' Check state of simulations
#' 
#' @param dir_sim (`character(1)`)\cr
#'   The directory of the simulation study.
#' 
#' @returns (`list()`)\cr
#'   A list with 3 elements: `finished`, `not_finished`, `not_started`
#'   Each element is a vector with the corresponding scenario / simulation IDs.
#'   
#' @export
check_sim_finished = function(dir_sim = fs$path("simulation"), num_sims = 5000L) {
  
  db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
  on.exit(dbDisconnect(db), add = TRUE)
  
  # If not specified check all scenario.ids
  scenario.ids = dbGetQuery(db, "SELECT `scenario.id` FROM scenarios")[[1]]
  
  # Also get algorithm IDs for checks
  algo.ids = dbGetQuery(db, "SELECT `algo.id` FROM algorithms")[[1]]
  
  # Output object
  out = list(finished = integer(), not_finished = integer(), not_started = integer())
  
  # Current results
  res = get_results_table(fs$path(dir_sim, "registry"))
  
  # All jobs/simulations that have at least started or are finished
  idx1 = scenario.ids[which(scenario.ids %in% unique(res$scenario.id))]
  # Jobs/simulations that have not started at all
  # (using a heuristic which should however always work)
  idx2 = setdiff(scenario.ids, idx1)
  out$not_started = idx2
  
  # Check for jobs/simulations that are completely finished
  is_finished = vapply(
    idx1, function(i) {
      tab = res[scenario.id == i, table(algo.id, useNA = "ifany")]
      check = (length(tab) == length(algo.ids)) && all(tab == num_sims) && all(names(tab) %in% as.character(algo.ids))
      return(check)
    },
    logical(1)
  )
  idx3 = idx1[is_finished]
  out$finished = idx3
  
  # Rest has been submitted but is not finished yet
  out$not_finished = setdiff(idx1, idx3)
  
  return(out)
}



#' Collect the results from the simulation study
#' 
#' @param join (`character()`)\cr
#'   A character vector with possible elements `"scenarios"` and `"algos"`.
#'   If those are supplied, then the actual names/values behind `scenario.id` and `algo.id` 
#'   will be appended (joined) to the data.
#' @param .save (`logical(1)`)\cr
#'   If `FALSE` the results are returned in the current R session.
#'   If `TRUE` the results are written to a file (see details below)
#' @param dir_sim (`character()`)\cr
#'   The directory of the simulation study.
#'   
#' @returns (`data.table` or `character(1)`)\cr
#'   If `.save = FALSE` returns the results as a `data.table`.
#'   Otherwise it saves the results to the directory `dir_sim`/results.
#' 
#' @details
#' The naming convention for saving the results to a file is as follows: `[DATE_TODAY]_results[id].rds`
#' `[id]` will be empty if no other file has been saved on `DATE_TODAY`.
#' Otherwise it will be incremented by 1 starting from 1.
#' 
#' @export
collect_results = function(join = character(),
                           .save = FALSE,
                           dir_sim = fs$path("simulation")) {
  message("Collecting results...")
  
  # Track execution time
  start = Sys.time()
  
  # Collect results
  dtr = get_results_table(fs$path(dir_sim, "registry"))
  
  if ("scenarios" %in% join) {
    dts = get_scenario_table(fs$path(dir_sim, "registry"))
    dtr = merge(dtr, dts, by = "scenario.id")
  }
  
  if ("algos" %in% join) {
    dta = get_algo_table(fs$path(dir_sim, "registry"))
    dtr = merge(dtr, dta, by = "algo.id")
  }
  
  # Row order
  setorder(dtr, scenario.id, algo.id, rep.id)
  # Column order
  setcolorder(dtr, c("scenario.id", "algo.id", "rep.id", "pval", "ci_lower", "ci_upper"))
  
  # Track execution time
  end = Sys.time()
  taken = round(as.numeric(end - start), 2)
  message(sprintf("Done! (%s seconds elapsed)", taken))
  
  # Simply return the object
  if (!.save) {
    return(dtr[])
  } else {
    # Otherwise write it to a file
    filename = paste0(Sys.Date(), "_results")
    
    # Check existing files for possible naming conflicts
    present_files = fs$path_file(fs$dir_ls(fs$path(dir_sim, "results")))
    idx = length(grep(paste0("^", filename, "[1-9][0-9]*|.rds$"), present_files))
    idx = if (idx == 0) NULL else idx
    filename = paste0(filename, idx)
    
    # Save results to file
    file_out = fs$path(dir_sim, "results", filename, ext = "rds")
    saveRDS(dtr, file_out)
    return(file_out)
  }
}


#' Remove (delete) temporary files
#' 
#' @description
#' When submitting jobs via `submit_jobs()` "temporary" `.R`- and `.sh`-files are created.
#' When the jobs have been successfully submitted these files are not required anymore and can be deleted.
#' This function does exactly this, i.e. it deletes all files in `dir_sim`/registry/temp.
#' 
#' @param dir_sim (`character(1)`)\cr
#'   Path to the directory of the simulation study.
#' 
#' @export
rm_temp_files = function(dir_sim = fs$path("simulation")) {
  dir_temp = fs$path(dir_sim, "registry", "temp")
  files = fs$dir_ls(dir_temp)
  
  tab = table(fs$path_ext(files))
  
  message(sprintf("About to delete %d files from %s:", sum(tab), dir_temp))
  print(tab)
  cat("\n")
  cont = readline("Continue? [y/n] ")
  stopifnot(cont %in% c("y", "n"))
  
  if (cont == "n") {
    stop("Stopped execution, no files deleted.", call. = FALSE)
  } else {
    fs$file_delete(files)
    message("All temporary files deleted.")
  }
}


#' Read logs of a job
#' 
#' @param scenario.id (`numeric(1)`)\cr
#'   The scenario / job ID for which to read the logs.
#' @param open (`logical(1)`)\cr
#'   If `FALSE` the logs are printed to the R console, otherwise the log file 
#'   gets opened in a separate program (e.g. in your text editor).
#' @param dir_sim (`character(1)`)\cr
#'   Path to the directory of the simulation study.
#'   
#' @returns (`character(1)`)\cr
#'   Invisibly returns the path to the log file.
#' 
#' @export
read_logs = function(scenario.id, open = FALSE, dir_sim = fs$path("simulation")) {
  file = fs$path(dir_sim, "registry", "logs", paste0("job", scenario.id), ext = "log")
  
  if (open) {
    fs$file_show(file)
  } else {
    cat(readLines(file), sep = "\n")
  }
  
  return(invisible(file))
}

options(box.path = "R")

box::use(
  bt = batchtools,
  chk = checkmate,
  fs,
  data.table[...]
)

box::use(
  simfuns/utils[get_job_pars]
)



#' Obtain results from the registry for the simulation study
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


#' Combine single files for each job into (new) grouped ones
#' 
#' @param dir_out (`character(1)`)\cr
#'   Directory to which the compressed results should be written.
#' @param reg (`Registry`)\cr
#'   The registry containing the results.
#' @param split_by_setting (`logical(1)`)\cr
#'   If `TRUE` one file for each simulation setting is created, otherwise all simulation 
#'   results are combined into one single file.
#'   
#' @export
compress_results = function(dir_out = fs$path("simresults", "results"),
                            reg = bt$getDefaultRegistry(),
                            split_by_setting = TRUE) {
  
  ids = get_job_pars(reg = reg, unwrap = 1)
  for (j in setdiff(colnames(ids), c("job.id", "chunk"))) {
    set(ids, j = j, value = NULL)
  }
  
  # Option 1: One file for each simulation scenario
  if (split_by_setting) {
    li_ids = split(ids, by = "chunk")
    settings = names(li_ids)
    invisible(lapply(settings, function(i) {
      x = get_results(ids = li_ids[[i]], reg = reg)
      saveRDS(x, file = fs$path(dir_out, i, ext = "rds"))
    }))
    message(sprintf("%s files written to %s", length(li_ids), dir_out))
  } else {
    # Option 2: One single file for all results
    x = get_results(reg = reg)
    x = merge(x, ids, by = "job.id")
    setnames(x, old = "chunk", new = "sim.id")
    file = fs$path(dir_out, "all", ext = "rds")
    saveRDS(x, file)
    message(sprintf("Results written to %s", file))
  }
}


# Maybe continue here later
get_results2 = function(sim.ids = NULL,
                        dir_results = fs$path("simresults", "results")) {
  1
}




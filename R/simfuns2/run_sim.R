options(box.path = "R")

box::use(
  fs,
  DBI[...],
  RSQLite[SQLite],
  withr[with_seed],
  data.table[...],
  parallel[mclapply]
)

box::use(
  simfuns2/scenarios[make_params],
  simfuns2/dgp[gen_surv_data],
  simfuns2/utils[trycatch_log]
)


#' Run a simulation (one scenario)
#' 
#' @param scenario.id (`numeric(1)`)\cr
#'   An identifier for the scenario for which simulations should be run.
#' @param sim_resources (`list()`)\cr
#'   A named list with specifications for the simulation to be run:
#'   * `num_sims` (`numeric(1)`): The number of simulation repetitions.
#'   * `algos` (`character()`): A vector of algorithms that should be evaluated.
#'   * `ncpus` (`numeric(1)`): The number of CPUs/cores per node.
#'   * `data_subset` (`numeric()`): Optional vector of indices that lie within `seq_len(num_sims)`.
#' @param constants (`list()`)\cr
#'   A list of constant objects passed to each algorithm.   
#' @param dir_sim (`character(1)`)\cr
#'   The path to the simulation directory.
#'   
#' @export
run_sim = function(scenario.id,
                   sim_resources,
                   constants,
                   dir_sim = fs$path("simulation")) {
  # Connect to database
  db = dbConnect(SQLite(), fs$path(dir_sim, "simdb", ext = "db"))
  on.exit(dbDisconnect(db))
  
  # Get parameters for data generating process
  params = dbGetQuery(
    db,
    sprintf("SELECT * FROM scenarios WHERE `scenario.id` = %d", scenario.id)
  )
  params = make_params(params)
  
  # Generate the data
  li_data = with_seed(scenario.id, {
    lapply(seq_len(sim_resources$num_sims), function(i) {
      do.call(gen_surv_data, params)
    })
  })
  if (!is.null(sim_resources$data_subset)) li_data = li_data[data_subset]
  
  #  Sort supplied algorithms (shortest to longest runtime)
  algos = sim_resources$algos[match(sim_resources$algos, c("asy", "pseudo_hc3", "studperm", "pseudo_ij_boot"))]
  # Get the algorithms
  li_algos = lapply(algos, function(x) {
    file_algo = fs$path(dir_sim, "algorithms", x, ext = "rds")
    readRDS(file_algo)
  })
  names(li_algos) = algos
  
  # Evaluate all algorithms on the generated data
  for (algo in names(li_algos)) {
    # Get algorithm id
    algo.id = dbGetQuery(
      db,
      sprintf("SELECT `algo.id` FROM algorithms WHERE algo = '%s'", algo)
    )[[1]]
    
    # Get actual function object
    .fun = li_algos[[algo]]
    
    # Evaluate algorithms
    out = with_seed(scenario.id, .rng_kind = "L'Ecuyer-CMRG", {
      mclapply(li_data, function(dt) {
        trycatch_log(.fun(dt, constants), err_val = rep(NA_real_, 3))
      }, mc.cores = if (algo == "asy") 1L else sim_resources$ncpus)
    })
    
    # Prepare output object
    out = as.data.table(do.call(rbind, out))
    out[, `:=`(scenario.id = scenario.id, algo.id = algo.id)]
    if (is.null(sim_resources$data_subset)) {
      out[, rep.id := seq_len(sim_resources$num_sims)]
    } else {
      out[, rep.id := sim_resources$data_subset]
    }
    setcolorder(out, new = c("scenario.id", "algo.id", "rep.id", "pval", "ci_lower", "ci_upper"))
    
    # Write results to database
    dbAppendTable(db, "results", out)
  }
  
  
  return(invisible(scenario.id))
}

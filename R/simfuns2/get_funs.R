box::use(
  fs,
  DBI[...],
  data.table[...],
  RSQLite[SQLite]
)


# Database tables ----

#' @export
get_results_table = function(dir_reg = fs$path("simulation", "registry")) {
  file_db = fs$path(dir_reg, "simdb", ext = "db")
  db = dbConnect(SQLite(), file_db)
  on.exit(dbDisconnect(db))
  
  dt = dbReadTable(db, "results")
  setDT(dt)
  
  return(dt[])
}


#' @export
get_scenario_table = function(dir_reg = fs$path("simulation", "registry")) {
  file_db = fs$path(dir_reg, "simdb", ext = "db")
  db = dbConnect(SQLite(), file_db)
  on.exit(dbDisconnect(db))
  
  dt = dbReadTable(db, "scenarios")
  setDT(dt)
  
  return(dt[])
}


#' @export
get_algo_table = function(dir_reg = fs$path("simulation", "registry")) {
  file_db = fs$path(dir_reg, "simdb", ext = "db")
  db = dbConnect(SQLite(), file_db)
  on.exit(dbDisconnect(db))
  
  dt = dbReadTable(db, "algorithms")
  setDT(dt)
  
  return(dt[])
}


# Slurm ----

#' Get the number of running jobs
#' @export
get_num_running = function() {
  n_running = system2(
    "squeue", c("-u", "$USER", "--states=RUNNING", "|", "wc", "-l"),
    stdout = TRUE
  )
  as.integer(n_running) - 1L
}


#' @export
get_jobs_submitted = function(which = c("all", "running", "pending"),
                              data_table = TRUE) {
  which = match.arg(which, c("all", "running", "pending"))
  
  command = "squeue"
  args_user = c("-u", "$USER")
  arg_which = switch(which,
    "all" = NULL,
    "running" = "--states=RUNNING",
    "pending" = "--states=PENDING"
  )
  arg_format = '--format="%.12i %.12j %.8u %.3P %.3q %.10l %.10M %.5D %.4C %.7m %16R %.8T"'
  all_args = c(args_user, arg_which, arg_format)
  
  if (!data_table) {
    # Just the console output
    system2(command, all_args)
  } else {
    # Read in as data.table
    x = system2(command, all_args, stdout = TRUE)
    tryCatch(
      fread(text = x),
      error = \(e) NULL
    )
  }
}


#' @export
get_jobs_finished = function(data_table = FALSE) {
  command = "sacct"
  args = c("--user=$USER", "--units=G", "--format=JobID,JobName,Partition,QOS,Timelimit,Elapsed,AllocNodes,AllocCPU,ReqMem,MaxRSS,State")
  
  if (data_table) {
    x = system2(comman, args, stdout = TRUE)
    fread(text = x[-2], fill = TRUE)
  } else {
    system2(command, args)
  }
}

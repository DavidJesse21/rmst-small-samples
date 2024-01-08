box::use(
  fs,
  DBI[...],
  data.table[...],
  RSQLite[SQLite]
)


#' @export
get_results_table = function(file_db = fs$path("simulation", "simdb", ext = "db")) {
  db = dbConnect(SQLite(), file_db)
  on.exit(dbDisconnect(db))
  
  dt = dbReadTable(db, "results")
  setDT(dt)
  
  return(dt[])
}


#' @export
get_scenarios_table = function(file_db = fs$path("simulation", "simdb", ext = "db")) {
  db = dbConnect(SQLite(), file_db)
  on.exit(dbDisconnect(db))
  
  dt = dbReadTable(db, "scenarios")
  setDT(dt)
  
  return(dt[])
}


#' This can be improved
#' @export
get_submitted_jobs = function(which = c("all", "running", "pending"),
                              stdout = "", stderr = "") {
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
  
  system2(command, all_args, stdout = stdout, stderr = stderr)
}


#' @export
get_num_running = function() {
  n_running = system2("squeue", c("-u", "$USER", "--states=RUNNING", "|", "wc", "-l"))
  as.integer(n_running) - 1L
}

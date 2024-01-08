box::use(
  fs,
  data.table[data.table],
  DBI[...],
  RSQLite[SQLite]
)


#' @export
setup_sim = function(dir_sim = fs$path("simulation"),
                     dt_scenarios) {
  # Base directory
  if (!fs$dir_exists(dir_sim)) fs$dir_create(dir_sim)
  # Other directories
  fs$dir_create(fs$path(dir_sim, "algorithms"))
  fs$dir_create(fs$path(dir_sim, "logs"))
  fs$dir_create(fs$path(dir_sim, "templates"))
  fs$dir_create(fs$path(dir_sim, "temp"))
  
  # Create resource files
  fs$file_create(fs$path(dir_sim, "resources_slurm", ext = "R"))
  fs$file_create(fs$path(dir_sim, "resources_sim", ext = "R"))
  
  # Create data base
  create_db(dir_sim)
  # Create corresponding tables
  create_table_scenarios(dt_scenarios, dir_sim)
  create_table_algos(dir_sim)
  create_table_results(dir_sim)
  
  msg1 = "Simulation directory is set up."
  msg2 = "Use `add_algorithm()` and `remove_algorithm()` to add/remove algorithms."
  message(msg1, "\n", msg2)
  
  return(invisible(NULL))
}


#' @export
create_db = function(dir_sim = fs$path("simulation")) {
  fs$file_create(fs$path(dir_sim, "simdb", ext = "db"))
}


#' @export
create_table_scenarios = function(dt, dir_sim = fs$path("simulation")) {
  con = dbConnect(SQLite(), fs$path(dir_sim, "simdb", ext = "db"))
  on.exit(dbDisconnect(con))
  
  dbWriteTable(con, "scenarios", dt)
  
  return(invisible(NULL))
}


#' @export
create_table_algos = function(dir_sim = fs$path("simulation")) {
  con = dbConnect(SQLite(), fs$path(dir_sim, "simdb", ext = "db"))
  on.exit(dbDisconnect(con))
  
  dt = data.table(algo.id = integer(), algo = character())
  dbWriteTable(con, "algorithms", dt)
  
  return(invisible(NULL))
}


#' @export
add_algorithm = function(name, fun, dir_sim = fs$path("simulation")) {
  con = dbConnect(SQLite(), fs$path(dir_sim, "simdb", ext = "db"))
  on.exit(dbDisconnect(con))
  
  id = dbGetQuery(con, "SELECT COUNT(`algo.id`) FROM algorithms")[1, ] + 1L
  new = data.table(algo.id = id, algo = name)
  dbAppendTable(con, "algorithms", new)
  
  saveRDS(fun, fs$path(dir_sim, "algorithms", name, ext = "rds"))
  
  return(invisible(NULL))
}


#' @export
remove_algorithm = function(name, dir_sim = fs$path("simulation")) {
  con = dbConnect(SQLite(), fs$path(dir_sim, "simdb", ext = "db"))
  on.exit(dbDisconnect(con))
  
  dbExecute(con, sprintf("DELETE FROM algorithms WHERE algo = '%s'", name))
  fs$file_delete(fs$path(dir_sim, "algorithms", name, ext = "rds"))
  
  return(invisible(NULL))
}


#' @export
create_table_results = function(dir_sim = fs$path("simulation")) {
  con = dbConnect(SQLite(), fs$path(dir_sim, "simdb", ext = "db"))
  on.exit(dbDisconnect(con))
  
  dbWriteTable(
    con, "results",
    data.table(scenario.id = integer(), algo.id = integer(), rep.id = integer(),
               pval = numeric(), ci_lower = numeric(), ci_upper = numeric())
  )
  
  return(invisible(NULL))
}

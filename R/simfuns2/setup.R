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

  # Registry directory
  dir_reg = fs$path(dir_sim, "registry")
  fs$dir_create(dir_reg)
  
  # Other directories
  fs$dir_create(fs$path(dir_reg, "algorithms"))
  fs$dir_create(fs$path(dir_reg, "logs"))
  fs$dir_create(fs$path(dir_reg, "temp"))
  fs$dir_create(fs$path(dir_reg, "backup"))
  
  # Create resource files
  saveRDS(list(), fs$path(dir_reg, "resources_slurm", ext = "rds"))
  saveRDS(list(), fs$path(dir_reg, "resources_sim", ext = "rds"))
  
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
  fs$file_create(fs$path(dir_sim, "registry", "simdb", ext = "db"))
}


#' @export
create_table_scenarios = function(dt, dir_sim = fs$path("simulation")) {
  db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
  on.exit(dbDisconnect(db))
  
  dbWriteTable(db, "scenarios", dt)
  
  return(invisible(NULL))
}


#' @export
create_table_algos = function(dir_sim = fs$path("simulation")) {
  db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
  on.exit(dbDisconnect(db))
  
  dt = data.table(algo.id = integer(), algo = character())
  dbWriteTable(db, "algorithms", dt)
  
  return(invisible(NULL))
}


#' @export
add_algorithm = function(name, fun, dir_sim = fs$path("simulation")) {
  db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
  on.exit(dbDisconnect(db))
  
  id = dbGetQuery(db, "SELECT COUNT(`algo.id`) FROM algorithms")[1, ] + 1L
  new = data.table(algo.id = id, algo = name)
  dbAppendTable(db, "algorithms", new)
  
  saveRDS(fun, fs$path(dir_sim, "registry", "algorithms", name, ext = "rds"))
  
  return(invisible(NULL))
}


#' @export
remove_algorithm = function(name, dir_sim = fs$path("simulation")) {
  db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
  on.exit(dbDisconnect(db))
  
  dbExecute(db, sprintf("DELETE FROM algorithms WHERE algo = '%s'", name))
  fs$file_delete(fs$path(dir_sim, "registry", "algorithms", name, ext = "rds"))
  
  return(invisible(NULL))
}


#' @export
create_table_results = function(dir_sim = fs$path("simulation")) {
  db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
  on.exit(dbDisconnect(db))
  
  dbWriteTable(
    db, "results",
    data.table(scenario.id = integer(), algo.id = integer(), rep.id = integer(),
               pval = numeric(), ci_lower = numeric(), ci_upper = numeric())
  )
  
  return(invisible(NULL))
}

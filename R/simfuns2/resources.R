box::use(fs)


# Get resources
get_resources = function(file) {
  readRDS(file)
}

#' @export
get_slurm_resources = function(dir_reg = fs$path("simulation", "registry")) {
  get_resources(fs$path(dir_reg, "resources_slurm", ext = "rds"))
}

#' @export
get_sim_resources = function(dir_reg = fs$path("simulation", "registry")) {
  get_resources(fs$path(dir_reg, "resources_sim", ext = "rds"))
}


# Set resources
set_resources = function(..., file) {
  res = get_resources(file)
  
  toset = list(...)
  if (any(names(toset) == "")) stop("All arguments in `...` must be named.")
  
  for (r in names(toset)) {
    res[[r]] = toset[[r]]
  }
  
  saveRDS(res, file)
}

#' @export
set_slurm_resources = function(..., dir_reg = fs$path("simulation", "registry")) {
  set_resources(..., file = fs$path(dir_reg, "resources_slurm", ext = "rds"))
}

#' @export
set_sim_resources = function(..., dir_reg = fs$path("simulation", "registry")) {
  set_resources(..., file = fs$path(dir_reg, "resources_sim", ext = "rds"))
}

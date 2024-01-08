#' @export
get_slurm_resources = function(file = "simulation/resources_slurm.R") {
  get_resources(file)
}

#' @export
get_sim_resources = function(file = "simulation/resources_sim.R") {
  get_resources(file)
}

get_resources = function(file) {
  source(file, local = TRUE)
  toget = sub("\\s*=.*", "", readLines(file))
  mget(toget)
}


#' @export
set_slurm_resources = function(..., file = "simulation/resources_slurm.R") {
  set_resources(..., file)
}

#' @export
set_sim_resources = function(..., file = "simulation/resources_sim.R") {
  set_resources(..., file)
}

set_resources = function(..., file) {
  res = get_resources(file)
  
  toset = list(...)
  if (any(names(toset) == "")) {
    stop("All arguments in `...` must be named.")
  }
  
  for (r in names(toset)) {
    res[[r]] = toset[[r]]
  }
  
  new = vapply(
    names(res),
    \(r) sprintf('%s = %s', r, quote_sign(res[[r]])),
    character(1)
  )
  
  writeLines(new, file)
}


# Helper for writing files
quote_sign = function(x) {
  if (is.character(x)) {
    sprintf('"%s"', x)
  } else {
    x
  }
}

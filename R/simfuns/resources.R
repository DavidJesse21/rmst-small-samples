#' Get resource specifications for batchtools registry
#' @export
get_resources = function(file = "simsetup/resources.R") {
  source(file, local = TRUE)
  toget = sub("\\s*=.*", "", readLines(file))
  mget(toget)
}


#' Set resource specifications for batchtools registry
#' @export
set_resources = function(...,
                         file = "simsetup/resources.R") {
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


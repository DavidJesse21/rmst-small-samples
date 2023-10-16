#' @export
mixture_surv = function(time, hazards, probs) {
  survs = vapply(hazards, \(lambda) exp(- lambda * time), numeric(length(time)))
  drop(survs %*% probs)
}

#' @export
mixture_hazard = function(time, hazards, probs) {
  survs = vapply(hazards, \(lambda) exp(- lambda * time), numeric(length(time)))
  
  # Numerator
  t_num = drop(survs %*% (hazards * probs))
  # Denominator
  t_denom = drop(survs %*% probs)
  
  t_num / t_denom
}

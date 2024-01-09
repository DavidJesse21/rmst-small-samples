#' @export
cancel_jobs = function(slurm.ids) {
  system2("scancel", paste0(slurm.ids, collapse = ", "))
}

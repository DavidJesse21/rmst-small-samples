box::use(
  data.table[as.data.table]
)

#' Extract results from a simulation
#' 
#' @param res (`matrix()`)\cr
#'   A matrix containing the results of one (i.e. one parameter setting) simulation.
#'   Each entry of the matrix is expected to be a list of length 3 with entries 
#'   "value", "warning" and "error".
#' @param as_data_table (`logical(1)`)\cr
#'   Whether to convert the resulting matrix to `data.table::data.table()`.
#'
#' @name get_values
NULL


#' @rdname get_values
#' @export
get_values = function(res, as_data_table = TRUE) {
  for (j in seq_len(ncol(res))) {
    res[, j] = lapply(res[, j], \(x) x$value)
  }
  
  if (as_data_table) as.data.table(res) else res
}


#' @rdname get_values
#' @export
get_warnings = function(res, as_data_table = TRUE) {
  res = apply(
    res, MARGIN = 2L,
    \(j) vapply(j, \(x) x$warning, character(1))
  )
  
  if (as_data_table) as.data.table(res) else res
}


#' @rdname get_values
#' @export
get_errors = function(res, as_data_table = TRUE) {
  res = apply(
    res, MARGIN = 2L,
    \(j) vapply(j, \(x) x$error, character(1))
  )
  
  if (as_data_table) as.data.table(res) else res
}


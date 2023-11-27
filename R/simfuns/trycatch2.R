#' Capture output (if any), warnings and errors all at once
#' 
#' @param expr (`expression()`)\cr
#'   Code/expression to be evaluated.
#'   
#' @return (`vector("list", 3)`)\cr
#'   A list containing the output (`"value"`), the last warning (`"warning"`) and 
#'   the error (`"error"`) if any of these is present.
#'   
#' @note
#' If multiple warnings occur during the evaluation of the expression, only the last one 
#' will be returned.
#' 
#' @export
trycatch2 = function(expr) {
  warn = NA_character_
  err = NA_character_
  
  value = withCallingHandlers(
    tryCatch(expr, error = \(e) {
      #err <<- e$message
      err <<- e
      NA
    }),
    warning = \(w) {
      #warn <<- w$message
      warn <<- w
      invokeRestart("muffleWarning")
    }
  )
  
  list(value = value, warning = warn, error = err)
}


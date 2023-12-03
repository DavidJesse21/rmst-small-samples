box::use(
  data.table[as.data.table],
  survival[Surv]
)


#' Safely obtain survival data from a formula as a data.table
#' @export
get_surv_data = function(formula, data = environment(formula)) {
  y = as.matrix(eval(formula[[2]], envir = data))
  x = if (formula[[3]] == 1) NULL else eval(formula[[3]], envir = data)
  
  out = cbind(
    as.data.table(y),
    as.data.table(x)
  )
  
  out
}


#' Check and handle inestimable cases
#' 
#' @description
#' If the cutoff value is larger than the latest event time and that event time is censored 
#' and not observed, then the Kaplan-Meier estimator is not uniquely defined up to the cutoff 
#' and therefore the RMST is actually not estimable.
#' This function checks and handles such cases.
#' 
#' @param x (`matrix()`)\cr
#'   A Kaplan-Meier table returned by `fsurvfit()`.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param inest_action (`character(1)`)\cr
#'   What to do if RMST is not estimable. See below for details.
#' @param group_label (`character(1)`)\cr
#'   An optional group label to be used if RMST is estimated for more than one sample.
#' 
#' @returns
#' If RMST is estimable, `FALSE` is returned invisibly.
#' If RMST is not estimable and `inest_action = "extend"` `TRUE` is returned invisibly.
#' The same holds for `inest_action = "extend_warn"` but in addition to that a warning 
#' message is generated.
#' If RMST is not estimable and `inest_action == "error"`, the function is only called for 
#' its side effect, i.e. throwing an error.
#' 
#' @export
check_inest = function(x, cutoff,
                       inest_action = c("error", "extend", "extend_warn"),
                       group_label = NULL) {
  # Check if RMST is (not) estimable
  last = x[nrow(x), ]
  inest = (last["time"] < cutoff) && (last["n_event"] == 0)
  
  if (!inest) return(invisible(FALSE))
  if (inest_action == "extend") return(invisible(TRUE))
  
  msg = if (is.null(group_label)) {
    sprintf(
      "Kaplan-Meier estimator not uniquely defined beyond %s\n(`cutoff` specified at %s)",
      last["time"], cutoff
    )
  } else {
    sprintf(
      "Kaplan-Meier estimator for group '%s' not uniquely defined beyond %s\n(`cutoff` specified at %s)",
      group_label, last["time"], cutoff
    )
  }
  
  if (inest_action == "error") {
    stop(msg, call. = FALSE)
  } else if (inest_action == "extend_warn") {
    warning(msg, call. = FALSE)
    return(invisible(TRUE))
  }
}


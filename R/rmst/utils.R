box::use(
  data.table[as.data.table],
  survival[Surv]
)



#' Check if more than two vectors are of the same length
#' @export
same_length = function(...) {
  x = vapply(list(...), length, integer(1))
  length(unique(x)) == 1
}


#' Get strata labels
#' 
#' @description
#' When using `survfit(Surv(time, event) ~ trt)`, the treatment strata are saved/labelled 
#' with the pattern "trt=value".
#' This function extracts the "values" from these patterns
#' 
#' @param x (`survfit`)\cr
#'   A `survfit` object.
#' 
#' @export
strata_labels = function(x) {
  if (is.null(x$strata)) {
    return(NULL)
  } else {
    out = strsplit(names(x$strata), split = "=") |>
      vapply(\(x) x[[2]], character(1))
    return(out)
  }
}


#' Check if the RMST is estimable for a single sample
#' 
#' @description
#' If the cutoff value is larger than the latest event time and that event time is censored 
#' and not observed, then the Kaplan-Meier estimator is not uniquely defined up to the cutoff 
#' and therefore the RMST is actually not estimable.
#' This function performs such a check.
#' 
#' @param dt_km (`data.table`)\cr
#'   A data.table with organized results from Kaplan-Meier estimation for a single stratum.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param handler (`character(1)`)\cr
#'   One out of `c("error", "warning", "ignore")`.
#'   
#' @export
is_estimable = function(dt_km, cutoff) {
  dt_km[.N, !((time < cutoff) && (n_event == 0))]
}

#' @export
inest_handler = function(dt_km, cutoff, handler = "error") {
  if (handler == "ignore") {
    return(invisible(NULL))
  }
  
  estimable = is_estimable(dt_km, cutoff)
  
  if (estimable) {
    return(invisible(NULL))
  } else {
    x = dt_km[.N]
    msg = sprintf(
      "RMST not uniquely estimable%s%s beyond time point %s\n(cutoff specified at %s)",
      if (!is.null(x$group)) " for group " else "", if (!is.null(x$group)) x$group else "",
      x$time, cutoff
    )
    switch(handler,
      "warning" = warning(msg, call. = FALSE),
      "error" = stop(msg, call. = FALSE)
    )
  }
}


#' Safely obtain survival data from a formula as a data.table
#' @export
get_surv_data = function(formula, data = environment(formula)) {
  y = as.matrix(eval(formula[[2]], envir = data))
  x = eval(formula[[3]], envir = data)
  
  out = cbind(
    as.data.table(y),
    as.data.table(x)
  )
  
  out
}

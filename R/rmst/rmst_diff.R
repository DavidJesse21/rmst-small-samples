box::use(
  chk = checkmate,
  data.table[data.table, setDT, setnames]
)

box::use(
  ./rmst[rmst],
  ./utils[same_length]
)

#' Calculate the difference in restricted mean survival time (RMST) between 
#' two groups in a randomized trial.
#' 
#' @param time (`numeric()`)\cr
#'   A numeric vector of the event times.
#' @param status (`numeric()`)\cr
#'   A numeric vector of the event indicators.
#'   Entries must be either 1 (event) or 0 (no event, censored).
#'   Must be the of the same length as `status`.
#' @param group (`any`)\cr
#'   A vector of group/treatment indicators.
#'   Can be of any type but must match the length of `time` and `status` and
#'   must contain exactly 2 unique values.
#' @param data (`data.frame()`)\cr
#'   A data.frame containing the `time` and `status` vectors.
#'   This argument is optional and can be set to `NULL` (default) if `time` and 
#'   `status` are supplied as vectors directly.
#' @param tau (`numeric(1)`)\cr
#'   A single number representing the restriction time.
#' @param var_bias_correct (`logical(1)`)\cr
#'   Whether to apply a bias correction to the estimator of the variance as proposed 
#'   by Kaplan & Meier.
#' @param contrast (`vector(length = 2)`)\cr
#'   A vector of length 2 indicating which difference contrast to calculate.
#'   E.g. if `contrast = c("trt", "ctrl")`, then the RMST difference between 
#'   treatment and control will be calculated and vice versa.
#'   Must match the set of `unique(group)`.
#'   
#' @return (`numeric(2)`)\cr
#'   A numeric vector containing the estimate of the RMST difference (first entry) 
#'   and its associated estimate of the variance (2nd entry).
#'   
#' @references 
#' Hasegawa, Takahiro, Saori Misawa, Shintaro Nakagawa, Shinichi Tanaka, Takanori Tanase, Hiroyuki Ugai, Akira Wakana, et. al
#' „Restricted Mean Survival Time as a Summary Measure of Time-to-Event Outcome“.
#' Pharmaceutical Statistics 19, Nr. 4 (2020): 436–53.
#' https://doi.org/10.1002/pst.2004.
#' 
#' Kaplan EL, Meier P. Nonparametric estimation from incomplete observations.
#' J Am Stat Assoc. 1958;53:457-481.
#' 
#' @export
rmst_diff = function(time, status, group, data = NULL,
                     tau, var_bias_correct = FALSE,
                     contrast = unique(group)) {
  # Collect/organize data
  
  ## Case 1: Data provided via vectors
  if (is.null(data)) {
    invisible(lapply(list(time, status, group), chk$assert_atomic_vector))
    if (!same_length(time, status, group)) {
      stop("`time`, `status` and `group` must be of the same length.")
    }
    dt = data.table(
      time = time,
      status = status,
      group = group
    )
  } else {
    ## Case 2: `data` provided
    dt = eval(
      substitute(list(time, status, group)),
      env = data
    )
    setDT(dt)
    setnames(dt, new = c("time", "status", "group"))
  }
  
  ## Only 2 groups should be compared
  chk$assert_atomic_vector(unique(dt$group), len = 2)
  ## `contrast` should fulfil the same condition and match the groups/labels in the data
  if (!is.null(data)) {
    group = eval(substitute(group), env = data)
  }
  chk$assert_atomic_vector(contrast, len = 2, unique = TRUE)
  chk$assert_set_equal(contrast, unique(dt$group))
  
  
  # Split data by treatment and calculate RMST
  li_dt = split(dt, by = "group")
  li_rmst = lapply(li_dt, function(x) {
    rmst(
      time, status, data = x,
      tau = tau, var_bias_correct = var_bias_correct
    )
  })
  
  # Matrix of RMST output
  mat_rmst = do.call(rbind, li_rmst)
  rownames(mat_rmst) = names(li_rmst)
  ## Sort matrix by desired contrast
  mat_rmst = mat_rmst[contrast, ]
  
  # RMST difference
  out = numeric(2)
  names(out) = c("diff", "Var(diff)")
  out[1] = mat_rmst[1, 1] - mat_rmst[2, 1]
  out[2] = sum(mat_rmst[, 2])
  
  return(out)
}

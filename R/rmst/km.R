box::use(
  stats[pchisq, quantile],
  data.table[...],
  chk = checkmate
)

box::use(
  ./utils[get_surv_data, check_inest]
)


#' Kaplan-Meier based estimation of the restricted mean survival time (RMST) and the 
#' difference thereof
#' 
#' @param formula (`formula()`)\cr
#'   A formula object with a `Surv` object on the left-hand side and an intercept (`1`) or 
#'   a single grouping (e.g. treatment) variable on the right hand side.
#' @param data (`data.frame()` or `environment()`)\cr
#'   The data or environment containing the objects that appear in `formula`.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param var_method (`character(1)`)\cr
#'   One of  `c("greenwood", "kaplan_meier", "nelson_aalen", "none")` to choose between the variance estimation method.
#' @param inest_action (`character(1)`)\cr
#'   How to handle situations in which the RMST is not estimable.
#'   Must be one of `c("error", "extend", "extend_warn")`.
#'   See below for details
#' @param contrast (`character(2)`)\cr
#'   A vector indicating which difference in RMST should be estimated.
#'   E.g. `contrast = c("treatment", "control")` would imply to estimate \eqn{RMST_{trt} - RMST_{ctrl}}.
#' @param ... Ignored, for future extensions.
#'
#' @return (`numeric(2)` or `matrix(ncol = 2)`)\cr
#'   `rmst()`\cr
#'   Depending on the term on the right-hand side of `formula` either a numeric vector with 
#'   the estimate of the RMST and its standard error or a matrix with the number of rows 
#'   depending on the number of unique groups (e.g. treatments).\cr
#'   `rmst_diff()`\cr
#'   Numeric vector with estimate of the difference and associated standard error.
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
#' @section Inestimable cases:
#' If the restriction time / cutoff value is larger than the latest event time and that event time is 
#' censored (and not observed), then the Kaplan-Meier estimate is not uniquely defined up to the 
#' specified restriction time, which in turn means that the RMST is not (uniquely) estimable.
#' The `inest_action` argument controls how to handle such situations.
#' If it is set to `"error"` the function will throw an error and stop the execution.
#' If it is set to `"extend"` the last (censored) event time will be extended up to the 
#' specified restriction time.
#' `"extend_warn"` does the same but in addition generates a warning message.
#' 
#' @name rmst
NULL


#' @rdname rmst
#' @export
rmst = function(formula, data = environment(formula),
                cutoff,
                var_method = "greenwood",
                inest_action = "error",
                ...) {
  # Safety checks
  chk$assert_formula(formula)
  if (!(chk$test_data_frame(data) || chk$test_environment(data))) {
    stop("`data` must be a data.frame or an environment containing the ",
         "vectors appearing in the `formula` object.")
  }
  chk$assert_number(cutoff, lower = 0)
  chk$assert_choice(var_method, choices = c("greenwood", "kaplan_meier", "nelson_aalen", "none"))
  chk$assert_choice(inest_action, choices = c("error", "extend", "extend_warn"))
  
  # Data
  dt = get_surv_data(formula, data = data)
  
  # Calculate RMST
  if (ncol(dt) > 2) {
    li_dt = split(dt, by = colnames(dt)[3])
    li_rmst = lapply(li_dt, function(x) {
      km = fsurvfit(x[[1]], x[[2]])
      check_inest(km, cutoff, inest_action, group_label = unique(x[[3]]))
      .rmst(km, cutoff, var_method)
    })
    out = do.call(rbind, li_rmst)
  } else {
    km = fsurvfit(dt[[1]], dt[[2]])
    check_inest(km, cutoff, inest_action)
    out = .rmst(km, cutoff, var_method)
  }

  return(out)
}


#' @rdname rmst
#' @export
rmst_diff = function(formula, data = environment(formula),
                     cutoff,
                     contrast,
                     var_method = "greenwood",
                     inest_action = "error",
                     ...) {
  # Kaplan-Meier RMST estimation
  mat_rmst = rmst(formula, data, cutoff, var_method, inest_action, ...)
  
  # Check if specification of formula is valid
  if (!is.matrix(mat_rmst) && (nrow(mat_rmst) != 2)) {
    stop("`formula` must contain a group variable on the right hand side with 2 levels only.")
  }
  
  # `contrast` must be specified and valid
  chk$assert_set_equal(contrast, rownames(mat_rmst))
  chk$assert_character(contrast, len = 2L)
  
  # Sort and calculate/estimate RMST difference
  mat_rmst = mat_rmst[contrast, ]
  out = c(diff = NA_real_, var_diff = NA_real_)
  out[1] = mat_rmst[1, 1] - mat_rmst[2, 1]
  out[2] = sum(mat_rmst[, 2])
  
  # Output
  return(out)
}


#' @rdname rmst
#' @export
rmst_diff_test = function(formula, data = environment(formula),
                          cutoff,
                          contrast,
                          var_method = "greenwood",
                          inest_action = "error",
                          ...) {
  x = rmst_diff(formula, data, cutoff, contrast, var_method, inest_action, ...)
  y = c(tstat = NA_real_, pval = NA_real_)
  y[1] = unname(x[1] / sqrt(x[2]))
  y[2] = unname(1 - pchisq(y[1]^2, df = 1))
  
  c(x, y)
}


#' Inference on the difference in restricted mean survival time based on studentized permutation
#' 
#' @inheritParams rmst
#' @param inest_action_asy (`character(1)`)\cr
#'   See `inest_action` argument in function `rmst()`.
#' @param num_samples (`numeric(1)`)\cr
#'   The number of permutations.
#' @param conf_level (`numeric(1)`)\cr
#'   The nominal confidence level for the permutation based confidence interval.
#' @param light (`logical(1)`)\cr
#'   Whether to discard or return the permutation samples.
#'
#' @references
#' Ditzhaus, Marc, Menggang Yu, and Jin Xu.
#' „Studentized Permutation Method for Comparing Two Restricted Mean Survival Times with Small Sample from Randomized Trials“.
#' Statistics in Medicine 42, Nr. 13 (2023): 2226–40. https://doi.org/10.1002/sim.9720.   
#' 
#' @note
#' Code for this function has been provided by Marc Ditzhaus.
#' 
#' @export
rmst_diff_studperm = function(formula, data = environment(formula),
                              cutoff,
                              contrast,
                              var_method = "nelson_aalen",
                              inest_action_asy = "error",
                              num_samples = 1000L,
                              conf_level = 0.95,
                              light = TRUE,
                              ...) {
  # Safety checks
  chk$assert_number(num_samples, lower = 1L)
  chk$assert_number(conf_level, lower = 0L, upper = 1L)
  chk$assert_flag(light)
  
  # Asymptotic results
  res_asy = rmst_diff_test(formula, data, cutoff, contrast, var_method, inest_action_asy, ...)
  
  # Obtain ordered survival data
  # Note:
  # The treatment column must be numeric then
  # The function might need to be refined later
  x = as.matrix(get_surv_data(formula, data = data))
  x = x[order(x[, 1]), ]

  
  # Permutation samples
  # Note:
  # The treatment column must not be a factor
  # The code can be refined later.
  group_shuffled = vapply(seq_len(num_samples), \(i) sample(x[, 3]), numeric(nrow(x)))
  
  # return(group_shuffled)
  
  # Permutation results
  perm = t(apply(
    group_shuffled, MARGIN = 2L,
    # Basically just what `rmst_diff()` does
    function(shuffled) {
      x[, 3] = shuffled
      
      mat_rmst = lapply(contrast, function(group) {
        xi = x[x[, 3] == as.numeric(group), ]
        km = fsurvfit(xi[, 1], xi[, 2])
        .rmst(km, cutoff, var_method, ...)
      })
      mat_rmst = do.call(rbind, mat_rmst)
      rownames(mat_rmst) = contrast
      
      out = c(diff = NA_real_, var_diff = NA_real_)
      out[1] = mat_rmst[1, 1] - mat_rmst[2, 1]
      out[2] = sum(mat_rmst[, 2])
      
      return(out)
    }
  ))
  
  # Analyse data based on permutations
  perm_suared_tstat = perm[, 1]^2 / perm[, 2]
  pval = mean(res_asy["tstat"]^2 <= perm_suared_tstat, na.rm = TRUE)
  null_quant = quantile(sqrt(perm_suared_tstat), probs = conf_level)
  
  res_perm = list(
    pval = pval,
    confint = unname(c(
      res_asy["diff"] - null_quant * sqrt(res_asy["var_diff"]),
      res_asy["diff"] + null_quant * sqrt(res_asy["var_diff"])
    )),
    permutations = if (light) NULL else perm
  )
  
  # Final output 
  out = list(
    asymptotic = res_asy,
    permutation = res_perm
  )
  
  return(out)
}


#' Internal function for calculating RMST for a single samples
#' 
#' @param x (`matrix()`)\cr
#'   A Kaplan-Meier table returned by `fsurvfit()`.
#'   
#' @export
.rmst = function(x, cutoff, var_method, ...) {
  # Subset Kaplan-Meier table to relevant time horizon
  x = x[x[, "time"] <= cutoff, ]
  
  # Point estimation
  time_diffs = diff(c(0, x[, "time"], cutoff))
  areas = time_diffs * c(1, x[, "surv"])
  rmst_mu = sum(areas)
  
  # Variance estimation
  rmst_var = switch(var_method,
    "greenwood" = {
      t1 = cumsum(rev(areas[-1]))^2
      t2 = fifelse(
        (x[, "n_risk"] - x[, "n_event"]) == 0, 0,
        x[, "n_event"] / (x[, "n_risk"] * (x[, "n_risk"] - x[, "n_event"]))
      )
      sum(t1 * rev(t2))
    },
    "kaplan_meier" = {
      t1 = cumsum(rev(areas[-1]))^2
      t2 = fifelse(
        (x[, "n_risk"] - x[, "n_event"]) == 0, 0,
        x[, "n_event"] / (x[, "n_risk"] * (x[, "n_risk"] - x[, "n_event"]))
      )
      var_greenwood = sum(t1 * rev(t2))
      n_event_all = sum(x[, "n_event"])
      (n_event_all / (n_event_all - 1)) * var_greenwood
    },
    "nelson_aalen" = {
      t1 = cumsum(rev(areas[-1]))^2
      t2 = x[, "n_event"] / x[, "n_risk"]^2
      sum(t1 * rev(t2))
    }
  )
  
  # Output
  out = c("rmst" = rmst_mu, "var_rmst" = rmst_var)
  return(out)
}


#' Simple and fast function for calculating Kaplan-Meier estimates
#' 
#' @param time (`numeric()`)\cr
#'   Vector of survival times.
#' @param event (`numeric()`)\cr
#'   Vector of event indicators. Entries should only be either 0 (no event) or 1 (event).
#'
#' @returns (`matrix()`)\cr
#'   A matrix of the Kaplan-Meier survival quantities (time, survival, number at risk, number of events).
#'   
#' @note
#' This code has originally been developed by Marc Ditzhaus (`simple_surv()` function).
#' 
#' @export
fsurvfit = function(time, event) {
  n_all = length(time)
  
  tab = table(time, event)
  
  n_event = tab[, 2]
  n_cens = tab[, 1]
  n_risk = c(n_all, n_all - cumsum(n_event) - cumsum(n_cens))
  n_risk = n_risk[- length(n_risk)]
  
  surv = cumprod(1 - (n_event / n_risk))
  
  out = matrix(
    c(as.numeric(rownames(tab)), surv, n_risk, n_event),
    ncol = 4, dimnames = list(NULL, c("time", "surv", "n_risk", "n_event"))
  )
  
  return(out)
}


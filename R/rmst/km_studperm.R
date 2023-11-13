box::use(
  ./km[rmst_diff, rmst_diff_test]
)

box::use(
  data.table[...],
  stats[get_all_vars, quantile],
  chk = checkmate
)


#' Inference on the difference in restricted mean survival time based on studentized permutation
#' 
#' @param formula,data,cutoff,contrast,var_method See `rmst_diff()`.
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
                              num_samples = 1000L,
                              conf_level = 0.95,
                              light = TRUE) {
  # Some safety checks
  chk$assert_number(num_samples, lower = 1L)
  chk$assert_number(conf_level, lower = 0L, upper = 1L)
  chk$assert_flag(light)
  
  # Asymptotic results
  res_asy = rmst_diff_test(formula, data, cutoff, contrast, var_method)
  
  # Obtain original survival data ordered by survival times
  dt = get_all_vars(formula, data = data)
  setDT(dt)
  dt = dt[order(dt[[1L]])]
  
  # Permutation samples and results
  shuffled_group = vapply(1:num_samples, \(i) sample(dt[[3L]]), numeric(nrow(dt)))
  
  perm = t(apply(
    shuffled_group, MARGIN = 2L,
    function(shuffled) {
      dt[[3L]] = shuffled
      rmst_diff(formula, dt, cutoff, contrast, var_method)
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

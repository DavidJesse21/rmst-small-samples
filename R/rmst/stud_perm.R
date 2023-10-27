box::use(
  data.table[...],
  data.table.extras[mapvalues],
  stats[get_all_vars, quantile],
  survival[Surv]
)

box::use(
  ./km[rmst, rmst_diff]
)



#' Permute the group or treatment indicator
#' 
#' @param mat (`matrix()`)\cr
#'  A matrix containing the survival data.
#' @param group_col (`character(1)` or `numeric(1)`)\cr
#'   A string or integerish value indicating the column containing the group/treatment 
#'   indicator in the data.
#'   
#' @returns (`matrix()`)\cr
#'   A matrix of the same dimension as `mat` but with `group_col` being randomly shuffled.
shuffle_data = function(mat, group_col) {
  x = mat[, group_col]
  x = sample(x)
  mat[, group_col] = x
  return(mat)
}


#' One permutation run
#' 
#' @param x (`matrix()`)\cr
#'   The original survival data as a matrix.
#' @param cutoff (`numeric(1)`)\cr
#'   The restriction time.
#' @param contrast (`character(2)`)\cr
#'   A vector indicating which difference in RMST should be estimated.
#'   E.g. `contrast = c("treatment", "control")` would imply to estimate \eqn{RMST_{trt} - RMST_{ctrl}}.
#' @param var_method (`character(1)`)\cr
#'   One of  `c("greenwood", "kaplan_meier", "nelson_aalen")` to choose between the variance estimation method.
#'
#' @returns (`numeric(1)`)\cr
#'   The RMSTD test statistic for the permuted data.
one_permutation = function(x, cutoff, contrast, var_method) {
  new_x = shuffle_data(x, group_col = 3)
  rmstd = rmst_diff(
    Surv(new_x[, 1], new_x[, 2]) ~ new_x[, 3],
    cutoff = cutoff, var_method = var_method, contrast = contrast
  )
  out = unname(rmstd[1] / rmstd[2])
  return(out)
}


#' Main function
#' 
#' @param formula,data,contrast,var_method See `rmst_diff()`.
#' @param num_samples (`numeric(1)`)\cr
#'   The number of permutations.
#' @param conf_level (`numeric(1)`)\cr
#'   The confidence level for the test (and confidence interval).
#' @param light (`logical(1)`)\cr
#'   `FALSE` if the test statistics from the permutations should be returned 
#'   and `TRUE` otherwise (not implemented yet).
#'   
#' @returns (`list()`)\cr
#'   TBD.
#' 
#' @references
#' Ditzhaus, Marc, Menggang Yu, and Jin Xu. 
#' „Studentized Permutation Method for Comparing Two Restricted Mean Survival Times with Small Sample from Randomized Trials“. 
#' Statistics in Medicine 42, Nr. 13 (2023): 2226–40. 
#' https://doi.org/10.1002/sim.9720.
#' 
#' @export
rmst_diff_stud_perm = function(formula, data = environment(formula),
                               cutoff,
                               contrast,
                               num_samples = 1000,
                               conf_level = 0.95,
                               var_method = "nelson_aalen",
                               light = TRUE) {
  # Obtain survival data as a matrix and obtain new group/contrast vector
  survmat = get_all_vars(formula, data = data)
  trt = c(0L, 1L)
  names(trt) = unique(survmat[, 3])
  survmat[, 3] = mapvalues(survmat[, 3], from = names(trt), to = trt)
  survmat = as.matrix(survmat)
  contrast2 = as.character(trt[contrast])
  
  # Original (asymptotic) estimates
  rmstd_orig = rmst_diff(
    Surv(survmat[, 1], survmat[, 2]) ~ survmat[, 3],
    cutoff = cutoff, var_method = var_method, contrast = contrast2
  )
  tstat_orig = unname(rmstd_orig[1] / rmstd_orig[2])
  
  # Permutation replications
  tstat_perm = replicate(num_samples, {
    one_permutation(survmat, cutoff, contrast2, var_method)
  })
  
  out = list(
    "diff" = unname(rmstd_orig["diff"]),
    "se(diff)" = unname(rmstd_orig["se(diff)"]),
    "tstat" = tstat_orig,
    "null_quantile" = unname(quantile(abs(tstat_perm), probs = conf_level)),
    "pval" = mean(abs(tstat_orig) <= abs(tstat_perm), na.rm = TRUE)
  )
  
  return(out)
}


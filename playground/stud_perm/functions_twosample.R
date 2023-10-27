#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



#wrapper for RMST, two groups in data
#Input:
# values    - matrix created by dat_gen, sorted by sort_data and with added 
#             columns by KME. Should contain several groups
# alpha     - numeric value between 0 and 1, alpha level
# group:    - integer vector containing the group of the observations. Default
#             is the third column of the values, the groups drawn by data_gen
#Output:    Standard deviation estimated with interval formula for each group

test_stat_twos <- function(values, group, tau, delta) {
  rmst <- numeric(2)
  var <- numeric(2)
  
  for(i in 1:2) {
    values2 <- values[group == i,]
    #calculate for each group
    temp <- RMST(values2$time, values2$status, tau)
    rmst[i] <- temp["rmst"]
    var[i] <- temp["var_rmst"]
  }
  rmst_diff <- rmst[1] - rmst[2]
  var_diff <-  (var[1] + var[2])   # the factor n is eliminated by the other factors
  # appearing in the formula for the test statistic and
  # the factor n_i in the variance formula
  rmst_rat_log <- log(rmst[1]) - log(rmst[2])
  var_rat_log <- var[1]/rmst[1]^2 + var[2]/rmst[2]^2
  
  out <- c( wts = (rmst_diff - delta)^2/var_diff,
            var = var_diff,
            rmst_diff = rmst_diff,
            wts_rat = abs(rmst_rat_log)/sqrt(var_rat_log),
            rmst_rat_log = rmst_rat_log,
            var_rat_log = var_rat_log
    )
  return( out )    
}


#Permutations
#Input:
# values    - Matrix, Data to be entered in Simulation
# n_perm    - Integer, Number of permutations
# t_mat     - matrix, test matrix
#Output
# Vector with the quantiles of the test-statistics and number of permutations
# 

perm_fun <- function(values, n_perm, tau){
  values2 <- sort_data(values)
  group_org <- values2[, 3]
  group_new <- replicate(n_perm, sample(group_org))
  
  test_stat_erg <- matrix(apply(group_new, 2, function(x) test_stat_twos(values = values2, group = x, tau = tau, delta = 0)), nrow = 6)
  
  q <- quantile(test_stat_erg[1,], 0.95, na.rm = TRUE)
  q_rat <- quantile(test_stat_erg[4,], 0.95, na.rm = TRUE)
  
return(list(q = q, q_rat = q_rat, test_stat_erg = test_stat_erg ) )   
}


#wrapper fuer testoutput
sim_test_twos <- function(values0, n_perm, group_s, delta0, tau, chi_quan_sq) {
  estimable <- values0[["estimable"]]
  values <- sort_data(values0[["data"]])
  #values = sort_data(do.call(fun, args = list(n_vec = group_s,  censp = censp_s)))
  erg_0 <- test_stat_twos(values, values$group, tau, delta = 0)
  erg_stat0 <- unname(erg_0["wts"])
  erg_delta<- test_stat_twos(values, values$group, tau, delta = delta0)
  erg_stat_delta <- unname(erg_delta["wts"])
  erg_perm <- perm_fun(values, n_perm, tau)
  
  q_perm <- erg_perm$test_stat_erg[1,]
  q_rat <- erg_perm$test_stat_erg[2,]
  #p-values
  t0_perm <- mean(erg_stat0 <= q_perm, na.rm = TRUE) #test for H0: mu1 - mu2 = 0
  t0_chi <- 1 - pchisq(erg_stat0, df = 1 ) #test for H0: mu1 - mu2 = 0
  tdelta_perm <- mean(erg_stat_delta <= q_perm, na.rm = TRUE) # test for H0: mu1 - mu2 = delta, this can be used later to judge whether delta lies in the confidence interval
  tdelta_chi <- 1 - pchisq(erg_stat_delta, df = 1 ) # test for H0: mu1 - mu2 = delta
  
  CI_lower_perm <- unname( erg_0["rmst_diff"] - sqrt(erg_0["var"]) * sqrt(erg_perm$q) )
  CI_upper_perm <- unname( erg_0["rmst_diff"] + sqrt(erg_0["var"]) * sqrt(erg_perm$q) )
  CI_length_perm <- unname( 2 * sqrt(erg_0["var"]) * sqrt(erg_perm$q) )
  
  CI_lower_asy <- unname( erg_0["rmst_diff"] - sqrt(erg_0["var"]) * chi_quan_sq )
  CI_upper_asy <- unname( erg_0["rmst_diff"] + sqrt(erg_0["var"]) * chi_quan_sq )
  CI_length_asy <- unname( 2 * sqrt(erg_0["var"]) * chi_quan_sq )
  
  CI_lower_perm_rat <- exp( unname( erg_0["rmst_rat_log"] - sqrt(erg_0["var_rat_log"]) * erg_perm$q_rat ) )
  CI_upper_perm_rat <- exp(unname( erg_0["rmst_rat_log"] + sqrt(erg_0["var_rat_log"]) * erg_perm$q_rat ))
  CI_length_perm_rat <- CI_upper_perm_rat - CI_lower_perm_rat
  
  CI_lower_asy_rat <- exp(unname( erg_0["rmst_rat_log"] - sqrt(erg_0["var_rat_log"]) * chi_quan_sq ))
  CI_upper_asy_rat <- exp(unname( erg_0["rmst_rat_log"] + sqrt(erg_0["var_rat_log"]) * chi_quan_sq ))
  CI_length_asy_rat <- CI_upper_asy_rat - CI_lower_asy_rat
  
  # Method of Horiguchi and Uno
  tau_data <- min(sapply(1:length(group_s), function(i) max(values$time[(values$group == i) & (values$status == 1)])))
  if(estimable){
    alt_meth <- rmst2perm(time = values$time, status = values$status, arm = values$group - 1 , tau= tau, mperm=c(2), nperm = n_perm)
      
    Hor_asy <- alt_meth$asymptotic_test_pval
    Hor_perm <- alt_meth$permutation_test_method2_pval
  }else{
    Hor_asy <- NA
    Hor_perm <- NA
  }
  # Method of Tian et al. ATTENTION: The function only works when the 
  #truncation time, tau, is shorter than or equal to 
  #the minimum of the largest observed time on each of the two 
  #groups
  
  tian <- tryCatch(
    {
      RMST1 <- surv2sample(time = values$time, status = values$status, arm = values$group -1 ,  npert = n_perm,
                           timepoints = tau, # when to calculate difference and ratio
                           tau =tau # restricted timepoint
      )$contrast.diff01
      tian_0 <- RMST1[1,4]
      tian_delta <-  (RMST1[1,2] - delta) * (RMST1[1,3] - delta) <= 0 # = TRUE if delta lies in the 95% confidence interval
      
      CI_length_tian <- unname(RMST1[1,3] - RMST1[1,2])
      CI_lower_tian <- unname(RMST1[1,2])
      CI_upper_tian <- unname(RMST1[1,3])
      
      c(tian_0, tian_delta, CI_length_tian , CI_lower_tian , CI_upper_tian)
    },
     error=function(cond) { # There is a warning when the file does not exist
      c(NA, NA, NA, NA, NA)
    }
  )
  # We multiply each entry corresponding to a test decision by 100 to get the empirical sizes and power values in %
  out <- c( test_asy = 100*(t0_chi <= 0.05), test_perm = 100*(t0_perm <= 0.05), test_delta_asy = 100*(tdelta_chi<= 0.05), test_delta_perm = 100*(tdelta_perm<= 0.05), Hor_asy = 100*(Hor_asy<= 0.05), Hor_perm = 100*(Hor_perm <= 0.05), tian_0 = 100*(tian[1] <= 0.05), tian_delta = 100*tian[2], tian_NA = is.na(tian[1]), estimable = estimable, CI_length_perm = CI_length_perm, CI_length_asy = CI_length_asy, CI_length_tian = unname(tian[3]))
  return(out)
}

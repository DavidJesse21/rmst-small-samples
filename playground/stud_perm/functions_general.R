#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#Sort object of data_gen
#Input: 
# values:   matrix - created by data_gen

sort_data <- function(values) values[order(values[, 1]), ]

#create Matrix C, containing the groups
#Input:
# values:   matrix - created by data_gen, possibly sorted by sort_data
#Output:
# Matrix in which a column represents a group and a row an individual
create_c_mat <- function(values) {
  group <- values[, 3]
  c_mat <- matrix(0, nrow = max(group), ncol = nrow(values))
  
  c_mat[(0:(nrow(values) - 1)) * 3 + group] <- 1
  return(t(c_mat))
}

#Example:
#create_c_mat(sort_data(test_dat))

# function: simple_surv() determines basic survival quantities in a faster 
#           manner than survfit()
# input: time - survival time
#        cens - indicator for censoring (1=censored, 0=not censored)
# output: matrix of dimensions (n, 4) to speed up later calculations
simple_surv <- function(time, cens) {
  sur <- Surv(time, cens)
  n.all <- length(sur)
  tab <- table(sur[,1], factor(sur[,2], levels = c(0,1)))
  d <- tab[,1] # number of events in time points
  w <- tab[,2] # number of withdrawals in time points
  n <- c(n.all, n.all - cumsum(d) - cumsum(w)) # calculate risk set
  n <- n[-length(n)]
  s <- cumprod(1 - (w /  n)) # calculate Kaplan-Meier-Estimator
  matrix(c(as.numeric(row.names(tab)), s, w, n), ncol = 4)
}

# function: RMST() estimates Restricted Mean Survival Time and its variance 
#           for a sample of tupel (time, cens), given a time point tau
# input: time - survival time
#        cens - indicator for censoring (1=censored, 0=not censored)
#        tau - restriction time point
# output: rmst - estimated restricted mean survival time for tau
#         var_rmst - estimated variance of the rmst

RMST <- function(time, cens, tau){
  #n <- length(time) # number of observation in the beginning of study
  survtab <- simple_surv(time, cens) # fit convenient model for quantities
  
  t_i <- survtab[,1] <= tau # identify time points t_i <= tau
  t_sel <- survtab[,1][t_i] # select relevent time points
  S_km <- survtab[,2][t_i] # calculate Kaplan-Meier estimator
  
  w.factor <- diff(c(0, t_sel,tau)) # width of the area under S(t) from t_0=0
  rmst <- sum(w.factor * c(1, S_km)) # calculate area under S(t) up to t_i=tau
  
  area <- rev(cumsum(rev(diff(c(t_sel,tau)) * S_km)))^2 # calculate areas under S(t) from t_i to tau for
  d <- survtab[,3][t_i] # determine number of events
  Y <- survtab[,4][t_i] # determine number of individuals under risk
  var_rmst <- sum(( d/Y^2) * area) # calculate final variance without 
                                          # the factor n_i, because this factor 
                                          # is eliminated by the factor in front 
                                          # of the variances (i.e. n/n_i) and in 
                                          # front of the test statistic (i.e. n)
  c(rmst = rmst, var_rmst = var_rmst)
}



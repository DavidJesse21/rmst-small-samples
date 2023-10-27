setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###################################
########### Data ################
###############################


#load("Ost_IPD_Guyot.Rdata")  # not interesting
load("Hellmann.Rdata")
#load("Saba2.Rdata")

############################################
########### Packages + own Code ###########
#####################################

source("functions_general.R")
source("functions_twosample.R")
library(surv2sampleComp)
library(survRM2perm)
library(survminer)
library(survival)

#######################################################
######### Modify the data from Antoniyas Code############
########################################

# for examples of Antoniya
if( "arm" %in% names(data) ){
  ind <- which( names(data) == "arm")
  names(data)[ind] <- "group"
}


#############################################
########### Nice Plots ##################
###################################

fit <- survfit(Surv(time, event) ~ group, data = data)
g <- ggsurvplot(fit, data = data,  linetype = "strata", risk.table = TRUE, 
           legend.title = "Treatment", legend.labs = c("Chemotherapy","Nivolumab+ipilimumab"), ggtheme = theme_bw(base_size = 20),
           palette = c("black", "black"), xlab = "Time in Months", censor.shape = 4, censor.size = 3)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ggsave("data_example_cens.eps", print(g), width = 30, height = 20.5, units = "cm") # Save the file 


###############################################################
############## Tests for S1 = S2 ##############################
##############################################################

head(data)

#### Log-Rank ###
library(survival)
logRank = survdiff(Surv(time, event)~ group, rho = 0, 
                   #rho indicates classical log-rank test
                   data = data)$chisq
1 - pchisq(logRank,1)

library(KONPsurv)   


# Gorfine et al
set.seed(345)
KONP = konp_test(data$time, data$event, data$group, 
                 # as input wee need time, censoring indicator and group
                 n_perm = 2000) 
# and number of iterations for permutation
# The two implemented test statistics
KONP$pv_chisq
KONP$pv_lr
KONP$pv_cau

###### Ditzhaus and Friedrich test ####
set.seed(678)
library(mdir.logrank) 

mdir.logrank(data, cross = FALSE, #include crossing alternative 
             nperm = 2000, dig_p = 5) 

#######################################################
###################### RMST tests #####################
#####################################################


n_perm <- 5000
tau <- 18


###### UNstudentisierter Permutationstests

set.seed(01041990)
RMST2 <- rmst2perm(time = data$time, status = data$event, arm = data$group  , tau= tau, mperm=c(2), nperm = n_perm)
RMST2


##### Vorbereitung für meinen Test

values <- sort_data(data)
values$group <- values$group + 1
values$status <- values$event

# Test auswerten, erg_delta testet RMST1 - RMST2 = delta0
erg_0 <- test_stat_twos(values, values$group, tau, delta = 0)
erg_stat0 <- unname(erg_0["wts"])
erg_delta<- test_stat_twos(values, values$group, tau, delta = delta0)
erg_stat_delta <- unname(erg_delta["wts"])

set.seed(150918)
erg_perm <- perm_fun(values, n_perm, tau)

q_perm <- erg_perm$test_stat_erg[1,]
#p-values
t0_perm <- mean(erg_stat0 <= q_perm, na.rm = TRUE) # Permutation test for H0: mu1 - mu2 = 0
t0_perm
t0_chi <- 1 - pchisq(erg_stat0, df = 1 ) #Asymptotic test for H0: mu1 - mu2 = 0
tdelta_perm <- mean(erg_stat_delta <= q_perm, na.rm = TRUE) # permutation test for H0: mu1 - mu2 = delta, this can be used later to judge whether delta lies in the confidence interval
tdelta_chi <- 1 - pchisq(erg_stat_delta, df = 1 ) # asymptotic test for H0: mu1 - mu2 = delta

# Permutation-based confidence intervals for difference RMST1-RMST2
CI_lower_perm <- unname( erg_0["rmst_diff"] - sqrt(erg_0["var"]) * sqrt(erg_perm$q) )
CI_upper_perm <- unname( erg_0["rmst_diff"] + sqrt(erg_0["var"]) * sqrt(erg_perm$q) )
CI_length_perm <- unname( 2 * sqrt(erg_0["var"]) * sqrt(erg_perm$q) )
c(CI_lower_perm,CI_upper_perm)


#Asymptotic confidence intervals for difference RMST1-RMST2
chi_quan_sq <- sqrt(qchisq(0.95,df=1))

CI_lower_asy <- unname( erg_0["rmst_diff"] - sqrt(erg_0["var"]) * chi_quan_sq )
CI_upper_asy <- unname( erg_0["rmst_diff"] + sqrt(erg_0["var"]) * chi_quan_sq )
CI_length_asy <- unname( 2 * sqrt(erg_0["var"]) * chi_quan_sq )
c(CI_lower_asy,CI_upper_asy)

# Permutation-based confidence intervals for ratio RMST1/RMST2
CI_lower_perm_rat <- exp( unname( erg_0["rmst_rat_log"] - sqrt(erg_0["var_rat_log"]) * erg_perm$q_rat ) )
CI_upper_perm_rat <- exp(unname( erg_0["rmst_rat_log"] + sqrt(erg_0["var_rat_log"]) * erg_perm$q_rat ))
CI_length_perm_rat <- CI_upper_perm_rat - CI_lower_perm_rat
c(CI_lower_perm_rat,CI_upper_perm_rat)

# Asymptotic-based confidence intervals for ratio RMST1/RMST2
CI_lower_asy_rat <- exp(unname( erg_0["rmst_rat_log"] - sqrt(erg_0["var_rat_log"]) * chi_quan_sq ))
CI_upper_asy_rat <- exp(unname( erg_0["rmst_rat_log"] + sqrt(erg_0["var_rat_log"]) * chi_quan_sq ))
CI_length_asy_rat <- CI_upper_asy_rat - CI_lower_asy_rat
c(CI_lower_asy_rat,CI_upper_asy_rat)



### Zhou's empirical likelihood test
library(KMsurv)
library(emplik)

time_gr1 <- values$time[values$group == 1] 
time_gr2 <- values$time[values$group == 2] 
cens_gr1 <- values$status[values$group == 1] 
cens_gr2 <- values$status[values$group == 2] 

# lower and upper bound for the "nuisance" parameter, here the RMST of group1.
# To be on the save side, we take here the trival bounds (which are definitely to wide...)
lower <- 0
upper <- tau

# The code is copied from the appendix of Zhou (2021), to be more specific for their Example 2
# First difference
RMSTdiff <- function(r, x1, d1, x2, d2, theta, tau){
  temp1 <- el.cen.EM2(x = x1, d = d1, fun = function(x){pmin(x,tau )}, mu = r)
  temp2 <- el.cen.EM2(x = x2, d = d2, fun = function(x){pmin(x, tau)}, mu = r-theta)
  return(temp1$"-2LLR" + temp2$"-2LLR")
}

ThetafunD <- function(theta, x1, d1, x2, d2, tau, lower, upper) {
  temp <- optimize(f = RMSTdiff, lower = lower, upper = upper, x1 = x1, d1 = d1, x2 = x2, d2 = d2, theta = theta, tau = tau)
  cstar <- temp$minimum
  val <- temp$objective
  
  list("-2LLR" = val, cstar = cstar, Pval = 1-pchisq(val, df = 1))
  
}


ThetafunD(theta = 0, x1 = time_gr1, d1 = cens_gr1, x2 = time_gr2, d2 = cens_gr2, tau = tau, lower = lower, upper = upper )

CI_Zhou <- findUL(step = 0.2, fun = ThetafunD, MLE = erg_0["rmst_diff"], x1 = time_gr1, d1 = cens_gr1, x2 = time_gr2, d2 = cens_gr2, tau = tau, lower = lower, upper = upper)
CI_Zhou  



# Second ratio
RMSTratio <- function(r, x1, d1, x2, d2, theta, tau){
  temp1 <- el.cen.EM2(x = x1,d = d1,fun = function(x){pmin(x, tau)},mu = r*theta)
  temp2 <- el.cen.EM2(x = x2,d = d2,fun = function(x){pmin(x, tau)},mu = r)
  return(temp1$"-2LLR" + temp2$"-2LLR")
}

ThetafunR <- function(theta, x1, d1, x2, d2, tau, lower, upper) {
  temp <- optimize(f = RMSTratio, lower = lower, upper = upper, x1 = x1, d1 = d1, x2 = x2, d2 = d2, theta = theta, tau = tau)
  cstar <- temp$minimum
  val <- temp$objective
  list("-2LLR" = val, cstar = cstar, Pval = 1-pchisq(val, df = 1))
}

ThetafunR(theta = 1, x1 = time_gr1, d1 = cens_gr1, x2 = time_gr2, d2 = cens_gr2, tau = tau, lower = lower, upper = upper )


  CI_Zhou_ratio <- findUL(step = .2, fun = ThetafunR, MLE = exp(erg_0["rmst_rat_log"]), x1 = time_gr1, d1 = cens_gr1, x2 = time_gr2, d2 = cens_gr2, tau = tau, lower = lower, upper = upper)
  CI_Zhou_ratio

#############################################################################
################### Plot ##########################################
###################################################################


for( i in c(1,2)){
  
  data0 <- data
  data0 <- data0[ order(data0$time),]
  maxtime <- 25
  
  data1 <- data0[ data0$group == 0, ]
  data2 <- data0[ data0$group == 1, ]
  
  
  library(survival)
  km1 <- survfit(Surv(time, status) ~ 1, data = data1)
  KME1 <- cumprod(1-km1$n.event/km1$n.risk)
  d1 <- data.frame( time = c(0,unique(data1$time), maxtime), E = c(1,KME1, KME1[length(KME1)]), Treatment = "Prednisone" )
  
  km2 <- survfit(Surv(time, status) ~ 1, data = data2)
  KME2 <- cumprod(1-km2$n.event/km2$n.risk)
  d2 <- data.frame( time = c(0,unique(data2$time), maxtime), E = c(1,KME2, KME2[length(KME2)]), Treatment = "Placebo"  ) # Auf jeden Fall hier unique benutzen, denn es k?nnten Bindungen auftreten
  
  d <- rbind(d2,d1)
  #install.packages("ggplot2")
  library(tidyverse)
  library(ggplot2); theme_set(theme_bw())  # Mit dem theme_set ?ndert man den Hintergrund
  ggplot(aes(x=time,y=E, lty = Treatment),data=d) + theme(axis.text=element_text(size=15), plot.title = 
                                                            element_text(size= 16, hjust = 0.5), # hjust=... brauchen wir, damit der Titel in der Mitte ist. Sonst ist er links
                                                          axis.title=element_text(size=15), legend.position = c(.95, .90), # ,it legendpositioon kann man die Platzierumg der egende innerhalb des plots bestimmen, via x /y Koordinate zwischen 0 und 1
                                                          legend.justification = c("right", "top"),legend.background = element_rect( color= "black"),
                                                          ,
                                                          legend.box.just = "right",
                                                          legend.margin = margin(5, 5, 5,5)) +
    geom_step( )+
    scale_linetype_manual(values=c("dotted", "solid")) + coord_cartesian(xlim =c(x_low, x_up), ylim = c(0, 1)) +
    ylab("") + xlab("time") + ggtitle(title[i]) 
  
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  ggsave(dataname[i], width = 15, height = 10.5, units = "cm") # Save the file 
  
}

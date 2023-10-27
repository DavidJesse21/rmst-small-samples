box::use(
  fs,
  data.table[...]
)

.path = fs$path("playground", "stud_perm")

load(fs$path(.path, "Hellmann.Rdata"))
setDT(data)

# create_c_mat(), RMST(), simple_surv(), sort_data()
source(fs$path(.path, "functions_general", ext = "R"))
# perm_fun(), sim_test_twos(), test_stat_twos() 
source(fs$path(.path, "functions_twosample", ext = "R"))

library(survival)
library(survminer)

# Plots only
fit = survfit(Surv(time, event) ~ group, data = data)
p = ggsurvplot(
  fit, data = data, linetype = "strata", risk.table = TRUE,
  legend.title = "Treatment", legend.labs = c("Chemotherapy","Nivolumab+ipilimumab"),
  ggtheme = theme_bw(),
  palette = c("black", "black"), xlab = "Time in Months", censor.shape = 4, censor.size = 3
)
p


# Studentized permutation test ----

set.seed(42)
n_perm = 5000
tau = 18

# Simply sorts the time to event data 
values = sort_data(data)
# Just renaming / other kind of representation
values$group <- values$group + 1
values$status <- values$event

# Numeric vector
# Squared test statistics, point estimates, associated variances
erg_0 = test_stat_twos(values, values$group, tau, delta = 0)
# (Squared) test statistic
erg_stat0 <- unname(erg_0["wts"])
# Where is delta0?
# Only has impact on wts
erg_delta = test_stat_twos(values, values$group, tau, delta = 1)
erg_stat_delta = unname(erg_delta["wts"])

set.seed(150918)
values = values[, c(1, 4, 3)]
setDF(values)

erg_perm = perm_fun(values, n_perm, tau)
class(erg_perm)
names(erg_perm)

x = erg_perm$test_stat_erg
class(x)
dim(x)
# 6 rows, 5000 columns
# rows: test statistics etc. (see erg_0)
# columns: permutation replications
# Note: like before, returns squared test statistic
# -> which is why chi-square distribution is used in the following

q_perm = erg_perm$test_stat_erg[1,]
# p-values
## Permutation
t0_perm = mean(erg_stat0 <= q_perm, na.rm = TRUE)
t0_perm
## Asymptotic
t0_chi = 1 - pchisq(erg_stat0, df = 1)
2 * (1 - pnorm(sqrt(erg_stat0))) # equivalent

ci_perm = unname(c(
  erg_0["rmst_diff"] - sqrt(erg_0["var"]) * sqrt(erg_perm$q),
  erg_0["rmst_diff"] + sqrt(erg_0["var"]) * sqrt(erg_perm$q)
))

chi_quan_sq = sqrt(qchisq(0.95, df=1))
ci_asy = unname(c(
  erg_0["rmst_diff"] - sqrt(erg_0["var"]) * chi_quan_sq,
  erg_0["rmst_diff"] + sqrt(erg_0["var"]) * chi_quan_sq
))


# Unstudentized test
library(survRM2perm)
set.seed(01041990)

RMST2 = rmst2perm(
  data$time, data$event, data$group,
  tau = tau, mperm = 2, nperm = n_perm,
  asy = "aj"
)
class(RMST2)
names(RMST2)

RMST2




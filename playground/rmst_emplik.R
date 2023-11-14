options(box.path = "R")

box::use(
  fs,
  data.table[...],
  survival[Surv]
)

box::use(
  rmst/km[rmst_diff]
)

load(fs$path("data", "Hellmann", ext = "Rdata"))
setDT(data)
data

res_asy = rmst_diff(
  Surv(time, event) ~ group, data, cutoff = 18,
  contrast = c("0", "1"), var_method = "nelson_aalen"
)
res_asy

data = data[order(time)]

time_gr1 = data[group == 0, time]
time_gr2 = data[group == 1, time]
cens_gr1 = data[group == 0, event]
cens_gr2 = data[group == 1, event]

# lower and upper bound for the "nuisance" parameter, here the RMST of group1.
lower = 0
upper = 18
tau = 18

library(emplik)

RMSTdiff = function(r, x1, d1, x2, d2, theta, tau) {
  temp1 = el.cen.EM2(x = x1, d = d1, fun = \(x) pmin(x, tau), mu = r)
  temp2 = el.cen.EM2(x = x2, d = d2, fun = \(x) pmin(x, tau), mu = r - theta)
  #return(temp1[["-2LLR"]] + temp2["-2LLR"])
  return(temp1$"-2LLR" + temp2$"-2LLR")
}

ThetafunD = function(theta, x1, d1, x2, d2, tau, lower, upper) {
  temp = optimize(
    f = RMSTdiff,
    lower = lower, upper = upper,
    x1 = x1, d1 = d1, x2 = x2, d2 = d2, theta = theta, tau = tau
  )
  cstar = temp$minimum
  val = temp$objective
  
  list(`-2LLR` = val, cstar = cstar, pval = 1 - pchisq(val, df = 1))
}

ThetafunD(theta = 0, x1 = time_gr1, d1 = cens_gr1, x2 = time_gr2, d2 = cens_gr2, tau = tau, lower = lower, upper = upper )

# This really takes a lot of time
ci_zhou = findUL(step = 0.2, fun = ThetafunD, MLE = res_asy["diff"],
                 x1 = time_gr1, d1 = cens_gr1, x2 = time_gr2, d2 = cens_gr2, tau = tau, lower = lower, upper = upper)

options(box.path = "R")

# Packages
box::use(
  survival[Surv, survfit, survdiff, coxph, cox_zph = cox.zph],
  data.table[...],
  ggplot2[...],
  survminer[ggsurvplot]
)

# Custom functions
box::use(
  rmst/rmst[rmst],
  rmst/rmst_diff[rmst_diff]
)

box::help(rmst)
box::help(rmst_diff)


# Data
data(cancer, package = "survival")

?veteran
?colon
?ovarian

invisible(lapply(list(veteran, colon, ovarian), setDT))

ggsurvplot(survfit(Surv(time, status) ~ trt, data = veteran))
ggsurvplot(survfit(Surv(time, status) ~ rx, data = colon))
ggsurvplot(
  survfit(Surv(time, status) ~ rx, data = colon[rx %in% c("Obs", "Lev+5FU")]),
  data = colon[rx %in% c("Obs", "Lev+5FU")]
)
ggsurvplot(survfit(Surv(futime, fustat) ~ as.factor(rx), data = ovarian))


# Test functions (two-sided)
test_logrank = function(time, status, group, data) {
  x = substitute(Surv(time, status) ~ group)
  survdiff(eval(x), data = data)
}

test_rmst_diff = function(time, status, group, data, tau) {
  x = eval(substitute(
    rmst_diff(time, status, group, data = data, tau = tau)
  ))
  tstat = unname(x[1] / sqrt(x[2]))
  # p-values
  2 * (1 - pnorm(abs(tstat)))
}

# veteran
test_logrank(time, status, trt, data = veteran)
test_rmst_diff(time, status, trt, data = veteran, tau = 500)

# colon
test_logrank(
  time, status, rx,
  data = colon[rx %in% c("Obs", "Lev+5FU")][, rx := droplevels(rx)]
)
test_rmst_diff(
  time, status, rx,
  data = colon[rx %in% c("Obs", "Lev+5FU")][, rx := droplevels(rx)],
  tau = 2500
)

# ovarian
test_logrank(futime, fustat, as.factor(rx), data = ovarian)
test_rmst_diff(futime, fustat, as.factor(rx), data = ovarian, tau = 600)
test_rmst_diff(futime, fustat, as.factor(rx), data = ovarian, tau = 500)
test_rmst_diff(futime, fustat, as.factor(rx), data = ovarian, tau = 400)

ovarian[, .N]
# Note that for the RMST test the issue of inflated type I error might play 
# a role here, as the sample size is very small.

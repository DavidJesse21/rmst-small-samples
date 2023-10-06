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
  rmst/km[rmst, rmst_diff]
)
box::help(rmst)


# Data
data(cancer, package = "survival")
?veteran
?colon
?ovarian
invisible(lapply(list(veteran, colon, ovarian), setDT))

ggsurvplot(survfit(Surv(time, status) ~ trt, data = veteran))
ggsurvplot(survfit(Surv(time, status) ~ rx, data = colon))
ggsurvplot(
  survfit(Surv(time, status) ~ rx, data = colon[rx %in% c("Lev", "Lev+5FU")]),
  data = colon[rx %in% c("Lev", "Lev+5FU")]
)
ggsurvplot(survfit(Surv(futime, fustat) ~ as.factor(rx), data = ovarian))


# Test functions (two-sided)
test_logrank = function(formula, data) {
  survdiff(formula, data = data)
}

test_rmst_diff = function(formula, data = environment(formula), cutoff, contrast) {
  x = rmst_diff(formula, data, cutoff, contrast = contrast)
  tstat = unname(x[1] / x[2])
  # p-value
  2 * (1 - pnorm(abs(tstat)))
}

# test_rmst_diff = function(time, status, group, data, tau) {
#   x = eval(substitute(
#     rmst_diff(time, status, group, data = data, tau = tau)
#   ))
#   tstat = unname(x[1] / sqrt(x[2]))
#   # p-values
#   2 * (1 - pnorm(abs(tstat)))
# }

# veteran
test_logrank(Surv(time, status) ~ trt, data = veteran)
test_rmst_diff(
  Surv(time, status) ~ trt, data = veteran,
  cutoff = 500, contrast = c("2", "1")
)

# colon
test_logrank(
  Surv(time, status) ~ rx,
  data = colon[rx %in% c("Lev", "Lev+5FU")][, rx := droplevels(rx)]
)
test_rmst_diff(
  Surv(time, status) ~ rx,
  data = colon[rx %in% c("Lev", "Lev+5FU")][, rx := droplevels(rx)],
  cutoff = 2000, contrast = c("Lev", "Lev+5FU")
)

# ovarian
test_logrank(Surv(futime, fustat) ~ as.factor(rx), data = ovarian)
test_rmst_diff(
  Surv(futime, fustat) ~ as.factor(rx), data = ovarian,
  contrast = c("2", "1"), cutoff = 600
)
test_rmst_diff(
  Surv(futime, fustat) ~ as.factor(rx), data = ovarian,
  contrast = c("2", "1"), cutoff = 500
)
test_rmst_diff(
  Surv(futime, fustat) ~ as.factor(rx), data = ovarian,
  contrast = c("2", "1"), cutoff = 900
)


ovarian[, .N]
# Note that for the RMST test the issue of inflated type I error might play 
# a role here, as the sample size is very small.

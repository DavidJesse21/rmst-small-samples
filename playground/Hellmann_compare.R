options(box.path = "R")

box::use(
  fs,
  data.table[...],
  survival[Surv, survfit, pseudo],
  eventglm[rmeanglm]
)

box::use(
  rmst/km[rmst_diff_studperm],
  rmst/pseudo[pseudo_strat, pseudo_infjack, rmst_pseudo_test, boot_pseudo],
)


load(fs$path("data", "Hellmann", ext = "Rdata"))
dt = data
rm(data)
setDT(dt)
dt


# Studentized permutation test ----

#' 
#' Expected results:
#' su
#' - point estimate: -4.02
#' - p-value: 1.1%
#' - confidence interval: [-7.09, -0.96]

set.seed(150918)
x1 = rmst_diff_studperm(
  Surv(time, event) ~ group, data = dt,
  cutoff = 18, contrast = c("0", "1"), var_method = "nelson_aalen",
  num_samples = 5000L, light = FALSE
)
round(x1$asymptotic["pval"] * 100, 2)
round(x1$permutation$pval * 100, 2)
round(x1$permutation$confint, 2)



# Pseudo-observations, HC3 ----

m2 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 18,
  model.censoring = pseudo_strat, formula.censoring = ~ group
)
x2 = rmst_pseudo_test(m2)
round(x2, 2)
round(x2[2, "pval"] * 100, 2)


# Pseudo-observations infjack, HC3 ----

m3 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 18,
  model.censoring = pseudo_infjack, formula.censoring = ~ group
)
x31 = rmst_pseudo_test(m3)
round(x31, 2)
round(x31[2, "pval"] * 100, 2)


# Pseudo-observations infjack, bootstrap cov ----

set.seed(150918)
m3_boot = boot_pseudo(m3, 1000)
x32 = rmst_pseudo_test(m3_boot, vcov_type = "boot")
round(x32, 3)
round(x32[2, "pval"] * 100, 3)
vcov(m3, "robust")
var(m3_boot$boot)


# Bootstrap hypothesis testing ----

m3 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 18,
  model.censoring = pseudo_infjack, formula.censoring = ~ group
)

boot_tstat = function(m, num_samples = 2000) {
  dt = m$data
  beta = coef(m)
  
  boot = lapply(seq_len(num_samples), function(i) {
    new_dt = dt[sample(1:.N, replace = TRUE)]
    new_m = update(m, data = new_dt)
    # new_beta = coef(new_m)
    # new_sigma = vcov(new_m)
    # # This mimics the test statistic under H0
    # new_tstat = (new_beta - beta) / sqrt(diag(new_sigma))
    
    #return(new_tstat)
    new_m
  })
  
  #boot = do.call(rbind, boot)
  
  boot
}

set.seed(150918)
# 10 seconds (a little less)
system.time({
  m3_boot = boot_tstat(m3, 1000)
})


# Maybe this is faster
boot_tstat2 = function(m, num_samples = 2000) {
  dt = m$data
  pseudo_fun = eval(m$call$model.censoring)
  formula = m$formula
  formula.censoring = eval(m$call$formula.censoring)
  type = m$type
  time = m$time
  weights = unname(m$weights)
  if (!all(weights == 1L)) stop("`boot()` not available for weighted regression yet.")
  control = m$control
  family = m$family
  
  boot = lapply(seq_len(num_samples), function(i) {
    new_dt = dt[sample(1:.N, replace = TRUE)]
    new_y = pseudo_fun(formula, time, data = new_dt, type = type, formula.censoring = formula.censoring)
    new_x = model.matrix(formula, data = new_dt)
    new_m = glm.fit(new_x, new_y,
                    family = family,
                    mustart = rep(mean(new_y), length(new_y)),
                    intercept = TRUE, singular.ok = TRUE,
                    control = control)
    
    class(new_m) = c("pseudoglm", "glm", "lm")
    new_m$terms = terms(m)
    
    return(m)
    # ret_m = m
    # ret_m$coefficients = new_m$coefficients
    # ret_m$residuals = new_m$residuals
    # ret_m$fitted.values = new_
  })
}
# 1 second faster (better than nothing)

set.seed(150918)
# 10 seconds (a little less)
system.time({
  m3_boot = boot_tstat2(m3, 1000)
})

m3_boot[[1]] |> vcov()

set.seed(150918)
# 20 seconds (a little less)
system.time({
  m3_boot = boot_tstat(m3)
})

head(m3_boot)

hist(m3_boot[, 2], freq = FALSE)
curve(dnorm(x), from = -3, to = 3, col = 20, add = TRUE)

quantile(m3_boot[, 2], c(0.025, 0.975))
quantile(abs(m3_boot[, 2]), 0.95)


# Another dataset
dt = survival::ovarian
setDT(dt)
setnames(dt, old = c("futime", "fustat", "rx"), new = c("time", "event", "group"))
dt[, group := group - 1L]
plot(
  survfit(Surv(time, event) ~ group, data = dt), col = 19:20
)

# 
m3 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 450,
  model.censoring = pseudo_infjack, formula.censoring = ~ group
)
summary(m3)

set.seed(150918)
# 18 seconds (a little less)
system.time({
  m3_boot = boot_tstat(m3)
})

head(m3_boot)

# hist(m3_boot[, 2], freq = FALSE)
# curve(dnorm(x), from = -3, to = 3, col = 20, add = TRUE)

quantile(m3_boot[, 2], c(0.025, 0.975))
quantile(abs(m3_boot[, 2]), 0.95)

summary(m3)

mean(abs(m3_boot[, 2]) > 2.041)

mean(2.041 <= abs(m3_boot[, 2])) * 100

pval = mean(res_asy["tstat"]^2 <= perm_suared_tstat, na.rm = TRUE)

set.seed(150918)
x1 = rmst_diff_studperm(
  Surv(time, event) ~ group, data = dt,
  cutoff = 750, contrast = c("0", "1"), var_method = "nelson_aalen",
  num_samples = 2000L
)
round(x1$asymptotic["pval"] * 100, 2)
round(x1$permutation$pval * 100, 2)
round(x1$permutation$confint, 2)


x31 = rmst_pseudo_test(m3)
round(x31, 2)
round(x31[2, "pval"] * 100, 2)


# "Naive" wild bootstrap ----

set.seed(150918)

m2 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 18,
  model.censoring = pseudo_strat, formula.censoring = ~ group
)

boot_wild = function(m, num_samples = 1000, rand_mult = rnorm) {
  # Retrieve required objects from model object
  x = model.matrix(m)
  weights = unname(m$weights)
  if (!all(weights == 1L)) stop("`boot()` not available for weighted regression yet.")
  control = m$control
  family = m$family
  
  # Apply bootstrap
  lapply(
    seq_len(num_samples),
    function(i) {
      new_y = m$y + rand_mult(nobs(m)) * resid(m)
      glm.fit(x, new_y,
              family = family,
              mustart = rep(mean(new_y), length(new_y)),
              intercept = TRUE, singular.ok = TRUE,
              control = control)
    }
  )
}


boot_wild = function(m, num_samples = 1000, rand_mult = rnorm) {
  dt_orig = m$data
  y_orig = dt_orig$
  
  lapply(seq_len(num_samples)) {
    
  }
}


li_boot = boot_wild(m2, num_samples = 5000)

vcov(m2, type = "robust")
vcov(m2, type = "naive")

mat_boot = do.call(rbind, lapply(li_boot, \(m) m$coefficients))
var(mat_boot)


# This time with Rademacher
set.seed(150918)
li_boot = boot_wild(m2, num_samples = 5000, rand_mult = \(n) sample(c(-1, 1), n, replace = TRUE))
mat_boot = do.call(rbind, lapply(li_boot, \(m) m$coefficients))
var(mat_boot)

# This time with Poisson
set.seed(150918)
li_boot = boot_wild(m2, num_samples = 5000, rand_mult = \(n) rpois(n, 1))
mat_boot = do.call(rbind, lapply(li_boot, \(m) m$coefficients))
var(mat_boot)

vcov(m2)

# This time with Mammen
set.seed(150918)
li_boot = boot_wild(
  m2, num_samples = 5000, rand_mult = function(n) {
    x1 = - (sqrt(5) - 1)/2
    p1 = (sqrt(5) + 1) / (2 * sqrt(5))
    
    x2 = (sqrt(5) - 1)/2
    p2 = (sqrt(5) - 1) / (2 * sqrt(5))
    
    sample(c(x1, x2), n, replace = TRUE, prob = c(p1, p2))
  }
)
# This is the HC3 estimator
vcov(m2)
#
mat_boot = do.call(rbind, lapply(li_boot, \(m) m$coefficients))
var(mat_boot)



# Bootstrap hypothesis test ---- 

#' 1. Fit normal model, estimate beta (and test statistics)
#' 2. Non-parametric bootstrap resampling
#'    - refit regression model
#'    - estimate test statistic using beta from (1.) as the true population model
#'      - estimate standard error using HC3 covariance matrix estimator (rmst_pseudo_test)
#' 

dt = survival::ovarian
setDT(dt)
setnames(dt, old = c("futime", "fustat", "rx"), new = c("time", "event", "group"))
dt[, group := group - 1L]
plot(
  survfit(Surv(time, event) ~ group, data = dt), col = 19:20
)

set.seed(150918)

m3 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 450,
  model.censoring = pseudo_infjack, formula.censoring = ~ group
)



fboot = function(m, num_samples = 2000, strata = NULL) {
  # Retrieve original data
  dt_orig = m$data
  if (!is.null(strata)) {
    li_dt_orig = split(dt_orig, by = strata)
  }
  
  # Bootstrap
  boot = lapply(seq_len(num_samples), function(i) {
    # non-stratified resampling
    if (is.null(strata)) {
      boot_dt = dt_orig[sample(1:nrow(dt_orig), replace = TRUE)]
    } else {
      # stratified resampling
      boot_dt = lapply(li_dt_orig, function(dt) {
        dt[sample(1:nrow(dt), replace = TRUE)]
      }) |>
        rbindlist()
    }
    
    # Refit model
    boot_m = update(m, data = boot_dt)
    
    boot_m
  })
  
  boot
}

set.seed(150918)

system.time({
  x = fboot(m3, 2000)
})

x2 = lapply(x, function(m) {
  x = rmst_pseudo_test(m)[, -4]
  x[, "est"] = x[, "est"] - coef(m3)
  x[, "tstat"] = x[, "est"] / sqrt(x[, "var_est"])
  x
})
tstat_group = vapply(x2, \(x) x[2, "tstat"], numeric(1))
hist(tstat_group[tstat_group > -10], breaks = 20)
summary(tstat_group)
quantile(abs(tstat_group), 0.95)


## Second approach ----

set.seed(150918)

m3 = rmeanglm(
  Surv(time, event) ~ group, data = dt, time = 18,
  model.censoring = pseudo_infjack, formula.censoring = ~ group
)

fboot = function(m, num_samples = 1000) {
  
}

box::use(
  simfuns2/utils[estimate_runtime]
)
estimate_runtime(20)
estimate_runtime(19)
estimate_runtime(15)
estimate_runtime(20, parallel = list(num_cores = 4, prop = 1))

box::use(
  microbenchmark[microbenchmark]
)
box::use(rmst/pseudo[pseudo_infjack])
box::use(eventglm[pseudo_stratified])


bench = microbenchmark(
  eventglm = pseudo_stratified(
    Surv(time, event) ~ group, time = 18, data = dt,
    formula.censoring = ~ group, type = "rmean"
  ),
  infjack = pseudo_infjack(
    Surv(time, event) ~ group, time = 18, data = dt,
    formula.censoring = ~ group, type = "rmean"
  ),
  strat = pseudo_strat(
    Surv(time, event) ~ group, time = 18, data = dt,
    formula.censoring = ~ group, type = "rmean"
  ),
  times = 100
)
bench

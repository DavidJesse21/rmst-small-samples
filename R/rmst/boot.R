box::use(./km[rmst_diff])


rmst_diff_boot = function(formula, data = environment(formula),
                          cutoff,
                          contrast,
                          num_samples = 1000,
                          conf_level = 0.95) {
  
  # Calculate RMSTD based on original data
  rmstd_orig = rmst_diff(formula, data, cutoff, contrast)
  
  # Obtain / organize survival data
  dt = get_all_vars(formula, data = data)
  setDT(dt)
  setnames(dt, new = c("time", "status", "trt"))
  li_dt = split(dt, by = "trt")
  
  # Bootstrap samples
  boot_samples = replicate(num_samples, {
    dt_new = lapply(li_dt, function(.dt) {
      idx = sample(1:nrow(.dt), replace = TRUE)
      .dt[idx]
    })
    rbindlist(dt_new)
  }, simplify = FALSE)
  
  # RMSTD estimates
  rmstd_samples = lapply(boot_samples, function(.dt) {
    x = rmst_diff(
      Surv(time, status) ~ trt, data = .dt,
      cutoff = cutoff, contrast = contrast
    )
    tstat = x[1] / x[2]
    return(tstat)
  })

  out = list(rmstd_orig, rmstd_samples)
  
  return(out)
}

library(survival)
library(data.table)
library(data.table.extras)
head(veteran)

set.seed(42)

test = rmst_diff_boot(
  Surv(time, event) ~ group, data = data,
  cutoff = 18, contrast = c("0", "1"),
  num_samples = 5000
)

x = test[[1]]
c(
  x[1] - qnorm(0.975) * x[2],
  x[1] + qnorm(0.975) * x[2]
) |>
  round(2)

unlist(test[[2]]) |>
  abs() |>
  quantile(probs = 0.95)

test2 = rmst_diff(
  Surv(time, event) ~ group, data = data,
  cutoff = 18, contrast = c("0", "1"), var_method = "nelson_aalen"
)
c(
  test2[1] - qnorm(0.975) * test2[2],
  test2[1] + qnorm(0.975) * test2[2]
)


x = test[[1]]
c(
  x[1] - qnorm(0.975) * x[2],
  x[1] + qnorm(0.975) * x[2]
)


x = rmst_diff_boot(
  Surv(time, status) ~ trt, data = veteran,
  cutoff = 400, contrast = c("2", "1")
)

x
set.seed(42)             
y = lapply(x, function(x) {
  sample(1:nrow(x), replace = TRUE)
})

test = replicate(3, {
  out = lapply(x, function(u) {
    idx = sample(1:nrow(u), replace = TRUE)
    dt_sample = u[idx]
    return(dt_sample)
  })
  rbindlist(out)
}, simplify = FALSE)
test

y
lapply(y, table)

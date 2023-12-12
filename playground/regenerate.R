options(box.path = "R")

box::use(
  data.table[...]
)

box::use(
  simfuns/designs[make_prob_design],
  simfuns/problems[gen_data_exp, gen_data_pwexp, gen_data_weibull]
)

# Check if RMST contrast is estimable
is_estimable = function(dt, cutoff) {
  idx = dt[, .I[which.max(time)], by = trt]$V1
  check = !dt[idx, (time < cutoff) & (event == 0)]
  all(check)
}

des = make_prob_design()
des = lapply(des, \(x) x[1]) |>
  rbindlist()

data = list(cutoff = 10)

x = replicate(
  2000,
  gen_data_exp(
    data,
    samples_alloc = des[1, samples_alloc][[1]], samples_k = des[1, samples_k],
    params_surv = des[1, params_surv][[1]], params_cens = des[1, params_cens][[1]]
  ),
  simplify = FALSE
)
all(vapply(x, is_estimable, logical(1), cutoff = 10))

x = replicate(
  2000,
  gen_data_pwexp(
    data,
    samples_alloc = des[2, samples_alloc][[1]], samples_k = des[2, samples_k],
    params_surv = des[2, params_surv][[1]], params_cens = des[2, params_cens][[1]]
  ),
  simplify = FALSE
)
all(vapply(x, is_estimable, logical(1), cutoff = 10))

x = replicate(
  2000,
  gen_data_weibull(
    data,
    samples_alloc = des[3, samples_alloc][[1]], samples_k = des[3, samples_k],
    params_surv = des[3, params_surv][[1]], params_cens = des[3, params_cens][[1]]
  ),
  simplify = FALSE
)
all(vapply(x, is_estimable, logical(1), cutoff = 10))

# Everything seems to work as I expect it

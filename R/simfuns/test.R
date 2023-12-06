options(box.path = "R")
rm(list = ls())
box::purge_cache()

set.seed(42)

box::use(
  survival[Surv, survfit]
)

box::use(
  ./designs[make_prob_design],
  ./problems[gen_data_exp, gen_data_pwexp, gen_data_weibull]
)

# For a quick visual glance
plot_surv = function(dt, xmax = 11) {
  fit = survfit(Surv(time, event) ~ trt, data = dt, se.fit = FALSE)
  plot(fit, col = c("#E69F00", "#56B4E9"), lwd = 2, xmax = xmax)
}

# Make design data.table
des = make_prob_design(split = FALSE)

# box::help(gen_data_pwexp)


# Test different "problem" functions ----

# Exponential / proportional hazards
args = des[problem == "ph_exp"][2, 3:6] |>
  as.list()
args = lapply(args, unlist)
x = do.call(gen_data_exp, args)
# There should be 24 control and 36 treatment subjects
x[, table(trt)]
x[, mean(event), by = trt]
plot_surv(x)

args = des[problem == "ph_exp"][104, 3:6] |>
  as.list()
args = lapply(args, unlist)
x = do.call(gen_data_exp, args)
plot_surv(x)

# Piecewise exponential
args = des[problem == "crossing_pwexp"][106, 3:6] |>
  as.list()
args = lapply(args, unlist)
x = do.call(gen_data_pwexp, args)
x[, table(trt)]
x[, mean(event), by = trt]
plot_surv(x)

# Weibull distribution
args = des[problem == "crossing_wb"][3, 3:6] |>
  as.list()
args = lapply(args, unlist)
x = do.call(gen_data_weibull, args)
x[, table(trt)]
x[, mean(event), by = trt]
plot_surv(x)

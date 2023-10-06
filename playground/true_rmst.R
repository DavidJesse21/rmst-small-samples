#' Test functions for computation of true RMST
#' (given the parameters are fixed and not estimated)

options(box.path = "R")
#set.seed(123)

box::use(
  pch = miniPCH,
  microbenchmark[microbenchmark]
)

box::use(
  rmst_true = rmst/true
)

lambda = c(1, 3)
knots = 4
cutoff = 7  

# Approximately equal
rmst_true$pwexp(lambda, knots, cutoff)
s_pwexp = pch$spch_fun(c(0, knots), lambda)
rmst_true$numint(s_pwexp, cutoff)

# Check performance gain
x = microbenchmark(
  numeric = rmst_true$numint(s_pwexp, cutoff),
  analytical = rmst_true$pwexp(lambda, knots, cutoff),
  times = 250L
)
x

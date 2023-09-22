# Preliminary/interactive testing of written functions
box::use(
  ./rmst[rmst],
  ./rmst_diff[rmst_diff]
)

box::use(
  data.table[setDT, setnames]
)

data(cancer, package = "survival")
setDT(veteran)
setnames(veteran, old = c("time", "status"), new = c("ti", "st"))

rmst(ti, st, data = veteran, tau = 400)
rmst(veteran$ti, veteran$st, tau = 400)

veteran[, unique(trt)]
rmst_diff(ti, st, trt, data = veteran, tau = 400)
rmst_diff(veteran$ti, veteran$st, veteran$trt, tau = 400)
rmst_diff(ti, st, trt, data = veteran, tau = 400, contrast = c(2, 1))

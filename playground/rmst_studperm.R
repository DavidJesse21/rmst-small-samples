options(box.path = "R")

box::use(
  rmst/km2[rmst_diff_studperm]
)
box::help(rmst_diff_studperm)


box::use(
  fs,
  data.table[...],
  survival[Surv]
)

load(fs$path("data", "Hellmann", ext = "Rdata"))
setDT(data)
data

#' Expected results:
#' 
#' - point estimate: -4.02
#' - p-value: 1.1%
#' - confidence interval: [-7.09, -0.96]
set.seed(150918)

time_taken = system.time({
  x = rmst_diff_studperm(
    Surv(time, event) ~ group, data = data,
    cutoff = 18, contrast = c("0", "1"), var_method = "nelson_aalen",
    num_samples = 5000L
  )
})


round(x$asymptotic, 3)
x$permutation$pval * 100
round(x$permutation$confint, 2)

time_taken

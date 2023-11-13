options(box.path = "R")

box::use(
  fs,
  data.table[...],
  survival[Surv]
)

box::use(
  rmst/km_studperm[rmst_diff_studperm]
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
x = rmst_diff_studperm(
  Surv(time, event) ~ group, data = data,
  cutoff = 18, contrast = c("0", "1"), var_method = "nelson_aalen",
  num_samples = 5000L
)

round(x$asymptotic, 3)
x$permutation$pval * 100
round(x$permutation$confint, 2)

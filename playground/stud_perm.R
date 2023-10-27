# Test/compare own implementation of studentized permutation test with that by
# Marc Ditzhaus.
# Use quite many permutations to ensure that the results will not differ too much due to 
# random variability.

options(box.path = "R")

box::use(
  fs,
  survival[Surv]
)

box::use(
  rmst/stud_perm[rmst_diff_stud_perm]
)
box::help(rmst_diff_stud_perm)

.dir = fs$path("playground", "stud_perm")

# Load data
load(fs$path(.dir, "Hellmann", ext = "Rdata"))
head(data)


# Own implementation ----

set.seed(150918)

results_own = rmst_diff_stud_perm(
  Surv(time, event) ~ group, data = data,
  cutoff = 18, contrast = c("0", "1"), num_samples = 10000
)
results_own
pval_own = results_own$pval * 100
ci_own = c(
  results_own$diff - results_own$null_quantile * results_own$`se(diff)`,
  results_own$diff + results_own$null_quantile * results_own$`se(diff)`
)


# Ditzhaus ----

source(fs$path(.dir, "functions_general", ext = "R"))
source(fs$path(.dir, "functions_twosample", ext = "R"))

n_perm = 10000
tau = 18

values = sort_data(data)
values$group = values$group + 1
values$status = values$event

erg_0 = test_stat_twos(values, values$group, tau, delta = 0)
erg_stat0 = unname(erg_0["wts"])

set.seed(150918)
values = values[, c(1, 4, 3)]
erg_perm = perm_fun(values, n_perm, tau)

q_perm = erg_perm$test_stat_erg[1,]
t0_perm = mean(erg_stat0 <= q_perm, na.rm = TRUE)
pval_ditz = t0_perm * 100

ci_ditz = unname(c(
  erg_0["rmst_diff"] - sqrt(erg_0["var"]) * sqrt(erg_perm$q),
  erg_0["rmst_diff"] + sqrt(erg_0["var"]) * sqrt(erg_perm$q)
))


# Compare ----
results_own
ci_asy = with(results_own, {
  c(diff - qnorm(0.975) * `se(diff)`, diff + qnorm(0.975) * `se(diff)`)
})

c(own = pval_own, ditz = pval_ditz)
list(asy = ci_asy, own = ci_own, ditz = ci_ditz) |>
  lapply(round, 2)

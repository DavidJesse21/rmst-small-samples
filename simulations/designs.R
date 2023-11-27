# Define / code the design parameters here 

box::use(
  data.table[...]
)

des = CJ(
  problem = c("ph_exp", "crossing_pwexp", "crossing_wb"),
  rmst_diff = c(0, 6, 12, 18),
  censoring = 1:3, # TODO: Add censoring exponential parameters (vectors of length 2)
  samples_alloc = list(c(12, 18), c(15, 15), c(18, 12)),
  samples_k = 1:4,
  # `CJ()` argument
  sorted = FALSE
)

# This works
# Maybe we won't need this
set(
  des, j = "num_samples",
  value = lapply(1:nrow(des), \(i) unlist(des[i, samples_alloc]) * des[i, samples_k])
)


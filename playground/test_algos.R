options(box.path = "R")

box::use(
  fs,
  data.table[...]
)

box::use(
  simfuns2/algos[rmst_asy, rmst_studperm, rmst_pseudo, rmst_pseudo_ij, rmst_pseudo_ij_boot]
)


load(fs$path("data", "Hellmann", ext = "Rdata"))
dt = data
rm(data)
setDT(dt)
setnames(dt, old = "group", new = "trt")

constants = list(
  cutoff = 18,
  alpha = 0.05,
  var_method_asy = "greenwood",
  var_method_studperm = "nelson_aalen",
  num_samples_studperm = 2000L,
  num_samples_boot = 2000L
)

# Asymptotic test
rmst_asy(dt, constants)

# Pseudo-Observations
rmst_pseudo(dt, constants)

# IJ pseudo-observations
rmst_pseudo_ij(dt, constants)

# Studentized permutation test
set.seed(42)
rmst_studperm(dt, constants)

# IJ pseudo-observations + bootstrap hypothesis testing
set.seed(42)
rmst_pseudo_ij_boot(dt, constants)

options(box.path = "R")

# batchtools and other utilities
box::use(
  fs,
  bt = batchtools,
  data.table[...]
)

reg = bt$loadRegistry(fs$path("simsetup", "registry"), writeable = TRUE)

bt$findDone()

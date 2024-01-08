options(box.path = "R")

box::use(
  simfuns2/utils[estimate_runtime]
)
box::help(estimate_runtime)

# Medians in miliseconds
# asy: 4.63
# studperm: 2055.37
# pseudo_hc3: 64.52
# pseudo_ij_boot: 6302.69

time_milisec = c(4.63, 2055.37, 64.52, 6302.69)
names(time_milisec) = c("asy", "studperm", "pseudo_hc3", "pseudo_ij_boot")

time_sec = 0.001 * time_milisec
time_min = time_sec / 60
time_hours = time_min / 60

estimate_runtime(sum(time_sec))
estimate_runtime(sum(time_sec), mult = 2)
estimate_runtime(
  sum(time_sec), mult = 2,
  parallel = list(num_cores = 4, prop = 0.9)
)
estimate_runtime(
  sum(time_sec), mult = 2,
  parallel = list(num_cores = 4, prop = 1)
)

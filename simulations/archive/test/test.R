options(box.path = "R")

box::use(
  future[plan, multisession, sequential],
  parallelly[availableCores],
  fs,
  qs,
  data.table[...]
)

box::use(
  simfuns/run_sim[run_sim_all],
  simfuns/analysis[get_values, get_warnings, get_errors]
)

box::use(
  ./test_mod[f1, f2, f3]
)

# box::help(run_sim_all)

sim_dir = fs$path("simulations", "test")


# Example setup ----

# parameters for data generating function
params = data.table(upper = c(1, 5, 10))

# Analysis functions
li_funs = list(
  b = f1,
  w = f2,
  e = f3
)

# Wrapper functions
f1w = function(x) {
  f1(x)
}
f2w = function(x) {
  f2(x)
}
f3w = function(x) {
  f3(x)
}

li_funs2 = list(
  b = f1w,
  w = f2w,
  e = f3w
)

# Data generating function
gen_data = function(params) {
  round(runif(1, max = params$upper))
}


# Run the simulation ----

plan(multisession, workers = availableCores(omit = 1))

run_sim_all(
  params,
  num_sims = 10,
  fun_generate = gen_data,
  funs_analyze = li_funs2,
  dir_out = fs$path(sim_dir, "results"),
  future_args = list(
    future.seed = 42L,
    future.globals = structure(TRUE, add = paste0("f", 1:3))
  )
)

plan(sequential)


# Analyze the results ----

# This would usually be done in a separate file

li_results = lapply(
  fs$dir_ls(fs$path(sim_dir, "results")),
  \(file) qs$qread(file)
)
res1 = li_results[[1]]
res1
get_values(res1$results)
get_warnings(res1$results)
get_errors(res1$results)

res3 = li_results[[3]]
get_values(res3$results)
get_warnings(res3$results)
get_errors(res3$results)


globals::findGlobals(li_funs)
globals::findGlobals()

globals::globalsOf(li_funs)

globals::Globals(li_funs)

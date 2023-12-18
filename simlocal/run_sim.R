box::use(
  future[plan, multisession, sequential],
  future.apply[future_lapply],
  parallelly[availableCores]
)

box::use(batchtools[unwrap])

box::use(
  ./simfuns[gen_data, rmst_all_methods]
)
#box::purge_cache()

plan(multisession, workers = availableCores(omit = 1))

tstart = Sys.time()

sim_results = future_lapply(
  1:5000,
  function(i) {
    dt = gen_data(regenerate = TRUE)
    rmst_all_methods(dt)
  },
  future.seed = 42L
)

tend = Sys.time()

plan(sequential)

(tend - tstart) |>
  as.numeric() * 60

sim_results[1]

invisible(lapply(
  seq_along(sim_results),
  function(i) {
    sim_results[[i]][, job.id := i]
    setcolorder(sim_results[[i]], neworder = "job.id")
  }
))

dt = rbindlist(sim_results)

dt[, sum(!is.na(erro))]
dt[, sum(!is.na(warning))]
for (j in c("error", "warning")) {
  set(dt, j = j, value = NULL)
}

dt = unwrap(dt)

dt[, .(reject = mean(pval <= 0.05)), by = method]
dt[, .(mean_width = mean(ci_upper - ci_lower),
       median_width = median(ci_upper - ci_lower)),
   by = method]

unwrap(dt)[1:5]

sim_results[1]

seq_along(sim_results)

lapply(sim_results)


set.seed(123)
time_taken = system.time({
  x = gen_data()
  res = rmst_all_methods(x)
})
time_taken
x = gen_data()

sim_results

dt
class(sim_results)

object.size(rbindlist(sim_results)) |> format(units = "Mb")
object.size(dt) |> format(units = "Mb")

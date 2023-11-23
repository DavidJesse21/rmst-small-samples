options(box.path = "R")

box::use(
  data.table[...],
  fs,
  qs
)

box::use(
  simfuns/analysis[get_values, get_warnings, get_errors]
)

sim_dir = fs$path("simulations", "first_sim")

li_sims = lapply(
  fs$dir_ls(fs$path(sim_dir, "results")),
  \(file) qs$qread(file)
)

sim1 = li_sims[[1]]
sim1

res1 = sim1$results
dt_val = get_values(res1)
dt_err = get_errors(res1)
dt_warn = get_warnings(res1)

# Calculate power/rejection rate
vapply(
  dt_val,
  function(m) {
    x = vapply(m, \(x) x[["decision"]], numeric(1))
    mean(x, na.rm = TRUE)
  },
  numeric(1)
)

# Calculate CI length
vapply(
  dt_val,
  function(m) {
    x = vapply(m, \(x) x[c("ci_lower", "ci_upper")], numeric(2))
    diff(x) |> unname() |> drop()
  },
  numeric(3)
)

# Calculate coverage
true_diff = sim1$simulator$params[, true_diff]

vapply(
  dt_val,
  function(m) {
    x = vapply(m, \(x) x[c("ci_lower", "ci_upper")], numeric(2)) |>
      t()
    covered = apply(x, MARGIN = 1L, \(x) between(true_diff, x["ci_lower"], x["ci_upper"]))
    mean(covered, na.rm = TRUE)
  },
  numeric(1)
)



options(box.path = "R")

box::use(
  data.table[...]
)

box::use(
  simfuns/designs[make_prob_design]
)

dt = make_prob_design(split = FALSE)
dt = lapply(unique(dt$problem), function(prob) {
  dt[problem == prob][1]
}) |>
  rbindlist()

dt[3, params_surv]

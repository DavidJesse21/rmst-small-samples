#' Table 3 from my thesis presenting the power values of the tests for each scenario.


# Setup ----

options(box.path = "R")

# Modules and functions
box::use(
  data.table[...],
  kableExtra[...],
  fs
)

box::use(
  simfuns2/analyze[calc_rejection_rates, calc_ci_metrics, setj_samples_alloc, setj_percent],
  simfuns2/get_funs[get_scenario_table]
)

# Simulation results
dtr = readRDS(fs$path("simulation", "results", "2024-02-08_results1", ext = "rds"))
dts = get_scenario_table()

# Function for formatting p-values in tables
format_pval = function(x) {
  make_bold = (x <= 5)
  xf = fifelse(
    x < 0.1, "<0.1",
    format(round(x, 1), digits = 2, nsmall = 1)
  )
  
  out = cell_spec(xf, bold = make_bold)
  fifelse(out == "NA", NA_character_, out)
}


# Table ----

## Data preparation ----

# Calculate rejection rates (power)
dt_pow = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 1.5, scenario.id] & algo.id != 4],
  stats_NA = FALSE
)
dt_pow = merge(dt_pow, dts[rmst_diff == 1.5], by = "scenario.id")

# Convert to percent and highlight method with largest power
setj_percent(dt_pow, "reject")
dt_pow[, reject := round(reject, 1)]
dt_pow[, max_pow := (reject == max(reject)), by = scenario.id]
dt_pow[, reject := cell_spec(format(reject, digits = 1, nsmall = 1), bold = max_pow)][, max_pow := NULL]

# For latex, directly starting off with a table in wide format might be easier
dt_pow = dcast(dt_pow, scenario.id ~ algo.id, value.var = "reject")
setnames(dt_pow, old = as.character(c(1:3, 5)), new = paste0("algo", c(1:3, 5)))

# Append scenario information...
dt_pow = merge(dt_pow, dts, by = "scenario.id")
# setj_percent(dt_pow, paste0("algo", c(1:3, 5)))
# for (j in paste0("algo", c(1:3, 5))) {
#   set(dt_pow, j = j, value = round(dt_pow[[j]], 1))
# }
dt_pow[, samples_alloc := factor(
  sprintf("n_%d_%d", n0 / samples_k, n1 / samples_k),
  levels = sprintf("n_%d_%d", c(12, 15, 18), c(18, 15, 12))
)]
# ... but some columns are or have become redundant
for (j in c("scenario.id", "rmst_diff", "n0", "n1")) {
  set(dt_pow, j = j, value = NULL)
}

dt_pow = dcast(
  dt_pow,
  surv_model + cens_model + samples_k ~ samples_alloc, value.var = paste0("algo", c(1:3, 5))
)

# Order columns and rows
setcolorder(
  dt_pow, neworder = c(
    "surv_model", "cens_model", "samples_k",
    sprintf("algo%d_n_12_18", c(1:3, 5)),
    sprintf("algo%d_n_15_15", c(1:3, 5)),
    sprintf("algo%d_n_18_12", c(1:3, 5))
  )
)
dt_pow[, `:=`(
  surv_model = factor(surv_model, levels = c("ph_exp", "crossing_pwexp", "crossing_wb")),
  cens_model = factor(cens_model, levels = c("uneq_wb", "eq_unif", "eq_wb"))
)]
setorder(dt_pow, surv_model, cens_model, samples_k)

# Make some cells of column `cens_model` empty
na_idx = setdiff(1:nrow(dt_pow), seq(1, nrow(dt_pow), by = 4))
dt_pow[na_idx, cens_model := NA]

# Also need to relabel this
dt_pow[, cens_model := as.character(cens_model)]
dt_pow[, cens_model := fcase(
  cens_model == "uneq_wb", "un. W.",
  cens_model == "eq_unif", "eq. U.",
  cens_model == "eq_wb", "eq. W."
)]


## LaTeX table ----

sink(fs$path("paper", "tables", "tbl03-power", ext = "tex"))

# NAs as empty cells
options(knitr.kable.NA = '')

# latex table
kbl(
  copy(dt_pow)[, surv_model := NULL],
  format = "latex",
  digits = 1,
  booktabs = TRUE,
  centering = TRUE,
  escape = FALSE,
  align = c("l", rep("c", 13)),
  col.names = c("Censoring", "K", rep(c("Asy", "Perm", "PO1", "PO2"), 3)),
  linesep = c(rep("", 11), "\\hline"),
  caption = "Rejection rates (power) of different methods in \\% (nominal level $\\alpha = 5\\%$)",
  label = "tbl-power"
) |>
  # Group the columns by their sample allocation
  add_header_above(
    setNames(
      c(1, 1, 4, 4, 4),
      c(" ", " ", sprintf(r"($N = K \\cdot (%d, %d)$)", c(12, 15, 18), c(18, 15, 12)))
    ),
    escape = FALSE, bold = TRUE
  ) |>
  # Borders between the groups make it more accessible
  column_spec(2, border_right = TRUE) |>
  column_spec(6, border_right = TRUE) |>
  column_spec(10, border_right = TRUE) |>
  # Group table (rows) by survival distributions
  pack_rows(
    "S1: Exponential distributions", 1, 12,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE
  ) |>
  pack_rows(
    "S7: Exponential and piecewise exponential distributions with crossing curves", 13, 24,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE
  ) |>
  pack_rows(
    "S8: Weibull distributions with crossing curves and shape alternatives", 25, 36,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE
  ) |>
  # Make it more accessible using stripes option
  kable_styling(
    latex_options = c("striped", "scale_down"),
    stripe_index = c(5:8, 17:20, 29:32),
    stripe_color = "#e9ecef"
  ) |>
  # Explain binomial confidence interval and abbreviations
  footnote(
    general = c(
      paste0(
        r"(\\textit{Note:} )",
        "Largest value per scenario is printed bold."
      ),
      paste0(
        r"(\\textit{Abbreviations:} )",
        "Asy, asymptotic test; ", "Perm, studentized permutation test; ",
        "PO1, pseudo-observations asymptotic; ",
        "PO2, pseudo-observations bootstrap; ",
        "un. W., unequal Weibull censoring; ",
        "eq. U., equal uniform censoring; ",
        "eq. W., equal Weibull censoring."
      )
    ),
    general_title = "",
    escape = FALSE,
    threeparttable = TRUE
  )

sink()

# Clean environment for next table
rm(list = ls())
box::purge_cache()

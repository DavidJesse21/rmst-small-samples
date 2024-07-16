#' Table 2 from my thesis presenting the empirical type I error rates from the simulation study.


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

# 
format_values = function(x) {
  make_bold = between(x, 4.4, 5.6)
  x = format(x, digits = 1, nsmall = 1)
  x = fifelse(
    make_bold,
    sprintf(r"(\textbf{%s})", x),
    x
  )
  
  return(x)
}


# Table ----

## (Re-)shaping the data etc. ----

# Calculate rejection rates
# For sake of illustration:
# - ignore pseudo_ij (no bootstrap)
# - ignore k = 6
dt_err = calc_rejection_rates(
  dtr[scenario.id %in% dts[rmst_diff == 0, scenario.id] &
        algo.id != 4],
  stats_NA = FALSE
)
dt_err = merge(dt_err, dts[rmst_diff == 0], by = "scenario.id")
# dt_err = dt_err[samples_k != 6]

# For latex, directly starting off with a table in wide format might be easier
dt_err = dcast(dt_err, scenario.id ~ algo.id, value.var = "reject")
setnames(dt_err, old = as.character(c(1:3, 5)), new = paste0("algo", c(1:3, 5)))

# Append scenario information...
dt_err = merge(dt_err, dts, by = "scenario.id")
setj_percent(dt_err, paste0("algo", c(1:3, 5)))
for (j in paste0("algo", c(1:3, 5))) {
  set(dt_err, j = j, value = round(dt_err[[j]], 1))
}
dt_err[, samples_alloc := factor(
  sprintf("n_%d_%d", n0 / samples_k, n1 / samples_k),
  levels = sprintf("n_%d_%d", c(12, 15, 18), c(18, 15, 12))
)]
# ... but some columns are or have become redundant
for (j in c("scenario.id", "rmst_diff", "n0", "n1")) {
  set(dt_err, j = j, value = NULL)
}

dt_err = dcast(
  dt_err,
  surv_model + cens_model + samples_k ~ samples_alloc, value.var = paste0("algo", c(1:3, 5))
)

# Order columns and rows
setcolorder(
  dt_err, neworder = c(
    "surv_model", "cens_model", "samples_k",
    sprintf("algo%d_n_12_18", c(1:3, 5)),
    sprintf("algo%d_n_15_15", c(1:3, 5)),
    sprintf("algo%d_n_18_12", c(1:3, 5))
  )
)
dt_err[, `:=`(
  surv_model = factor(surv_model, levels = c("ph_exp", "crossing_pwexp", "crossing_wb")),
  cens_model = factor(cens_model, levels = c("uneq_wb", "eq_unif", "eq_wb"))
)]
setorder(dt_err, surv_model, cens_model, samples_k)

# Make some cells of column `cens_model` empty
na_idx = setdiff(1:nrow(dt_err), seq(1, nrow(dt_err), by = 4))
dt_err[na_idx, cens_model := NA]

# Also need to relabel this
dt_err[, cens_model := as.character(cens_model)]
dt_err[, cens_model := fcase(
  cens_model == "uneq_wb", "un. W.",
  cens_model == "eq_unif", "eq. U.",
  cens_model == "eq_wb", "eq. W."
)]


## Creating the LaTeX table ----

# NAs as empty cells
options(knitr.kable.NA = '')

# Conditional formatting
for (j in colnames(dt_err)[-(1:3)]) {
  set(dt_err, j = j, value = format_values(dt_err[[j]]))
}


# simple latex table
x = kbl(
  copy(dt_err)[, surv_model := NULL],
  format = "latex",
  digits = 1,
  booktabs = TRUE,
  centering = TRUE,
  escape = FALSE,
  align = c("l", rep("c", 13)),
  col.names = c("Censoring", "K", rep(c("Asy", "Perm", "PO1", "PO2"), 3)),
  linesep = c(rep("", 3), "\\hline"),
  caption = "Type I error rates of different methods in \\% (nominal level $\\alpha = 5\\%$). The values inside the binomial confidence interval $[4.4\\%, 5.6\\%]$ are printed bold.",
  label = "tbl-type1err"
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
  # kable_styling(
  #   latex_options = c("scale_down")
  #   # stripe_index = c(5:8, 17:20, 29:32),
  #   # stripe_color = "#e9ecef"
  # ) |>
  # Explain binomial confidence interval and abbreviations
  footnote(
    general = c(
      # paste0(
      #   r"(\\textit{Note:} )",
      #   "The values inside the binomial confidence interval [4.4\\\\%, 5.6\\\\%] are printed bold."
      # ),
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

sink(fs$path("paper", "tables", "tbl02-type_I_err", ext = "tex"))
print(x)
sink()

# For Wiley template we need to change/modify many things
x = readLines(fs$path("paper", "tables", "tbl02-type_I_err", ext = "tex"))

# Remove "\\addlinespace[0.5em]
x = x[x != r"(\addlinespace[0.5em])"]
# Replace \bottomrule with \hline
x = gsub(r"(\\bottomrule)", r"(\\hline)", x)
# Remove \toprule
x = x[x != r"(\toprule)"]
# Remove "\cmidrule" stuff
x = x[!grepl(r"(\\cmidrule)", x)]
# Make header row(s)
x = append(x, r"(\headrow)", after = 7) |>
  append(r"(\headrow)", after = 6)
# Make font size smaller
x = append(x, r"(\footnotesize)", after = grep(r"(\\begin\{threeparttable\})", x))
x = append(x, r"(\hiderowcolors)", after = grep(r"(\midrule)", x))

writeLines(x, fs$path("paper", "tables", "tbl02-type_I_err", ext = "tex"))


# Clean environment for next table
rm(list = ls())
box::purge_cache()

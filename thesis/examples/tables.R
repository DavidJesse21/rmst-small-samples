options(box.path = "R")

box::use(
  data.table[...],
  kableExtra[...],
  data.table.extras[setj_at],
  survival[survfit, Surv, survdiff],
  fs
)

box::use(
  simfuns2/analyze[setj_percent]
)

# format p-values
format_pval = function(x) {
  fifelse(
    x < 0.1, "<0.1",
    format(round(x, 1), digits = 2, nsmall = 1)
  )
}


# Hellmann ----

# Data set
load(fs$path("data", "Hellmann", ext = "Rdata"))
dt = data
rm(data)
setDT(dt)
dt

# Results
dt_res = readRDS(fs$path("thesis", "objects", "res_hellmann", ext = "rds"))

# Obtain p-values and reshape the results for latex table
dtt = dt_res[variable == "group", .(method, cutoff, pval)]
dtt = dcast(dtt, method ~ cutoff, value.var = "pval")
setnames(dtt, old = as.character(c(12, 15, 18)), new = \(x) paste0("t", x))

# Reorder rows by method
dtt = dtt[c(1, 4, 2:3)]

# Also include log-rank test
pval_lr = survdiff(Surv(time, event) ~ group, data = dt)$pvalue
dtt = rbindlist(list(
  dtt,
  data.table(method = "lr", t12 = NA_real_, t15 = pval_lr, t18 = NA_real_)
))

# Convert to percent
setj_percent(dtt, 2:4)
setj_at(dtt, 2:4, format_pval)

# Need to relabel methods
dtt[, method := c("Asy", "Perm", "PO1", "PO2", "LR")]

# NAs as empty cells
options(knitr.kable.NA = '')

# Table
kbl(
  dtt,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("Method", sprintf("$t^* = %d$", c(12, 15, 18))),
  align = c("l", rep("r", 3)),
  linesep = c(rep("", 3), "\\hline", "")
) |>
  # Column specifications
  column_spec(1, width = "2.5cm", border_right = TRUE) |>
  column_spec(2:4, width = "3cm") |>
  #
  kable_styling(position = "center", font_size = 12) |>
  # Organize rows by type of method
  pack_rows(
    r"(Tests for $\\mu_1(t^*) = \\mu_0(t^*)$)", 1, 4,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE, escape = FALSE
  ) |>
  pack_rows(
    r"(Test for $S_1 = S_0$)", 5, 5,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE, escape = FALSE
  ) |>
  # Legend
  footnote(
    general = paste0(
      r"(\\textit{Abbreviations:} )",
      "Asy, asymptotic test; ", "Perm, studentized permutation test; ",
      "PO1, pseudo-observations; ",
      "PO2, pseudo-observations + bootstrap test; ",
      "LR, log-rank test."
    ),
    escape = FALSE,
    general_title = "",
    threeparttable = TRUE
  )


# Ovarian ----

# Data set
dt = survival::ovarian
setDT(dt)
setnames(dt, old = c("futime", "fustat", "rx"), new = c("time", "event", "group"))
dt[, group := group - 1L]
dt[, ecog.ps := factor(ecog.ps, levels = as.character(2:1))]

# Results
dt_res = readRDS(fs$path("thesis", "objects", "res_ovarian", ext = "rds"))

# Obtain p-values and reshape the results for latex table
dtt = dt_res[variable == "group", .(method, cutoff, pval)]
dtt = dcast(dtt, method ~ cutoff, value.var = "pval")
setnames(dtt, old = as.character(c(500, 750, 1000)), new = \(x) paste0("t", x))

# Reorder rows by method
dtt = dtt[c(1, 6, 2, 4, 3, 5)]

# Also include log-rank test
pval_lr = survdiff(Surv(time, event) ~ group, data = dt)$pvalue
dtt = rbindlist(list(
  dtt,
  data.table(method = "lr", t500 = NA_real_, t750 = pval_lr, t1000 = NA_real_)
))

# Convert to percent
setj_percent(dtt, 2:4)
setj_at(dtt, 2:4, format_pval)

# Need to relabel methods
dtt[, method := c("Asy", "Perm", "PO1", "PO2", "PO1 Adj", "PO2 Adj", "LR")]

# NAs as empty cells
options(knitr.kable.NA = '')

# Table
kbl(
  dtt,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("Method", sprintf("$t^* = %d$", c(500, 750, 1000))),
  align = c("l", rep("r", 3)),
  linesep = c(rep("", 3), "\\hline", "", "\\hline", "")
) |>
  # Column specifications
  column_spec(1, width = "2.5cm", border_right = TRUE) |>
  column_spec(2:4, width = "3cm") |>
  #
  kable_styling(position = "center") |>
  # Organize rows by type of method
  pack_rows(
    r"(Tests for $\\mu_1(t^*) = \\mu_0(t^*)$)", 1, 4,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE, escape = FALSE
  ) |>
  pack_rows(
    r"(Adjusted tests for $\\mu_1(t^*) = \\mu_0(t^*)$)", 5, 6,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE, escape = FALSE
  ) |>
  pack_rows(
    r"(Test for $S_1 = S_0$)", 7, 7,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE, escape = FALSE
  ) |>
  # Legend
  footnote(
    general = paste0(
      r"(\\textit{Abbreviations:} )",
      "Asy, asymptotic test; ", "Perm, studentized permutation test; ",
      "PO1, pseudo-observations; ",
      "PO2, pseudo-observations + bootstrap test; ",
      "Adj, adjusted; ",
      "LR, log-rank test."
    ),
    escape = FALSE,
    general_title = "",
    threeparttable = TRUE
  )


# Glioma ----

# Data set
dt = fread("data/glioma.csv")
dt[, pid := NULL]
setnames(dt, new = \(x) sub(".*?_", "", x))
dt[, group := fifelse(group == "Control", 0L, 1L)]

# Results
dt_res = readRDS(fs$path("thesis", "objects", "res_glioma", ext = "rds"))

# Obtain p-values and reshape the results for latex table
dtt = dt_res[variable == "group", .(method, cutoff, pval)]
dtt = dcast(dtt, method ~ cutoff, value.var = "pval")
setnames(dtt, old = as.character(c(20, 30, 40)), new = \(x) paste0("t", x))

# Reorder rows by method
dtt = dtt[c(1, 6, 2, 4, 3, 5)]

# Also include log-rank test
pval_lr = survdiff(Surv(time, event) ~ group, data = dt)$pvalue
dtt = rbindlist(list(
  dtt,
  data.table(method = "lr", t20 = NA_real_, t30 = pval_lr, t40 = NA_real_)
))

# Convert to percent
setj_percent(dtt, 2:4)
setj_at(dtt, 2:4, format_pval)

# Need to relabel methods
dtt[, method := c("Asy", "Perm", "PO1", "PO2", "PO1 Adj", "PO2 Adj", "LR")]

# NAs as empty cells
options(knitr.kable.NA = '')

# Table
kbl(
  dtt,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  col.names = c("Method", sprintf("$t^* = %d$", c(20, 30, 40))),
  align = c("l", rep("r", 3)),
  linesep = c(rep("", 3), "\\hline", "", "\\hline", "")
) |>
  # Column specifications
  column_spec(1, width = "2.5cm", border_right = TRUE) |>
  column_spec(2:4, width = "3cm") |>
  #
  kable_styling(position = "center") |>
  # Organize rows by type of method
  pack_rows(
    r"(Tests for $\\mu_1(t^*) = \\mu_0(t^*)$)", 1, 4,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE, escape = FALSE
  ) |>
  pack_rows(
    r"(Adjusted tests for $\\mu_1(t^*) = \\mu_0(t^*)$)", 5, 6,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE, escape = FALSE
  ) |>
  pack_rows(
    r"(Test for $S_1 = S_0$)", 7, 7,
    bold = FALSE, latex_gap_space = "0.5em", hline_after = TRUE, escape = FALSE
  ) |>
  # Legend
  footnote(
    general = paste0(
      r"(\\textit{Abbreviations:} )",
      "Asy, asymptotic test; ", "Perm, studentized permutation test; ",
      "PO1, pseudo-observations; ",
      "PO2, pseudo-observations + bootstrap test; ",
      "Adj, adjusted; ",
      "LR, log-rank test."
    ),
    escape = FALSE,
    general_title = "",
    threeparttable = TRUE
  )

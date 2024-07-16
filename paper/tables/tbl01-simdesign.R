#' Table 1 from my thesis displaying the design parameters of the simulation study for the DGP.
#' Maybe this table is rather optional.

options(box.path = "R")

box::use(
  data.table[...],
  kableExtra[...],
  fs
)

dt = data.table(
  Factor = c(
    rep("Survival models", 3),
    rep("Censoring models", 3),
    r"(RMST difference ($\Delta$))",
    "Sample allocations",
    r"(Sample multipliers ($K$))"
  ),
  Level = c(
    "S1: Exponential distributions",
    "S7: Exponential and piecewise exponential distributions with crossing curves",
    "S8: Weibull distributions with crossing curves and shape alternatives",
    "C1: unequal Weibull",
    "C2: equal uniform",
    "C3: equal Weibull",
    "0; 1.5",
    "(12, 18); (15, 15); (18, 12)",
    "1; 2; 4; 6"
  )
)
# View(dt)


# Old / original ----

# sink(fs$path("paper", "tables", "tbl01-simdesign", ext = "tex"))
# 
# kbl(
#   dt,
#   format = "latex",
#   booktabs = TRUE,
#   centering = TRUE,
#   escape = FALSE,
#   linesep = "\\addlinespace",
#   caption = "Factors and their levels for the data-generating mechanisms used in the simulation study",
#   label = "tbl-simdesign"
# ) |>
#   collapse_rows(
#     columns = 1,
#     latex_hline = "major"
#   ) |>
#   column_spec(1, width = "4.5cm") |>
#   kable_styling(
#     latex_options = "scale_down"
#   )
# 
# sink()


# New / Wiley paper ----

dt[c(1, 3, 4, 6), Factor := NA_character_]
# NAs as empty cells
options(knitr.kable.NA = '')

x = kbl(
  dt,
  format = "latex",
  booktabs = TRUE,
  centering = TRUE,
  escape = FALSE,
  linesep = c("", "", "\\hline", "", "", "\\hline", "\\hline", "\\hline"),
  caption = "Factors and their levels for the data-generating mechanisms used in the simulation study.",
  label = "tbl-simdesign"
)

sink(fs$path("paper", "tables", "tbl01-simdesign", ext = "tex"))
print(x)
sink()

x = readLines(fs$path("paper", "tables", "tbl01-simdesign", ext = "tex"))

# Replace \toprule and \bottomrule with appropriate Wiley equivalents
x = gsub(r"(\\bottomrule)", r"(\\hline)", x)
x = gsub(r"(\\toprule)", r"(\\headrow)", x)

# Smaller font size
x = append(x, r"(\small)", after = grep(r"(\\centering)", x))

# No row colors
x = append(x, r"(\hiderowcolors)", after = grep(r"(\midrule)", x))

writeLines(x, fs$path("paper", "tables", "tbl01-simdesign", ext = "tex"))

# Clean environment for next table
rm(list = ls())
box::purge_cache()

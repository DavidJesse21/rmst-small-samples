# Make file for list of abbreviations in thesis document
box::use(fs)

# Read all abbreviations and sort them in alphabetic order
x = readLines(fs$path("thesis", "abbreviations", ext = "txt"))
x = sort(x)

# Add spaces and line breaks for table
x = paste0("  ", x, r"( \\)")

# Complete vector of code/lines to write
to_write = c(
  r"(\section*{List of Abbreviations})",
  "",
  r"(\begin{tabular}{@{} l @{\hskip 1in} l})",
  x,
  r"(\end{tabular})"
)

# Write to file
writeLines(to_write, fs$path("thesis", "tex", "abbreviations.tex"))

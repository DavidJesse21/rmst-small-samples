# Write/update file packages.bib
pkgs = c(
  "base",
  "renv",
  "withr",
  "parallel",
  "survival"
)

file = fs::path("thesis", "bib", "packages", ext = "bib")

knitr::write_bib(pkgs, file)

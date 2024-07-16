#' Test


# Setup ----

options(box.path = "R")

box::use(
  data.table[...],
  ggplot2[...],
  fs
)

dt = readRDS(fs$path("thesis", "objects", "res_ovarian", ext = "rds"))


# Plot ----

dtp = dt[variable == "group"]
dtp[, method := factor(
  method, levels = c("asy", "studperm", "po_asy", "po_boot", "po_asy_adj", "po_boot_adj"),
  labels = c("Asy", "Stud Perm", "PO", "PO Boot", "PO Adj", "PO Boot Adj")
)]
dtp[, cutoff := factor(
  cutoff, levels = c(15, 20, 25),
  labels = sprintf("t^`*` == %d~months", c(15, 20, 25))
)]

ggplot(dtp, aes(x = method, y = est, ymin = ci_lower, ymax = ci_upper)) +
  geom_point(size = 3) +
  geom_errorbar(linewidth = 1.1, width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.9, color = "#222222") +
  facet_wrap(~ cutoff, labeller = label_parsed, scales = "free_x") +
  labs(
    x = "Method\n", y = "\nEstimate of RMST difference (months)"
  ) +
  scale_x_discrete(limits = rev(levels(dtp$method))) +
  coord_flip()


ggsave(
  "fig12-edmonson_results.pdf",
  path = fs$path("paper", "figures"),
  width = 10, height = 6,
  device = cairo_pdf
)

# Clean environment for next figure
rm(list = ls())
box::purge_cache()
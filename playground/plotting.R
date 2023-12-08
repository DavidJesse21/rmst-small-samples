options(box.path = "R")

box::use(
  plotph = plotting/ph_exp,
  plotcross1 = plotting/crossing_pwexp,
  plotcross2 = plotting/crossing_wb
)

box::use(
  simfuns/designs[make_prob_design]
)

des = make_prob_design()
# Subset for testing
des = lapply(des, \(dt) dt[(samples_k == 2) & (rmst_diff %in% c(0, 1.5))])
des = lapply(des, function(dt) {
  idx = vapply(dt$samples_alloc, \(x) identical(unname(x), c(15, 15)), logical(1))
  dt[idx]
})
des = lapply(des, function(dt) {
  idx = vapply(
    dt$params_cens,
    function(x) {
      isTRUE(all.equal(
        unname(x), c(0.01335314, 0.04700036), tolerance = 0.0001
      ))
    },
    logical(1)
  )
  dt[idx]
})



# ph_exp ----

des$ph_exp

plotph$plot_hazards(0.2, 0.2, linewidth = 1)
plotph$plot_hr(0.2, 0.2, linewidth = 1)
plotph$plot_survs(0.2, 0.2, linewidth = 1)
plotph$plot_joint(0.2, 0.2, linewidth = 1, align = "column")

plotph$plot_hazards(0.2, 0.1200025, linewidth = 1)
plotph$plot_hr(0.2, 0.1200025, linewidth = 1)
plotph$plot_survs(0.2, 0.1200025, linewidth = 1)
plotph$plot_joint(0.2, 0.1200025, linewidth = 1, align = "column")


# crossing_pwexp ----

des$crossing_pwexp[, params_surv]

plotcross1$plot_hazards(0.2, 0.5, 0.05, 1.501968, linewidth = 1)
plotcross1$plot_hr(0.2, 0.5, 0.05, 1.501968, linewidth = 1)
plotcross1$plot_survs(0.2, 0.5, 0.05, 1.501968, linewidth = 1)
plotcross1$plot_joint(0.2, 0.5, 0.05, 1.501968, linewidth = 1, align = "column")

plotcross1$plot_hazards(0.2, 0.5, 0.05, 0.7035125, linewidth = 1)
plotcross1$plot_hr(0.2, 0.5, 0.05, 0.7035125, linewidth = 1)
plotcross1$plot_survs(0.2, 0.5, 0.05, 0.7035125, linewidth = 1)
plotcross1$plot_joint(0.2, 0.5, 0.05, 0.7035125, linewidth = 1, align = "column")

# crossing_wb ----

des$crossing_wb[, params_surv]

plotcross2$plot_hazards(3, 8, 0.9098285, 14, linewidth = 1)
plotcross2$plot_hr(3, 8, 0.9098285, 14, linewidth = 1)
plotcross2$plot_survs(3, 8, 0.9098285, 14, linewidth = 1)
plotcross2$plot_joint(3, 8, 0.9098285, 14, linewidth = 1, align = "column")

plotcross2$plot_hazards(3, 8, 1.914223, 14, linewidth = 1)
plotcross2$plot_hr(3, 8, 1.914223, 14, linewidth = 1)
plotcross2$plot_survs(3, 8, 1.914223, 14, linewidth = 1)
plotcross2$plot_joint(3, 8, 1.914223, 14, linewidth = 1, align = "column")

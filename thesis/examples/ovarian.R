# Setup ----

options(box.path = "R")

box::use(
  data.table[...],
  ggplot2[...],
  survival[Surv, survfit, survdiff, coxph, cox_zph = cox.zph],
  eventglm[rmeanglm],
  withr[with_seed],
  fs,
  ggsurvfit[survfit2, ggsurvfit, add_censor_mark],
  data.table.extras[setj_at],
  kableExtra[...],
  flexsurv[flexsurvspline]
)

box::use(
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  rmst/pseudo[pseudo_strat, pseudo_infjack, rmst_pseudo_test, rmst_pseudo_boot_test],
  simfuns2/analyze[setj_percent]
)


dt = survival::ovarian
setDT(dt)
setnames(dt, old = c("futime", "fustat", "rx"), new = c("time", "event", "group"))
dt[, group := group - 1L]
dt[, ecog.ps := factor(ecog.ps, levels = as.character(2:1))]
# Use months instead of days
dt[, time := time / 30.417]

?ovarian

theme_set(theme_bw())

blank_x = theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)


# FPM functions ----

# Fit FPM
fit_fpm = function(data, df1 = 3, df2 = 2) {
  li_anc = rep(list(~ group), df2)
  names(li_anc) = paste0("gamma", 1:df2)
  
  m = flexsurvspline(
    Surv(time, event) ~ group, data = data,
    k = df1 - 1, anc = li_anc
  )
  
  return(m)
}


# Plot FPM
plot_fpm = function(m, t_eval, type = c("survival", "hazard", "HR"), hr_order = 0:1, ...) {
  type = match.arg(type, c("survival", "hazard", "HR"))
  
  li_dt = summary(
    m,
    type = if (type == "survival") "survival" else "hazard",
    t = t_eval,
    ci = FALSE
  )
  
  invisible(lapply(li_dt, setDT))
  invisible(lapply(names(li_dt), function(x) {
    li_dt[[x]][, group := sub("group=(.*)", "\\1", x)]
  }))
  
  dt = rbindlist(li_dt)
  dt[, group := factor(group, levels = sub("group=(.*)", "\\1", names(li_dt)))]
  
  # Survival and hazards
  if (type %in% c("survival", "hazard")) {
    p = ggplot(dt, aes(time, est, color = group)) +
      geom_line(...)
  } else {
    # Hazard ratio
    dt = dcast(dt, time ~ group, value.var = "est")
    setnames(dt, old = 2:3, new = paste0("haz", hr_order))
    dt[, hr := haz0 / haz1]
    
    p = ggplot(dt, aes(time, hr)) +
      geom_line(...)
  }
  
  return(p)
}



# EDA  ----

cox = coxph(Surv(time, event) ~ group, data = dt, x = TRUE)
cox_hr = unname(exp(coef(cox)))
cox_zph(cox)$table[1, "p"]


p_km = ggsurvfit(
  survfit2(Surv(time, event) ~ group, data = dt),
  linewidth = 1.1
) +
  # theme_bw(base_size = 16) +
  scale_color_manual(
    name = "Treatment",
    values = c("#E69F00", "#56B4E9"),
    labels = c("Control", "Experimental")
  ) +
  xlab(NULL) +
  blank_x +
  ylab("Survival probability\n") +
  scale_y_continuous(limit = c(0, 1)) +
  theme(legend.position = "top") +
  #scale_x_continuous(limits = c(0, 1250))
  scale_x_continuous(limits = c(0, 40))
  

dt2 = copy(dt)
dt2[, group := fifelse(group == 0, "Control", "Experimental")]

m = fit_fpm(dt2, 3, 2)
t_eval = seq(0.1, 40, by = 0.2)
#t_eval = seq(1, 1250, by = 4)

p_haz = plot_fpm(m , t_eval, "hazard", linewidth = 1.1)
p_haz = p_haz +
  xlab(NULL) +
  blank_x +
  ylab("Hazard Rate\n") +
  scale_color_manual(
    name = "Treatment",
    values = c("Control" = "#E69F00", "Experimental" = "#56B4E9")
  ) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 40))

p_hr = plot_fpm(m, t_eval, "HR", linewidth = 1.1, hr_order = 1:0)
p_hr = p_hr +
  xlab(NULL) +
  ylab(expression(paste("Hazard Ratio ", h[1](t)/h[0](t), "\n"))) +
  scale_x_continuous(limits = c(0, 40)) +
  geom_hline(
    yintercept = cox_hr,
    linetype = "dashed",
    linewidth = 1
  )

wrap_plots(p_km, p_haz, p_hr) +
  plot_layout(ncol = 1, axes = "collect_x") &
  xlab("\nTime (months)")


if (!fs$file_exists(fs$path("thesis", "objects", "res_ovarian", ext = "rds"))) {
  
  # Methods ----
  
  #cutoffs = c(500, 750, 1000)
  cutoffs = c(15, 20, 25)
  
  # Asymptotic 2-sample test
  res_asy = lapply(cutoffs, function(cutoff) {
    rmst_diff_test(
      Surv(time, event) ~ group, data = dt, cutoff = cutoff,
      contrast = c("1", "0")
    )
  })
  
  # Studentized permutation test
  res_studperm = lapply(cutoffs, function(cutoff) {
    with_seed(42, {
      rmst_diff_studperm(
        Surv(time, event) ~ group, data = dt, cutoff = cutoff,
        contrast = c("1", "0"), num_samples = 5000L
      )
    })
  })
  
  # Pseudo-Observations Asymptotic
  res_po_asy = lapply(cutoffs, function(cutoff) {
    m = rmeanglm(
      Surv(time, event) ~ group, time = cutoff, data = dt,
      model.censoring = pseudo_strat, formula.censoring = ~ group
    )
    
    rmst_pseudo_test(m)
  })
  
  # Pseudo-Observations Bootstrap
  res_po_boot = lapply(cutoffs, function(cutoff) {
    m = rmeanglm(
      Surv(time, event) ~ group, time = cutoff, data = dt,
      model.censoring = pseudo_infjack, formula.censoring = ~ group
    )
    
    with_seed(42, {
      rmst_pseudo_boot_test(m, num_samples = 5000L)
    })
  })
  
  
  # Pseudo-Observations Asymptotic Adjusted
  res_po_asy2 = lapply(cutoffs, function(cutoff) {
    m = rmeanglm(
      Surv(time, event) ~ group + age + ecog.ps, time = cutoff, data = dt,
      model.censoring = pseudo_strat, formula.censoring = ~ group
    )
    
    rmst_pseudo_test(m)
  })
  
  
  # Pseudo-Observations Bootstrap Adjusted
  res_po_boot2 = lapply(cutoffs, function(cutoff) {
    m = rmeanglm(
      Surv(time, event) ~ group + age + ecog.ps, time = cutoff, data = dt,
      model.censoring = pseudo_infjack, formula.censoring = ~ group
    )
    
    with_seed(42, {
      rmst_pseudo_boot_test(m, num_samples = 5000L)
    })
  })
  
  
  # Structure and combine results ----
  
  # Standard aysmptotic
  dt_asy = rbindlist(lapply(res_asy, \(x) as.list(x)))
  
  dt_asy[, cutoff := cutoffs]
  dt_asy[, `:=`(ci_lower = diff - qnorm(0.975) * sqrt(var_diff),
                ci_upper = diff + qnorm(0.975) * sqrt(var_diff))]
  dt_asy[, method := "asy"]
  dt_asy[, variable := "group"]
  
  setnames(dt_asy, old = c("diff", "var_diff"), new = c("est", "var_est"))
  setcolorder(dt_asy, c("method", "variable", "cutoff"))
  
  
  # Studentized permutation
  dt_studperm = rbindlist(
    lapply(res_studperm, function(x) {
      data.table(
        method = "studperm",
        variable = "group",
        cutoff = NA_real_,
        est = x$asymptotic[["diff"]],
        var_est = x$asymptotic[["var_diff"]],
        tstat = x$asymptotic[["tstat"]],
        pval = x$permutation$pval,
        ci_lower = x$permutation$confint[[1]],
        ci_upper = x$permutation$confint[[2]]
      )
    })
  )
  dt_studperm[, cutoff := cutoffs]
  
  
  # Pseudo-Observations Asymptotic
  dt_po_asy = rbindlist(
    lapply(res_po_asy, function(x) {
      dt1 = data.table(
        method = rep("po_asy", nrow(x)),
        variable = rownames(x),
        cutoff = NA_real_
      )
      dt2 = as.data.table(x)
      dt3 = cbind(dt1, dt2)
      
      dt3[, `:=`(ci_lower = est - qnorm(0.975) * sqrt(var_est),
                 ci_upper = est + qnorm(0.975) * sqrt(var_est))]
      
      # Intercept is not of interest for us
      dt3 = dt3[variable != "(Intercept)"]
      
      dt3
    })
  )
  dt_po_asy[, cutoff := cutoffs]
  
  
  # Pseudo-Observations Bootstrap
  dt_po_boot = rbindlist(
    lapply(res_po_boot, function(x) {
      dt1 = data.table(
        method = rep("po_boot", nrow(x)),
        variable = rownames(x),
        cutoff = NA_real_
      )
      dt2 = as.data.table(x)
      dt2[, z_boot := NULL]
      setnames(dt2, old = "pval_boot", new = "pval")
      dt3 = cbind(dt1, dt2)
      
      # Intercept is not of interest for us
      dt3 = dt3[variable != "(Intercept)"]
      
      dt3
    })
  )
  dt_po_boot[, cutoff := cutoffs]
  
  
  # Pseudo-Observations Asymptotic Adjusted
  dt_po_asy2 = rbindlist(
    lapply(res_po_asy2, function(x) {
      dt1 = data.table(
        method = rep("po_asy_adj", nrow(x)),
        variable = rownames(x),
        cutoff = NA_real_
      )
      dt2 = as.data.table(x)
      dt3 = cbind(dt1, dt2)
      
      dt3[, `:=`(ci_lower = est - qnorm(0.975) * sqrt(var_est),
                 ci_upper = est + qnorm(0.975) * sqrt(var_est))]
      
      # Intercept is not of interest for us
      dt3 = dt3[variable != "(Intercept)"]
      
      dt3
    })
  )
  dt_po_asy2[, cutoff := rep(cutoffs, each = 3)]
  
  
  # Pseudo-Observations Bootstrap Adjusted
  dt_po_boot2 = rbindlist(
    lapply(res_po_boot2, function(x) {
      dt1 = data.table(
        method = rep("po_boot_adj", nrow(x)),
        variable = rownames(x),
        cutoff = NA_real_
      )
      dt2 = as.data.table(x)
      dt2[, z_boot := NULL]
      setnames(dt2, old = "pval_boot", new = "pval")
      dt3 = cbind(dt1, dt2)
      
      # Intercept is not of interest for us
      dt3 = dt3[variable != "(Intercept)"]
      
      dt3
    })
  )
  dt_po_boot2[, cutoff := rep(cutoffs, each = 3)]
  
  
  # All results
  dt_res = rbindlist(list(
    # Unadjusted
    dt_asy, dt_studperm, dt_po_asy, dt_po_boot,
    # Adjusted
    dt_po_asy2, dt_po_boot2
  ))
  
  saveRDS(dt_res, fs$path("thesis", "objects", "res_ovarian", ext = "rds"))
  
} else {
  # Just read results
  dt_res = readRDS(fs$path("thesis", "objects", "res_ovarian", ext = "rds"))
}



# Plot results ----

dtp = dt_res[variable == "group"]
dtp[, method := factor(
  method, levels = c("asy", "studperm", "po_asy", "po_boot", "po_asy_adj", "po_boot_adj"),
  labels = c("Asy", "Stud Perm", "PO", "PO Boot", "PO Adj", "PO Boot Adj")
)]
dtp[, cutoff := factor(
  cutoff, levels = c(15, 20, 25),
  labels = sprintf("t^`*` == %d", c(15, 20, 25))
)]


ggplot(dtp, aes(x = method, y = est, ymin = ci_lower, ymax = ci_upper)) +
  geom_point(size = 3) +
  geom_errorbar(linewidth = 1.1, width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.9, color = "#222222") +
  theme_bw() +
  facet_wrap(~ cutoff, labeller = label_parsed, scales = "free_x") +
  labs(
    x = "Method\n", y = "\nEstimate"
  ) +
  scale_x_discrete(limits = rev(levels(dtp$method))) +
  coord_flip()


# Table ----

# Obtain p-values and reshape the results for latex table
dtt = dt_res[variable == "group", .(method, cutoff, pval)]
dtt = dcast(dtt, cutoff ~ method, value.var = "pval")
setcolorder(dtt, neworder = c("cutoff", "asy", "studperm", "po_asy", "po_boot", "po_asy_adj", "po_boot_adj"))

# Also include log-rank test
pval_lr = survdiff(Surv(time, event) ~ group, data = dt)$pvalue
dtt[, lr := c(NA_real_, pval_lr, NA_real_)]

# Convert to percent
setj_percent(dtt, 2:8)

# NAs as empty cells
options(knitr.kable.NA = '')

# Table
kbl(
  dtt,
  format = "latex",
  digits = 2,
  booktabs = TRUE,
  centering = TRUE,
  escape = FALSE,
  col.names = c(r"($t^*$)", "Asy", "Perm", "PO1", "PO2", "PO1 Adj", "PO2 Adj", "LR")
) |>
  kable_styling(
    position = "center",
    font_size = 12
  ) |>
  # Borders
  column_spec(1, border_right = TRUE) |>
  column_spec(5, border_right = TRUE) |>
  # Make it more accessible using stripes options
  kable_styling(
    latex_options = "striped",
    stripe_index = 2,
    stripe_color = "#e9ecef"
  ) |>
  # Legend
  footnote(
    general = paste0(
      r"(\\textit{Abbreviations:} )",
      "Asy, asymptotic test; ", "Perm, studentized permutation test; ",
      "PO1, pseudo-observations; ",
      "PO2, pseudo-observations + bootstrap test; ",
      "LR, log-rank test; ",
      "Adj, adjusted."
    ),
    general_title = "",
    escape = FALSE,
    threeparttable = TRUE
  )


# Table 2 ----

# Obtain p-values and reshape the results for latex table
dtt = dt_res[variable == "group", .(method, cutoff, pval)]
dtt = dcast(dtt, method ~ cutoff, value.var = "pval")
setnames(dtt, old = as.character(c(15, 20, 25)), new = \(x) paste0("t", x))

# Change row order (based on methods)
dtt = dtt[c(1, 6, 2:5)]

# Also include log-rank test
pval_lr = survdiff(Surv(time, event) ~ group, data = dt)$pvalue
dtt = rbindlist(list(
  dtt,
  data.table(method = "lr", t15 = NA_real_, t20 = pval_lr, t25 = NA_real_)
))

# Convert to percent
setj_percent(dtt, 2:4)

# Format p-values for table
format_pval = \(x) fifelse(x < 0.01, "<0.01", as.character(round(x, 2)))
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
  col.names = c("Method", sprintf("$t^* = %d$", c(15, 20, 25)))
) |>
  kable_styling(
    full_width = TRUE
  ) |>
  column_spec(1, width = "1.5cm", border_right = TRUE)


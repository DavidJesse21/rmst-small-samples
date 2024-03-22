# Setup ----

options(box.path = "R")

box::use(
  data.table[...],
  ggplot2[...],
  survival[Surv, survfit, survdiff, coxph, cox_zph = cox.zph],
  eventglm[rmeanglm],
  withr[with_seed],
  fs,
  ggsurvfit[survfit2, ggsurvfit, add_censor_mark]
)

box::use(
  rmst/km[rmst_diff_test, rmst_diff_studperm],
  rmst/pseudo[pseudo_strat, pseudo_infjack, rmst_pseudo_test, rmst_pseudo_boot_test]
)

dt = survival::ovarian
setDT(dt)
setnames(dt, old = c("futime", "fustat", "rx"), new = c("time", "event", "group"))
dt[, group := group - 1L]
dt[, ecog.ps := factor(ecog.ps, levels = as.character(2:1))]

?ovarian


# EDA  ----

x = cox_zph(coxph(Surv(time, event) ~ group, data = dt, x = TRUE))
pval_gt = x$table[1, "p"]

ggsurvfit(
  survfit2(Surv(time, event) ~ group, data = dt),
  linewidth = 1.1
) +
  scale_color_manual(
    name = "Treatment",
    values = c("#E69F00", "#56B4E9"),
    labels = c("Control", "Experimental")
  ) +
  labs(x = "\nTime (Days)", y = "Survival probability\n") +
  theme(legend.position = "top") +
  scale_y_continuous(limit = c(0, 1)) +
  annotate(
    "text", x = 1000, y = 0.95,
    label = sprintf("Grambsch-Therneau test (p-value): %.2f%%", pval_gt * 100)
  )


if (!fs$file_exists(fs$path("thesis", "objects", "res_ovarian", ext = "rds"))) {
  
  # Methods ----
  
  cutoffs = c(500, 750, 1000)
  
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
  cutoff, levels = c(500, 750, 1000),
  labels = sprintf("t^`*` == %d", c(500, 750, 1000))
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


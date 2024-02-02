x = readLines("job25.log")
x[1:10]
x = x[6:length(x)]
x = head(x, -10)
length(unique(x))
y = table(x)
View(y)


# Packages and functions ----

options(box.path = "R")

box::use(
  fs,
  DBI[...],
  RSQLite[SQLite],
  withr[with_seed],
  data.table[...],
  parallel[mclapply]
)

box::use(
  simfuns2/scenarios[make_params],
  simfuns2/dgp[gen_surv_data],
  simfuns2/utils[trycatch_log]
)

# Get parameters for simulating the data
get_params = function(scenario.id, dir_sim = fs$path("simulation"), timeout = 60) {
  db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
  on.exit(dbDisconnect(db), add = TRUE)
  
  start_time = Sys.time()
  params = try(stop("init"), silent = TRUE)
  iter = 0
  
  while (inherits(params, "try-error")) {
    # Stop if it takes too long
    if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
      stop("Time limit elapsed.")
    }
    
    # Usual behaviour: get parameters
    params = try(
      dbGetQuery(
        db,
        sprintf("SELECT * FROM scenarios WHERE `scenario.id` = %d", scenario.id)
      ) |>
        make_params()
    )
    
    # Wait 1 second until next try except for first attempt
    if (iter > 0L) Sys.sleep(1)
    iter = iter + 1
  }
  
  return(params)
}


# Recreate data ----

params = get_params(25)
li_data = with_seed(25, .rng_kind = "Mersenne-Twister", {
  lapply(seq_len(5000), function(i) {
    do.call(gen_surv_data, params)
  })
})


# Function (step by step) ----

box::use(
  eventglm[rmeanglm],
  rmst/pseudo[pseudo_infjack, rmst_pseudo_boot_test],
  survival[Surv],
  sandwich[vcovHC]
)


get_coef_mat = function(m, vcov_type = "HC3", delta = 0) {
  if (inherits(m, "pseudoglm")) {
    class(m) = c("glm", "lm")
  }
  
  beta = coef(m)
  sigma = vcovHC(m, type = vcov_type)
  
  out = matrix(
    NA_real_, nrow = length(beta), ncol = 3L,
    dimnames = list(names(beta), c("est", "var_est", "tstat"))
  )
  out[, 1] = beta
  out[, 2] = diag(sigma)
  out[, 3] = (out[, 1] - delta) / sqrt(out[, 2])
  
  return(out)
}


m = rmeanglm(
  Surv(time, event) ~ trt, data = li_data[[4998]], time = 10,
  model.censoring = pseudo_infjack, formula.censoring = ~ trt
)

## Now I do everything step by step as in `rmst_pseudo_boot_test()` ----

set.seed(42)

# Results / test statistics based on original/full data set
beta = coef(m)
res_orig = get_coef_mat(m, "HC3")
# Check

# Collect information/specifications from original model for bootstrapping
dt = m$data
pseudo_fun = eval(m$call$model.censoring)
formula = m$formula
formula.censoring = eval(m$call$formula.censoring)
type = m$type
time = m$time
weights = unname(m$weights)
if (!all(weights == 1L)) stop("`boot()` not available for weighted regression yet.")
control = m$control
family = m$family
# Check

# Do the bootstrapping
boot = lapply(seq_len(2000), function(i) {
  tryCatch(
    {
      new_dt = dt[sample(1:.N, replace = TRUE)]
      new_y = pseudo_fun(formula, time, data = new_dt, type = type, formula.censoring = formula.censoring)
      new_x = model.matrix(formula, data = new_dt)
      new_m = glm.fit(new_x, new_y,
                      family = family,
                      mustart = rep(mean(new_y), length(new_y)),
                      intercept = TRUE, singular.ok = TRUE,
                      control = control)
      class(new_m) = c("glm", "lm")
      new_m$terms = terms(m)
      new_m$x = new_x
      
      # Get the coefficient matrix
      coefmat = get_coef_mat(new_m, "HC3", delta = beta)
      
      # For the bootstrap samples we only need the test statistics in the end
      return(coefmat[, "tstat", drop = TRUE])
    },
    error = \(e) NULL
  )
})
# (Check)

boot2 = Filter(Negate(is.null), boot)
boot2 = do.call(rbind, boot)
anyNA(boot2)

# repeat the above until we observe an NaN
# View(boot)

matrix(c(1:3, NA_real_), nrow = 2) |>
  anyNA()

check = vapply(boot, anyNA, logical(1)) |>
  sum()

boot[1:10]

c(1:2, NaN)

anyNA(c(1:2, NaN))

boot[[1]] |>
  anyNA()

test = boot[1:5]
test = c(test, list(NULL))
test = c(test, list(c("(Intercept)" = 0.2, "trt" = 1.2)))
test = c(test, list(c("(Intercept)" = 0.2, "trt" = NaN)))

Filter(\(x) !(is.null(x) | anyNA(x)), test)
Filter(Negate(is.null), test)

length(boot)
Filter(Negate(is.null), boot) |>
  length()

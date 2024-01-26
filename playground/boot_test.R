box::use(data.table[...])

dt = as.data.table(cars)
dt

m1 = lm(dist ~ speed, data = dt)
x1 = summary(m1)
x1
round(x1$coefficients["speed", 4] * 100, 5)
confint(m1)


# Approach 1 ----

set.seed(123)
li_boot = lapply(
  seq_len(2000),
  function(i) {
    idx = sample(1:nrow(dt), replace = TRUE)
    new_y = dt[idx, dist]
    lm(new_y ~ dt$dist)
  }
)

tstats1 = vapply(li_boot, function(m) {
  beta = coef(m)
  sigma = vcov(m)
  
  (beta[2]) / (sqrt(diag(sigma))[2])
}, numeric(1))


# Approach 2 ----

set.seed(123)
li_boot = lapply(
  seq_len(2000),
  function(i) {
    idx = sample(1:nrow(dt), replace = TRUE)
    update(m1, data = dt[idx])
  }
)

tstats2 = vapply(li_boot, function(m) {
  beta = coef(m)
  sigma = vcov(m)
  
  (beta[2] - coef(m1)[2]) / (sqrt(diag(sigma))[2])
}, numeric(1))


# Compare ----

hist(tstats1, freq = FALSE, breaks = 22)
curve(dnorm(x), from = -3, to = 3, add = TRUE, col = 20)

hist(tstats2, freq = FALSE, breaks = 22)
curve(dnorm(x), from = -3, to = 3, add = TRUE, col = 20)

quantile(tstats1, c(0.025, 0.975))
quantile(tstats2, c(0.025, 0.975))

quantile(abs(tstats1), 0.95)
quantile(abs(tstats2), 0.95)

var(tstats1)
var(tstats2)

# tstats_h0
hist(tstats_h0, freq = FALSE, breaks = 22)
curve(dnorm(x), from = -3, to = 3, add = TRUE, col = 20)

quantile(abs(tstats_h0), probs = 0.95)
quantile(tstats_h0, probs = 0.975)
quantile(tstats_h0^2, probs = 0.95) |>
  sqrt()

mu_beta0 = mean(vapply(li_boot, \(m) coef(m)[1], numeric(1)))



tstats_h0 = vapply(li_boot, function(m) {
  beta = coef(m)
  sigma = vcov(m)
  
  (beta[1] - mu_beta0) / (sqrt(diag(sigma))[1])
}, numeric(1))
hist(tstats_h0, freq = FALSE, breaks = 22)
curve(dnorm(x), from = -3, to = 3, add = TRUE, col = 20)
quantile(abs(tstats_h0), probs = 0.95)
quantile(tstats_h0, probs = 0.975)

x = li_boot[[6]]
names(x)
x$coefficients[2] / sqrt(diag(vcov(x)))[2]

summary(li_boot[[6]])

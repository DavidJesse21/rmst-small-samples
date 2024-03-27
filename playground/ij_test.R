set.seed(42)
x = rnorm(100)

pseudomean = function(x) {
  n = length(x)
  mu = mean(x)
  loo = vapply(seq_len(n), \(i) mean(x[-i]), numeric(1))
  
  mu + (n - 1) * (mu - loo)
}

mean(x)
pseudomean(x) |> mean()

ij_pseudomean = function(x) {
  n = length(x)
  mu = mean(x)
  mu + n
}

loo_mean = function(x) {
  vapply(seq_along(x), \(i) mean(x[-i]), numeric(1))
}
jack = loo_mean(x)

loo_mean_approx = function(x) {
  n = length(x)
  out = x
  add = vapply(seq_len(n), \(i) sum(x[-i]), numeric(1)) / n
  out + add
}
jack2 = loo_mean_approx(x)  

mean(x)
mean(jack)
mean(jack2)

sd(x) / 100
sd(jack)

loo_mean_approx = function(x) {
  n = length(x)
  out = x
  add = vapply(seq_len(n), \(i) sum(x[-i]), numeric(1)) / n
  out + add
}
jack2 = loo_mean_approx(x)  

jack2 


options(box.path = "R")

box::use(
  survival[Surv, survfit],
  rmst/pseudo[pseudo_strat, pseudo_infjack],
  data.table[...]
)

load("data/Hellmann.Rdata")
dt = data
setDT(dt)

dt

dt = survival::ovarian
setDT(dt)

plot(survfit(Surv(time, event) ~ group, data = dt), col = 19:20)

box::help(pseudo_infjack)

po1 = pseudo_strat(
  Surv(time, event) ~ group, time = 10, data = dt,
  formula.censoring = ~ group
)

po2 = pseudo_infjack(
  Surv(time, event) ~ group, time = 10, data = dt,
  formula.censoring = ~ group
)

cbind(po1, po2)
mean(po1)
mean(po2)
var(po1)
var(po2)




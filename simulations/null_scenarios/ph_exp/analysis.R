options(box.path = "R")

box::use(
  # Read results
  fs,
  qs,
  # Data wrangling
  data.table[...],
  # Plotting
  ggplot2[...],
  ggsci[scale_color_jco, pal_jco]
)


# Read results
dt = qs$qread(
  fs$path("simulations", "null_scenarios", "ph_exp", "results", ext = "qs")
)


# Analysis ----

num_sims = dt[1, length(unlist(results))]

## Empirical type 1 error ----

type1_error = function(x, alpha = 0.05) {
  if (is.list(x)) x = unlist(x)
  
  reject_h0 = abs(x) > qnorm(1 - alpha/2)
  sum(reject_h0) / length(x)
}

dt_err = dt[, .(type1_error = type1_error(results)), by = num_samples]
dt_err[, `:=`(
  lower95 = type1_error - qnorm(0.975) * sqrt(type1_error * (1 - type1_error) / num_sims),
  upper95 = type1_error + qnorm(0.975) * sqrt(type1_error * (1 - type1_error) / num_sims)
)]
dt_err

# Compare seemingly similar results (check/avoid mistakes made during simulation)
all.equal(
  dt[num_samples == 25, unlist(results)],
  dt[num_samples == 100, unlist(results)]
)
all.equal(
  dt[num_samples == 50, unlist(results)],
  dt[num_samples == 250, unlist(results)]
)


## Test statistic distribution ----

# Unnest data.table
dt2 = dt[, .(results = unlist(results)), by = num_samples]

# Density plots
p_dens = ggplot(dt2, aes(x = results)) +
  geom_density(aes(color = "Empirical"), linewidth = 1, adjust = 1.5) +
  geom_function(fun = dnorm, aes(color = "Standard normal"), linewidth = 1) +
  facet_wrap(~ factor(num_samples)) +
  theme_bw() +
  xlim(c(-4, 4)) +
  labs(x = "x", y = "Density") +
  scale_color_manual(
    name = "Distributions",
    values = c("Empirical" = pal_jco()(1), "Standard normal" = "black")
  ) +
  theme(legend.position = "top")

p_dens


# (e)CDF plots
p_cdf = ggplot(dt2, aes(x = results)) +
  stat_ecdf(aes(color = "Empirical"), geom = "step", linewidth = 1) +
  geom_function(fun = pnorm, aes(color = "Standard normal"), linewidth = 1) +
  facet_wrap(~ num_samples) +
  theme_bw() +
  scale_color_manual(
    name = "Distributions",
    values = c("Empirical" = pal_jco()(1), "Standard normal" = "black")
  ) +
  theme(legend.position = "top") +
  labs(x = "x", y = "CDF")

p_cdf

# Investigate lower/upper tails and mid part
p_cdf +
  coord_cartesian(xlim = c(-3, -1.5), ylim = c(0, 0.05)) +
  geom_hline(yintercept = 0.025, linetype = "dashed")
p_cdf +
  coord_cartesian(xlim = c(1.5, 3), ylim = c(0.95, 1)) +
  geom_hline(yintercept = 0.975, linetype = "dashed")
p_cdf +
  coord_cartesian(xlim = c(-0.75, 0.75), ylim = c(0.2, 0.8))

options(box.path = "R")

box::use(
  niconvert = nimargin/convert
)

box::help(niconvert$drmst_to_hr)

# HR margin
theta = 1.2
# Rate parameter in control arm
lambda0 = 0.5
# RMST cutoff
tau = 10
# DRMST margin conversion
delta = niconvert$hr_to_drmst(theta, lambda0, tau)
delta
# Conversion of delta to HR margin should (approximately) equal theta
niconvert$drmst_to_hr(delta, lambda0, tau)

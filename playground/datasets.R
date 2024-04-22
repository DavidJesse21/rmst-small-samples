library(KMsurv)
library(survival)

data(aids)

dt = survival::diabetic
names(dt)

m = survfit(Surv(time, status) ~ trt, data = dt)
plot(m)

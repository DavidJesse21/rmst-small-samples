dt = bt$getJobTable()

dt[2, time.running]
bt$getLog(2)

dt

dt[, summary(as.numeric(time.running))]

dt$time.running |> class()

dt[1, time.running] |>
  as.numeric()

bt$showLog(1)



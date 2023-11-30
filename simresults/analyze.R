options(box.path = "R")

box::use(
  fs,
  bt = batchtools,
  data.table[...]
)

reg = bt$loadRegistry(
  fs$path("simresults", "registry"),
  work.dir = getwd()
)

test = bt$reduceResultsDataTable()
test[10, result]

x = vapply(
  1:10,
  function(i) {
    test[i, result][[1]][3, is.na(error)]
  },
  logical(1)
) 

test[2, result][[1]][4, error]

x
sum(x)
mean(x)

bt$getJobPars()[1, prob.pars]

reg$problems
test[2, result][[1]][3, !is.na(error)]
#[, is.na(error)]

bt$getLog(1)

bt$getDefaultRegistry()

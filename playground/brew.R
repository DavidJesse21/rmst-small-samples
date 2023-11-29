# Just to check how brew package works and if my template seems valid

box::use(
  brew[brew],
  fs
)

log.file = fs$path("test.log")
job.name = "testjob"
array.jobs = FALSE
uri = "something"

resources = list(
  mail.type = "BEGIN,END",
  mail.user = "david.jesse@stud.uni-goettingen.de",
  walltime = "00:05:00",
  ncpus = 1,
  memory = 1234
)

box::use(
  fs,
  utils[txtProgressBar, setTxtProgressBar],
  brew[brew]
)


#' @export
submit_jobs = function(sim_resources = list(),
                       slurm_resources = list(),
                       constants = list(),
                       dir_sim = fs$path("simulation"),
                       template_slurm = fs$path(dir_sim, "templates", "slurm-gwdg", ext = "tmpl"),
                       template_r = fs$path(dir_sim, "templates", "run_r", ext = "tmpl"),
                       progress = TRUE,
                       submit = TRUE) {

  # Extract jobs/scenarios that we need to iterate over
  if (is.null(sim_resources$scenario.ids)) {
    # Connect to simulation database
    db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
    on.exit(dbDisconnect(db))
    njobs = dbGetQuery(db, "SELECT COUNT(`scenario.id`) FROM scenarios")[[1]]
    jobs = 1:njobs
  } else {
    jobs = sim_resources$scenario.ids
  }
  
  # Make `ncpus` available to `run_sim()` as well
  sim_resources$ncpus = slurm_resources$ncpus
  
  # Save `sim_resources` to disk
  file_sim_resources = fs$path(dir_sim, "registry", "sim_resources", ext = "rds")
  saveRDS(sim_resources, file_sim_resources)
  # on.exit(fs$file_delete(file_sim_resources))
  
  # Save `constants` to disk
  file_constants = fs$path(dir_sim, "registry", "constants", ext = "rds")
  saveRDS(constants, file_constants)
  
  # Print some information...
  message(sprintf("About to submit %d jobs...\n", length(jobs)))
  
  # Track progress
  if (progress) pb = txtProgressBar(min = 0L, max = length(jobs), style = 3)
  
  # Submit jobs
  for (job in jobs) {
    job.name = paste0("rmst", job)
    log.file = fs$path(dir_sim, "registry", "logs", paste0("job", job), ext = "log")
    fs$file_create(log.file)
    
    # Brew R script
    r.file = fs$path(dir_sim, "registry", "temp", paste0("run", job), ext = "R")
    brew(template_r, r.file)
    
    # Brew bash script (slurm)
    sh.file = fs$path(dir_sim, "registry", "temp", paste0("submit", job), ext = "sh")
    brew(template_slurm, sh.file)
    
    # Submit job
    if (submit) system2("sbatch", sh.file, stdout = NULL)
    
    # Track progress
    if (progress) setTxtProgressBar(pb, job)
  }
  
  if (progress) close(pb)
  
  # if (submit) {
  #   temp_files = fs$dir_ls(fs$path(dir_sim, "registry", "temp"))
  #   on.exit(fs$file_delete(temp_files)) 
  # }
  
  # Return number of submitted jobs invisibly
  return(invisible(jobs))
}

box::use(
  fs,
  utils[txtProgressBar, setTxtProgressBar],
  brew[brew]
)


#' Submit simulation jobs
#' 
#' @param sim_resources (`list()`)\cr
#'   A list of resource specifications for each single simulation:
#'   * `algos` (`character()`): Algorithms to evaluate
#'   * `num_sims` (`numeric(1)`): Number of simulation replications, should always be 5000
#'   * `scenario.ids` (`numeric()`): IDs of simulations/scenarios to evaluate
#' @param slurm_resources (`list()`)\cr
#'   A list of resource specifications for slurm.
#'   Can contain any valid slurm parameter, provided that it matches with the template.
#'   But in my case the following parameters should be set:
#'   * `mail.type` (`character(1)`)
#'   * `mail.user` (`character(1)`)
#'   * `walltime` (`character(1)`)
#'   * `partition` (`character(1)`)
#'   * `ncpus` (`numeric(1)`)
#'   * `memory` (`character(1)`)
#' @param constants (`list()`)\cr
#'   A list of constant parameters for the data-generating functions and the algorithms.
#'   Current entries include:
#'   * `cutoff` (`numeric(1)`): The restriction time
#'   * `alpha` (`numeric(1)`): The significance level
#'   * `var_method_asy` (`character(1)`): The variance method/formula for the asymptotic test
#'   * `var_method_studperm` (`character(1)`): The variance method/formula for the permutation test
#'   * `num_samples_studperm` (`numeric(1)`): The number of samples/replications for the permutation test
#'   * `num_samples_boot` (`numeric(1)`): The number of bootstrap samples for the IJ pseudo-observations approach
#' @param wait (`numeric(1)`)\cr
#'   Time in seconds to wait between submitting two jobs.
#'   This might be useful to prevent errors due to database locking and by that failed jobs.
#' @param db_timeout (`numeric(1)`)\cr
#'   Timeout in seconds for database operations.
#'   If an operation does not succeed within this time an error will be thrown.
#'   Will be passed to `run_sim()`.
#' @param dir_sim (`character(1)`)\cr
#'   The directory of the simulation study.
#' @param template_slurm (`character(1)`)\cr
#'   Path to the template file for the slurm bash scripts.
#' @param template_r (`character(1)`)\cr
#'   Path to the template file for the R scripts to be executed.
#' @param progress (`logical(1)`)\cr
#'   Whether to show/enable a progress bar during submitting the jobs.
#' @param submit (`logical(1)`)\cr
#'   If `TRUE` the bash scripts will be executed right away, i.e. the jobs will be submitted right away.
#'   Otherwise, the bash scripts will be "brewed" (created) but not executed.
#' 
#' @export
submit_jobs = function(sim_resources = list(),
                       slurm_resources = list(),
                       constants = list(),
                       wait = 1,
                       db_timeout = 60,
                       dir_sim = fs$path("simulation"),
                       template_slurm = fs$path(dir_sim, "templates", "slurm-gwdg", ext = "tmpl"),
                       template_r = fs$path(dir_sim, "templates", "run_r", ext = "tmpl"),
                       progress = TRUE,
                       submit = TRUE) {

  # Extract jobs/scenarios that we need to iterate over
  if (is.null(sim_resources$scenario.ids)) {
    # Connect to simulation database
    db = dbConnect(SQLite(), fs$path(dir_sim, "registry", "simdb", ext = "db"))
    on.exit(dbDisconnect(db), add = TRUE)
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
  
  # Save `constants` to disk
  file_constants = fs$path(dir_sim, "registry", "constants", ext = "rds")
  saveRDS(constants, file_constants)
  
  # Print some information...
  message(sprintf("About to submit %d jobs...\n", length(jobs)))
  
  # Track progress
  if (progress) pb = txtProgressBar(min = 0L, max = length(jobs), style = 3)
  
  # Submit jobs
  for (job in jobs) {
    # Wait a bit to prevent database locking
    Sys.sleep(wait)
    
    # Job name / identifier for Slurm
    job.name = paste0("rmst", job)
    
    # Create file for logs (if it does not exist yet)
    log.file = fs$path(dir_sim, "registry", "logs", paste0("job", job), ext = "log")
    if (!fs$file_exists(log.file)) fs$file_create(log.file)
    
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
  
  if (progress) on.exit(close(pb), add = TRUE)
  
  
  # Return number of submitted jobs invisibly
  return(invisible(jobs))
}

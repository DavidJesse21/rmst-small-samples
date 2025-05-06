# Small sample tests for comparing restricted mean survival times

This repository contains code accompanying the paper [_Comparing restricted mean survival times in small sample clinical trials using pseudo-observations_](https://arxiv.org/abs/2408.15607).


## Structure & organization

- `/R`: all relevant R functions, organized as [`{box}`](https://klmr.me/box/) modules
  - `/plotting`: functions for creating plots of survival functions
  - `/rmst`: functions for estimating and testing differences in restricted mean survival times including all methods investigated in our paper
  - `/simfuns`: legacy code for simulation study, not used
  - `/simfuns2`: functions for setting up the simulation study, submitting jobs via [slurm](https://slurm.schedmd.com/documentation.html), and analyzing the results
- `/data`: some example data sets
- `/paper`: code for creating the figures and tables in our paper; note that the file with the raw simulation results is required for this
- `/simulation`: code for submitting all jobs, i.e. running the simulation study
- `/playground`: mostly useless R code for experimentation and interactive testing only


## Reproducing the results

Reproducing the simulation results from our paper is possible.
However, you need access to an HPC system based on slurm (or adjust the code substantially).
To achieve this, you need to do the following:

1. Clone the repository and `renv::restore()` the project.
2. On your local computer, run the file `simulation/hpcenv/local.R` to get the list of packages required for the simulation as an rds-file.
3. On the remote system from which you will access the HPC system, first upload all necessary folders and files, including
  - `/R` (`/R/plotting` and `/R/simfuns` could be excluded)
  - `/simulation`
    - `/hpcenv`
    - `/templates`
    - `/01-setup.R`
    - `/02-submit.R`
  - `.Rprofile`
  - `renv.lock`
4. On the remote system, run the file `simulation/hpcenv/remote.R` to setup the remote R environment, in particular installing all required dependencies.
5. On the remote system, run the files `simulation/01-setup.R` and `simulation/02-submit.R` in that order. This will set up the simulation environment and submit all jobs.
Note, however, that some jobs can fail for various reasons, and therefore resubmitting some jobs can be required.

The results are stored in a local database located in the directory `/simulation/registry`.
The module `/R/simfuns2/post_submit.R` contains utility functions for collecting and saving the results as an rds-file which can then be downloaded.


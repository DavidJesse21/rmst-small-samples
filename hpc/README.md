# High Performance Computing

Copy the following files and/or directories to the remote system:

- `hpc`
- `.Rprofile`
- `renv.lock`
- `renv` (the following files only)
  - `activate.R`
  - `settings.json`
  - (`.gitignore`)
- `simulations/registry`
  - maybe even complete `simulations` directory
  
On the remote system execute the following program: `hpc/remote.R`<br>
This will:

1. Install `{renv}`
2. Install those packages only that are required for running the simulations

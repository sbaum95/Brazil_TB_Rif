# Author: Sarah Baum
# Created: 2024-03-23
# Updated:

# Description: Runs models, gets fitted values and uncertainity results, and compiles results 

source("code/dependencies.R")

tictoc::tic()
source("code/02_run_models/01_run_models.R")
tictoc::toc()

tictoc::tic()
source("code/02_run_models/02_get_fitted_values.R")
tictoc::toc()

# Not sure why this isn't running through lapply?
tictoc::tic()
source("code/02_run_models/03_get_uncertainity_intervals.R")
tictoc::toc()

tictoc::tic()
source("code/02_run_models/04_compile_results.R")
tictoc::toc()


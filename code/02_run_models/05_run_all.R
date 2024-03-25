# Author: Sarah Baum
# Created: 2024-03-23
# Updated: 2024-03-25

# Description: Runs models, gets fitted values and uncertainity results, and compiles results 

source("code/dependencies.R")

## drop sex from create_an dataset
tictoc::tic()
source("code/02_run_models/01_run_models.R") # ~ 5 hours for all models
tictoc::toc()

tictoc::tic()
source("code/02_run_models/02_get_fitted_values.R") # 8 minutes
tictoc::toc()

# Not sure why this isn't running through lapply?
tictoc::tic()
source("code/02_run_models/03_get_uncertainity_intervals.R") # 1.5 hours
tictoc::toc()

tictoc::tic()
source("code/02_run_models/04_compile_results.R")
tictoc::toc()


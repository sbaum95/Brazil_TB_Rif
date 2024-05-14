# Author: Sarah Baum
# Created: 2024-03-23
# Updated: 2024-03-25

# Description: Runs models, gets fitted values and uncertainity results, and compiles results 

source("code/dependencies.R")
library(rlang)


# Select analytic datasets to load
load("data/mdf_new_ind_tmp_2024.Rdata")
load("data/mdf_prev_ind_tmp_2024.Rdata")


# Set up paths to store output
file_version <- "2024"

# Select models to run
models_to_run <- c("sp_2015", "sens_1")


# 1. Run models --------------------------------------------------------------
tictoc::tic()

# fitted_models <- list() 

source("code/02_run_models/01_run_models.R") # ~ 5 hours for all models


save(fitted_models, file = paste0("fitted_models_", file_version, ".Rdata"))

tictoc::toc()



# 2. Get fitted values ----------------------------------------------------
tictoc::tic()

source("code/02_run_models/02_get_fitted_values.R") # 8 minutes

save(fitted_values, file = paste0("output/fitted_values_", file_version, ".Rdata"))

tictoc::toc()


# Get uncertainty intervals -----------------------------------------------

tictoc::tic()

source("code/02_run_models/03_get_uncertainity_intervals.R") # 1.5 hours, Not sure why this isn't running through lapply?

save(intervals, file = paste0("output/intervals_", file_version, ".Rdata"))

tictoc::toc()




# Compile results ---------------------------------------------------------

## Load observed data ---------------------------------------------------------------
load(paste0("data/sinan_tmp_", file_version,".Rdata"))

sinan_xpert <- sinan_tmp



## Output compiled results -------------------------------------------------
tictoc::tic()

source("code/02_run_models/04_compiled_results.R") # 30 minutes

save(compiled_results, file = paste0("output/compiled_results_", file_version, ".Rdata"))

tictoc::toc()






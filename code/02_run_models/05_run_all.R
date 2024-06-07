# Author: Sarah Baum
# Created: 2024-03-23
# Updated: 2024-05-26

# Description: Runs models, gets fitted values and uncertainty results, and compiles results 

source("code/dependencies.R")

# Set up paths to store output and load
file_version_load <- "20240604"
file_version_save <- "20240604"

# Select analytic datasets to load
load(paste0("data/mdf_new_ind_tmp_", file_version_load,".Rdata"))
load(paste0("data/mdf_prev_ind_tmp_", file_version_load,".Rdata"))

# Select models to run
models_to_run <- c("sp_2014_new", "sp_2014_prev","sp_2017_new", "sp_2017_prev", "sens_1_new", "sens_1_prev", "sens_2_new", "sens_2_prev")

# Store fitted models
fitted_models <- list() # To store model output (if not adding models to existing fitted values)


# 1. Run models --------------------------------------------------------------
tictoc::tic()

source("code/02_run_models/01_run_models.R") # ~ 11 hours for all models

save(fitted_models, file = paste0("output/fitted_models_", file_version_save, ".Rdata"))

tictoc::toc()


# 2. Get fitted values ----------------------------------------------------
tictoc::tic()

source("code/02_run_models/02_get_fitted_values.R") # 30 minutes

save(fitted_values, file = paste0("output/fitted_values_", file_version_save, ".Rdata"))

tictoc::toc()


# Get uncertainty intervals -----------------------------------------------

tictoc::tic()

source("code/02_run_models/03_get_uncertainty_intervals.R") # 2 hours

save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

tictoc::toc()


# Compile results ---------------------------------------------------------

## Load observed data ---------------------------------------------------------------
load(paste0("data/sinan_tmp_", file_version_load,".Rdata"))

sinan_xpert <- sinan_tmp

model_list <- names(fitted_values)#[grepl("sens_", names(fitted_values))]

## Output compiled results -------------------------------------------------
tictoc::tic()

source("code/02_run_models/04_compiled_results.R") # 30 minutes

save(compiled_results, file = paste0("output/compiled_results_", file_version_save, ".Rdata"))

tictoc::toc()
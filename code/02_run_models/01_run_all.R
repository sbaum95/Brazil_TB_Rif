# Description: 
# 1. Runs all models 
# 2. Generates fitted values 
# 3. Generates uncertainty results
# 4. Compiles modeled and observed results 

source("code/dependencies.R")



# 1. Run models --------------------------------------------------------------
tictoc::tic()


# Store fitted models
fitted_models <- list() # To store model output (if not adding models to existing fitted values)

source("code/02_run_models/run_models.R")           

tictoc::toc()



# 2. Get fitted values ----------------------------------------------------
tictoc::tic()

# Store fitted values 
fitted_values <- list()

source("code/02_run_models/get_fitted_values.R")

# Selects models to output fitted results 
model_name <- names(fitted_models)

# Get fitted values 
fitted_values_list <- lapply(model_name, get_fitted_values)
fitted_values <- setNames(fitted_values_list, model_name)

save(fitted_values, file = paste0("output/fitted_values_", file_version_save, ".Rdata"))

tictoc::toc()


# Get uncertainty intervals -----------------------------------------------

tictoc::tic()

source("code/02_run_models/get_uncertainty_intervals.R")

# Store intervals
intervals <- list()

# Get intervals for each model
intervals[["sp_2014_new"]] <- get_intervals(model_name = "sp_2014_new")
save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

intervals[["sp_2017_new"]] <- get_intervals(model_name = "sp_2017_new")
save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

intervals[["sp_2014_prev"]] <- get_intervals(model_name = "sp_2014_prev")
save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

intervals[["sp_2017_prev"]] <- get_intervals(model_name = "sp_2017_prev")
save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

intervals[["sens_1_new"]] <- get_intervals(model_name = "sens_1_new")
save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

intervals[["sens_1_prev"]] <- get_intervals(model_name = "sens_1_prev")
save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

intervals[["sens_2_new"]] <- get_intervals(model_name = "sens_2_new")
save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

intervals[["sens_2_prev"]] <- get_intervals(model_name = "sens_2_prev")
save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

save(intervals, file = paste0("output/intervals_", file_version_save, ".Rdata"))

tictoc::toc()


# Compile results ---------------------------------------------------------

## Load observed data ---------------------------------------------------------------
load(paste0("data/sinan_tmp_", file_version_load,".Rdata"))

model_list <- names(fitted_values)

## Output compiled results -------------------------------------------------
tictoc::tic()

source("code/02_run_models/compile_results.R")

save(compiled_results, file = paste0("output/compiled_results_", file_version_save, ".Rdata"))

tictoc::toc()
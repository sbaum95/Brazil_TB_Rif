# Author: Sarah Baum
# Created: 2024-03-23
# Updated: 2024-03-25

# Description: Runs models, gets fitted values and uncertainity results, and compiles results 

source("code/dependencies.R")
library(rlang)

path <- "output"
file_ending <- "tmp.Rdata"



# load analytic datasets --------------------------------------------------

load(here::here("data/mdf_new_ind_tmp.Rdata"))
load(here::here("data/mdf_prev_ind_tmp.Rdata"))


# Prep for saving results -------------------------------------------------


# 1. Run models --------------------------------------------------------------
tictoc::tic()

model_file_name <- paste0("fitted_models_",file_ending)

source("code/02_run_models/01_run_models.R") # ~ 5 hours for all models
## Make it so you can control which models you want to be run from here
tictoc::toc()


# 2. Get fitted values ----------------------------------------------------
tictoc::tic()

fitted_file_name <- paste0("fitted_values_", file_ending)

source("code/02_run_models/02_get_fitted_values.R") # 8 minutes

tictoc::toc()


# Get uncertainty intervals -----------------------------------------------
# load("output/fitted_models_tmp.rdata")

intervals_file_name <- paste0("intervals_", file_ending)

tictoc::tic()
load(paste0(model_file_name))

source("code/02_run_models/03_get_uncertainity_intervals.R") # 1.5 hours

# Not sure why this isn't running through lapply?
tictoc::toc()




# Compile results ---------------------------------------------------------

## Load data ---------------------------------------------------------------
load("data/sinan_xpert_tmp.rdata") 

sinan_xpert <- sinan_tmp 

results_file_name <- paste0("compiled_results_", file_ending)

## Output compiled results -------------------------------------------------
tictoc::tic()

source("code/02_run_models/04_compiled_results.R") # 30 minutes

tictoc::toc()






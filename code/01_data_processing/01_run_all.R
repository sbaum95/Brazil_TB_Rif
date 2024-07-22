# Description: Cleans SINAN data and prepares datasets for models. 

# Clean sinan ----------------------------------------------------

tictoc::tic()

source("code/01_data_processing/clean_sinan.R")

sinan_tmp <- load_and_clean_sinan()

tictoc::toc()


# Create analytic datasets ------------------------------------------------
tictoc::tic()

source("code/01_data_processing/create_analytic_dataset.R")

# Create dataset for new cases
mdf_new_ind <- create_analytic_dataset(
  first_quarter = "2014-01-01",
  last_quarter = "2023-10-01",
  covariates = covariates_to_pull,
  tratamento = "new"
)

# Create dataset for previous cases
mdf_prev_ind <- create_analytic_dataset(
  first_quarter = "2014-01-01",
  last_quarter = "2023-10-01",
  covariates = covariates_to_pull,
  tratamento = "prev"
)

tictoc::toc()


# Write files --------------------------------------------------------------
save(sinan_tmp, file = paste0("data/sinan_tmp_", file_version_save, ".Rdata"))
save(mdf_new_ind, file = paste0("data/mdf_new_ind_tmp_", file_version_save, ".Rdata"))
save(mdf_prev_ind, file = paste0("data/mdf_prev_ind_tmp_", file_version_save, ".Rdata"))

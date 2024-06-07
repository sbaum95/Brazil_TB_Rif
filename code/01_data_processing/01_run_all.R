# Author: Sarah Baum
# Created: 2024-03-23

# Description: Cleans SINAN data and then prepares datasets for models. Manually
# update version of sinan, covariates and years to run, and file version

source("code/dependencies.R")

# Load sinan and set covariates  ----------------------------------------------------

sinan <- foreign::read.dta("data/base_abr_2024_final.dta")

covariates_to_pull <- c(
  "state", "state_nm", "id_mn_resi_clean", "diag_qrt", "diag_yr", "tested", "result",
  "sex", "age_cat", "race", "tratamento", "hiv_status", "lat", "lon",
  "pop_imig", "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga",
  "agravdiabe", "cs_escol_n", "health_unit"
)

years_to_pull <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

file_version <- "20240604"


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
save(sinan_tmp, file = paste0("data/sinan_tmp_", file_version, ".Rdata"))
save(mdf_new_ind, file = paste0("data/mdf_new_ind_tmp_", file_version, ".Rdata"))
save(mdf_prev_ind, file = paste0("data/mdf_prev_ind_tmp_", file_version, ".Rdata"))

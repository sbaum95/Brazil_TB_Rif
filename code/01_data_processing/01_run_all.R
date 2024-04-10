# Author: Sarah Baum
# Created: 2024-03-23
# Updated: 2024-03-25

# Description: Cleans sinan data and then prepares datasets for models 

source("code/dependencies.R")


# Load and clean sinan ----------------------------------------------------

tictoc::tic()

source("code/01_data_processing/clean_sinan.R") 

sinan_tmp <- load_and_clean_sinan()

tictoc::toc()

## 6 people had a missing lat and long 


# Create analytic datasets ------------------------------------------------
tictoc::tic()

source("code/01_data_processing/create_analytic_dataset.R")

# Create dataset for new cases
mdf_new_ind <- create_analytic_dataset(
  first_quarter = "2014-01-01",
  last_quarter = "2020-01-01",
  covariates = c(
    "state", "state_nm", "id_mn_resi", "diag_qrt", "diag_yr", "tested", "result", 
    "sex", "age_cat", "tratamento", "hiv_status", "health_unit", "lat", "lon",
    "pop_imig", "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga", 
    "agravdiabe", "cs_escol_n"
  ),
  tratamento = "new"
)

# Create dataset for previous cases
mdf_prev_ind <- create_analytic_dataset(
  first_quarter = "2014-01-01",
  last_quarter = "2020-01-01",
  covariates = c(
    "state", "state_nm", "id_mn_resi", "diag_qrt", "diag_yr", "tested", "result", 
    "sex", "age_cat", "tratamento", "hiv_status", "health_unit", "lat", "lon",
    "pop_imig", "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga", 
    "agravdiabe", "cs_escol_n"
  ),
  tratamento = "prev"
)

tictoc::toc()




# write files --------------------------------------------------------------
save(sinan_tmp, file = "data/sinan_xpert_tmp.Rdata")
save(mdf_new_ind, file = "data/mdf_new_ind_tmp.Rdata")
save(mdf_prev_ind, file = "data/mdf_prev_ind_tmp.Rdata")

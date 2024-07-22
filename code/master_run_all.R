
source("code/dependencies.R")

# Set up paths to store output and load
file_version_load <- "20240721"
file_version_save <- "20240721"

sinan <- foreign::read.dta("data/base_abr_2024_final.dta")

covariates_to_pull <- c(
  "state", "state_nm", "id_mn_resi_clean", "diag_qrt", "diag_yr", "tested", "result",
  "sex", "age_cat", "race", "tratamento", "hiv_status", "lat", "lon",
  "pop_imig", "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga",
  "agravdiabe", "cs_escol_n", "health_unit"
)

years_to_pull <- c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)



# Clean SINAN and create analytic datasets ------------------------------------------------
source("code/01_data_processing/01_run_all.R")



# Run models and compile results ------------------------------------------
# Select models to run
models_to_run <- c(
  "sp_2017_new",  # Main: 2017-2023 new cases
  "sp_2017_prev", # Main: 2017-2023 previously treated cases
  "sp_2014_new",  # Sensitivity (Time period): 2014-2023 new cases
  "sp_2014_prev", # Sensitivity (Time period): 2014-2023 previously treated cases
  "sens_1_new",   # Sensitivity (Additional covariates): 2017-2023 new cases
  "sens_1_prev",  # Sensitivity (Additional covariates): 2017-2023 previously treated cases
  "sens_2_new",   # Sensitivity (Time varying selection): 2017-2023 new cases
  "sens_2_prev")  # Sensitivity (Time varying selection): 2017-2023 previously treated cases

source("code/02_run_models/01_run_all.R")



# Create figures and tables -----------------------------------------------
source("code/03_make_figures_and_tables/01_run_all.R")


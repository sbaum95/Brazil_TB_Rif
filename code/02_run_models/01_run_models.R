# Author: Sarah Baum
# Created: 2024-03-22
# Updated:

# Description: Run all models



# load analytic datasets --------------------------------------------------
source("code/dependencies.R")

load(here::here("data/mdf_new_ind.Rdata"))
load(here::here("data/mdf_prev_ind.Rdata"))

load("output/fitted_models.Rdata")

# Create model functions ---------------------------------------------------
## 




## Create time trend model with patient and municipality covariates
run_tt_model <- function(name, data, outcome, covariates, k) {
  
  # Prepare data 
  model_df <- data %>% 
    group_by(across(all_of(covariates)), result, time, diag_qrt, state, id_municip) %>% 
    count() %>% 
    pivot_wider(names_from = "result", values_from = "n") %>%
    rename(neg = "Negative",
           pos = "Positive",
           miss = "Missing") %>%
    mutate(Negative = if_else(is.na(neg), 0, neg),
           Positive = if_else(is.na(pos), 0, pos),
           Miss = if_else(is.na(miss), 0, miss)) %>% 
    mutate(cases = Negative + Positive + Miss, 
           pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
           obs_pct_positive = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive))
    ) %>% 
    dplyr::select(-c(miss, neg, pos))
  
  
  # Write out model
  formula_string <- paste(outcome, "~", paste(covariates, collapse = " + "), "+ s(state, bs = 're') + s(time, k =", paste(k), ") + s(time, by = state, id = 1, k =", paste(k), ")")

  # Run model
  model_object <- bam(as.formula(formula_string),
    data = model_df,
    family = binomial(link = "logit"),
    method = "REML"
  )

  return(list(model_object, 
              model_df))
}


run_sp_model <- function(name, data, outcome, covariates, latitude, longitude, k_t, k_sp) {
  
  # Prepare data 
  model_df <- data %>% 
    group_by(across(all_of(covariates)), result, time, diag_qrt, state, lat, lon) %>% 
    count() %>% 
    pivot_wider(names_from = "result", values_from = "n") %>%
    rename(neg = "Negative",
           pos = "Positive",
           miss = "Missing") %>%
    mutate(Negative = if_else(is.na(neg), 0, neg),
           Positive = if_else(is.na(pos), 0, pos),
           Miss = if_else(is.na(miss), 0, miss)) %>% 
    mutate(cases = Negative + Positive + Miss, 
           pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
           obs_pct_positive = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive))
    ) %>% 
    dplyr::select(-c(miss, neg, pos))
  
  # Write out model
  formula_string <- paste(outcome, "~", paste(covariates, collapse = " + "), "+ s(state, bs = 're') + s(time, k =", paste(k_t),") + s(time, by = state, id = 1, k =", paste(k_t), ")  + s(",paste(latitude, longitude, sep = ","), ", k =", paste(k_sp),")")
  
  # Run model
  model_object <- bam(as.formula(formula_string),
                      data = model_df,
                      family = binomial(link = "logit"),
                      method = "REML"
  )
  
  return(list(model_object, 
              model_df))
}


run_sel_model <- function(name, data, outcome, covariates, latitude, longitude, k_t, k_sp) {
  
  # Prepare data 
  model_df <- data %>% 
    group_by(across(all_of(covariates)), result, time, diag_qrt, state, lat, lon) %>% 
    count() %>% 
    pivot_wider(names_from = "result", values_from = "n") %>%
    rename(neg = "Negative",
           pos = "Positive",
           miss = "Missing") %>%
    mutate(Negative = if_else(is.na(neg), 0, neg),
           Positive = if_else(is.na(pos), 0, pos),
           Miss = if_else(is.na(miss), 0, miss)) %>% 
    mutate(cases = Negative + Positive + Miss, 
           pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
           obs_pct_positive = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive))
    ) %>% 
    dplyr::select(-c(miss, neg, pos))
  
  # Write out model
  formula_string <- paste(outcome, "~", paste(covariates, collapse = " + "), 
                          "+ s(time, by = age_cat) + s(time, by = hiv_status) + s(time, by = sex) + s(time, by = health_unit) + s(state, bs = 're') + s(time, k =", paste(k_t),") + s(time, by = state, id = 1, k =", paste(k_t), ")  + s(",paste(latitude, longitude, sep = ","),
                          ", k =", paste(k_sp),")")
  
  # Run model
  model_object <- bam(as.formula(formula_string),
                      data = model_df,
                      family = binomial(link = "logit"),
                      method = "REML"
  )
  
  return(list(model_object, 
              model_df))
}



# Run models and store fitted results -------------------------------------

fitted_models <- list() # To store model output


# Time trend models -------------------------------------------------------

## New Cases: 2014-2019
tictoc::tic()
fitted_models[["tt_2014-2019_new"]] <- run_tt_model(
  data = mdf_new_ind %>% filter(sex != "Missing"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "hiv_status", "sex", "health_unit", "mun_has_prison", "mun_urban_cat", "mun_bf_cat", "mun_fhs_cat", "border"),
  k = 23
)
tictoc::toc()

save(fitted_models, file = "output/fitted_models.tmp.Rdata")


## New Cases: 2015-2019, dropping quarter 12
tictoc::tic()
fitted_models[["tt_2015-2019_new"]] <- run_tt_model(
  data = mdf_new_ind %>% filter(sex != "Missing") %>% filter(time != 12 & time >= 5),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "hiv_status", "sex", "health_unit", "mun_has_prison", "mun_urban_cat", "mun_bf_cat", "mun_fhs_cat", "border"),
  k = 19
)
tictoc::toc()

save(fitted_models, file = "output/fitted_models.tmp.Rdata")


## Previous Cases: 2014-2019
tictoc::tic()
fitted_models[["tt_2014-2019_prev"]] <- run_tt_model(
  data = mdf_prev_ind %>% filter(age_cat != "0-4"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("tratamento", "age_cat", "hiv_status", "sex", "health_unit", "mun_has_prison", "mun_urban_cat", "mun_bf_cat", "mun_fhs_cat", "border"),
  k = 23
)
tictoc::toc()
save(fitted_models, file = "output/fitted_models.tmp.Rdata")



tictoc::tic()
## Time trend model: 2015-2019, dropping quarter 12
fitted_models[["tt_2015-2019_prev"]] <- run_tt_model(
  data = mdf_prev_ind %>% filter(time != 12 & time >= 5) %>% filter(age_cat != "0-4"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("tratamento", "age_cat", "hiv_status", "sex", "health_unit", "mun_has_prison", "mun_urban_cat", "mun_bf_cat", "mun_fhs_cat", "border"),
  k = 19
)
tictoc::toc()
save(fitted_models, file = "output/fitted_models.tmp.Rdata")

# Spatial Models ----------------------------------------------------------

## New Cases: 2014-2019
tictoc::tic()
fitted_models[["sp_2014-2019_new"]] <- run_sp_model(
  data = mdf_new_ind %>% filter(sex != "Missing"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "hiv_status", "sex", "health_unit"),
  latitude = "lat", 
  longitude = "lon",
  k_t = 23,
  k_sp = 50
)
tictoc::toc()
save(fitted_models, file = "output/fitted_models.tmp.Rdata")



## New Cases: 2015-2019
fitted_models[["sp_2015-2019_new"]] <- run_sp_model(
  data = mdf_new_ind %>% filter(sex != "Missing") %>% filter(time != 12 & time >= 5),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "hiv_status", "sex", "health_unit"),
  latitude = "lat", 
  longitude = "lon",
  k_t = 19,
  k_sp = 50
)
save(fitted_models, file = "output/fitted_models.tmp.Rdata")




## Previous Cases: 2014-2019
fitted_models[["sp_2014-2019_prev"]] <- run_sp_model(
  data = mdf_prev_ind %>% filter(sex != "Missing"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("tratamento","age_cat", "hiv_status", "sex", "health_unit"),
  latitude = "lat", 
  longitude = "lon",
  k_t = 23,
  k_sp = 50
)
save(fitted_models, file = "output/fitted_models.tmp.Rdata")



## Previous Cases: 2015-2019
fitted_models[["sp_2015-2019_prev"]] <- run_sp_model(
  data = mdf_prev_ind %>% filter(sex != "Missing") %>% filter(time != 12 & time >= 5),
  outcome = "cbind(Positive, Negative)",
  covariates = c("tratamento","age_cat", "hiv_status", "sex", "health_unit"),
  latitude = "lat", 
  longitude = "lon",
  k_t = 19,
  k_sp = 50
)
save(fitted_models, file = "output/fitted_models.tmp.Rdata")






# Sensitivity Analyses ----------------------------------------------------


## 1. Additional patient covariates ----------------------------------------
tictoc::tic()
fitted_models[["se1_tt_2015-2019_new"]] <- run_tt_model(
  data = mdf_new_ind %>% filter(time != 12 & time >= 5) %>% filter(sex != "Missing"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "sex", "hiv_status", "health_unit", 
                 "mun_urban_cat", "mun_has_prison", "mun_bf_cat", "mun_fhs_cat", 
                 "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga", "agravdiabe"),
  k = 19
)
tictoc::toc()
save(fitted_models, file = "output/fitted_models.tmp.Rdata")

tictoc::tic()
fitted_models[["se1_sp_2015-2019_new"]] <- run_sp_model(
  data = mdf_new_ind %>% filter(time != 12 & time >= 5) %>% filter(sex != "Missing"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "sex", "hiv_status", "health_unit",
                 "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga", "agravdiabe"),
  latitude = "lat", 
  longitude = "lon",
  k_t = 19,
  k_sp = 50
)
tictoc::toc()
save(fitted_models, file = "output/fitted_models.tmp.Rdata")



tictoc::tic()
fitted_models[["se1_tt_2015-2019_prev"]] <- run_tt_model(
  data = mdf_prev_ind %>% filter(time != 12 & time >= 5) %>% filter(age_cat != "0-4"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "sex", "hiv_status", "health_unit", "tratamento",
                 "mun_urban_cat", "mun_has_prison", "mun_bf_cat", "mun_fhs_cat", 
                 "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga", "agravdiabe"),
  k = 19
)
tictoc::toc()
save(fitted_models, file = "output/fitted_models.tmp.Rdata")

tictoc::tic()
fitted_models[["se1_sp_2015-2019_prev"]] <- run_sp_model(
  data = mdf_prev_ind %>% filter(time != 12 & time >= 5) %>% filter(age_cat != "0-4"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "sex", "hiv_status", "health_unit","tratamento",
                 "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga", "agravdiabe"),
  latitude = "lat", 
  longitude = "lon",
  k_t = 19,
  k_sp = 50
)
tictoc::toc()
save(fitted_models, file = "output/fitted_models.tmp.Rdata")




## 2. Selection overtime ---------------------------------------------------
tictoc::tic()
fitted_models[["se2_sp_2015-2019_new"]] <- run_sel_model(
  data = mdf_new_ind %>% filter(time != 12 & time >= 5) %>% filter(sex != "Missing"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "sex", "hiv_status", "health_unit"),
  latitude = "lat", 
  longitude = "lon",
  k_t = 19,
  k_sp = 50
)
tictoc::toc()
save(fitted_models, file = "output/fitted_models.tmp.Rdata")

tictoc::tic()
fitted_models[["se2_sp_2015-2019_prev"]] <- run_sel_model(
  data = mdf_prev_ind %>% filter(time != 12 & time >= 5) %>% filter(age_cat != "0-4"),
  outcome = "cbind(Positive, Negative)",
  covariates = c("age_cat", "sex", "hiv_status", "health_unit"),
  latitude = "lat", 
  longitude = "lon",
  k_t = 19,
  k_sp = 50
)
tictoc::toc()
save(fitted_models, file = "output/fitted_models.tmp.Rdata")










# Store model output ------------------------------------------------------
save(fitted_models, file = "output/fitted_models.Rdata")


# Create model functions ---------------------------------------------------
ctrl <- gam.control(trace = TRUE)

# Main model 
run_sp_model <- function(name, data, filter_expr, outcome, covariates, latitude, longitude, k_t, k_sp) {
  
  # Prepare data
  model_df <- data %>%
    group_by(across(all_of(covariates)), result, time, diag_qrt, state, lat, lon) %>%
    count() %>%
    pivot_wider(names_from = "result", values_from = "n") %>%
    rename(
      neg = "negative",
      pos = "positive",
      miss = "missing"
    ) %>%
    mutate(
      negative = if_else(is.na(neg), 0, neg),
      positive = if_else(is.na(pos), 0, pos),
      miss = if_else(is.na(miss), 0, miss)
    ) %>%
    mutate(
      cases = negative + positive + miss,
      pct_tested = (negative + positive) / (negative + positive + miss),
      obs_pct_positive = if_else(is.nan(positive / (negative + positive)), 0, positive / (negative + positive))
    ) %>%
    dplyr::select(-c(miss, neg, pos))

  # Write out model
  formula_string <- paste(outcome, "~", paste(covariates, collapse = " + "), "+ s(state, bs = 're') + s(time, k =", paste(k_t), ") + s(time, by = state, id = 1, k =", paste(k_t), ")  + s(", paste(latitude, longitude, sep = ","), ", k =", paste(k_sp), ")")

  # Run model
  model_object <- bam(formula = as.formula(formula_string),
                      data = model_df,
                      family = binomial(link = "logit"),
                      method = "fREML", 
                      control = ctrl
                      )

  return(list(
    model_object,
    model_df
  ))
}

# Sensitivity models 
run_sel_model <- function(name, data, filter_expr, outcome, covariates, latitude, longitude, k_t, k_sp) {
  # Prepare data
  model_df <- data %>%
    group_by(across(all_of(covariates)), result, time, diag_qrt, state, lat, lon) %>%
    count() %>%
    pivot_wider(names_from = "result", values_from = "n") %>%
    rename(
      neg = "negative",
      pos = "positive",
      miss = "missing"
    ) %>%
    mutate(
      negative = if_else(is.na(neg), 0, neg),
      positive = if_else(is.na(pos), 0, pos),
      miss = if_else(is.na(miss), 0, miss)
    ) %>%
    mutate(
      cases = negative + positive + miss,
      pct_tested = (negative + positive) / (negative + positive + miss),
      obs_pct_positive = if_else(is.nan(positive / (negative + positive)), 0, positive / (negative + positive))
    ) %>%
    dplyr::select(-c(miss, neg, pos))

  # Write out model
  if (!"tratamento" %in% covariates) {
    formula_string <- paste(
      outcome, "~", paste(covariates, collapse = " + "),
      "+ s(time, by = age_cat) + s(time, by = hiv_status) + s(time, by = sex) + s(time, by = health_unit) + s(state, bs = 're') + s(time, k =", paste(k_t), ") + s(time, by = state, id = 1, k =", paste(k_t), ")  + s(", paste(latitude, longitude, sep = ","),
      ", k =", paste(k_sp), ")"
    )
  } else {
    formula_string <- paste(
      outcome, "~", paste(covariates, collapse = " + "),
      "+ s(time, by = age_cat) + s(time, by = tratamento) + s(time, by = hiv_status) + s(time, by = sex) + s(time, by = health_unit) + s(state, bs = 're') + s(time, k =", paste(k_t), ") + s(time, by = state, id = 1, k =", paste(k_t), ")  + s(", paste(latitude, longitude, sep = ","),
      ", k =", paste(k_sp), ")"
    )
  }

  # Run model
  model_object <- bam(as.formula(formula_string),
                      data = model_df,
                      family = binomial(link = "logit"),
                      method = "fREML", 
                      control = ctrl
                      )

  return(list(
    model_object,
    model_df
  ))
}

# Spatial Models ----------------------------------------------------------

if ("sp_2014_new" %in% models_to_run) {
  tictoc::tic()

  fitted_models[["sp_2014_new"]] <- run_sp_model(
    data = mdf_new_ind,
    outcome = "cbind(positive, negative)",
    covariates = c("age_cat", "hiv_status", "sex", "health_unit"),
    latitude = "lat",
    longitude = "lon",
    k_t = 40,
    k_sp = 50
  )

  save(fitted_models, file = paste0("output/fitted_models_", file_version_save, ".Rdata"))

  tictoc::toc()
}

if ("sp_2014_prev" %in% models_to_run) {
  tictoc::tic()

  fitted_models[["sp_2014_prev"]] <- run_sp_model(
    data = mdf_prev_ind,
    outcome = "cbind(positive, negative)",
    covariates = c("tratamento", "age_cat", "hiv_status", "sex", "health_unit"),
    latitude = "lat",
    longitude = "lon",
    k_t = 40,
    k_sp = 50
  )

  tictoc::toc()

  save(fitted_models, file = paste0("output/fitted_models_", file_version_save, ".Rdata"))
}


if ("sp_2017_new" %in% models_to_run) {
  tictoc::tic()

  fitted_models[["sp_2017_new"]] <- run_sp_model(
    data = mdf_new_ind %>% filter(time >= 13),
    outcome = "cbind(positive, negative)",
    covariates = c("age_cat", "hiv_status", "sex", "health_unit"),
    latitude = "lat",
    longitude = "lon",
    k_t = 28,
    k_sp = 50
  )

  save(fitted_models, file = paste0("output/fitted_models_", file_version_save, ".Rdata"))

  tictoc::toc()
}


if ("sp_2017_prev" %in% models_to_run) {
  tictoc::tic()

  fitted_models[["sp_2017_prev"]] <- run_sp_model(
    data = mdf_prev_ind %>% filter(time >= 13),
    outcome = "cbind(positive, negative)",
    covariates = c("tratamento", "age_cat", "sex", "hiv_status", "health_unit"),
    latitude = "lat",
    longitude = "lon",
    k_t = 28,
    k_sp = 50
  )
  tictoc::toc()

  save(fitted_models, file = paste0("output/fitted_models_", file_version_save, ".Rdata"))
}

# Sensitivity Analyses ----------------------------------------------------
## 1. Additional patient covariates ----------------------------------------

if ("sens_1_new" %in% models_to_run) {
  tictoc::tic()

  fitted_models[["sens_1_new"]] <- run_sp_model(
    data = mdf_new_ind %>% filter(time >= 13),
    outcome = "cbind(positive, negative)",
    covariates = c(
      "age_cat", "sex", "hiv_status",
      "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga",
      "agravdiabe", "cs_escol_n", "pop_imig", "health_unit", "race"
    ),
    latitude = "lat",
    longitude = "lon",
    k_t = 28,
    k_sp = 50
  )

  tictoc::toc()

  save(fitted_models, file = paste0("output/fitted_models_", file_version_save, ".Rdata"))
}

if ("sens_1_prev" %in% models_to_run) {
  tictoc::tic()
  fitted_models[["sens_1_prev"]] <- run_sp_model(
    data = mdf_prev_ind %>% filter(time >= 13),
    outcome = "cbind(positive, negative)",
    covariates = c(
      "age_cat", "sex", "hiv_status", "tratamento",
      "pop_rua", "pop_liber", "agravtabac", "agravalcoo", "agravdroga",
      "agravdiabe", "cs_escol_n", "pop_imig", "health_unit", "race"
    ),
    latitude = "lat",
    longitude = "lon",
    k_t = 28,
    k_sp = 50
  )
  tictoc::toc()
  save(fitted_models, file = paste0("output/fitted_models_", file_version_save, ".Rdata"))
}


## 2. Selection overtime ---------------------------------------------------
if ("sens_2_new" %in% models_to_run) {
  tictoc::tic()

  fitted_models[["sens_2_new"]] <- run_sel_model(
    data = mdf_new_ind %>% filter(time >= 13),
    outcome = "cbind(positive, negative)",
    covariates = c("age_cat", "sex", "hiv_status", "health_unit"),
    latitude = "lat",
    longitude = "lon",
    k_t = 28,
    k_sp = 50
  )

  tictoc::toc()

  save(fitted_models, file = paste0("output/fitted_models_", file_version_save, ".Rdata"))
}

if ("sens_2_prev" %in% models_to_run) {
  tictoc::tic()

  fitted_models[["sens_2_prev"]] <- run_sel_model(
    data = mdf_prev_ind %>% filter(time >= 13),
    outcome = "cbind(positive, negative)",
    covariates = c("tratamento", "age_cat", "sex", "hiv_status", "health_unit"),
    latitude = "lat",
    longitude = "lon",
    k_t = 28,
    k_sp = 50
  )

  tictoc::toc()

  save(fitted_models, file = paste0("output/fitted_models_", file_version_save, ".Rdata"))
}

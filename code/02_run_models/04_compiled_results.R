# Author: Sarah Baum
# Created: 2024-03-22
# Updated: 2024-03-25

# Description: Compile fitted values and uncertainty intervals for all models 


# # Load data ---------------------------------------------------------------


## Load state population for 2010
pop_UF <- read_excel("data/Brazil_population_UF.xls", skip = 6) %>% 
  rename(state = "...1", 
         pop_2010 = "...2") %>% 
  select(state, pop_2010) %>% 
  filter(!is.na(pop_2010)) %>% 
  filter(!state %in% c("Brasil", "Centro-Oeste", "Norte", "Nordeste", "Sudeste", "Sul"))


# Write functions to compile results --------------------------------------

aggregate_fitted_values <- function(model_name, agg_level) {
  
  # Group by desired aggregation level 
  fitted <- fitted_values[[model_name]] %>% 
    filter(!is.na(fitted)) %>%
    mutate(year = case_when(time <= 4 ~ 2014,
                            time > 4 & time <= 8 ~ 2015,
                            time > 8 & time <= 12 ~ 2016,
                            time > 12 & time <= 16 ~ 2017,
                            time > 16 & time <= 20 ~ 2018,
                            time > 20 & time <= 24 ~ 2019, 
                            time > 24 & time <= 28 ~ 2020,
                            time > 28 & time <= 32 ~ 2021, 
                            time > 32 & time <= 36 ~ 2022, 
                            time > 36 & time <= 40 ~ 2023)) %>%
    group_by(
      case_when(
        agg_level == "nat_yr" ~ as.character(year), 
        agg_level == "nat_qrt" ~ paste(as.character(diag_qrt)), 
        agg_level == "state_yr" ~ paste(state, as.character(year), sep = ","), 
        agg_level == "state_qrt" ~ paste(state, as.character(diag_qrt), sep = ",")
      )
    ) %>% 
    summarize(fitted_RR = sum(fitted * cases))
  
  
  
  # Rename first column 
if (agg_level == "state_qrt") {
    
  fitted <- fitted %>% 
    separate(colnames(.)[1], into = c("state", "diag_qrt"), sep = ",", remove = TRUE) %>% 
    mutate(model = model_name, 
           diag_qrt = as.Date(diag_qrt))
    
  } else if (agg_level == "state_yr"){ 
    
    fitted <- fitted %>% separate(colnames(.)[1], into = c("state", "year"), sep = ",", remove = TRUE) %>% 
      mutate(model = model_name, 
             year = as.numeric(year))
  
  } else if (agg_level == "nat_yr"){ 
    fitted <- fitted %>% rename(year = colnames(.)[1]) %>% 
      mutate(model = model_name, 
             year = as.numeric(year))
    
  } else { 
    
    fitted <- fitted %>% rename(diag_qrt = colnames(.)[1]) %>% 
      mutate(model = model_name, 
             diag_qrt = as.Date(diag_qrt))
  }
  
  return(fitted)
}


get_observed <- function(model_name, agg_level)  {
    
  if (grepl("new", model_name)) {
    
    observed <- sinan_xpert %>% 
      filter(tratamento %in% c("1")) %>% 
        rename("year" = "diag_yr") %>% 
        group_by(
          case_when(
            agg_level == "nat_yr" ~ as.character(year), 
            agg_level == "nat_qrt" ~ paste(as.character(diag_qrt)), 
            agg_level == "state_yr" ~ paste(state_nm, state, as.character(year), sep = ","), 
            agg_level == "state_qrt" ~ paste(state_nm, state, as.character(diag_qrt), sep = ",")
          )
        ) %>% 
        summarize(total_TB_cases = n(), 
                  obs_num_tested = sum(test_molec %in% c("1", "2"), na.rm = TRUE), 
                  obs_pct_tested = obs_num_tested/total_TB_cases, 
                  obs_RR = sum(test_molec %in% c("2"), na.rm = TRUE), 
                  obs_pct_pos = obs_RR/obs_num_tested)
    
  } else { 
    
  observed <- sinan_xpert %>% 
  filter(tratamento %in% c("2", "3")) %>% 
    rename("year" = "diag_yr") %>% 
    group_by(
      case_when(
        agg_level == "nat_yr" ~ as.character(year), 
        agg_level == "nat_qrt" ~ paste(as.character(diag_qrt)), 
        agg_level == "state_yr" ~ paste(state_nm, state, as.character(year), sep = ","), 
        agg_level == "state_qrt" ~ paste(state_nm, state, as.character(diag_qrt), sep = ",")
      )
    ) %>% 
    summarize(total_TB_cases = n(), 
              obs_num_tested = sum(test_molec %in% c("1", "2"), na.rm = TRUE), 
              obs_pct_tested = obs_num_tested/total_TB_cases, 
              obs_RR = sum(test_molec %in% c("2"), na.rm = TRUE), 
              obs_pct_pos = obs_RR/obs_num_tested)
  
  }
  
  if (agg_level == "state_qrt") {
    
    observed <- observed %>% 
      separate(colnames(.)[1], into = c("state_nm", "state", "diag_qrt"), sep = ",", remove = TRUE) %>% 
      mutate(model = model_name, 
             diag_qrt = as.Date(diag_qrt))
    
  } else if (agg_level == "state_yr"){ 
    
    observed <- observed %>% separate(colnames(.)[1], into = c("state_nm", "state", "year"), sep = ",", remove = TRUE) %>% 
      mutate(model = model_name, 
             year = as.numeric(year))
    
  } else if (agg_level == "nat_yr"){ 
    observed <- observed %>%  rename(year = colnames(.)[1]) %>% 
      mutate(model = model_name, 
             year = as.numeric(year))
    
  } else { 
    
    observed <- observed %>% rename(diag_qrt = colnames(.)[1]) %>% 
      mutate(model = model_name, 
             diag_qrt = as.Date(diag_qrt))
  }
  
  
  # Add state population 
  if (grepl("state", agg_level)) {
    
    observed <- left_join(observed, pop_UF, by = c("state_nm" = "state"))

    
  } else { 
    
  }
  
  return(observed)
}
  
  
compile_results <- function(agg_level, model_list) {


  # Get fitted values -------------------------------------------------------
  # Aggregate fitted values for relevant models for case type 
  fitted_list <- lapply(model_list, aggregate_fitted_values, agg_level)
  
  combined_fitted <- do.call(rbind, fitted_list)
  


  # Get intervals -----------------------------------------------------------
  # Pull intervals for model based on aggregation level 
  interval_list <- lapply(model_list, function(model_name) {
    
    if (grepl("state", agg_level)) {
      
    interval <- intervals[[model_name]][[paste0("proj_", agg_level)]] %>% 
      mutate(model = model_name)
    
    } else { 
      interval <- intervals[[model_name]][[paste0("proj_", agg_level)]] %>% 
        mutate(model = model_name)
    }
    
  }
  )
  
  combined_intervals <- do.call(rbind, interval_list)
  
  

  # Combine model estimates ------------------------------------------------
  # Combine fitted values and intervals for each model by model name
  if (agg_level == "state_qrt") {

  combined_est <- left_join(combined_fitted, combined_intervals, by = c("model", "state", "diag_qrt"))

  } else if (agg_level == "state_yr"){

    combined_est <- left_join(combined_fitted, combined_intervals, by = c("model", "state", "year"))

  } else if (agg_level == "nat_yr"){

    combined_est <- left_join(combined_fitted, combined_intervals, by = c("model", "year"))

  } else{

    combined_est <- left_join(combined_fitted, combined_intervals, by = c("model", "diag_qrt"))
  }


  # Get observed data -------------------------------------------------------
  observed_list <- lapply(model_list, get_observed, agg_level = agg_level)
  
  observed <- do.call(rbind, observed_list)
  

  # Combined model estimates and observed -----------------------------------
  if (agg_level == "state_qrt") {
    
    results <- left_join(combined_est, observed, by = c("model", "state", "diag_qrt"))
    
  } else if (agg_level == "state_yr"){
    
    results <- left_join(combined_est, observed, by = c("model", "state", "year"))
    
  } else if (agg_level == "nat_yr"){
    
    results <- left_join(combined_est, observed, by = c("model", "year"))
    
  } else{
    
    results <- left_join(combined_est, observed, by = c("model", "diag_qrt"))
  }
  
  split_model <- str_match(results$model, "^(.*)_([^_]+)$")
  results$model <- split_model[, 2]
  results$case_type <- split_model[, 3]
  
  
  if (agg_level == "state_qrt") {
    
    results <- results %>% select(model, case_type, state_nm, state, time, diag_qrt, fitted_RR, proj_median, everything())
    
  } else if (agg_level == "state_yr"){
    
    results <- results %>% select(model, case_type, state_nm, state, year, fitted_RR, proj_median, everything())
    
  } else if (agg_level == "nat_yr"){
    
    results <- results %>% select(model, case_type, year, fitted_RR, proj_median, everything())
    
  } else{
    
    results <- results %>% select(model, case_type, time, diag_qrt, fitted_RR, proj_median, everything())
  }
  
  return(results)
  
}




# Output results for each aggregation level -------------------------------
# levels_to_aggregate <- c("nat_yr", "nat_qrt", "state_yr", "state_qrt")

model_list <- names(fitted_values)[grepl("sp_", names(fitted_values))]

results_list <- list()

# results_list <- lapply(levels_to_aggregate, compile_results)
# compiled_results <- setNames(results_list, levels_to_aggregate)

results_list[["nat_yr"]] <- compile_results(agg_level = "nat_yr", model_list = model_list)
results_list[["nat_qrt"]] <- compile_results(agg_level = "nat_qrt", model_list = model_list)
results_list[["state_yr"]] <- compile_results(agg_level = "state_yr", model_list = model_list)
results_list[["state_qrt"]] <- compile_results(agg_level = "state_qrt", model_list = model_list)

compiled_results <- results_list







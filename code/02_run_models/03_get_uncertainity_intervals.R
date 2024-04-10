# Author: Sarah Baum
# Created: 2024-03-22
# Updated: 2024-03-25

# Description: Calculate uncertainity intervals for each model as various aggregation levels


# Create functions --------------------------------------------------------

# Get simulated probabilities for each simulation for each individual 
get_sim_preds <- function(model_object, data){

  
  ## 1 Get mean and VCV 
  par_est <- coef(model_object)
  par_vcv <- vcov(model_object, unconditional=T)
  
  
  # 2 Prepare linear predictor matrix - include all the strata you want predictions for.
  # It has to have the same variables (names, factor levels, etc) as used to fit
  # the model (ie fit_object).
  lpm <- predict(model_object, newdata = data, type = "lpmatrix")
  
  ## 3 Simulate from parameter posterior - Draw different coefficients from the
  ## posterior dist, determined by mu and signma
  n.sim <- 1000
  set.seed(1234)
  sim_pars <- MASS::mvrnorm(n.sim, mu = par_est, Sigma = par_vcv) # check to see whether mean and SD match summary to summary on model 
  
  ## 4 Calc results on transformed variables 
  sim_preds0 <- lpm %*% t(sim_pars) %>% data.frame()
  
  
  ## 5 get output by state and time 
  pred_output <- cbind(data, sim_preds0)
  
  return(pred_output)
  
}

# Get projected number of cases for each simulation 
get_proj_cases <- function(pred_output){
  
  # Project number of cases from each simulation
  proj_cases <- pred_output %>% 
    ungroup() %>% 
    mutate(
      # Convert simulated log odds to probabilities
      across(starts_with("X"), ~ plogis(.), .names = "prob_{.col}"), 
      # Calculate number of cases from simulation 
      across(starts_with("prob"), ~ . * cases, .names = "proj_{.col}")) %>% 
    select(-c(starts_with("X"), starts_with("prob")))
  
  return(proj_cases)
  
}

# Get intervals for each area and time period 
get_nat_qrt_int <- function(proj_cases) {
  
  proj_nat_qrt <- proj_cases %>% 
    group_by(time, diag_qrt) %>% 
    # Calculate number of projected RR-TB cases for each simulation 
    summarize(across(starts_with("proj"), ~sum(.), .names = "cases_{.col}")) %>% 
    rowwise() %>% 
    summarize(
      diag_qrt = diag_qrt,
      # Calculate point estimates - mean
      # proj_mean = rowMeans(across(starts_with("cases"))), 
      # Calculate LCI
      proj_lci = quantile(c_across(starts_with("cases")), probs = 0.025),
      # Calculate point estimate - median
      proj_median = quantile(c_across(starts_with("cases")), probs = 0.5),
      # Calculate HCI
      proj_hci= quantile(c_across(starts_with("cases")), probs = 0.975))
  
  return(proj_nat_qrt)
  
}
get_state_qrt_int <- function(proj_cases) {
  
  proj_state_qrt <- proj_cases %>% 
    group_by(state, time, diag_qrt) %>% 
    # Calculate number of projected RR-TB cases for each simulation 
    summarize(across(starts_with("proj"), ~sum(.), .names = "cases_{.col}")) %>% 
    rowwise() %>% 
    summarize(
      state = state, 
      diag_qrt = diag_qrt, 
      # Calculate point estimates - mean
      # proj_mean = rowMeans(across(starts_with("cases"))), 
      # Calculate LCI
      proj_lci = quantile(c_across(starts_with("cases")), probs = 0.025),
      # Calculate point estimate - median
      proj_median = quantile(c_across(starts_with("cases")), probs = 0.5),
      # Calculate HCI
      proj_hci= quantile(c_across(starts_with("cases")), probs = 0.975))
  
  return(proj_state_qrt)

  
}
get_nat_yr_int <- function(proj_cases) {
  
  proj_nat_yr <- proj_cases %>% 
    mutate(year = case_when(time <= 4 ~ 2014,
                            time > 4 & time <= 8 ~ 2015,
                            time > 8 & time <= 12 ~ 2016,
                            time > 12 & time <= 16 ~ 2017,
                            time > 16 & time <= 20 ~ 2018,
                            time > 20 & time <= 24 ~ 2019)) %>%
    group_by(year) %>% 
    # Calculate number of projected RR-TB cases for each simulation 
    summarize(across(starts_with("proj"), ~sum(.), .names = "cases_{.col}")) %>% 
    rowwise() %>% 
    summarize(
      year = year,
      # Calculate point estimates - mean
      # proj_mean = rowMeans(across(starts_with("cases"))), 
      # Calculate LCI
      proj_lci = quantile(c_across(starts_with("cases")), probs = 0.025),
      # Calculate point estimate - median
      proj_median = quantile(c_across(starts_with("cases")), probs = 0.5),
      # Calculate HCI
      proj_hci= quantile(c_across(starts_with("cases")), probs = 0.975))
  
  return(proj_nat_yr)
  
}
get_state_yr_int <- function(proj_cases) {
  
  proj_state_yr <- proj_cases %>% 
    mutate(year = case_when(time <= 4 ~ 2014,
                            time > 4 & time <= 8 ~ 2015,
                            time > 8 & time <= 12 ~ 2016,
                            time > 12 & time <= 16 ~ 2017,
                            time > 16 & time <= 20 ~ 2018,
                            time > 20 & time <= 24 ~ 2019)) %>%
    group_by(state, year) %>% 
    # Calculate number of projected RR-TB cases for each simulation 
    summarize(across(starts_with("proj"), ~sum(.), .names = "cases_{.col}")) %>% 
    rowwise() %>% 
    summarize(
      state = state, 
      year = year,
      # Calculate point estimates - mean
      # proj_mean = rowMeans(across(starts_with("cases"))), 
      # Calculate LCI
      proj_lci = quantile(c_across(starts_with("cases")), probs = 0.025),
      # Calculate point estimate - median
      proj_median = quantile(c_across(starts_with("cases")), probs = 0.5),
      # Calculate HCI
      proj_hci= quantile(c_across(starts_with("cases")), probs = 0.975))
  
  return(proj_state_yr)

}



# Combine all of the above functions into a single function to get intervals 
get_intervals <- function(model_name) {
  
  # Pull model object and data 
  model_object <- fitted_models[[model_name]][[1]]
  data <- fitted_models[[model_name]][[2]] %>% filter(!is.na(lat))
  
  # Get simulated predictions based on simulation and data
  pred_output <- get_sim_preds(model_object, data)
  
  # Get projected number of cases for each simulations
  proj_cases <- get_proj_cases(pred_output)
  
  # Get intervals for each area and time period 
  proj_nat_yr <- get_nat_yr_int(proj_cases)
  proj_nat_qrt <- get_nat_qrt_int(proj_cases) 
  proj_state_yr <- get_state_yr_int(proj_cases) 
  proj_state_qrt <- get_state_qrt_int(proj_cases)
  
  # Rename for each model 
  intervals <- list(proj_nat_yr = proj_nat_yr, 
                    proj_nat_qrt = proj_nat_qrt, 
                    proj_state_yr = proj_state_yr, 
                    proj_state_qrt = proj_state_qrt)

  # Create a list of interval schemes for each model 
  return(intervals)
  
}
  
  





# Calculate uncertainty intervals for each model --------------------------
intervals <- list()

# model_name <- names(fitted_models)
# intervals <- lapply(model_name, get_intervals)
# intervals <- setNames(intervals, model_name)


intervals[["sp_2014-2019_new"]] <- get_intervals(model_name = "sp_2014-2019_new")
intervals[["sp_2015-2019_new"]] <- get_intervals(model_name = "sp_2015-2019_new")
intervals[["sp_2014-2019_prev"]] <- get_intervals(model_name = "sp_2014-2019_prev")
intervals[["sp_2015-2019_prev"]] <- get_intervals(model_name = "sp_2015-2019_prev")


intervals[["se1_sp_2015-2019_new"]] <- get_intervals(model_name = "se1_sp_2015-2019_new")
intervals[["se1_sp_2015-2019_prev"]] <- get_intervals(model_name = "se1_sp_2015-2019_prev")
intervals[["se2_sp_2015-2019_new"]] <- get_intervals(model_name = "se2_sp_2015-2019_new")
intervals[["se2_sp_2015-2019_prev"]] <- get_intervals(model_name = "se2_sp_2015-2019_prev")



# store intervals ---------------------------------------------------------
save(intervals, file = intervals_file_name)






# intervals[["tt_2014-2019_new"]] <- get_intervals(model_name = "tt_2014-2019_new")
# intervals[["tt_2014-2019_prev"]] <- get_intervals(model_name = "tt_2014-2019_prev")
# intervals[["tt_2015-2019_new"]] <- get_intervals(model_name = "tt_2015-2019_new")
# intervals[["tt_2015-2019_prev"]] <- get_intervals(model_name = "tt_2015-2019_prev")
# intervals[["se1_tt_2015-2019_new"]] <- get_intervals(model_name = "se1_tt_2015-2019_new")
# intervals[["se1_tt_2015-2019_prev"]] <- get_intervals(model_name = "se1_tt_2015-2019_prev")



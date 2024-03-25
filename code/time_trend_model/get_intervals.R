# Author: Sarah Baum
# Date Created: 
# Date Updated: 
# Description: Get posterior preds from RR-TB models 

library(tictoc)

source(here::here("code/dependencies.R"))



###########################################################################
###########################################################################
###                                                                     ###
###                        CREATE PREDS FUNCTION                        ###
###                                                                     ###
###########################################################################
###########################################################################

# 1. Write preds function ----------------------------------------------------

get_preds <- function(fit_object, data){
  
  ## 1 Get mean and VCV 
  par_est <- coef(fit_object)
  par_vcv <- vcov(fit_object, unconditional=T)
  
  
  # 2 Prepare linear predictor matrix - include all the strata you want predictions for.
  # It has to have the same variables (names, factor levels, etc) as used to fit
  # the model (ie fit_object).
  lpm <- predict(fit_object, newdata = data, type = "lpmatrix")
  
  ## 3 Simulate from parameter posterior - Draw different coefficients from the
  ## posterior dist, determined by mu and signma
  n.sim <- 100
  set.seed(1234)
  sim_pars <- MASS::mvrnorm(n.sim, mu = par_est, Sigma = par_vcv) # check to see whether mean and SD match summary to summary on model 
  
  ## 4 Calc results on transformed variables 
  sim_preds0 <- lpm %*% t(sim_pars) %>% data.frame()
  
  
  ## 5 get output by state and time 
  pred_output <- cbind(data, sim_preds0)
  
  return(pred_output)
  
}






# 2. Write function to get projected cases  -----------------------------------
calculate_proj <- function(pred_output){
  
  # Project number of cases from each simulation
  pred_output1 <- pred_output %>% 
    ungroup() %>% 
    mutate(
      # Convert simulated log odds to probabilities
      across(starts_with("X"), ~ plogis(.), .names = "prob_{.col}"), 
      # Calculate number of cases from simulation 
      across(starts_with("prob"), ~ . * cases, .names = "proj_{.col}")) %>% 
    select(-c(starts_with("X"), starts_with("prob")))
  
  
  # national level 
  pred.nat_qrt <- pred_output1 %>% 
    group_by(time, diag_qrt) %>% 
    # Calculate number of projected RR-TB cases for each simulation 
    summarize(across(starts_with("proj"), ~sum(.), .names = "cases_{.col}")) %>% 
    rowwise() %>% 
    summarize(
      # Calculate point estimates - mean
      proj_mean = rowMeans(across(starts_with("cases"))), 
      # Calculate LCI
      proj_lci = quantile(c_across(starts_with("cases")), probs = 0.025),
      # Calculate point estimate - median
      proj_median = quantile(c_across(starts_with("cases")), probs = 0.5),
      # Calculate HCI
      proj_hci= quantile(c_across(starts_with("cases")), probs = 0.975))
    
  
# ggplot() + 
#   # geom_point(data = pred.nat_qrt_cc, aes(x = time, y = proj_mean, color = "CC")) +
#   # geom_point(data = pred.nat_qrt_cc, aes(x = time, y = proj_median, color = "CC")) +
#   # geom_errorbar(data = pred.nat_qrt_cc,
#   #               aes(x = time, ymin = proj_lci, ymax = proj_hci,
#   # 
#   #                               width = 0.5, color = "CC"))
#   geom_point(data = pred.nat_qrt, aes(x = time, y = proj_mean, color = "Miss")) +
#   geom_point(data = pred.nat_qrt, aes(x = time, y = proj_median, color = "Miss")) +
#   geom_errorbar(data = pred.nat_qrt,
#                 aes(x = time, ymin = proj_lci, ymax = proj_hci,
#                     width = 0.5, color = "Miss"))

  # pred.nat_yr <- projected %>%
  #   mutate(year = case_when(time <= 4 ~ 2014,
  #                           time > 4 & time <= 8 ~ 2015,
  #                           time > 8 & time <= 12 ~ 2016,
  #                           time > 12 & time <= 16 ~ 2017,
  #                           time > 16 & time <= 20 ~ 2018,
  #                           time > 20 & time <= 24 ~ 2019)) %>%
  #   group_by(year) %>% 
  #   summarize(obs_TB_cases = sum(cases),
  #             proj_mean_RR_cases = sum(proj_mean_cases), 
  #             proj_med_RR_cases = sum(proj_med_cases),
  #             lci_RR_cases = sum(proj_lci_cases),
  #             hci_RR_cases = sum(proj_hci_cases), 
  #             mean_pct = mean(mean_val), 
  #             med_pct = median(med_val), 
  #             mean_lci_pct = mean(lci_val), 
  #             mean_hci_pct = mean(hci_val), 
  #             med_lci_pct = median(lci_val), 
  #             med_hci_pct = median(hci_val))
  # 
  # 
  # pred.state_qrt <- projected %>% 
  #   group_by(state_nm, diag_qrt) %>% 
  #   summarize(obs_TB_cases = sum(cases),
  #             proj_mean_RR_cases = sum(proj_mean_cases), 
  #             proj_med_RR_cases = sum(proj_med_cases),
  #             lci_RR_cases = sum(proj_lci_cases),
  #             hci_RR_cases = sum(proj_hci_cases), 
  #             mean_pct = mean(mean_val), 
  #             med_pct = median(med_val), 
  #             mean_lci_pct = mean(lci_val), 
  #             mean_hci_pct = mean(hci_val), 
  #             med_lci_pct = median(lci_val), 
  #             med_hci_pct = median(hci_val))

  
  return(list(pred.nat_qrt = pred.nat_qrt
              # pred.state_qrt = pred.state_qrt
              )
         )
              
         
  
}




###########################################################################
###########################################################################
###                                                                     ###
###                         PREDS FOR NEW CASES                         ###
###                                                                     ###
###########################################################################
###########################################################################

# 2. Get preds for new cases -------------------------------------------------

## 2.1 Load data  ----------------------------------------------------------
load(here::here("data/mdf_new_ind.Rdata"))
load(here::here("data/mdf_mun_new_grp.Rdata"))
load(here::here("data/mdf_mun_new_grp.miss.Rdata"))

# load(here::here("data/mdf_mun_new_grp.xpert.Rdata"))
# load(here::here("data/mdf_mun_new_grp.tmp.Rdata"))



## 2.2 Municipality: Model 1 (2014-2019) -----------------------------------


#################################################################
##              Municipality: Model 1 (2014-2019)              ##
#################################################################


# complete cases ----------------------------------------------------------

# load("output/fits/mun_new_mod1.Rda")
# 
# tic()
# out.mun_new_1 <- get_preds(fit_object = mun_new_mod1, data = mdf_mun_new_grp %>%
#                               filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                              age_cat, health_unit, mun_urban_cat,
#                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                         all_vars(!is.na(.))))
# toc()
# 
# tic()
# boot.mun_new_1 <- calculate_proj(pred_output = out.mun_new_1)
# toc()
# 
# save("boot.mun_new_1", file = "output/fits/boot.mun_new_1.Rda")




# Missing  ----------------------------------------------------------------
# load("output/fits/mun_new_mod1.miss.Rda")
# 
# tic()
# out.mun_new_1_miss <- get_preds(fit_object = mun_new_mod1.miss, data = mdf_mun_new_grp.miss %>%
#                                    filter(sex != "Missing") %>% 
#                              filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                             age_cat, health_unit, mun_urban_cat,
#                                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                        all_vars(!is.na(.))))
# toc()
# 
# tic()
# boot.mun_new_1_miss <- calculate_proj(pred_output = out.mun_new_1_miss)
# toc()
# 
# save("boot.mun_new_1_miss", file = "output/fits/boot.mun_new_1_miss.Rda")
# 







#################################################################
##              Municipality: Model 2 (2016-2019)              ##
#################################################################
# load("output/fits/mun_new_mod2.Rda")
# 
# out.mun_new_2 <- get_preds(fit_object = mun_new_mod2, data = mdf_mun_new_grp %>%
#                               filter(time > 8) %>%
#                               filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                             age_cat, health_unit, mun_urban_cat,
#                                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                         all_vars(!is.na(.))))
# 
# tic()
# boot.mun_new_2 <- calculate_proj(pred_output = out.mun_new_2)
# toc()
# 
# save("boot.mun_new_2", file = "output/fits/boot.mun_new_2.Rda")


# Missing  ----------------------------------------------------------------
## Model hasn't been run yet
# load("output/fits/mun_new_mod2.miss.Rda")
# 
# tic()
# out.mun_new_2_miss <- get_preds(fit_object = mun_new_mod2.miss, data = mdf_mun_new_grp.miss %>%
#                                   filter(sex != "Missing") %>% filter(time > 8) %>%
#                                   filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                                  age_cat, health_unit, mun_urban_cat,
#                                                  mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                             all_vars(!is.na(.))))
# toc()
# 
# tic()
# boot.mun_new_2_miss <- calculate_proj(pred_output = out.mun_new_2_miss)
# toc()
# 
# save("boot.mun_new_2_miss", file = "output/fits/boot.mun_new_2_miss.Rda")



#################################################################
##              Municipality: Model 3 (2017-2019)              ##
#################################################################
# load("output/fits/mun_new_mod3.Rda")
# 
# 
# out.mun_new_3 <- get_preds(fit_object = mun_new_mod3, data = mdf_mun_new_grp %>%
#                               filter(time > 12) %>%
#                               filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                              age_cat, health_unit, mun_urban_cat,
#                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                         all_vars(!is.na(.))))
# 
# tic()
# boot.mun_new_3 <- calculate_proj(pred_output = out.mun_new_3)
# toc()
# 
# save("boot.mun_new_3", file = "output/fits/boot.mun_new_3.Rda")


# Missing  ----------------------------------------------------------------
# load("output/fits/mun_new_mod3.miss.Rda")
# 
# tic()
# out.mun_new_3_miss <- get_preds(fit_object = mun_new_mod3.miss, data = mdf_mun_new_grp.miss %>%
#                                   filter(sex != "Missing") %>% filter(time > 12) %>% 
#                                   filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                                  age_cat, health_unit, mun_urban_cat,
#                                                  mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                             all_vars(!is.na(.))))
# toc()
# 
# tic()
# boot.mun_new_3_miss <- calculate_proj(pred_output = out.mun_new_3_miss)
# toc()
# 
# save("boot.mun_new_3_miss", file = "output/fits/boot.mun_new_3_miss.Rda")




#################################################################
##              Municipality: Model 4 (2015-2019)              ##
#################################################################
# 
# load("output/fits/mun_new_mod4.Rda")
# 
# 
# out.mun_new_4 <- get_preds(fit_object = mun_new_mod4, data = mdf_mun_new_grp %>%
#                              filter(time !=12 & time >=5) %>%
#                              filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                             age_cat, health_unit, mun_urban_cat,
#                                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                        all_vars(!is.na(.))))
# 
# tic()
# boot.mun_new_4 <- calculate_proj(pred_output = out.mun_new_4)
# toc()
# 
# save("boot.mun_new_4", file = "output/fits/boot.mun_new_4.Rda")



# Missing  ----------------------------------------------------------------
# load("output/fits/mun_new_mod4.miss.Rda")
# 
# tic()
# out.mun_new_4_miss <- get_preds(fit_object = mun_new_mod4.miss, data = mdf_mun_new_grp.miss %>%
#                                   filter(sex != "Missing") %>% filter(time !=12 & time >=5)  %>% 
#                                   filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                                  age_cat, health_unit, mun_urban_cat,
#                                                  mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                             all_vars(!is.na(.))))
# toc()
# 
# tic()
# boot.mun_new_4_miss <- calculate_proj(pred_output = out.mun_new_4_miss)
# toc()
# 
# save("boot.mun_new_4_miss", file = "output/fits/boot.mun_new_4_miss.Rda")




############################################################################
############################################################################
###                                                                      ###
###                       PREDS FOR PREVIOUS CASES                       ###
###                                                                      ###
############################################################################
############################################################################

# 3. Get preds for previous cases -----------------------------------------


## 3.1 Load data -----------------------------------------------------------

## Note: because of weird outliers, filter those 0-4 years old for now
# load dataset
load("data/mdf_prev_ind.Rdata")
load("data/mdf_mun_prev_grp.miss.Rdata")



#################################################################
##              Municipality: Model 1 (2014-2019)              ##
#################################################################
load("output/fits/mun_prev_mod1.Rda")


out.mun_prev_1 <- get_preds(fit_object = mun_prev_mod1, data = mdf_prev_ind %>%
                               filter(!age_cat == "0-4") %>%
                              mutate(cases = 1) %>%
                               filter_at(vars(state, time, result, tratamento, sex, hiv_status,
                                              age_cat, health_unit, mun_urban_cat,
                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                         all_vars(!is.na(.))))


tic()
boot.mun_prev_1 <- calculate_proj(pred_output = out.mun_prev_1)
toc()

save("boot.mun_prev_1", file = "output/fits/boot.mun_prev_1.Rda")



# missing -----------------------------------------------------------------
# load("output/fits/mun_prev_mod1.miss.Rda")
# 
# 
# out.mun_prev_1.miss <- get_preds(fit_object = mun_prev_mod1.miss, data = mdf_mun_prev_grp.miss %>%
#                               filter(!age_cat == "0-4") %>%
#                               filter_at(vars(state, time, Positive, Negative, tratamento, sex, hiv_status,
#                                              age_cat, health_unit, mun_urban_cat,
#                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                         all_vars(!is.na(.))))
# 
# 
# tic()
# boot.mun_prev_1.miss <- calculate_proj(pred_output = out.mun_prev_1.miss)
# toc()
# 
# save("boot.mun_prev_1.miss", file = "output/fits/boot.mun_prev_1.miss.Rda")





#################################################################
##              Municipality: Model 2 (2016-2019)              ##
#################################################################
# load("output/fits/mun_prev_mod2.Rda")
# 
# out.mun_prev_2 <- get_preds(fit_object = mun_prev_mod2, data = mdf_prev_ind %>%
#                               filter(!age_cat == "0-4") %>%
#                                filter(time > 8) %>%
#                                mutate(cases = 1) %>%
#                                filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                                               age_cat, health_unit, mun_urban_cat,
#                                               mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                          all_vars(!is.na(.))))
# 
# tic()
# boot.mun_prev_2 <- calculate_proj(pred_output = out.mun_prev_2)
# toc()
# 
# save("boot.mun_prev_2", file = "output/fits/boot.mun_prev_2.Rda")



# # missing -----------------------------------------------------------------
# load("output/fits/mun_prev_mod2.miss.Rda")
# 
# 
# out.mun_prev_2.miss <- get_preds(fit_object = mun_prev_mod2.miss, data = mdf_mun_prev_grp.miss %>%
#                                    filter(!age_cat == "0-4") %>%
#                                    filter(time > 8) %>%
#                                    filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                                                   age_cat, health_unit, mun_urban_cat,
#                                                   mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                              all_vars(!is.na(.))))
# 
# 
# tic()
# boot.mun_prev_2.miss <- calculate_proj(pred_output = out.mun_prev_2.miss)
# toc()
# 
# save("boot.mun_prev_2.miss", file = "output/fits/boot.mun_prev_2.miss.Rda")
# 




#################################################################
##              Municipality: Model 3 (2017-2019)              ##
#################################################################
# load("output/fits/mun_prev_mod3.Rda")
# 
# out.mun_prev_3 <- get_preds(fit_object = mun_prev_mod3, data = mdf_prev_ind %>%
#                                filter(!age_cat == "0-4") %>%
#                                filter(time > 12) %>%
#                                mutate(cases = 1) %>%
#                                filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                                               age_cat, health_unit, mun_urban_cat,
#                                               mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                          all_vars(!is.na(.))))
# 
# tic()
# boot.mun_prev_3 <- calculate_proj(pred_output = out.mun_prev_3)
# toc()
# 
# save("boot.mun_prev_3", file = "output/fits/boot.mun_prev_3.Rda")




# missing -----------------------------------------------------------------
# load("output/fits/mun_prev_mod3.miss.Rda")
# 
# 
# out.mun_prev_3.miss <- get_preds(fit_object = mun_prev_mod3.miss, data = mdf_mun_prev_grp.miss %>%
#                                    filter(!age_cat == "0-4") %>%
#                                    filter(time > 12) %>%
#                                    filter_at(vars(state, time, Positive, Negative, tratamento, sex, hiv_status,
#                                                   age_cat, health_unit, mun_urban_cat,
#                                                   mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                              all_vars(!is.na(.))))
# 
# 
# tic()
# boot.mun_prev_3.miss <- calculate_proj(pred_output = out.mun_prev_3.miss)
# toc()
# 
# save("boot.mun_prev_3.miss", file = "output/fits/boot.mun_prev_3.miss.Rda")




#################################################################
##              Municipality: Model 4 (2015-2019)              ##
#################################################################
# load("output/fits/mun_prev_mod4.Rda")
# 
# out.mun_prev_4 <- get_preds(fit_object = mun_prev_mod4, data = mdf_prev_ind %>% 
#                               filter(!age_cat == "0-4") %>%
#                               filter(time != 12 & time >= 5) %>%
#                               mutate(cases = 1) %>% 
#                               filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                                              age_cat, health_unit, mun_urban_cat,
#                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                         all_vars(!is.na(.))))
# 
# tic()
# boot.mun_prev_4 <- calculate_proj(pred_output = out.mun_prev_4)
# toc()
# 
# save("boot.mun_prev_4", file = "output/fits/boot.mun_prev_4.Rda")
# 
# 


# missing -----------------------------------------------------------------
# load("output/fits/mun_prev_mod4.miss.Rda")
# 
# 
# out.mun_prev_4.miss <- get_preds(fit_object = mun_prev_mod4.miss, data = mdf_mun_prev_grp.miss %>%
#                                    filter(!age_cat == "0-4") %>%
#                                    filter(time != 12 & time >= 5) %>%
#                                    filter_at(vars(state, time, Positive, Negative, tratamento, sex, hiv_status,
#                                                   age_cat, health_unit, mun_urban_cat,
#                                                   mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                              all_vars(!is.na(.))))
# 
# 
# tic()
# boot.mun_prev_4.miss <- calculate_proj(pred_output = out.mun_prev_4.miss)
# toc()
# 
# save("boot.mun_prev_4.miss", file = "output/fits/boot.mun_prev_4.miss.Rda")
# 








############################################################################
############################################################################
###                                                                      ###
###                               APPENDIX                               ###
###                                                                      ###
############################################################################
############################################################################



#################################################################
##                          New Cases                          ##
#################################################################
# load(here::here("data/mdf_mic_new_grp.Rdata"))

#################################################################
##              Micro-region: Model 1 (2014-2019)              ##
#################################################################
# load("output/fits/mic_new_mod1a.Rda")
#
# boot.mic_new_1 <- get_preds(fit_object = mic_new_mod1a, data = mdf_mic_new_grp %>% 
#                           filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                          age_cat, health_unit, mic_urban_cat,
#                                          mic_fhs_cat, mic_has_prison, mic_bf_cat),
#                                     all_vars(!is.na(.))))
# 
# boot.mic_new_1[["pred.int_state"]] <- boot.mic_new_1[["pred.int_state"]]  %>% 
#   merge(mdf_mic_new_grp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# save("boot.mic_new_1", file = "output/fits/boot.mic_new_1.Rda")



#################################################################
##              Micro-region: Model 2 (2016-2019)              ##
#################################################################
# load("output/fits/mic_new_mod2.Rda")


# boot.mic_new_2 <- get_preds(fit_object = mic_new_mod2, data = mdf_mic_new_grp %>%
#                               filter(time > 8) %>%
#                               filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                          age_cat, health_unit, mic_urban_cat,
#                                          mic_fhs_cat, mic_has_prison, mic_bf_cat),
#                                     all_vars(!is.na(.))))
# 
# boot.mic_new_2[["pred.int_state"]] <- boot.mic_new_2[["pred.int_state"]]  %>%
#   merge(mdf_mic_new_grp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# 
# save("boot.mic_new_2", file = "output/fits/boot.mic_new_2.Rda")
# 
# 




##################################################################
##                        Previous Cases                        ##
##################################################################

#################################################################
##              Micro-region: Model 1 (2014-2019)              ##
#################################################################
# load("output/fits/mic_prev_mod1.Rda")

# boot.mic_prev_1 <- get_preds(fit_object = mic_prev_mod1a, data = mdf_prev_ind %>% 
#                               filter(!age_cat == "0-4") %>% # Filtering outliers for now
#                               filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                                              age_cat, health_unit, mic_urban_cat,
#                                              mic_fhs_cat, mic_has_prison, mic_bf_cat),
#                                         all_vars(!is.na(.))))
# 
# boot.mic_prev_1[["pred.int_state"]] <- boot.mic_prev_1[["pred.int_state"]]  %>% 
#   merge(mdf_prev_ind %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# 
# save("boot.mic_prev_1", file = "output/fits/boot.mic_prev_1.Rda")
# 
# 
# 
# 
# #################################################################
# ##              Micro-region: Model 2 (2016-2019)              ##
# #################################################################
# load("output/fits/mic_prev_mod2.Rda")
# 
# boot.mic_prev_2 <- get_preds(fit_object = mic_prev_mod2, data = mdf_prev_ind %>%
#                                filter(!age_cat == "0-4") %>%
#                                filter(time > 8) %>%
#                                filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                                               age_cat, health_unit, mic_urban_cat,
#                                               mic_fhs_cat, mic_has_prison, mic_bf_cat),
#                                          all_vars(!is.na(.))))
# 
# boot.mic_prev_2[["pred.int_state"]] <- boot.mic_prev_2[["pred.int_state"]]  %>%
#   merge(mdf_prev_ind %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# boot.mic_prev_2[["pred.int_state_year"]] <- boot.mic_prev_2[["pred.int_state_year"]]  %>%
#   merge(mdf_prev_ind %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# save("boot.mic_prev_2", file = "output/fits/boot.mic_prev_2.Rda")
# 












## 5a - state (quarter)
# pred.int_state <- output %>%
#   group_by(state, time) %>%
#   summarize(mean = mean(c_across(starts_with("X"))),
#             lci = quantile(c_across(starts_with("X")), c(0.025)),
#             hci = quantile(c_across(starts_with("X")), c(0.975))) %>%
#   mutate(mean = plogis(mean),
#          lci = plogis(lci),
#          hci = plogis(hci))
#   
# pred.int_state <- output %>%
#   group_by(state, time) %>%
#   summarize(mean_log = mean(c_across(starts_with("X")), na.rm = TRUE),
#             lci_log = quantile(c_across(starts_with("X")), c(0.025), na.rm = TRUE),
#             hci_log = quantile(c_across(starts_with("X")), c(0.975), na.rm = TRUE)) %>%
#   mutate(mean = plogis(mean_log),
#          lci = plogis(lci_log),
#          hci = plogis(hci_log))

## 5b - state (year)
# pred.int_state_year <- output %>%
#   mutate(year = case_when(time <= 4 ~ 2014,
#                           time > 4 & time <= 8 ~ 2015,
#                           time > 8 & time <= 12 ~ 2016,
#                           time > 12 & time <= 16 ~ 2017,
#                           time > 16 & time <= 20 ~ 2018,
#                           time > 20 & time <= 24 ~ 2019)) %>%
#   group_by(state, year) %>%
#   summarize(mean_log = mean(c_across(starts_with("X")), na.rm = TRUE),
#             lci_log = quantile(c_across(starts_with("X")), c(0.025), na.rm = TRUE),
#             hci_log = quantile(c_across(starts_with("X")), c(0.975), na.rm = TRUE)) %>%
#   mutate(mean = plogis(mean_log),
#          lci = plogis(lci_log),
#          hci = plogis(hci_log))

## 5c - national (quarter)
pred.int_nat <- output %>%
  group_by(time) %>%
  summarize(mean_log = mean(c_across(starts_with("X")), na.rm = TRUE),
            lci_log = quantile(c_across(starts_with("X")), c(0.025), na.rm = TRUE),
            hci_log = quantile(c_across(starts_with("X")), c(0.975), na.rm = TRUE)) %>%
  mutate(mean = plogis(mean_log),
         lci = plogis(lci_log),
         hci = plogis(hci_log))


## 5d - national (year)
# pred.int_nat_year <- output %>%
#   mutate(year = case_when(time <= 4 ~ 2014,
#                           time > 4 & time <= 8 ~ 2015,
#                           time > 8 & time <= 12 ~ 2016,
#                           time > 12 & time <= 16 ~ 2017,
#                           time > 16 & time <= 20 ~ 2018,
#                           time > 20 & time <= 24 ~ 2019)) %>%
#   group_by(year) %>%
#   summarize(mean_log = mean(c_across(starts_with("X")), na.rm = TRUE),
#             lci_log = quantile(c_across(starts_with("X")), c(0.025), na.rm = TRUE),
#             hci_log = quantile(c_across(starts_with("X")), c(0.975), na.rm = TRUE)) %>%
#   mutate(mean = plogis(mean_log),
#          lci = plogis(lci_log),
#          hci = plogis(hci_log))


## 5e - municipality (2016, 2019)
# pred.int_mun <- output %>%
#   filter(time > 8 & time <= 12 | time > 20) %>% 
#   mutate(year = case_when(
#     # time <= 4 ~ 2014, 
#     #                       time > 4 & time <= 8 ~ 2015, 
#                           time > 8 & time <= 12 ~ 2016, 
#                           # time > 12 & time <= 16 ~ 2017, 
#                           # time > 16 & time <= 20 ~ 2018, 
#                           time > 20 & time <= 24 ~ 2019)) %>% 
#   group_by(id_municip, year) %>% 
#   summarize(mean_log = mean(c_across(starts_with("X")), na.rm = TRUE),
#             lci_log = quantile(c_across(starts_with("X")), c(0.025), na.rm = TRUE),
#             hci_log = quantile(c_across(starts_with("X")), c(0.975), na.rm = TRUE)) %>%
#   mutate(mean = plogis(mean_log),
#          lci = plogis(lci_log),
#          hci = plogis(hci_log))

return(list(
  # pred.int_state = pred.int_state, 
  pred.int_nat = pred.int_nat, 
  # pred.int_nat_year = pred.int_nat_year,
  pred.int_state_year = pred.int_state_year
  # pred.int_mun = pred.int_mun
))

}



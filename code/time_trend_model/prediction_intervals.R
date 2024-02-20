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
  par_vcv <- vcov(fit_object,unconditional=T)
  
  
  # 2 Prepare linear predictor matrix - include all the strata you want predictions for.
  # It has to have the same variables (names, factor levels, etc) as used to fit
  # the model (ie fit_object).
  lpm <- predict(fit_object, newdata = data, type = "lpmatrix")
  
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


# 2. Write function to get projected cases  -----------------------------------
calculate_proj <- function(pred_output){
  
  x_columns <- pred_output %>% ungroup() %>% select(starts_with("X"))
  
  log_mean_row <- rowMeans(x_columns, na.rm = TRUE)
  
  log_quant_row <- apply(x_columns, 1, quantile, c(0.025, 0.5, 0.975)) # Confirm that you should be taking quantile and then transforming
  
  log_quant_df <- as.data.frame(t(log_quant_row))
  
  projected <- cbind(log_mean_row, log_quant_df, pred_output) %>% 
    mutate(mean_val = plogis(log_mean_row), 
           lci_val = plogis(`2.5%`),
           med_val = plogis(`50%`),
           hci_val = plogis(`97.5%`), 
           proj_mean_cases = cases * mean_val, 
           proj_med_cases = cases * med_val,
           proj_lci_cases = cases * lci_val, 
           proj_hci_cases = cases * hci_val)
    
    
  pred.nat_qrt <- projected %>% 
    group_by(time, diag_qrt) %>% 
    summarize(obs_TB_cases = sum(cases),
              proj_mean_RR_cases = sum(proj_mean_cases), 
              proj_med_RR_cases = sum(proj_med_cases),
              lci_RR_cases = sum(proj_lci_cases),
              hci_RR_cases = sum(proj_hci_cases), 
              mean_pct = mean(mean_val), 
              med_pct = median(med_val), 
              mean_lci_pct = mean(lci_val), 
              mean_hci_pct = mean(hci_val), 
              med_lci_pct = median(lci_val), 
              med_hci_pct = median(hci_val))
  
  
  pred.nat_yr <- projected %>%
    mutate(year = case_when(time <= 4 ~ 2014,
                            time > 4 & time <= 8 ~ 2015,
                            time > 8 & time <= 12 ~ 2016,
                            time > 12 & time <= 16 ~ 2017,
                            time > 16 & time <= 20 ~ 2018,
                            time > 20 & time <= 24 ~ 2019)) %>%
    group_by(year) %>% 
    summarize(obs_TB_cases = sum(cases),
              proj_mean_RR_cases = sum(proj_mean_cases), 
              proj_med_RR_cases = sum(proj_med_cases),
              lci_RR_cases = sum(proj_lci_cases),
              hci_RR_cases = sum(proj_hci_cases), 
              mean_pct = mean(mean_val), 
              med_pct = median(med_val), 
              mean_lci_pct = mean(lci_val), 
              mean_hci_pct = mean(hci_val), 
              med_lci_pct = median(lci_val), 
              med_hci_pct = median(hci_val))
  
  
  pred.state_qrt <- projected %>% 
    group_by(state_nm, diag_qrt) %>% 
    summarize(obs_TB_cases = sum(cases),
              proj_mean_RR_cases = sum(proj_mean_cases), 
              proj_med_RR_cases = sum(proj_med_cases),
              lci_RR_cases = sum(proj_lci_cases),
              hci_RR_cases = sum(proj_hci_cases), 
              mean_pct = mean(mean_val), 
              med_pct = median(med_val), 
              mean_lci_pct = mean(lci_val), 
              mean_hci_pct = mean(hci_val), 
              med_lci_pct = median(lci_val), 
              med_hci_pct = median(hci_val))
  
  
  pred.state_yr <- projected %>%
    mutate(year = case_when(time <= 4 ~ 2014,
                            time > 4 & time <= 8 ~ 2015,
                            time > 8 & time <= 12 ~ 2016,
                            time > 12 & time <= 16 ~ 2017,
                            time > 16 & time <= 20 ~ 2018,
                            time > 20 & time <= 24 ~ 2019)) %>%
    group_by(state_nm, year) %>% 
    summarize(obs_TB_cases = sum(cases),
              proj_mean_RR_cases = sum(proj_mean_cases), 
              proj_med_RR_cases = sum(proj_med_cases),
              lci_RR_cases = sum(proj_lci_cases),
              hci_RR_cases = sum(proj_hci_cases), 
              mean_pct = mean(mean_val), 
              med_pct = median(med_val), 
              mean_lci_pct = mean(lci_val), 
              mean_hci_pct = mean(hci_val), 
              med_lci_pct = median(lci_val), 
              med_hci_pct = median(hci_val))
  
  
  return(list(pred.nat_qrt = pred.nat_qrt,
              pred.nat_yr = pred.nat_yr, 
              pred.state_qrt = pred.state_qrt,
              pred.state_yr = pred.state_yr
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
load(here::here("data/mdf_mun_new_grp.xpert.Rdata"))

# load(here::here("data/mdf_mun_new_grp.xpert.Rdata"))
# load(here::here("data/mdf_mun_new_grp.tmp.Rdata"))



## 2.2 Municipality: Model 1 (2014-2019) -----------------------------------


#################################################################
##              Municipality: Model 1 (2014-2019)              ##
#################################################################


# complete cases ----------------------------------------------------------


load("output/fits/mun_new_mod1.Rda")

tic()
out.mun_new_1 <- get_preds(fit_object = mun_new_mod1, data = mdf_mun_new_grp %>%
                              filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
                                             age_cat, health_unit, mun_urban_cat,
                                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                        all_vars(!is.na(.))))
toc()

tic()
boot.mun_new_1 <- calculate_proj(pred_output = out.mun_new_1)
toc()

save("boot.mun_new_1", file = "output/fits/boot.mun_new_1.Rda")




# Missing  ----------------------------------------------------------------
load("output/fits/mun_new_mod1.xpert.Rda")

tic()
out.mun_new_1_xpert <- get_preds(fit_object = mun_new_mod1.xpert, data = mdf_mun_new_grp.xpert %>%
                             filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
                                            age_cat, health_unit, mun_urban_cat,
                                            mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                       all_vars(!is.na(.))))
toc()

tic()
boot.mun_new_1_xpert <- calculate_proj(pred_output = out.mun_new_1_xpert)
toc()

save("boot.mun_new_1_xpert", file = "output/fits/boot.mun_new_1_xpert.Rda")




#################################################################
##                  TMP - With missing levels                  ##
#################################################################
# load("output/fits/mun_new_mod1.tmp.Rda")
# mun_new_mod1.tmp <- mun_new_mod1
# rm(mun_new_mod1)
# 
# boot.mun_new_1.tmp <- get_preds(fit_object = mun_new_mod1.tmp, data = mdf_mun_new_grp.tmp %>% 
#                               filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                              age_cat, health_unit, mun_urban_cat,
#                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                         all_vars(!is.na(.))))
# 
# 
# # add state names
# boot.mun_new_1.tmp[["pred.int_state"]] <- boot.mun_new_1.tmp[["pred.int_state"]]  %>% 
#   merge(mdf_mun_new_grp.tmp %>% ungroup() %>% dplyr::select(state_nm, state, time, diag_qrt) %>% unique(), by = c("state", "time"))
# 
# boot.mun_new_1.tmp[["pred.int_state_year"]] <- boot.mun_new_1.tmp[["pred.int_state_year"]]  %>% 
#   merge(mdf_mun_new_grp.tmp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# boot.mun_new_1.tmp[["pred.int_nat"]] <- boot.mun_new_1.tmp[["pred.int_nat"]]  %>% 
#   merge(mdf_mun_new_grp.tmp %>% ungroup() %>% dplyr::select(time, diag_qrt) %>% unique(), by = "time")
# 
# 
# save("boot.mun_new_1.tmp", file = "output/fits/boot.mun_new_1.tmp.Rda")
# 






# #2.3 Municipality: Model 2 (2016-2019) -----------------------------------


#################################################################
##              Municipality: Model 2 (2016-2019)              ##
#################################################################
load("output/fits/mun_new_mod2.Rda")

out.mun_new_2 <- get_preds(fit_object = mun_new_mod2, data = mdf_mun_new_grp %>%
                              filter(time > 8) %>%
                              filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
                                            age_cat, health_unit, mun_urban_cat,
                                            mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                        all_vars(!is.na(.))))

tic()
boot.mun_new_2 <- calculate_proj(pred_output = out.mun_new_2)
toc()

save("boot.mun_new_2", file = "output/fits/boot.mun_new_2.Rda")




#################################################################
##                  TMP - With missing levels                  ##
#################################################################
# load("output/fits/mun_new_mod2.tmp.Rda")
# mun_new_mod2.tmp <- mun_new_mod2
# rm(mun_new_mod2)
# 
# boot.mun_new_2.tmp <- get_preds(fit_object = mun_new_mod2.tmp, data = mdf_mun_new_grp.tmp %>% 
#                                   filter(time > 8) %>% 
#                                   filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                                  age_cat, health_unit, mun_urban_cat,
#                                                  mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                             all_vars(!is.na(.))))
# 
# 
# # add state names
# boot.mun_new_2.tmp[["pred.int_state"]] <- boot.mun_new_2.tmp[["pred.int_state"]]  %>% 
#   merge(mdf_mun_new_grp.tmp %>% ungroup() %>% dplyr::select(state_nm, state, time, diag_qrt) %>% unique(), by = c("state", "time"))
# 
# boot.mun_new_2.tmp[["pred.int_state_year"]] <- boot.mun_new_2.tmp[["pred.int_state_year"]]  %>% 
#   merge(mdf_mun_new_grp.tmp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# boot.mun_new_2.tmp[["pred.int_nat"]] <- boot.mun_new_2.tmp[["pred.int_nat"]]  %>% 
#   merge(mdf_mun_new_grp.tmp %>% ungroup() %>% dplyr::select(time, diag_qrt) %>% unique(), by = "time")
# 
# 
# save("boot.mun_new_2.tmp", file = "output/fits/boot.mun_new_2.tmp.Rda")




## 2.4 Municipality: Model 3 (2017-2019) -----------------------------------

#################################################################
##              Municipality: Model 3 (2017-2019)              ##
#################################################################
load("output/fits/mun_new_mod3.Rda")


out.mun_new_3 <- get_preds(fit_object = mun_new_mod3, data = mdf_mun_new_grp %>%
                              filter(time > 12) %>%
                              filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
                                             age_cat, health_unit, mun_urban_cat,
                                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                        all_vars(!is.na(.))))

tic()
boot.mun_new_3 <- calculate_proj(pred_output = out.mun_new_3)
toc()

save("boot.mun_new_3", file = "output/fits/boot.mun_new_3.Rda")




#################################################################
##                  TMP - With missing levels                  ##
#################################################################
# load("output/fits/mun_new_mod3.tmp.Rda")
# mun_new_mod3.tmp <- mun_new_mod3
# rm(mun_new_mod3)
# 
# boot.mun_new_3.tmp <- get_preds(fit_object = mun_new_mod3.tmp, data = mdf_mun_new_grp.tmp %>% 
#                                   filter(time > 12) %>% 
#                                   filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
#                                                  age_cat, health_unit, mun_urban_cat,
#                                                  mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                             all_vars(!is.na(.))))
# 
# 
# # add state names
# boot.mun_new_3.tmp[["pred.int_state"]] <- boot.mun_new_3.tmp[["pred.int_state"]]  %>% 
#   merge(mdf_mun_new_grp.tmp %>% ungroup() %>% dplyr::select(state_nm, state, time, diag_qrt) %>% unique(), by = c("state", "time"))
# 
# boot.mun_new_3.tmp[["pred.int_state_year"]] <- boot.mun_new_3.tmp[["pred.int_state_year"]]  %>% 
#   merge(mdf_mun_new_grp.tmp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# boot.mun_new_3.tmp[["pred.int_nat"]] <- boot.mun_new_3.tmp[["pred.int_nat"]]  %>% 
#   merge(mdf_mun_new_grp.tmp %>% ungroup() %>% dplyr::select(time, diag_qrt) %>% unique(), by = "time")
# 
# 
# save("boot.mun_new_3.tmp", file = "output/fits/boot.mun_new_3.tmp.Rda")









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

# load("data/mdf_prev_ind.tmp.Rdata")




## 3.2 Municipality: Model 1 (2014-2019) -----------------------------------

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


#################################################################
##                  TMP - With missing levels                  ##
#################################################################
# load("output/fits/mun_prev_mod1.tmp.Rda")
# mun_prev_mod1.tmp <- mun_prev_mod1
# rm(mun_prev_mod1)
# 
# boot.mun_prev_1.tmp <- get_preds(fit_object = mun_prev_mod1.tmp, data = mdf_prev_ind.tmp %>% 
#                                filter(!age_cat == "0-4") %>%
#                                filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                                               age_cat, health_unit, mun_urban_cat,
#                                               mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                          all_vars(!is.na(.))))
# 
# # add state names
# boot.mun_prev_1.tmp[["pred.int_state"]] <- boot.mun_prev_1.tmp[["pred.int_state"]]  %>% 
#   merge(mdf_prev_ind.tmp %>% ungroup() %>% dplyr::select(state_nm, state, time, diag_qrt) %>% unique(), by = c("state", "time"))
# 
# boot.mun_prev_1.tmp[["pred.int_state_year"]] <- boot.mun_prev_1.tmp[["pred.int_state_year"]]  %>% 
#   merge(mdf_prev_ind.tmp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# boot.mun_prev_1.tmp[["pred.int_nat"]] <- boot.mun_prev_1.tmp[["pred.int_nat"]]  %>% 
#   merge(mdf_prev_ind.tmp %>% ungroup() %>% dplyr::select(time, diag_qrt) %>% unique(), by = "time")
# 
# 
# save("boot.mun_prev_1.tmp", file = "output/fits/boot.mun_prev_1.tmp.Rda")







#################################################################
##              Municipality: Model 2 (2016-2019)              ##
#################################################################
load("output/fits/mun_prev_mod2.Rda")

out.mun_prev_2 <- get_preds(fit_object = mun_prev_mod2, data = mdf_prev_ind %>% 
                              filter(!age_cat == "0-4") %>%
                               filter(time > 8) %>%
                               mutate(cases = 1) %>% 
                               filter_at(vars(state, time, result, tratamento, sex, hiv_status,
                                              age_cat, health_unit, mun_urban_cat,
                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                         all_vars(!is.na(.))))

tic()
boot.mun_prev_2 <- calculate_proj(pred_output = out.mun_prev_2)
toc()

save("boot.mun_prev_2", file = "output/fits/boot.mun_prev_2.Rda")



#################################################################
##                  TMP - With missing levels                  ##
#################################################################
# load("output/fits/mun_prev_mod2.tmp.Rda")
# mun_prev_mod2.tmp <- mun_prev_mod2
# rm(mun_prev_mod2)
# 
# 
# boot.mun_prev_2.tmp <- get_preds(fit_object = mun_prev_mod2.tmp, data = mdf_prev_ind.tmp %>% 
#                                    filter(time > 8) %>% filter(!age_cat == 
#                                                                  "0-4") %>% 
#                                    filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                                                   age_cat, health_unit, mun_urban_cat,
#                                                   mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                              all_vars(!is.na(.))))
# 
# # add state names
# boot.mun_prev_2.tmp[["pred.int_state"]] <- boot.mun_prev_2.tmp[["pred.int_state"]]  %>% 
#   merge(mdf_prev_ind.tmp %>% ungroup() %>% dplyr::select(state_nm, state, time, diag_qrt) %>% unique(), by = c("state", "time"))
# 
# boot.mun_prev_2.tmp[["pred.int_state_year"]] <- boot.mun_prev_2.tmp[["pred.int_state_year"]]  %>% 
#   merge(mdf_prev_ind.tmp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# boot.mun_prev_2.tmp[["pred.int_nat"]] <- boot.mun_prev_2.tmp[["pred.int_nat"]]  %>% 
#   merge(mdf_prev_ind.tmp %>% ungroup() %>% dplyr::select(time, diag_qrt) %>% unique(), by = "time")
# 
# 
# save("boot.mun_prev_2.tmp", file = "output/fits/boot.mun_prev_2.tmp.Rda")
# 

#################################################################
##              Municipality: Model 3 (2017-2019)              ##
#################################################################
load("output/fits/mun_prev_mod3.Rda")

out.mun_prev_3 <- get_preds(fit_object = mun_prev_mod3, data = mdf_prev_ind %>% 
                               filter(!age_cat == "0-4") %>%
                               filter(time > 12) %>%
                               mutate(cases = 1) %>% 
                               filter_at(vars(state, time, result, tratamento, sex, hiv_status,
                                              age_cat, health_unit, mun_urban_cat,
                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                         all_vars(!is.na(.))))

tic()
boot.mun_prev_3 <- calculate_proj(pred_output = out.mun_prev_3)
toc()

save("boot.mun_prev_3", file = "output/fits/boot.mun_prev_3.Rda")




#################################################################
##                  TMP - With missing levels                  ##
#################################################################
# load("output/fits/mun_prev_mod3.tmp.Rda")
# mun_prev_mod3.tmp <- mun_prev_mod3
# rm(mun_prev_mod3)
# 
# 
# boot.mun_prev_3.tmp <- get_preds(fit_object = mun_prev_mod3.tmp, data = mdf_prev_ind.tmp %>% 
#                                    filter(time > 12) %>% filter(!age_cat == 
#                                                                   "0-4") %>% 
#                                    filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                                                   age_cat, health_unit, mun_urban_cat,
#                                                   mun_fhs_cat, mun_has_prison, mun_bf_cat),
#                                              all_vars(!is.na(.))))
# 
# # add state names
# boot.mun_prev_3.tmp[["pred.int_state"]] <- boot.mun_prev_3.tmp[["pred.int_state"]]  %>% 
#   merge(mdf_prev_ind.tmp %>% ungroup() %>% dplyr::select(state_nm, state, time, diag_qrt) %>% unique(), by = c("state", "time"))
# 
# boot.mun_prev_3.tmp[["pred.int_state_year"]] <- boot.mun_prev_3.tmp[["pred.int_state_year"]]  %>% 
#   merge(mdf_prev_ind.tmp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")
# 
# boot.mun_prev_3.tmp[["pred.int_nat"]] <- boot.mun_prev_3.tmp[["pred.int_nat"]]  %>% 
#   merge(mdf_prev_ind.tmp %>% ungroup() %>% dplyr::select(time, diag_qrt) %>% unique(), by = "time")
# 
# 
# save("boot.mun_prev_3.tmp", file = "output/fits/boot.mun_prev_3.tmp.Rda")
# 
# 
# # inspect - previously treated outliers -----------------------------------
# fit_object = mic_prev_mod2a
# 
# data = mdf_prev_ind %>% 
#   filter(time > 8) %>% 
#   filter_at(vars(state, time, result, tratamento, sex, hiv_status,
#                  age_cat, health_unit, mun_urban_cat,
#                  mic_fhs_cat, mic_has_prison, mic_bf_cat),
#             all_vars(!is.na(.)))
# 
# 
# ## calculate output by setting prev model = fit object, and using prev data = data 
# ## There are several states with massive means
# ## Calculate output by setting prev model = fit object, and using prev data = data 
# inspect.outliers <- output %>%
#   # group_by(state, time) %>%
#   rowwise() %>% 
#   mutate(mean = mean(c_across(starts_with("X")), na.rm = TRUE)) %>% 
#   filter(mean > 0) %>% 
#   dplyr::select(state, time, age, sex, tratamento, hiv_status, mean) 
# 
# ## there are only 11 0-4 previously treated cases 
# output %>% 
#   filter(age_cat == "0-4") %>% 
#   count()
# 
# 
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



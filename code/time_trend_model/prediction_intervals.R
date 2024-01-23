# Author: Sarah Baum
# Date Created: 
# Date Updated: 
# Description: Get posterior preds from RR-TB models 


library(MASS)

source(here::here("code/dependencies.R"))


## Load data
load(here::here("data/mdf_mun_new_grp.Rdata"))
# load(here::here("data/mdf_mic_new_grp.Rdata"))
load("data/mdf_prev_ind.Rdata")


## Load models 
# load("output/fits/mic_new_mod1a.Rda")
# load("output/fits/mic_new_mod2a.Rda")
# 
# load("output/fits/mic_prev_mod1a.Rda")
# load("output/fits/mic_prev_mod2a.Rda")

load("output/fits/mun_new_mod1a.Rda")
load("output/fits/mun_new_mod2a.Rda")
load("output/fits/mun_new_mod2b.Rda") # created with updated definitions of results (excluding inconclusive)

load("output/fits/mun_prev_mod1a.Rda")
load("output/fits/mun_prev_mod2a.Rda")
load("output/fits/mun_prev_mod2b.Rda") # created with updated definitions of results (excluding inconclusive)



###########################################################################
###########################################################################
###                                                                     ###
###                        CREATE PREDS FUNCTION                        ###
###                                                                     ###
###########################################################################
###########################################################################

get_preds <- function(fit_object, data){
  ## 1 Get mean and VCV 
  par_est <- coef(fit_object)
  par_vcv <- vcov(fit_object,unconditional=T)
  
  
  # 2 Prepare linear predictor matrix - include all the strata you want predictions for.
  # It has to have the same variables (names, factor levels, etc) as used to fit
  # the model (ie fit_object).
  lpm <- predict(fit_object, newdata = data, type = "lpmatrix")
  
  ## 3 Simulate from parameter posterior 
  n.sim <- 1000
  set.seed(1234)
  sim_pars <- mvrnorm(n.sim, mu = par_est, Sigma = par_vcv)
  
  ## 4 Calc results on transformed variables 
  sim_preds0 <- lpm %*% t(sim_pars) %>% data.frame()
  
  
  ## 5 get output by state and time 
  output <- cbind(data, sim_preds0)
  
  ## 5a - state (quarter)
# 
  pred.int_state <- output %>%
    group_by(state, time) %>%
    summarize(mean = mean(c_across(starts_with("X"))),
              lci = quantile(c_across(starts_with("X")), c(0.025)),
              hci = quantile(c_across(starts_with("X")), c(0.975))) %>%
    mutate(mean = plogis(mean),
           lci = plogis(lci),
           hci = plogis(hci))
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
  pred.int_state_year <- output %>%
    mutate(year = case_when(time <= 4 ~ 2014,
                            time > 4 & time <= 8 ~ 2015,
                            time > 8 & time <= 12 ~ 2016,
                            time > 12 & time <= 16 ~ 2017,
                            time > 16 & time <= 20 ~ 2018,
                            time > 20 & time <= 24 ~ 2019)) %>%
    group_by(state, year) %>%
    summarize(mean_log = mean(c_across(starts_with("X")), na.rm = TRUE),
              lci_log = quantile(c_across(starts_with("X")), c(0.025), na.rm = TRUE),
              hci_log = quantile(c_across(starts_with("X")), c(0.975), na.rm = TRUE)) %>%
    mutate(mean = plogis(mean_log),
           lci = plogis(lci_log),
           hci = plogis(hci_log))
  
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
  pred.int_nat_year <- output %>%
    mutate(year = case_when(time <= 4 ~ 2014,
                            time > 4 & time <= 8 ~ 2015,
                            time > 8 & time <= 12 ~ 2016,
                            time > 12 & time <= 16 ~ 2017,
                            time > 16 & time <= 20 ~ 2018,
                            time > 20 & time <= 24 ~ 2019)) %>%
    group_by(year) %>%
    summarize(mean_log = mean(c_across(starts_with("X")), na.rm = TRUE),
              lci_log = quantile(c_across(starts_with("X")), c(0.025), na.rm = TRUE),
              hci_log = quantile(c_across(starts_with("X")), c(0.975), na.rm = TRUE)) %>%
    mutate(mean = plogis(mean_log),
           lci = plogis(lci_log),
           hci = plogis(hci_log))
  
  
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
  
  return(list(pred.int_state = pred.int_state, 
              pred.int_nat = pred.int_nat, 
              pred.int_nat_year = pred.int_nat_year,
              pred.int_state_year = pred.int_state_year
              # pred.int_mun = pred.int_mun
              ))
  
}



###########################################################################
###########################################################################
###                                                                     ###
###                         PREDS FOR NEW CASES                         ###
###                                                                     ###
###########################################################################
###########################################################################


#################################################################
##              Micro-region: Model 1 (2014-2019)              ##
#################################################################
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
# boot.mic_new_2 <- get_preds(fit_object = mic_new_mod2a, data = mdf_mic_new_grp %>% 
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




#################################################################
##              Municipality: Model 1 (2014-2019)              ##
#################################################################
boot.mun_new_1 <- get_preds(fit_object = mun_new_mod1, data = mdf_mun_new_grp %>% 
                              filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
                                             age_cat, health_unit, mun_urban_cat,
                                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                        all_vars(!is.na(.))))

# add state names
boot.mun_new_1[["pred.int_state"]] <- boot.mun_new_1[["pred.int_state"]]  %>% 
  merge(mdf_mun_new_grp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")

boot.mun_new_1[["pred.int_state_year"]] <- boot.mun_new_1[["pred.int_state_year"]]  %>% 
  merge(mdf_mun_new_grp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")



save("boot.mun_new_1", file = "output/fits/boot.mun_new_1.Rda")




#################################################################
##              Municipality: Model 2 (2016-2019)              ##
#################################################################
boot.mun_new_2 <- get_preds(fit_object = mun_new_mod2, data = mdf_mun_new_grp %>% 
                              filter(time > 8) %>% 
                              filter_at(vars(state, time, Positive, Negative, sex, hiv_status,
                                            age_cat, health_unit, mun_urban_cat,
                                            mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                        all_vars(!is.na(.))))

# add state names
boot.mun_new_2[["pred.int_state"]] <- boot.mun_new_2[["pred.int_state"]]  %>% 
  merge(mdf_mun_new_grp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")

boot.mun_new_2[["pred.int_state_year"]] <- boot.mun_new_2[["pred.int_state_year"]]  %>% 
  merge(mdf_mun_new_grp %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")




save("boot.mun_new_2", file = "output/fits/boot.mun_new_2.Rda")








############################################################################
############################################################################
###                                                                      ###
###                       PREDS FOR PREVIOUS CASES                       ###
###                                                                      ###
############################################################################
############################################################################

## Note: because of weird outliers, filter those 0-4 years old for now


#################################################################
##              Micro-region: Model 1 (2014-2019)              ##
#################################################################
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
# boot.mic_prev_2 <- get_preds(fit_object = mic_prev_mod2a, data = mdf_prev_ind %>% 
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
# save("boot.mic_prev_2", file = "output/fits/boot.mic_prev_2.Rda")



#################################################################
##              Municipality: Model 1 (2014-2019)              ##
#################################################################
boot.mun_prev_1 <- get_preds(fit_object = mun_prev_mod1, data = mdf_prev_ind %>% 
                               filter(!age_cat == "0-4") %>%
                               filter_at(vars(state, time, result, tratamento, sex, hiv_status,
                                              age_cat, health_unit, mun_urban_cat,
                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                         all_vars(!is.na(.))))

# add state names
boot.mun_prev_1[["pred.int_state"]] <- boot.mun_prev_1[["pred.int_state"]]  %>% 
  merge(mdf_prev_ind %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")

boot.mun_prev_1[["pred.int_state_year"]] <- boot.mun_prev_1[["pred.int_state_year"]]  %>% 
  merge(mdf_prev_ind %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")



save("boot.mun_prev_1", file = "output/fits/boot.mun_prev_1.Rda")




#################################################################
##              Municipality: Model 2 (2016-2019)              ##
#################################################################
boot.mun_prev_2 <- get_preds(fit_object = mun_prev_mod2, data = mdf_prev_ind %>%
                               filter(!age_cat == "0-4") %>%
                               filter(time > 8) %>%
                               filter_at(vars(state, time, result, tratamento, sex, hiv_status,
                                              age_cat, health_unit, mun_urban_cat,
                                              mun_fhs_cat, mun_has_prison, mun_bf_cat),
                                         all_vars(!is.na(.))))

# add state names
boot.mun_prev_2[["pred.int_state"]] <- boot.mun_prev_2[["pred.int_state"]]  %>% 
  merge(mdf_prev_ind %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")

boot.mun_prev_2[["pred.int_state_year"]] <- boot.mun_prev_2[["pred.int_state_year"]]  %>% 
  merge(mdf_prev_ind %>% ungroup() %>% dplyr::select(state_nm, state) %>% unique(), by = "state")


save("boot.mun_prev_2", file = "output/fits/boot.mun_prev_2.Rda")



# inspect - previously treated outliers -----------------------------------
fit_object = mic_prev_mod2a

data = mdf_prev_ind %>% 
  filter(time > 8) %>% 
  filter_at(vars(state, time, result, tratamento, sex, hiv_status,
                 age_cat, health_unit, mun_urban_cat,
                 mic_fhs_cat, mic_has_prison, mic_bf_cat),
            all_vars(!is.na(.)))


## calculate output by setting prev model = fit object, and using prev data = data 
## There are several states with massive means
## Calculate output by setting prev model = fit object, and using prev data = data 
inspect.outliers <- output %>%
  # group_by(state, time) %>%
  rowwise() %>% 
  mutate(mean = mean(c_across(starts_with("X")), na.rm = TRUE)) %>% 
  filter(mean > 0) %>% 
  dplyr::select(state, time, age, sex, tratamento, hiv_status, mean) 

## there are only 11 0-4 previously treated cases 
output %>% 
  filter(age_cat == "0-4") %>% 
  count()



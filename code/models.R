# Author: Sarah Baum
# Created: 2023-06-26
# Updated: 2023-06-29

# Description: 
# - Run Model 1: Probability of receiving Xpert, conditional on covariates
# - Run Model 2: Probability of RR positive outcome, conditional on covariates 


source(here::here("code/dependencies.R"))

library(fixest)
library(lmtest)
library(sandwich)
library(gtsummary)

load("data/sinan_xpert.Rdata")



# Cases -------------------------------------------------------------------
## prep model data for cases ---------------------------------------------------------
model_case <- sinan_xpert

# Create outcome variable - Received Xpert: test_molec responses are sensitive, resistant, not detectable, or inconclusive
# tabyl(sinan_xpert_model, test_molec, diag_yr) # Most missing are from 2014 and 2015 
model_case$received_xpert <- if_else(sinan_xpert$test_molec %in% c("1", "2", "3", "4"), 1, 0)



# Covariates: Age, Previous diagnosis with TB, HIV status, Type of Facility, and year
## Age - Defined as difference between diagnosis and birth date



## Previous TB history - Previous TB diagnosis = 1 if tratamento == 2 or 3; 0 if new case 
model_case$tb_hist <- if_else(model_case$tratamento %in% c("2", "3"), 1, 
                                     if_else(model_case$tratamento %in% c("1"), 0, 
                                             if_else(model_case$tratamento %in% c("5"), 2, 
                                                     if_else(model_case$tratamento %in% c("6"), 3, NA))))
# tabyl(model_case, tratamento)                              



## HIV Status - There are two HIV/AIDS variables, agravaids and hiv. I think agravaids is asked before you would know the hiv test result. 
# tabyl(model_case,hiv_status)

model_case$hiv_status <- if_else(model_case$agravaids == 1, 1, # 1 - HIV positive
                                        if_else(model_case$agravaids == 2, 0, NA)) # 0 - HIV negative


## sex
model_case$sex <- if_else(model_case$cs_sexo == "M", 1, if_else(model_case$cs_sexo == "F", 0, NA))

tabyl(model_case, nu_idade_n)



# distance to municipality with xpert
source(here::here("code/00_calculate_distance_to_xpert.R"))

list_of_years <- unique(sinan_xpert$diag_yr) %>% as.character()
calculate_distance_to_xpert(list_of_years)

model_case <- model_case %>% left_join(final_result %>% mutate(diag_yr = as.Date(diag_yr)), by = c("id_municip" = "CD_MUN_merge", "diag_yr"))




## Summary Table -----------------------------------------------------------

for (i in list_of_years) {

model_case %>% 
    filter(diag_yr == "2014-01-01") %>% 
    select(received_xpert, age, tb_hist, hiv_status, diag_yr, health_unit, min_dist) %>% 
    tbl_summary(
    by = received_xpert, 
    # missing = "no"
    ) %>%
    add_n() %>% # add column with total number of non-missing observations
    add_p() %>% 
    modify_header(label = "**Variable**")
}







# #Model - Predictors of whether patients have access to xpert -------
pr_mod_year <- list() 
years <- unique(sinan_xpert$diag_yr) %>% as.character()


for (i in years) {
  
  i <- "2017-01-01"
  pr_mod <- glm(received_xpert ~ access_xpert + age + I(age^2) + factor(tb_hist) + 
                  factor(agravaids) + factor(health_unit) + factor(pop_liber) + 
                  factor(pop_rua) + factor(pop_imig) + factor(benef_gov),
                family = "binomial" (link = "logit"),
                data = model_case %>% filter(diag_yr == i))
  
  summary(pr_mod)
  exp(coef(pr_mod))
  pr_mod_year[[i]] <- pr_mod
  
}

# output results
pr_year <- bind_rows(broom::tidy(pr_mod_year[["2014-01-01"]], conf.int=TRUE) %>% mutate(model = "2014"), 
                     broom::tidy(pr_mod_year[["2015-01-01"]], conf.int=TRUE) %>% mutate(model = "2015"),
                     broom::tidy(pr_mod_year[["2016-01-01"]], conf.int=TRUE) %>% mutate(model = "2016"),
                     broom::tidy(pr_mod_year[["2017-01-01"]], conf.int=TRUE) %>% mutate(model = "2017"),
                     broom::tidy(pr_mod_year[["2018-01-01"]], conf.int=TRUE) %>% mutate(model = "2018"),
                     broom::tidy(pr_mod_year[["2019-01-01"]], conf.int=TRUE) %>% mutate(model = "2019"))




# xpert neg but sput/smear + ----------------------------------------------
# Differences between those who are positive on smear + culture but neg on Xpert vs. those who are pos on xpert
sinan_xpert$xpert_neg <- if_else((sinan_xpert$cultura_es == "1" | sinan_xpert$bacilosc_e == "1") & sinan_xpert$test_molec == "3", 1, 
                                 if_else(sinan_xpert$test_molec %in% c("1", "2"), 0, NA))

tabyl(sinan_xpert, xpert_neg, test_molec)


mod_neg <- glm(xpert_neg ~ age + tb_hist + sex + hiv_status,
               family = "binomial" (link = "logit"),
               data = sinan_xpert)

summary(mod_neg)
exp(coef(mod_neg))












## CLEAN THIS UP 
## Characteristics of those who receive Xpert vs. those who do not 
# 
# 
# mod_received_access <- glm(received_xpert ~ access_xpert + factor(diag_yr),
#                        family = "binomial" (link = "logit"),
#                        data = sinan_xpert)
# 
# summary(mod_received_access)
# exp(coef(mod_received_access))
# mod_received_access <- feglm(received_xpert ~ access_xpert | factor(diag_yr) + state_nm,
#                            family = "binomial" (link = "logit"),
#                            data = sinan_xpert)
# 
# 
# 
# mod_char <- glm(received_xpert ~ age + factor(tb_hist) + hiv_status + factor(sex) + factor(diag_yr) + factor(hiv_status), 
#                 family = "binomial" (link = "logit"), 
#                 data = sinan_xpert)
# 
# summary(mod_char)
# exp(coef(mod_char))
# 





# Facilities --------------------------------------------------------------
## Model - Predictors of whether facilities have access to xpert  ------

## data for facility 
cor <- sinan_xpert %>% 
  filter(diag_yr >= "2014-01-01" & diag_yr < "2022-01-01") %>% 
  # mutate(receive_xpert = if_else(test_molec %in% c("1", "2", "3", "4"), 1, 0)) %>% 
  group_by(id_municip, diag_yr, access_xpert, test_molec) %>% 
  summarize(cases = n()) %>% 
  group_by(id_municip, diag_yr) %>% 
  # percent of diagnoses with xpert and RR-TB positive by quarter 
  summarize(access_xpert = if_else(sum(access_xpert) > 0, 1, 0), 
            total_cases = sum(cases), 
            # number of cases diagnosed with Xpert
            diag_with_xpert = sum(cases[test_molec %in% c("1", "2", "3", "4")]),
            # percent of cases diagnosed with Xpert
            pct_cases_xpert = if_else(is.nan(diag_with_xpert/total_cases), 0, diag_with_xpert/total_cases), 
            # number of cases that are TB positive
            diag_TB_pos = sum(cases[test_molec %in% c("1", "2")]),
            # percent of cases that are TB positive
            pct_TB_pos = if_else(is.nan(diag_TB_pos/diag_with_xpert), 0, diag_TB_pos/diag_with_xpert),
            # number of Xpert cases that are RR+
            diag_RR_pos = sum(cases[test_molec == "2"]), 
            # percent of Xpert cases that are RR+
            pct_RR_pos = if_else(is.nan(diag_RR_pos/diag_TB_pos), 0, diag_RR_pos/diag_TB_pos))

# ggplot(cor, aes(x = pct_cases_xpert, y = pct_RR_pos, color = factor(access_xpert))) + 
#   geom_point() + 
#   facet_wrap(~ diag_yr) + 
#   geom_smooth(method = "lm", se = FALSE)


# cor between municip access to xpert and prev of RR TB 
mod_cor <- glm(pct_RR_pos ~ access_xpert + factor(diag_yr), data = cor)
summary(mod_cor)







# move this to patients 

























## Model - Pr receiving Xpert  ------------------------------------------------------------
# pr_mod_all <- glm(receive_xpert ~ age + I(age^2) + tb_hist + hiv_status + factor(health_unit), 
#                 family = "binomial" (link = "logit"),
#                 data = sinan_xpert_model)
# 
# 
# broom::tidy(pr_mod_all, exponentiate=TRUE)
# 
# feols_test <- feglm(receive_xpert ~ age + I(age^2) + tb_hist + hiv_status + factor(health_unit) | diag_yr, 
#                     family = "binomial" (link = "logit"), 
#                     data = sinan_xpert_model)
# summary(feols_test)
# 
# # remove missing to be able to predict
# sinan_xpert_mod_pr <- sinan_xpert_model %>% filter(!is.na(receive_xpert) & !is.na(age) & !is.na(tb_hist) & !is.na(hiv_status) & !is.na(diag_yr) & !is.na(health_unit) & !is.na(state_nm))
# 
# 
# sinan_xpert_mod_pr$pr_score <- predict(pr_mod_all, type = "response")
# 
# 
# # look at distributions of missing vars
# treated <- sinan_xpert_mod_pr %>% 
#   filter(receive_xpert == 1) %>%
#   ggplot(., aes(pr_score)) +
#   geom_histogram() + 
#   labs (x = "Propensity Score", 
#         y = "Count") + 
#   theme_bw() + 
#   ggtitle("Treated")
# 
# untreated <- sinan_xpert_mod_pr %>% 
#   filter(receive_xpert == 0) %>%
#   ggplot(., aes(pr_score)) +
#   geom_histogram() + 
#   labs (x = "Propensity Score", 
#         y = "Count") + 
#   theme_bw() + 
#   ggtitle("Untreated")
# 
# 
# 
# 
# 



## Model - By Year ---------------------------------------------------------
pr_mod_year <- list() 
years <- unique(sinan_xpert$diag_yr) %>% as.character()


for (i in years) {
  pr_mod <- glm(receive_xpert ~ age + I(age^2) + factor(tratamento) + factor(agravaids) + factor(health_unit),
                   family = "binomial" (link = "logit"),
                   data = sinan_xpert %>% filter(diag_yr == i))
  
  pr_mod_year[[i]] <- pr_mod

}

# output results
pr_year <- bind_rows(broom::tidy(pr_mod_year[["2014-01-01"]], conf.int=TRUE) %>% mutate(model = "2014"), 
                     broom::tidy(pr_mod_year[["2015-01-01"]], conf.int=TRUE) %>% mutate(model = "2015"),
                     broom::tidy(pr_mod_year[["2016-01-01"]], conf.int=TRUE) %>% mutate(model = "2016"),
                     broom::tidy(pr_mod_year[["2017-01-01"]], conf.int=TRUE) %>% mutate(model = "2017"),
                     broom::tidy(pr_mod_year[["2018-01-01"]], conf.int=TRUE) %>% mutate(model = "2018"),
                     broom::tidy(pr_mod_year[["2019-01-01"]], conf.int=TRUE) %>% mutate(model = "2019"))

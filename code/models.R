# Author: Sarah Baum
# Created: 2023-06-26
# Updated: 2023-06-27
# Description: 
# - Run Model 1: Probability of receiving Xpert, conditional on covariates
# - Run Model 2: Probability of RR positive outcome, conditional on covariates 


source(here::here("code/dependencies.R"))

load("data/sinan_xpert.Rdata")

## Model 1: Probability of receiving Xpert, conditional on covariates



# Create outcome variable: test_molec is sensitive, resistant, not detectable, or inconclusive
tabyl(sinan_xpert, test_molec, diag_yr) # Most missing are from 2014 and 2015 
sinan_xpert$receive_xpert <- if_else(sinan_xpert$test_molec %in% c("1", "2", "3", "4"), 1, 
                                     if_else(sinan_xpert$test_molec %in% c("5"), 0, NA))
levels(sinan_xpert$test_molec)

# Covariates: Age, Previous diagnosis with TB, HIV status, Type of Facility, Time, and Geography (hard with municipalities and data)
## Age: Defined as difference between diagnosis and birth date

## Previous TB history:  Previous TB diagnosis = 1 if tratamento == 2 or 3; 0 if new case 
sinan_xpert$tb_hist <- if_else(sinan_xpert$tratamento %in% c("2", "3"), 1, 
                               if_else(sinan_xpert$tratamento %in% c("1"), 0, 
                                       if_else(sinan_xpert$tratamento %in% c("5"), 2, 
                                               if_else(sinan_xpert$tratamento %in% c("6"), 3, NA))))
tabyl(sinan_xpert, tratamento)                              
    ### I'm not sure if I need to be concerned about post-mortems 
    ### Remove if unknown 
    ### What to do about transfers?

## HIV Status: There are two HIV/AIDS variables, agravaids and hiv. I think agravaids is asked before you would know the hiv test result. 
tabyl(sinan_xpert, agravaids, hiv)

sinan_xpert$agravaids <- if_else(sinan_xpert$agravaids == 1, 1, 
                                 if_else(sinan_xpert$agravaids == 2, 0, NA))


# run model 
prop_mod <- glm(receive_xpert ~ age + I(age^2) + factor(tb_hist) + factor(agravaids) + factor(cs_sexo) + factor(cs_zona), 
                family = "binomial" (link = "logit"), 
                data = sinan_xpert)
## Add cs_zona in 


summary(prop_mod)

exp(coef(prop_mod))
exp(confint(prop_mod))



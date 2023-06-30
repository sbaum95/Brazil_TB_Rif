# Author: Sarah Baum
# Created: 2023-06-26
# Updated: 2023-06-29

# Description: 
# - Run Model 1: Probability of receiving Xpert, conditional on covariates
# - Run Model 2: Probability of RR positive outcome, conditional on covariates 


source(here::here("code/dependencies.R"))

load("data/sinan_xpert.Rdata")

sinan_xpert <- sinan_xpert %>% filter(diag_yr >= "2014-01-01" & diag_yr < "2022-01-01")


## Model 1: Probability of receiving Xpert, conditional on covariates

# Create outcome variable: 
# test_molec responses are sensitive, resistant, not detectable, or inconclusive

tabyl(sinan_xpert, test_molec, diag_yr) # Most missing are from 2014 and 2015 

sinan_xpert$receive_xpert <- if_else(sinan_xpert$test_molec %in% c("1", "2", "3", "4"), 1, 0) 




# Covariates: Age, Previous diagnosis with TB, HIV status, Type of Facility, and year
## Age: Defined as difference between diagnosis and birth date



## Previous TB history:  Previous TB diagnosis = 1 if tratamento == 2 or 3; 0 if new case 
sinan_xpert$tb_hist <- if_else(sinan_xpert$tratamento %in% c("2", "3"), 1, 
                               if_else(sinan_xpert$tratamento %in% c("1"), 0, 
                                       if_else(sinan_xpert$tratamento %in% c("5"), 2, 
                                               if_else(sinan_xpert$tratamento %in% c("6"), 3, NA))))
tabyl(sinan_xpert, tratamento)                              




## HIV Status: There are two HIV/AIDS variables, agravaids and hiv. I think agravaids is asked before you would know the hiv test result. 
tabyl(sinan_xpert, agravaids)

sinan_xpert$agravaids <- if_else(sinan_xpert$agravaids == 1, 1, # 1 - HIV positive
                                 if_else(sinan_xpert$agravaids == 2, 0, NA)) # 0 - HIV negative


## Sex: 
sinan_xpert$sex <- if_else(sinan_xpert$cs_sexo == "M", 1, 
                           if_else(sinan_xpert$cs_sexo == "F", 0, NA))

tabyl(sinan_xpert, nu_idade_n)

# Model A - All years
pr_mod_all <- glm(receive_xpert ~ age + factor(tratamento), 
                family = "binomial" (link = "logit"), 
                data = sinan_xpert)


summary(pr_mod_all)

exp(coef(pr_mod_all))
exp(confint(pr_mod_all))

sinan_xpert$pr_score <- predict(pr_mod_all, type = "response")

treated <- sinan_xpert %>% 
  filter(treat == 1) %>%
  ggplot(., aes(pr_score)) +
  geom_histogram() + 
  labs (x = "Propensity Score", 
        y = "Count") + 
  theme_bw() + 
  ggtitle("Treated")

# 2 - untreated units
untreated <-lalonde_psid %>% 
  filter(pr_score > 0.0000157, treat == 0) %>%
  ggplot(., aes(pr_score)) +
  geom_histogram() + 
  labs (x = "Propensity Score", 
        y = "Count") + 
  theme_bw() + 
  ggtitle("Untreated")


# Model: By year 
pr


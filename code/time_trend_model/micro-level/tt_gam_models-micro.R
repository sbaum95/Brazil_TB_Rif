# Author: Sarah Baum
# Date Created: 2023-11-13
# Date Modified: 

# Description: 
## -- This script compares various hierarchical GAM models for RR-TB at the micro-region level, including: 
##    -- Null model for state-level time trend (New cases)
##    -- Adusted (HIV, sex, age cat) state-level time trend (new cases)
##  -- Added health unit (09-19) and FHS coverage

source(here::here("code/dependencies.R"))

library(mgcv)
library(gratia)
library(itsadug)
library(tidygam)




# New cases ---------------------------------------------------------------
load(here::here("data/mdf_mic_new_grp.Rdata"))
load(here::here("data/mdf_new_ind.Rdata"))



## Model 1 - 2014-2019  ------------------------------------------------------
mic_new_mod1a <- gam(cbind(Positive,Negative)~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + mic_has_prison + mic_urban_cat + mic_bf_cat + mic_fhs_cat,
                data = mdf_mic_new_grp,
                family = binomial (link = "logit"),
                method = "REML")

save(mic_new_mod1a , file = "output/fits/mic_new_mod1a.rda")

summary(mic_new_mod1)
appraise(mic_new_mod1)
gam.check(mic_new_mod1)
plot(mic_new_mod1)



# mic_new_mod1 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + mic_has_prison + mic_urban_cat + mic_bf_cat + mic_fhs_cat,
#                     data = mdf_new_ind,
#                     family = binomial (link = "logit"),
#                     method = "REML")
# 
# 
# 
# save(mic_new_mod1 , file = "output/fits/mic_new_mod1.rda")


# Model 2 - 2016-2019
mic_new_mod2a <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + age_cat + hiv_status + sex + health_unit + mic_has_prison + mic_urban_cat + mic_bf_cat + mic_fhs_cat,
                data = mdf_mic_new_grp %>% filter(time > 8),
                family = binomial (link = "logit"),
                method = "REML")



save(mic_new_mod2a, file = "output/fits/mic_new_mod2a.rda")

summary(mic_new_mod2)
appraise(mic_new_mod2)
gam.check(mic_new_mod2)
plot(mic_new_mod2)



## Model 3 - All covs + interaction term -----------------------------------
# mic_new_mod3 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + (age_cat*hiv_status*sex) + health_unit + mic_has_prison + mic_urban_cat + mic_bf_cat + mic_fhs_cat,
#                 data = mdf_mic_new_grp,
#                 family = binomial (link = "logit"),
#                 method = "REML")
# 
# save(mic_new_mod3, file = "output/fits/mic_new_mod3.rda")
# 
# summary(mic_new_mod3)
# gam.check(mic_new_mod3)
# plot(mic_new_mod3)






# Previous Cases ----------------------------------------------------------
# Combines patient who are identified as relapsed and re-treatment
load(here::here("data/mdf_prev_ind.Rdata"))


## Model 1 - 2014-2019 ------------------------------------------------------
mic_prev_mod1a <- gam(result  ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + tratamento + age_cat + hiv_status + sex + health_unit + mic_has_prison + mic_urban_cat + mic_bf_cat + mic_fhs_cat,
                     data = mdf_prev_ind,
                     family = binomial (link = "logit"),
                     method = "REML")


save(mic_prev_mod1a, file = "output/fits/mic_prev_mod1a.Rda")

summary(mic_prev_mod1)
appraise(mic_prev_mod1)
gam.check(mic_prev_mod1)
plot(mic_prev_mod1)



## Model 2 - 2016-2019 ------------------------------------------------------
mic_prev_mod2a <- gam(result~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + hiv_status + age_cat + sex + health_unit + mic_has_prison + mic_urban_cat + mic_bf_cat + mic_fhs_cat,
                              data = mdf_prev_ind %>% filter(time > 8),
                              family = binomial (link = "logit"),
                              method = "REML")

save(mic_prev_mod2a, file = "output/fits/mic_prev_mod2a.Rda")
plot(mic_prev_mod2)


# ## Model 2 - By treatment type  ------------------------------------------------------
# mic_prev_mod3 <- gam(result  ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + factor(tratamento) + mic_has_prison + mic_urban_cat + mic_bf_cat + mic_fhs_cat,
#                      data = mdf_prev_ind,
#                      family = binomial (link = "logit"),
#                      method = "REML")
# 
# 
# save(mic_prev_mod3, file = "output/fits/mic_prev_mod3.Rda")

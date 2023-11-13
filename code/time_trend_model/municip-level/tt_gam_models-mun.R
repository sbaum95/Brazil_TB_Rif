# Author: Sarah Baum
# Date Created: 2023-09-13
# Date Modified: 2023-11-13

# Description: 
## -- This script compares various hierarchical GAM models for RR-TB at the municipality level, including: 
##    -- Null model for state-level time trend (New cases)
##    -- Adusted (HIV, sex, age cat) state-level time trend (new cases)
##  -- Added health unit (09-19) and FHS coverage

source(here::here("code/dependencies.R"))

library(mgcv)
library(gratia)
library(itsadug)
library(tidygam)




# New cases ---------------------------------------------------------------
load(here::here("data/mdf_new_grp.Rdata"))
# load(here::here("data/mdf_new_ind.Rdata"))

## Model 1 - All covs ------------------------------------------------------
mun_new_mod1 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + mun_has_prison + mun_urban_cat + mun_bf_cat,
                    data = mdf_new_grp,
                    family = binomial (link = "logit"),
                    method = "REML")



save(mun_new_mod1, file = "output/fits/mun_new_mod1.rda")

summary(mun_new_mod1)
appraise(mun_new_mod1)
gam.check(mun_new_mod1)
plot(mun_new_mod1)


# Model 2 - All covs > 2015
mun_new_mod2 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + age_cat + hiv_status + sex + health_unit + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                data = mdf_new_grp %>% filter(time > 8),
                family = binomial (link = "logit"),
                method = "REML")



save(mun_new_mod2, file = "output/fits/mun_new_mod2.rda")

summary(mun_new_mod2)
appraise(mun_new_mod2)
gam.check(mun_new_mod2)
plot(mun_new_mod2)



## Model 3 - All covs + interaction term -----------------------------------
mun_new_mod3 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + (age_cat*hiv_status*sex) + health_unit + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                data = mdf_new_grp,
                family = binomial (link = "logit"),
                method = "REML")

save(mun_new_mod3, file = "output/fits/mun_new_mod3.rda")

summary(mun_new_mod3)
gam.check(mun_new_mod3)
plot(mun_new_mod3)






# Previous Cases ----------------------------------------------------------
# Combines patient who are identified as relapsed and re-treatment
load(here::here("data/mdf_previous_ind.Rdata"))

## Adjusted model  ------------------------------------------
mun_prev_mod1  <- gam(result~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                        data = mdf_prev_ind,
                        family = binomial (link = "logit"),
                        method = "REML")


save(mun_prev_mod1 , file = "output/fits/mun_prev_mod1 .Rda")

summary(mun_prev_mod1)
appraise(mun_prev_mod1)
gam.check(mun_prev_mod1)
plot(mun_prev_mod1)



## Adjusted model post 2015 ------------------------------------------
mun_prev_mod2 <- gam(result~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + hiv_status + age_cat + sex + health_unit + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                                 data = mdf_prev_ind %>% filter(time > 8),
                                 family = binomial (link = "logit"),
                                 method = "REML")

save(mun_prev_mod2, file = "output/fits/mun_prev_mod2.Rda")
plot(mun_prev_mod2)
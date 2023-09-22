# Author: Sarah Baum
# Date Created: 2023-09-13
# Date Modified: 2023-0919

# Description: 
## -- This script compares various hierarchical GAM models for RR-TB, including: 
##    -- Null model for state-level time trend (New cases)
##    -- Adusted (HIV, sex, age cat) state-level time trend (new cases)
##  -- Added health unit (09-19) and FHS coverage

source(here::here("code/dependencies.R"))

library(mgcv)
library(gratia)
library(itsadug)
library(tidygam)




load(here::here("data/mdf_relapse_ind.Rdata"))



# New cases ---------------------------------------------------------------
load(here::here("data/mdf_new_grp.Rdata"))
# load(here::here("data/mdf_new_ind.Rdata"))


## Null model --------------------------------------------------------------
### Run model 
new_null <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23),
                data = mdf_new_grp,
                family = binomial (link = "logit"),
                method = "REML")

save(new_null, file = "output/fits/new_null.rda")

summary(new_null)
appraise(new_null)
gam.check(new_null)
plot(new_null)












## Adjusted model (hiv, age, sex) ------------------------------------------
new_adjusted <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex,
                    data = mdf_new_grp,
                    family = binomial (link = "logit"),
                    method = "REML")


save(new_adjusted, file = "output/fits/new_adjusted.rda")

summary(new_adjusted)
appraise(new_adjusted)
gam.check(new_adjusted)
plot(new_adjusted)




## Adjusted model (hiv, age, sex, health_unit) ------------------------------------------
new_adjusted_hu <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit,
                    data = mdf_new_grp,
                    family = binomial (link = "logit"),
                    method = "REML")

save(new_adjusted_hu, file = "output/fits/new_adjusted_hu.rda")

summary(new_adjusted_hu)
gam.check(new_adjusted_hu)
plot(new_adjusted_hu)


## Adjusted model (hiv, age, sex, health_unit, FHS) ------------------------------------------
new_adjusted_hu_fhs <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time) + s(time, by = state, id = 1) + age_cat + hiv_status + sex + health_unit + pct_fhs_cov,
                       data = mdf_new_grp,
                       family = binomial (link = "logit"),
                       method = "REML")

save(new_adjusted_hu_fhs, file = "output/fits/new_adjusted_hu_fhs.rda")

summary(new_adjusted_hu)
gam.check(new_adjusted_hu)
plot(new_adjusted_hu)



## Adjusted model - > 2015 -----------------------------------------------------
new_adjusted_post2015 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time) + s(time, by = state, id = 1) + age_cat + hiv_status + sex + health_unit,
                             data = mdf_new_grp %>% filter(time > 8),
                             family = binomial (link = "logit"),
                             method = "REML")

save(new_adjusted_post2015, file = "output/fits/new_adjusted_post2015.rda")
summary(new_adjusted_post2015)
gam.check(new_adjusted_post2015)
plot(new_adjusted_post2015)


## Adjusted - + urban -----------------------------------------------------
new_adjusted_post2015 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time) + s(time, by = state, id = 1) + age_cat + hiv_status + sex + health_unit + urban_cat,
                             data = mdf_new_grp,
                             family = binomial (link = "logit"),
                             method = "REML")

save(new_adjusted_post2015, file = "output/fits/new_adjusted_post2015.rda")
summary(new_adjusted_post2015)
gam.check(new_adjusted_post2015)
plot(new_adjusted_post2015)





# Re-Entry Cases ----------------------------------------------------------
load(here::here("data/mdf_reentry_ind.Rdata"))


## Null Model --------------------------------------------------------------
### Run model 
reentry_null <- gam(result ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23),
                    data = mdf_reentry_ind,
                    family = binomial (link = "logit"),
                    method = "REML")

save(reentry_null, file = "output/fits/reentry_null.Rda")

summary(reentry_null)
appraise(reentry_null)
gam.check(reentry_null)
plot(reentry_null)






## Adjusted model (HIV, age, sex) ------------------------------------------
reentry_adjusted <- gam(result~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex,
                        data = mdf_reentry_ind,
                        family = binomial (link = "logit"),
                        method = "REML")


save(reentry_adjusted, file = "output/fits/reentry_adjusted.Rda")

summary(reentry_adjusted)
appraise(reentry_adjusted)
gam.check(reentry_adjusted)
plot(reentry_adjusted)


## Adjusted model post 2015 (HIV, age, sex, health unit) ------------------------------------------
reentry_adjusted_post2015 <- gam(result~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + age_cat + hiv_status + sex + health_unit,
                                 data = mdf_reentry_ind %>% filter(time > 8),
                                 family = binomial (link = "logit"),
                                 method = "REML")

save(reentry_adjusted_post2015, file = "output/fits/reentry_adjusted_post2015.Rda")
plot(reentry_adjusted_post2015)







# Relapse Cases -----------------------------------------------------------
load(here::here("data/mdf_relapse_ind.Rdata"))


## Null Model --------------------------------------------------------------
### Run model 
relapse_null <- gam(result ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23),
                    data = mdf_relapse_ind,
                    family = binomial (link = "logit"),
                    method = "REML")

save(relapse_null, file = "output/fits/relapse_null.Rda")

summary(relapse_null)
appraise(relapse_null)
gam.check(relapse_null)
plot(relapse_null)



### Predict output
relapse_null_fit <- fitted_values(relapse_null, scale = "response") %>% 
  right_join(mdf_new_ind %>% select(state, state_nm) %>% unique(), by = "state")






## Adjusted model (HIV, age, sex) ------------------------------------------
relapse_adjusted <- gam(result~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex,
                        data = mdf_relapse_ind,
                        family = binomial (link = "logit"),
                        method = "REML")


save(relapse_adjusted, file = "output/fits/relapse_adjusted.Rda")


summary(relapse_adjusted)
appraise(relapse_adjusted)
gam.check(relapse_adjusted)
plot(relapse_adjusted)




### Predict output
relapse_adjusted_fit <- fitted_values(relapse_adjusted, scale = "response") %>%
  right_join(mdf_relapse_ind %>% select(state, state_nm) %>% unique(), by = "state")




## Adjusted model post 2015 (HIV, age, sex, health unit) ------------------------------------------
relapse_adjusted_post2015 <- gam(result~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + age_cat + hiv_status + sex + health_unit,
                        data = mdf_relapse_ind %>% filter(time > 8),
                        family = binomial (link = "logit"),
                        method = "REML")


save(relapse_adjusted_post2015, file = "output/fits/relapse_adjusted_post2015.Rda")

summary(relapse_adjusted_post2015)
appraise(relapse_adjusted)
gam.check(relapse_adjusted_post2015)
plot(relapse_adjusted_post2015)




### Predict output






# Author: Sarah Baum
# Date Created: 2023-09-13
# Date Modified: 2023-09-19

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




# New cases ---------------------------------------------------------------
load(here::here("data/mdf_new_grp.Rdata"))
# load(here::here("data/mdf_new_ind.Rdata"))

## Model 1 - All covs ------------------------------------------------------
mod1_new <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + has_prison + urban_cat + bf_cat,
                    data = mdf_new_grp,
                    family = binomial (link = "logit"),
                    method = "REML")



save(mod1_new, file = "output/fits/mod1_new.rda")

summary(mod1_new)
appraise(mod1_new)
gam.check(mod1_new)
plot(mod1_new)


# Model 2 - All covs > 2015
mod2_new <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + age_cat + hiv_status + sex + health_unit + has_prison + urban_cat + bf_cat,
                data = mdf_new_grp %>% filter(time > 8),
                family = binomial (link = "logit"),
                method = "REML")



save(mod2_new, file = "output/fits/mod2_new.rda")

summary(mod2_new)
appraise(mod2_new)
gam.check(mod2_new)
plot(mod2_new)



## Model 3 - All covs + interaction term -----------------------------------
mod3_new <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + (age_cat*hiv_status*sex) + health_unit + has_prison + urban_cat + bf_cat,
                    data = mdf_new_grp,
                    family = binomial (link = "logit"),
                    method = "REML")

save(mod3_new, file = "output/fits/mod3_new.rda")

summary(mod3_new)
gam.check(mod3_new)
plot(mod3_new)



# All cases ---------------------------------------------------------------
## Model 4 - All diagnosis types -------------------------------------------
# modify grp dataset to include tratamento
mdf_grp_diag <- df %>% 
  filter(tratamento %in% c("1","2","3")) %>%
  group_by(state, state_nm, id_municip, time, age_cat, sex, hiv_status, result, health_unit, urban_cat, has_prison, bf_cat, tratamento) %>% 
  count() %>% 
  pivot_wider(names_from = "result", values_from = "n") %>%
  rename(neg = "0",
         pos = "1",
         miss = `NA`) %>%
  mutate(Negative = if_else(is.na(neg), 0, neg),
         Positive = if_else(is.na(pos), 0, pos),
         Miss = if_else(is.na(miss), 0, miss)) %>% 
  mutate(cases = Negative + Positive + Miss, 
         pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
         obs_pct_positive = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive))
  ) %>% 
  select(-c(miss, neg, pos))

save(mdf_grp_diag, file = "data/mdf_grp_diag.Rdata")



mod4_all <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + tratamento + age_cat + hiv_status + sex + health_unit + has_prison + urban_cat + bf_cat,
                       data = mdf_grp_diag,
                       family = binomial (link = "logit"),
                       method = "REML")

save(mod4_all, file = "output/fits/mod4_all.rda")

summary(mod4_all)
gam.check(mod4_all)
plot(mod4_all)




## Model 5 - All diagnosis types > 2015-------------------------------------------
mod5_new <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time) + s(time, by = state, id = 1) + tratamento + age_cat + hiv_status + sex + health_unit + has_prison + urban_cat + bf_cat,
                data = mdf_grp_diag %>% filter (time > 8),
                family = binomial (link = "logit"),
                method = "REML")

save(mod5_all, file = "output/fits/mod5_all.rda")

summary(mod5_all)
gam.check(mod5_all)
plot(mod5_all)



# ## Adjusted model - > 2015 -----------------------------------------------------
# new_adjusted_post2015 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time) + s(time, by = state, id = 1) + age_cat + hiv_status + sex + health_unit,
#                              data = mdf_new_grp %>% filter(time > 8),
#                              family = binomial (link = "logit"),
#                              method = "REML")
# 
# save(new_adjusted_post2015, file = "output/fits/new_adjusted_post2015.rda")
# summary(new_adjusted_post2015)
# gam.check(new_adjusted_post2015)
# plot(new_adjusted_post2015)
# 
# 
# ## Adjusted - + urban -----------------------------------------------------
# new_adjusted_post2015 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time) + s(time, by = state, id = 1) + age_cat + hiv_status + sex + health_unit + urban_cat,
#                              data = mdf_new_grp,
#                              family = binomial (link = "logit"),
#                              method = "REML")
# 
# save(new_adjusted_post2015, file = "output/fits/new_adjusted_post2015.rda")
# summary(new_adjusted_post2015)
# gam.check(new_adjusted_post2015)
# plot(new_adjusted_post2015)
# 




# Re-Entry Cases ----------------------------------------------------------
load(here::here("data/mdf_reentry_ind.Rdata"))


## Null Model --------------------------------------------------------------
### Run model 
reentry_null <- gam(result ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23),
                    data = mdf_reentry_ind,
                    family = binomial (link = "logit"),
                    method = "REML")

save(reentry_null, file = "output/fits/reentry_null.Rda")
load("output/fits/reentry_null.Rda")

summary(reentry_null)
appraise(reentry_null)
gam.check(reentry_null)
plot(reentry_null)






## Adjusted model  ------------------------------------------
reentry_adjusted <- gam(result~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + has_prison + urban_cat + bf_cat,
                        data = mdf_reentry_ind,
                        family = binomial (link = "logit"),
                        method = "REML")


save(reentry_adjusted, file = "output/fits/reentry_adjusted.Rda")

summary(reentry_adjusted)
appraise(reentry_adjusted)
gam.check(reentry_adjusted)
plot(reentry_adjusted)



## Adjusted model post 2015 ------------------------------------------
reentry_adjusted_post2015 <- gam(result~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + hiv_status + age_cat + sex + health_unit + has_prison + urban_cat + bf_cat,
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








## Adjusted model  ------------------------------------------
relapse_adjusted <- gam(result~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + has_prison + urban_cat + bf_cat,
                        data = mdf_relapse_ind,
                        family = binomial (link = "logit"),
                        method = "REML")


save(relapse_adjusted, file = "output/fits/relapse_adjusted.Rda")


summary(relapse_adjusted)
appraise(relapse_adjusted)
gam.check(relapse_adjusted)
plot(relapse_adjusted)




## Adjusted model post 2015  ------------------------------------------
relapse_adjusted_post2015 <- gam(result~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + hiv_status + age_cat + sex + health_unit + has_prison + urban_cat + bf_cat,
                        data = mdf_relapse_ind %>%  filter(time > 8),
                        family = binomial (link = "logit"),
                        method = "REML")


save(relapse_adjusted_post2015, file = "output/fits/relapse_adjusted_post2015.Rda")



summary(relapse_adjusted_post2015)
appraise(relapse_adjusted)
gam.check(relapse_adjusted_post2015)
plot(relapse_adjusted_post2015)




### Predict output






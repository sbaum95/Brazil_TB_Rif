# Author: Sarah Baum
# Date Created: 2023-09-13
# Date Modified: 2023-11-13

# Description: 
## -- This script compares various hierarchical GAM models for RR-TB at the municipality level, including: 
##    -- Null model for state-level time trend (New cases)
##    -- Adusted (HIV, sex, age cat) state-level time trend (new cases)
##  -- Added health unit (09-19) and FHS coverage


source(here::here("code/dependencies.R"))



###########################################################################
###########################################################################
###                                                                     ###
###                              NEW CASES                              ###
###                                                                     ###
###########################################################################
###########################################################################

load(here::here("data/mdf_mun_new_grp.Rdata"))
load(here::here("data/mdf_new_ind.Rdata"))



#################################################################
##                     Model 1 - 2014-2019                     ##
#################################################################

mun_new_mod1 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                    data = mdf_mun_new_grp,
                    family = binomial (link = "logit"),
                    method = "REML")



save(mun_new_mod1, file = "output/fits/mun_new_mod1.rda")

summary(mun_new_mod1)
appraise(mun_new_mod1)
gam.check(mun_new_mod1)
plot(mun_new_mod1)


#################################################################
##                     Model 2 - 2016-2019                     ##
#################################################################
mun_new_mod2 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 12) + s(time, by = state, id = 1, k = 12) + age_cat + hiv_status + sex + health_unit + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                        data = mdf_mun_new_grp %>% filter(time > 12),
                        # %>% filter(time > 8),
                        family = binomial (link = "logit"),
                        method = "REML")

save(mun_new_mod2, file = "output/fits/mun_new_mod2.rda")




#################################################################
##                     Model 3 - 2017-2019                     ##
#################################################################
mun_new_mod3 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re") + s(time, k = 12) + s(time, by = state, id = 1, k = 12) + age_cat + hiv_status + sex + health_unit + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                data = mdf_mun_new_grp %>% filter(time > 12),
                family = binomial (link = "logit"),
                method = "REML")


save(mun_new_mod3, file = "output/fits/mun_new_mod3.rda")







############################################################################
############################################################################
###                                                                      ###
###                            PREVIOUS CASES                            ###
###                                                                      ###
############################################################################
############################################################################

# Combines patient who are identified as relapsed and re-treatment
load(here::here("data/mdf_prev_ind.Rdata"))



#################################################################
##                     Model 1 - 2014-2019                     ##
#################################################################
mun_prev_mod1  <- gam(result ~ s(state, bs = "re") + s(time, k = 23) + s(time, by = state, id = 1, k = 23) + age_cat + hiv_status + sex + health_unit + tratamento + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                      data = mdf_prev_ind %>% filter(!age_cat == "0-4"),
                      family = binomial (link = "logit"),
                      method = "REML")


save(mun_prev_mod1, file = "output/fits/mun_prev_mod1.Rda")

summary(mun_prev_mod1)
appraise(mun_prev_mod1)
gam.check(mun_prev_mod1)
plot(mun_prev_mod1a)




#################################################################
##                     Model 2 - 2016-2019                     ##
#################################################################
mun_prev_mod2 <- gam(result~ s(state, bs = "re") + s(time, k = 15) + s(time, by = state, id = 1, k = 15) + hiv_status + age_cat + sex + health_unit + tratamento + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                                 data = mdf_prev_ind %>% filter(time > 8) %>% filter(!age_cat == "0-4"),
                                 family = binomial (link = "logit"),
                                 method = "REML")

save(mun_prev_mod2, file = "output/fits/mun_prev_mod2.Rda")





#################################################################
##                     Model 3 - 2017-2019                     ##
#################################################################
mun_prev_mod3 <- gam(result~ s(state, bs = "re") + s(time, k = 12) + s(time, by = state, id = 1, k = 12) + hiv_status + age_cat + sex + health_unit + tratamento + mun_has_prison + mun_urban_cat + mun_bf_cat + mun_fhs_cat,
                     data = mdf_prev_ind %>% filter(time > 12) %>% filter(!age_cat == "0-4"),
                     family = binomial (link = "logit"),
                     method = "REML")

save(mun_prev_mod3, file = "output/fits/mun_prev_mod3.Rda")

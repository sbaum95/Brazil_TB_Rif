# Relapse -----------------------------------------------------------------


## Adding relapse here in case I can't save the file 
# Author: Sarah Baum
# Created: 2023-08-28
# Updated: 
# Description/Decisions: 
# -- Runs GAM models for relapse TB cases


source(here::here("code/dependencies.R"))

library(mgcv)
library(gratia)
library(lme4)
# library(itsadug)


load("data/mdf_relapse_ind.Rdata")




## Model 1 - Global smooth + random intercept ------------------------------
relapse_intercept <- gam(result ~ s(state, bs = "re", k = 27) + s(time, bs = "tp") + age_cat + hiv_status + sex, 
                         data = mdf_relapse_ind, 
                         family = binomial (link = "logit"), 
                         method = "REML")


# Output doesn't make sense - States with few cases should be penalized towards smooth, but that's not what is happening...

# Note: Removed health unit for now because a number are missing, especially for Sao Paulo. Go back and double check merge. 
# ind_intercept <- gam(result ~ s(state, bs = "re", k = 27) + s(time, bs = "tp", k = 10) + age_cat + hiv_status + sex + health_unit, 
#                      data = mdf_reentry_ind, 
#                      family = binomial (link = "logit"), 
#                      method = "REML")


summary(relapse_intercept)
variance_comp(relapse_intercept)
plot(relapse_intercept)





## Model 2 - Global smooth + Random slope --------------------------------------------------
relapse_slope <- gam(result ~ s(time, bs = "tp", k = 10) + s(time, state, bs = "re", k = 27) + age_cat + hiv_status + sex, 
                     data = mdf_relapse_ind, 
                     family = binomial (link = "logit"), 
                     method = "REML")





summary(relapse_slope)
print(variance_comp(relapse_slope), n = 30) 
## std dev is larger (I believe compared to the re-entry cases)






## Model 3 - Random intercept + random slope -------------------------------
relapse_intercept_slope <- gam(result ~ s(state, bs = "re", k = 27) + s(state, time, bs = "re", k = 27) + age_cat + hiv_status + sex, 
                               data = mdf_relapse_ind, 
                               family = binomial (link = "logit"), 
                               method = "REML")


summary(relapse_intercept_slope) 
variance_comp(relapse_intercept_slope) # state-specific effect of time is basically 0, suggesting there is no variation overtime







## Model 4 - GI ------------------------------------------------------------
# Smooths have different smoothing parameters by state
relapse_gi <- gam(result ~ s(state, bs = "re", k=27) + s(time, bs = "tp", k = 20) + s(time, by = state, bs = "tp", m=1, k = 20) + age_cat + hiv_status + sex, 
                  data = mdf_relapse_ind, 
                  family = binomial (link = "logit"), 
                  method = "REML")


summary(relapse_gi)
print(variance_comp(relapse_gi), n = 30)






## Model 5 - GS ------------------------------------------------------------
# Smooths share the same smoothing parameters over levels of state (which is why this warning is appearing)
relapse_gs <- gam(result ~ s(time, bs = "tp", m=2) + s(time, state, bs = "fs", m=2) + age_cat + hiv_status + sex, 
                  data = mdf_relapse_ind, 
                  family = binomial (link = "logit"), 
                  method = "REML")



summary(relapse_gs)
print(variance_comp(relapse_gs), n = 30) # why does this show only 3 s(time, state)?




# save models
save(relapse_intercept, file = "output/fits/gam_relapse_intercept.rda")
save(relapse_slope, file = "output/fits/gam_relapse_slope.rda")
save(relapse_intercept_slope, file = "output/fits/gam_relapse_intercept_slope.rda")
save(relapse_gi, file = "output/fits/gam_relapse_gi.rda")
save(relapse_gs, file = "output/fits/gam_relapse_GS.rda")






## Load models -------------------------------------------------------------
load("output/fits/gam_relapse_intercept.rda")
load("output/fits/gam_relapse_slope.rda")
load("output/fits/gam_relapse_intercept_slope.rda")
load("output/fits/gam_relapse_gi.rda")
load("output/fits/gam_relapse_GS.rda")







## Predict  ----------------------------------------------------------------
load("data/mdf_relapse_ind.Rdata")

# Model 1 - Random intercept
mdf_relapse_ind <- cbind(mdf_relapse_ind, predict(relapse_intercept, mdf_relapse_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_rand.int = fit, 
         se.fit_rand.int = se.fit)

summary(ind_intercept)

# Model 2 - Random slope 
mdf_relapse_ind<- cbind(mdf_relapse_ind, predict(relapse_slope, mdf_relapse_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_rand.slope = fit, 
         se.fit_rand.slope = se.fit)


# Model 3 - Random intercept + Random slope
mdf_relapse_ind <- cbind(mdf_relapse_ind, predict(relapse_intercept_slope, mdf_relapse_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_rand.int.slope = fit, 
         se.fit_rand.int.slope = se.fit)



# Model 4 - GI 
mdf_relapse_ind <- cbind(mdf_relapse_ind, predict(relapse_gi, mdf_relapse_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_gi = fit, 
         se.fit_gi = se.fit)



# Model 5 - GS
mdf_relapse_ind <- cbind(mdf_relapse_ind, predict(relapse_gs, mdf_relapse_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_gs = fit, 
         se.fit_gs = se.fit)


## Compare models ----------------------------------------------------------
# Plot all models together: 
relapse_all_mod_plot <- ggplot() + 
  # Observed
  geom_point(
    data = mdf_relapse_ind %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
    aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) +
  # Model- Random intercept
  geom_line(
    data = mdf_relapse_ind %>%
      group_by(state_nm, time) %>%
      filter(!is.na(fit_rand.int)) %>% 
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.int, na.rm = TRUE)/cases), 0, sum(fit_rand.int, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int")) +
  # # # Model - Random slope
  geom_line(
    data = mdf_relapse_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.slope, na.rm = TRUE)/cases), 0, sum(fit_rand.slope, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Slope")) +
  # # # Model - Random intercept + random slope
  geom_line(
    data = mdf_relapse_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.int.slope, na.rm = TRUE)/cases), 0, sum(fit_rand.int.slope, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int & Slope")) +
  # # Model - GS
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_gs, na.rm = TRUE)/cases), 0, sum(fit_gs, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "GS")) +
  # # # Model - GI
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_gi, na.rm = TRUE)/cases), 0, sum(fit_gi, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "GI")) +
  scale_size(
    range = c(0.5, 5)
  ) + 
  scale_color_discrete(name = "Model") + 
  facet_wrap(~state_nm, scale = "free") + 
  ggtitle("Relapse - Modeled Trends + Num. Tested") + 
  theme_bw()

ggsave("figures/gam_all_plot_relapse.png", plot = relapse_all_mod_plot, width = 10, height = 6, dpi = 300)






# Without observed # tested
# Plot all models together: 
relapse_all_mod_plot_trend.only <- ggplot() + 
  # Model- Random intercept
  geom_line(
    data = mdf_relapse_ind %>%
      group_by(state_nm, time) %>%
      filter(!is.na(fit_rand.int)) %>% 
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.int, na.rm = TRUE)/cases), 0, sum(fit_rand.int, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int")) +
  # # # Model - Random slope
  geom_line(
    data = mdf_relapse_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.slope, na.rm = TRUE)/cases), 0, sum(fit_rand.slope, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Slope")) +
  # # # Model - Random intercept + random slope
  geom_line(
    data = mdf_relapse_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.int.slope, na.rm = TRUE)/cases), 0, sum(fit_rand.int.slope, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int & Slope")) +
  # # Model - GS
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_gs, na.rm = TRUE)/cases), 0, sum(fit_gs, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "GS")) +
  # # # Model - GI
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_gi, na.rm = TRUE)/cases), 0, sum(fit_gi, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "GI")) +
  scale_size(
    range = c(0.5, 5)
  ) + 
  scale_color_discrete(name = "Model") + 
  facet_wrap(~state_nm, scales = "free") + 
  ggtitle("Relapse - Modeled Trends") + 
  theme_bw()



ggsave("figures/gam_all_plot_relapse_trends.only.png", plot = relapse_all_mod_plot_trend.only, width = 10, height = 6, dpi = 300)

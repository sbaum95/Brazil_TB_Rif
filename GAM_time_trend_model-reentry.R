# Author: Sarah Baum
# Created: 2023-08-25
# Updated: 2023-08-29

# Description/Decisions: 
# -- Runs GAM models for re-entry TB cases


source(here::here("code/dependencies.R"))

library(mgcv)
library(gratia)
library(lme4)
# library(itsadug)


load("data/mdf_reentry_ind.Rdata")


# Filter to periods where states are consistently testing > 5% of cases
# test <- mdf_reentry_ind %>% 
#   group_by(state, time) %>% 
#   summarize(number_tested = sum(!is.na(result)),
#             cases = n(), 
#             pct_tested = number_tested/cases) %>% 
#   filter(pct_tested >= 0.05) %>% 
#   mutate(flag = if_else(time - lag(time) == 1, 0, 1)) %>%  # remove periods with >5% of cases, but not immediately preceding current period
#   filter(flag == 0) %>%  # remove obs that are more than 1 time period away
#   filter(time - lag(time) == 1) %>% 
#   filter(if_else(time !=24, ))



















# Model 1 - Global smooth + random intercept ------------------------------
ind_intercept <- gam(result ~ s(state, bs = "re", k = 27) + s(time, bs = "tp", k = 1) + age_cat + hiv_status + sex, 
                     data = mdf_reentry_ind, 
                     family = binomial (link = "logit"), 
                     method = "REML")


# Output doesn't make sense - States with few cases should be penalized towards smooth, but that's not what is happening...

# Note: Removed health unit for now because a number are missing, especially for Sao Paulo. Go back and double check merge. 
# ind_intercept <- gam(result ~ s(state, bs = "re", k = 27) + s(time, bs = "tp", k = 10) + age_cat + hiv_status + sex + health_unit, 
#                      data = mdf_reentry_ind, 
#                      family = binomial (link = "logit"), 
#                      method = "REML")


summary(ind_intercept)
variance_comp(ind_intercept)
plot(ind_intercept)



# Model 2 - Global smooth + Random slope --------------------------------------------------
ind_slope <- gam(result ~ s(time, bs = "tp", k = 10) + s(time, state, bs = "re", k = 27) + age_cat + hiv_status + sex, 
                 data = mdf_reentry_ind, 
                 family = binomial (link = "logit"), 
                 method = "REML")



summary(ind_slope)
print(variance_comp(ind_slope), n = 30) 
## Variance by time and state is very small (I tried estimating a separate smoothing parameter for each state - basically no variation)








# Model 3 - Random intercept + random slope -------------------------------
ind_intercept_slope <- gam(result ~ s(state, bs = "re", k = 27) + s(state, time, bs = "re", k = 27) + age_cat + hiv_status + sex, 
                           data = mdf_reentry_ind, 
                           family = binomial (link = "logit"), 
                           method = "REML")


summary(ind_intercept_slope) 
variance_comp(ind_intercept_slope) # state-specific effect of time is basically 0, suggesting there is no variation overtime







# Model 4 - GI 
# Smooths have different smoothing parameters by state
ind_GI <- gam(result ~ s(state, bs = "re", k=27) + s(time, bs = "tp", k = 10) + s(time, by = state, bs = "tp", m=1, k = 10) + age_cat + hiv_status + sex, 
              data = mdf_reentry_ind, 
              family = binomial (link = "logit"), 
              method = "REML")



summary(ind_GI)
print(variance_comp(ind_GI), n = 30)


# Model 5 - GS
# Smooths share the same smoothing paramters over levels of state (which is why this warning is appearing)
ind_GS <- gam(result ~ s(time, bs = "tp", m=2) + s(time, state, bs = "fs", m=2, k = 5) + age_cat + hiv_status + sex, 
              data = mdf_reentry_ind, 
              family = binomial (link = "logit"), 
              method = "REML")

summary(ind_GS)
print(variance_comp(ind_GS), n = 30) # why does this show only 3 s(time, state)?




# Model 6 - GS + random intercept 
# Smooths share the same smoothing paramters over levels of state (which is why this warning is appearing)
ind_GS_intercept <- gam(result ~ s(state, bs = "re") + s(time, bs = "tp", m=2) + s(time, state, bs = "fs", m=2, k = 5) + age_cat + hiv_status + sex, 
              data = mdf_reentry_ind, 
              family = binomial (link = "logit"), 
              method = "REML")

summary(ind_GS_intercept)
print(variance_comp(ind_GS_intercept), n = 30)
summary(ind_GS)

# If intercepts aren't needed then they should get penalized to 0 (source: https://stats.stackexchange.com/questions/494970/use-of-random-intercepts-and-slopes-in-combination-with-factor-smooth-interactio)


# save models
save(ind_intercept, file = "output/fits/gam_reentry_ind_intercept.rda")
save(ind_slope, file = "output/fits/gam_reentry_ind_slope.rda")
save(ind_intercept_slope, file = "output/fits/gam_reentry_ind_intercept_slope.rda")
save(ind_GI, file = "output/fits/gam_reentry_ind_GI.rda")
save(ind_GS, file = "output/fits/gam_reentry_ind_GS.rda")


# Load models -------------------------------------------------------------
load("output/fits/gam_reentry_ind_intercept.rda")
load("output/fits/gam_reentry_ind_slope.rda")
load("output/fits/gam_reentry_ind_intercept_slope.rda")
load("output/fits/gam_reentry_ind_GI.rda")
load("output/fits/gam_reentry_ind_GS.rda")







# Predict  ----------------------------------------------------------------
load("data/mdf_reentry_ind.Rdata")
# Model 1 - Random intercept

mdf_reentry_ind <- cbind(mdf_reentry_ind, predict(ind_intercept, mdf_reentry_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_rand.int = fit, 
         se.fit_rand.int = se.fit)


# Model 2 - Random slope 
mdf_reentry_ind<- cbind(mdf_reentry_ind, predict(ind_slope, mdf_reentry_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_rand.slope = fit, 
         se.fit_rand.slope = se.fit)


# Model 3 - Random intercept + Random slope
mdf_reentry_ind <- cbind(mdf_reentry_ind, predict(ind_intercept_slope, mdf_reentry_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_rand.int.slope = fit, 
         se.fit_rand.int.slope = se.fit)



# Model 4 - GI 
mdf_reentry_ind <- cbind(mdf_reentry_ind, predict(ind_GI, mdf_reentry_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_gi = fit, 
         se.fit_gi = se.fit)


# Model 5 - GS
mdf_reentry_ind<- cbind(mdf_reentry_ind, predict(ind_GS, mdf_reentry_ind, se.fit = TRUE, type = "response")) %>% 
  rename(fit_gs = fit, 
         se.fit_gs = se.fit)


save(mdf_reentry_ind, file = "data/reentry_ind_pred.Rdata")





# compare models ----------------------------------------------------------


# Plot all models together: 
all_mod_plot_fixed.axes <- ggplot() + 
  # Observed
  geom_point(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
    aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) +
  # Model- Random intercept
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      filter(!is.na(fit_rand.int)) %>% 
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.int, na.rm = TRUE)/cases), 0, sum(fit_rand.int, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int.")) +
  # # # Model - Random slope
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.slope, na.rm = TRUE)/cases), 0, sum(fit_rand.slope, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Slope")) +
  # # # Model - Random intercept + random slope
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.int.slope, na.rm = TRUE)/cases), 0, sum(fit_rand.int.slope, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int & Slope")) +
  # # # Model - GS  
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
  facet_wrap(~state_nm) + 
  theme_bw()



ggsave("figures/gam_all_plot_reentry_fixed.axes.png", plot = all_mod_plot_fixed.axes , width = 10, height = 6, dpi = 300)





# Plot all models together: 
all_mod_plot_trends.only <- ggplot() + 
  # Observed
  # geom_point(
  #   data = mdf_reentry_ind %>%
  #     group_by(state_nm, time) %>%
  #     summarize(num_tested = sum(!is.na(result)),
  #               positive = sum(result == "1", na.rm = TRUE),
  #               obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
  #   aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) +
  # Model- Random intercept
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      filter(!is.na(fit_rand.int)) %>% 
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.int, na.rm = TRUE)/cases), 0, sum(fit_rand.int, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int.")) +
  # # # Model - Random slope
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.slope, na.rm = TRUE)/cases), 0, sum(fit_rand.slope, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Slope")) +
  # # # Model - Random intercept + random slope
  geom_line(
    data = mdf_reentry_ind %>%
      group_by(state_nm, time) %>%
      summarize(cases = n(),
                pred_pct_pos = if_else(is.na(sum(fit_rand.int.slope, na.rm = TRUE)/cases), 0, sum(fit_rand.int.slope, na.rm = TRUE)/cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int & Slope")) +
  # # # Model - GS  
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
  facet_wrap(~state_nm) + 
  theme_bw()



ggsave("figures/gam_all_plot_reentry_trends.only.png", plot = all_mod_plot_trends.only  , width = 10, height = 6, dpi = 300)






## Attempt to figure out why things are acting weird
## Just comparing Acre, Roraima and Tocantins -- Three states with Very few cases and no positives...
### I'm not sure why this makes their lines so funky when comparing them to the other states (in the scheme of things they aren't that funky..)
### Worth showing Nick how they compare 
# 
# 
# load("data/mdf_reentry_ind.Rdata")
# 
# 
# 
# 
# # random intercept + random slope
# gam_m1 <- gam(result ~ s(state, bs = "re", k = 27) + s(time, state, bs = "re"), 
#                      data = mdf_reentry_ind, 
#                      family = binomial (link = "logit"), 
#                      method = "REML")
# 
# 
# lmer_m1 <- glmer(result ~ (1 | state) + (0 + time | state), 
#                         data = mdf_reentry_ind, 
#                         family = binomial (link = "logit"))
# 
# 
# summary(gam_m1)
# summary(lmer_m1)
# 
# summary(lmer_m1)$varcor
# variance_comp(gam_m1)
# 
# fixef(lmer_intercept)
# 
# 
# mdf_reentry_ind <- cbind(mdf_reentry_ind, predict(gam_m1, mdf_reentry_ind, type = "response"))
# 
# mdf_reentry_ind  <- cbind(mdf_reentry_ind, predict(lmer_m1, mdf_reentry_ind, type = "response")) 
# 
# colnames(mdf_reentry_ind)[13] <- "gam_m1"
# colnames(mdf_reentry_ind)[14] <- "lmer_m1"
# 
# 
# 
# # random intercept, slope, + smooths
# gam_m2 <- gam(result ~ s(state, bs = "re", k = 27) + s(time, state, bs = "re") + sex + hiv_status + age_cat, 
#               data = mdf_reentry_ind, 
#               family = binomial (link = "logit"), 
#               method = "REML")
# 
# 
# mdf_reentry_ind <- cbind(mdf_reentry_ind, predict(gam_m2, mdf_reentry_ind, type = "response"))
# 
# colnames(mdf_reentry_ind)[15] <- "gam_m2"
# 
# 
# ggplot() + 
#   # Observed
#   geom_point(
#     data = mdf_reentry_ind %>%
#       group_by(state_nm, time) %>%
#       summarize(num_tested = sum(!is.na(result)),
#                 positive = sum(result == "1", na.rm = TRUE),
#                 obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
#     aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) +
#   # GAM
#   geom_line(
#     data = mdf_reentry_ind %>%
#       group_by(state_nm, time) %>%
#       filter(!is.na(gam_m1)) %>% 
#       summarize(cases = n(),
#                 pred_pct_pos = gam_m1),
#     aes(x = time, y = pred_pct_pos, group = state_nm, color = "red")) +
#   # LMER
#   geom_line(
#     data = 
#       test <- mdf_reentry_ind %>%
#       group_by(state_nm, time) %>%
#       filter(!is.na(gam_m2)) %>% 
#       summarize(cases = n(),
#                 pred_pct_pos = sum(gam_m2)/cases),
#     aes(x = time, y = pred_pct_pos, group = state_nm, color = "blue")) +
# scale_size(
#   range = c(0.5, 5)
# ) + 
#   scale_color_discrete(name = "Model", labels = c("GAM 1", "GAM 2", "R. Int & Slope", "GS", "GI")) + 
#   facet_wrap(~state_nm)
# 
# 
# 



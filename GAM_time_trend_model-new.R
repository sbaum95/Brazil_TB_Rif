# Author: Sarah Baum
# Created: 2023-08-21
# Updated: 2023-08-29 
  


# Description/Decisions: 
# -- Updated full random effects model (removed misspecified global smooth for time
# based on
# https://stats.stackexchange.com/questions/552880/by-group-random-effect-gam)
# -- Updated figures for presentation and tried things with higher valued Ks

source(here::here("code/dependencies.R"))

library(mgcv)
library(gratia)
# library(itsadug)

load("data/mdf_new_grp.Rdata")





# Model 1 - Global smooth + random intercept ------------------------------
new_intercept <- gam(cbind(Positive, Negative) ~ s(state, bs = "re", k = 27) + s(time, bs = "tp", k = 10) +  age_cat + hiv_status + sex, 
             data = mdf_new_grp, 
             family = binomial (link = "logit"), 
             method = "REML")

summary(new_intercept)
variance_comp(new_intercept)





# Model 2 - Global smooth + Random slope --------------------------------------------------
new_slope <- gam(cbind(Positive, Negative) ~ s(time, bs = "tp", k = 10) + s(time, state, bs = "re", k = 27) + age_cat + hiv_status + sex, 
                           data = mdf_new_grp, 
                           family = binomial (link = "logit"), 
                           method = "REML")

summary(new_slope)
variance_comp(grp_slope)



# Model 3 - Random intercept + random slope -------------------------------
new_intercept_slope  <- gam(cbind(Positive, Negative) ~ s(state, bs = "re", k = 27) + s(time, state, bs = "re", k = 27) + age_cat + hiv_status + sex, 
                            data = mdf_new_grp, 
                            family = binomial (link = "logit"), 
                            method = "REML")


# origina/incorrect specification
# new_intercept_slope <- gam(cbind(Positive, Negative) ~ s(state, bs = "re", k = 27) + s(time, bs = "tp", k = 20) + s(time, state, bs = "re", k = 27) + age_cat + hiv_status + sex, 
#               data = mdf_new_grp, 
#               family = binomial (link = "logit"), 
#               method = "REML")





summary(new_intercept_slope)
variance_comp(new_intercept_slope)







# Model 4 - GI model ------------------------------------------------------
## Random intercept + Group-specific smoother (each group gets it's own wiggliness)
new_gi <- gam(cbind(Positive, Negative) ~ s(state, bs = "re", k=27) + s(time, bs = "tp", k = 20) + s(time, by = state, bs = "tp", m=1, k = 20) + age_cat + hiv_status + sex, 
                           data = mdf_new_grp, 
                           family = binomial (link = "logit"), 
                           method = "REML")


summary(new_gi)

print(variance_comp(new_gi), n = 30)





# Model 5 - GS model ------------------------------------------------------

# Global Smooth + different shapes of smooths for each grouping level
# Groups have a similar functional response, but intergroup variation in 
# responses is allowed and penalized ones too far from the average
## - Add one term for a global smoother 
## - Add a second term specifying group-level smooth terms (penalty draws them to 0) (This is the factor smoother)

new_GS <- gam(cbind(Positive, Negative) ~ s(time, bs = "tp", m=2, k = 20) + s(time, state, bs = "fs", m=2, k = 27) + age_cat + hiv_status + sex, 
              data = mdf_new_grp, 
              family = binomial (link = "logit"), 
              method = "REML")

summary(new_gs)
print(variance_comp(new_gs), n = 30)





# save models
save(new_intercept, file = "output/fits/gam_new_intercept.rda")
save(new_slope, file = "output/fits/gam_new_slope.rda")
save(new_intercept_slope , file = "output/fits/gam_new_intercept_slope.rda")
save(new_gi, file = "output/fits/gam_new_gi.rda")
save(new_gs, file = "output/fits/gam_new_gs.rda")



# Load models -------------------------------------------------------------
load("output/fits/gam_new_intercept.rda")
load("output/fits/gam_new_slope.rda")
load("output/fits/gam_new_intercept_slope.rda")

summary(new_intercept_slope)
coef(new_intercept_slope)
load("output/fits/gam_new_gi.rda")
load("output/fits/gam_new_gs.rda")






# Predict  ----------------------------------------------------------------
# Model 1 - Random intercept
mdf_new_grp <- cbind(mdf_new_grp, predict(new_intercept, mdf_new_grp, se.fit = TRUE, type = "response")) %>% 
  rename(fit_rand.int = fit, 
         se.fit_rand.int = se.fit)


# Model 2 - Random slope 
mdf_new_grp <- cbind(mdf_new_grp, predict(new_slope, mdf_new_grp, se.fit = TRUE, type = "response")) %>% 
  rename(fit_rand.slope = fit, 
         se.fit_rand.slope = se.fit)


# Model 3 - Random intercept + Random slope
mdf_new_grp <- cbind(mdf_new_grp, predict(new_intercept_slope, mdf_new_grp, se.fit = TRUE, type = "response")) %>% 
  rename(fit_rand.int.slope = fit, 
         se.fit_rand.int.slope = se.fit)



# Model 4 - GI 
mdf_new_grp <- cbind(mdf_new_grp, predict(new_gi, mdf_new_grp, se.fit = TRUE, type = "response")) %>% 
  rename(fit_gi = fit, 
         se.fit_gi = se.fit)


# Model 4a - GI (with hiher ks)
mdf_new_grp <- cbind(mdf_new_grp, predict(new_gi, mdf_new_grp, se.fit = TRUE, type = "response")) %>% 
  rename(fit_gi = fit, 
         se.fit_gi = se.fit)


# Model 5 - GS
mdf_new_grp <- cbind(mdf_new_grp, predict(new_gs, mdf_new_grp, se.fit = TRUE, type = "response")) %>% 
  rename(fit_gs = fit, 
         se.fit_gs = se.fit)


save(mdf_new_grp, file = "data/new_grp_pred.Rdata")
load("data/new_grp_pred.Rdata")




# Compare models ----------------------------------------------------------
AIC(new_intercept, new_intercept_slope, new_slope, new_gi, new_gs) # This is the conditional AIC





# Plot all models together: 
all_mod_plot <- ggplot() + 
  # # Observed
  geom_point(
    data = mdf_new_grp %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(Positive + Negative),
                obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
    aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) +
  # Model- Random intercept
  geom_line(
    data = mdf_new_grp %>%
      filter(!is.na(fit_rand.int)) %>%
      group_by(state_nm, time) %>%
      summarize(pred_pct_pos = sum(cases*fit_rand.int, na.rm = TRUE)/sum(cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int")) +
  # Model - Random slope
  geom_line(
  data = mdf_new_grp %>%
    filter(!is.na(fit_rand.slope)) %>%
    group_by(state_nm, time) %>%
    summarize(pred_pct_pos = sum(cases*fit_rand.slope, na.rm = TRUE)/sum(cases)),
  aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Slope")) +
  # # Model - Random intercept + random slope (original spec)
  geom_line(
    data = mdf_new_grp %>%
      filter(!is.na(fit_rand.int.slope)) %>%
      group_by(state_nm, time) %>%
      summarize(pred_pct_pos = sum(cases*fit_rand.int.slope, na.rm = TRUE)/sum(cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int & Slope")) +
  # # # Model - GS
  geom_line(
    data = mdf_new_grp %>%
      group_by(state_nm, time) %>%
      filter(!is.na(fit_gs)) %>%
      summarize(pred_pct_pos = sum(cases*fit_gs, na.rm = TRUE)/sum(cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "GS")) +
  # # # Model - GI
  geom_line(
    data = mdf_new_grp %>%
      group_by(state_nm, time) %>%
      filter(!is.na(fit_gi)) %>%
      summarize(pred_pct_pos = sum(cases*fit_gi, na.rm = TRUE)/sum(cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "GI")) +



  scale_size_area ("Number Tested") + 
  scale_color_discrete(name = "Model") +
  facet_wrap(~state_nm) + 
  theme_bw()



ggsave("figures/gam_all_plot.png", plot = all_mod_plot, width = 10, height = 6, dpi = 300)



all_mod_plot_trend.only <- ggplot() + 
  # Observed
  # geom_point(
  #   data = mdf_new_grp %>%
  #     group_by(state_nm, time) %>%
  #     summarize(num_tested = sum(Positive + Negative),
  #               obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
  #   aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) + 
  # Model- Random intercept
  geom_line(
    data = mdf_new_grp %>%
      filter(!is.na(fit_rand.int)) %>% 
      group_by(state_nm, time) %>%
      summarize(pred_pct_pos = sum(cases*fit_rand.int, na.rm = TRUE)/sum(cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int")) + 
  # Model - Random slope
  geom_line(
    data = mdf_new_grp %>%
      filter(!is.na(fit_rand.slope)) %>% 
      group_by(state_nm, time) %>%
      summarize(pred_pct_pos = sum(cases*fit_rand.slope, na.rm = TRUE)/sum(cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Slope")) + 
  # # Model - Random intercept + random slope
  geom_line(
    data = mdf_new_grp %>%
      filter(!is.na(fit_rand.int.slope)) %>%
      group_by(state_nm, time) %>%
      summarize(pred_pct_pos = sum(cases*fit_rand.int.slope, na.rm = TRUE)/sum(cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "R. Int & Slope")) +
  # # Model - GS  
  geom_line(
    data = mdf_new_grp %>%
      group_by(state_nm, time) %>%
      filter(!is.na(fit_gs)) %>%
      summarize(pred_pct_pos = sum(cases*fit_gs, na.rm = TRUE)/sum(cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "GS")) +
  # # Model - GI
  geom_line(
    data = mdf_new_grp %>%
      group_by(state_nm, time) %>%
      filter(!is.na(fit_gi)) %>%
      summarize(pred_pct_pos = sum(cases*fit_gi, na.rm = TRUE)/sum(cases)),
    aes(x = time, y = pred_pct_pos, group = state_nm, color = "GI")) +
  scale_size_area ("Number Tested") + 
  scale_color_discrete(name = "Model") +
  facet_wrap(~state_nm, scales = "free") + 
  theme_bw()



ggsave("figures/gam_all_plot_trend.only.png", plot = all_mod_plot_trend.only, width = 10, height = 6, dpi = 300)



m1_plot_df <- cbind(m1_plot_df, predict(new_intercept, mdf_new_grp, type = "response", se.fit = TRUE)) %>% 
  rename(pr_fit = fit, 
         pr_se = se.fit) 


# Plots for each model ----------------------------------------------------
## Plot each model separately with standard errors 
## Note: Standard errors calculated based on this video (https://www.youtube.com/watch?v=Ji5dm0G2GAU)
## Model 1 - 
m1_plot_df <- cbind(mdf_new_grp, predict(new_intercept, mdf_new_grp, se.fit = TRUE)) %>% 
  rename(log_fit = fit, 
         log_se = se.fit)  %>% 
  filter(!is.na(log_fit)) %>% 
  group_by(state_nm, time) %>% 
  summarize(
    # # num tested 
    # num_tested = sum(Positive + Negative),
    # # observed estimate
    # obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative)),
    # Number of groups
    m = n(), 
    # Pooled SE on log odds scale
    log_pooled_se = sqrt(sum(log_se^2)/m),
    # Pooled estimate on log odds scale 
    log_pooled_est = sum(log_fit)/m, 
    # Pooled estimate on probability scale 
    pr_pooled_est = exp(log_pooled_est)/(1 + exp(log_pooled_est)),
    # Pooled SE on probability scale 
    pr_pooled_se = log_pooled_se * pr_pooled_est * (1-pr_pooled_est))



m1_plot <- ggplot() + 
  # Observed 
  # geom_point(
  #   data = mdf_new_grp %>%
  #     group_by(state_nm, time) %>%
  #     summarize(num_tested = sum(Positive + Negative),
  #               obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
  #   aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) + 
  
  # Modeled 
  geom_line(data = m1_plot_df, aes(x = time, y = pr_pooled_est, group = state_nm), color = "red") + 
  geom_ribbon(data = m1_plot_df, aes(x = time, ymin = exp(log_pooled_est - (1.96*log_pooled_se)), ymax = exp(log_pooled_est + (1.96*log_pooled_se)), group = state_nm), fill = "red", alpha = 0.4) + 
  facet_wrap(~state_nm) + 
  xlab("Quarter") + 
  ylab("Prevalence") +
  ggtitle("Random intercept") + 
  theme_bw()



## Model 2 
m2_plot_df <- cbind(mdf_new_grp, predict(new_slope, mdf_new_grp, se.fit = TRUE)) %>% 
  rename(log_fit = fit, 
         log_se = se.fit) %>% 
  filter(!is.na(log_fit)) %>% 
  group_by(state_nm, time) %>% 
  summarize(
    # # num tested 
    # num_tested = sum(Positive + Negative),
    # # observed estimate
    # obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative)),
    # # Number of groups
    m = n(), 
    # Pooled SE on log odds scale
    log_pooled_se = sqrt(sum(log_se^2)/m),
    # Pooled estimate on log odds scale 
    log_pooled_est = sum(log_fit)/m, 
    # Pooled estimate on probability scale 
    pr_pooled_est = exp(log_pooled_est)/(1 + exp(log_pooled_est)),
    # Pooled SE on probability scale 
    pr_pooled_se = log_pooled_se * pr_pooled_est * (1-pr_pooled_est))

m2_plot <- ggplot() + 
  # Observed 
  # geom_point(
  #   data = mdf_new_grp %>%
  #     group_by(state_nm, time) %>%
  #     summarize(num_tested = sum(Positive + Negative),
  #               obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
  #   aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) + 
  
  # Modeled 
  geom_line(data = m2_plot_df, aes(x = time, y = pr_pooled_est, group = state_nm), color = "red") + 
  geom_ribbon(data = m2_plot_df, aes(x = time, ymin = pr_pooled_est - (1.96*pr_pooled_se), ymax = pr_pooled_est + (1.96*pr_pooled_se), group = state_nm), fill = "red", alpha = 0.4) + 
  facet_wrap(~state_nm, scales = "free") + 
  xlab("Quarter") + 
  ylab("Prevalence") +
  ggtitle("Random slope") + 
  theme_bw()




## Model 3
m3_plot_df <- cbind(mdf_new_grp, predict(new_intercept_slope, mdf_new_grp, se.fit = TRUE)) %>% 
  rename(log_fit = fit, 
         log_se = se.fit) %>% 
  filter(!is.na(log_fit)) %>% 
  group_by(state_nm, time) %>% 
  summarize(
    # # num tested 
    # num_tested = sum(Positive + Negative),
    # # observed estimate
    # obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative)),
    # Number of groups
    m = n(), 
    # Pooled SE on log odds scale
    log_pooled_se = sqrt(sum(log_se^2)/m),
    # Pooled estimate on log odds scale 
    log_pooled_est = sum(log_fit)/m, 
    # Pooled estimate on probability scale 
    pr_pooled_est = exp(log_pooled_est)/(1 + exp(log_pooled_est)),
    # Pooled SE on probability scale 
    pr_pooled_se = log_pooled_se * pr_pooled_est * (1-pr_pooled_est))

m3_plot <- ggplot() + 
  # # Observed 
  # geom_point(
  #   data = mdf_new_grp %>%
  #     group_by(state_nm, time) %>%
  #     summarize(num_tested = sum(Positive + Negative),
  #               obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
  #   aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) + 
  
  # Modeled 
  geom_line(data = m3_plot_df, aes(x = time, y = pr_pooled_est, group = state_nm), color = "red") + 
  geom_ribbon(data = m3_plot_df, aes(x = time, ymin = pr_pooled_est - (1.96*pr_pooled_se), ymax = pr_pooled_est + (1.96*pr_pooled_se), group = state_nm), fill = "red", alpha = 0.4) + 
  facet_wrap(~state_nm, scales = "free") + 
  xlab("Quarter") + 
  ylab("Prevalence") +
  ggtitle("Random intercept & slope") + 
  theme_bw()

m3_plot_fixed.scales <- ggplot() + 
  # # Observed 
  # geom_point(
  #   data = mdf_new_grp %>%
  #     group_by(state_nm, time) %>%
  #     summarize(num_tested = sum(Positive + Negative),
  #               obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
  #   aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) + 
  
  # Modeled 
  geom_line(data = m3_plot_df, aes(x = time, y = pr_pooled_est, group = state_nm), color = "red") + 
  geom_ribbon(data = m3_plot_df, aes(x = time, ymin = pr_pooled_est - (1.96*pr_pooled_se), ymax = pr_pooled_est + (1.96*pr_pooled_se), group = state_nm), fill = "red", alpha = 0.4) + 
  facet_wrap(~state_nm) + 
  xlab("Quarter") + 
  ylab("Prevalence") +
  ggtitle("Random intercept & slope") + 
  theme_bw()





## Model 4
m4_plot_df <- cbind(mdf_new_grp, predict(new_gi, mdf_new_grp, se.fit = TRUE)) %>% 
  rename(log_fit = fit, 
         log_se = se.fit) %>% 
  filter(!is.na(log_fit)) %>% 
  group_by(state_nm, time) %>% 
  summarize(
    # # num tested 
    # num_tested = sum(Positive + Negative),
    # # observed estimate
    # obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative)),
    # Number of groups
    m = n(), 
    # Pooled SE on log odds scale
    log_pooled_se = sqrt(sum(log_se^2)/m),
    # Pooled estimate on log odds scale 
    log_pooled_est = sum(log_fit)/m, 
    # Pooled estimate on probability scale 
    pr_pooled_est = exp(log_pooled_est)/(1 + exp(log_pooled_est)),
    # Pooled SE on probability scale 
    pr_pooled_se = log_pooled_se * pr_pooled_est * (1-pr_pooled_est))

m4_plot <- ggplot() + 
  # # Observed 
  # geom_point(
  #   data = mdf_new_grp %>%
  #     group_by(state_nm, time) %>%
  #     summarize(num_tested = sum(Positive + Negative),
  #               obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
  #   aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) +

  # Modeled 
  geom_line(data = m4_plot_df, aes(x = time, y = pr_pooled_est, group = state_nm), color = "red") + 
  geom_ribbon(data = m4_plot_df, aes(x = time, ymin = pr_pooled_est - (1.96*pr_pooled_se), ymax = pr_pooled_est + (1.96*pr_pooled_se), group = state_nm), fill = "red", alpha = 0.4) + 
  facet_wrap(~state_nm) + 
  xlab("Quarter") + 
  ylab("Prevalence") +
  ggtitle("GI") + 
  theme_bw()




## Model 5
m5_plot_df <- cbind(mdf_new_grp, predict(new_gs, mdf_new_grp, se.fit = TRUE)) %>% 
  rename(log_fit = fit, 
         log_se = se.fit) %>% 
  filter(!is.na(log_fit)) %>% 
  group_by(state_nm, time) %>% 
  summarize(
    # # num tested 
    # num_tested = sum(Positive + Negative),
    # # observed estimate
    # obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative)),
    # Number of groups
    m = n(), 
    # Pooled SE on log odds scale
    log_pooled_se = sqrt(sum(log_se^2)/m),
    # Pooled estimate on log odds scale 
    log_pooled_est = sum(log_fit)/m, 
    # Pooled estimate on probability scale 
    pr_pooled_est = exp(log_pooled_est)/(1 + exp(log_pooled_est)),
    # Pooled SE on probability scale 
    pr_pooled_se = log_pooled_se * pr_pooled_est * (1-pr_pooled_est))



m5_plot <- ggplot() + 
  # # Observed 
  # geom_point(
  #   data = mdf_new_grp %>%
  #     group_by(state_nm, time) %>%
  #     summarize(num_tested = sum(Positive + Negative),
  #               obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
  #   aes(x = time, y = obs_pct_pos, group = state_nm, size = num_tested), alpha = 0.3) + 
  # 
  # Modeled 
  geom_line(data = m5_plot_df, aes(x = time, y = pr_pooled_est, group = state_nm), color = "red") + 
  geom_ribbon(data = m5_plot_df, aes(x = time, ymin = pr_pooled_est - (1.96*pr_pooled_se), ymax = pr_pooled_est + (1.96*pr_pooled_se), group = state_nm), fill = "red", alpha = 0.4) + 
  facet_wrap(~state_nm) + 
  xlab("Quarter") + 
  ylab("Prevalence") +
  ggtitle("GS") + 
  theme_bw()

ggsave("figures/gam_m1_plot.png", plot = m1_plot, width = 10, height = 6, dpi = 300)
ggsave("figures/gam_m2_plot.png", plot = m2_plot, width = 10, height = 6, dpi = 300)
ggsave("figures/gam_m3_plot.png", plot = m3_plot, width = 10, height = 6, dpi = 300)
ggsave("figures/gam_m3_plot_fixed.scales.png", plot = m3_plot_fixed.scales, width = 10, height = 6, dpi = 300)
ggsave("figures/gam_m4_plot.png", plot = m4_plot, width = 10, height = 6, dpi = 300)
ggsave("figures/gam_m5_plot.png", plot = m5_plot, width = 10, height = 6, dpi = 300)





# Models with additional covs ---------------------------------------------
## Health unit - Plot data
mdf_new_ind %>% 
  group_by(state, health_unit, result) %>% 
  count() %>% 
  ggplot(aes(x = health_unit, y = n, fill = result)) + 
  geom_bar(position = "fill", stat = "identity") + 
  facet_wrap(~state)








grp_intercept_covs <- gam(cbind(Positive, Negative) ~ s(state, bs = "re", k = 27) + s(time, bs = "tp", k = 10) +  age_cat + hiv_status + sex + health_unit, 
                          data = mdf_new_grp_covs, 
                          family = binomial (link = "logit"), 
                          method = "REML")

grp_slope_covs <- gam(cbind(Positive, Negative) ~ s(time, bs = "tp", k = 10) + s(time, state, bs = "re", k = 27) + age_cat + hiv_status + sex + health_unit, 
                      data = mdf_new_grp_covs, 
                      family = binomial (link = "logit"), 
                      method = "REML")


grp_intercept_slope_covs <- gam(cbind(Positive, Negative) ~ s(state, bs = "re", k = 27) + s(time, bs = "tp", k = 10) + s(time, state, bs = "re", k = 27) + age_cat + hiv_status + sex + health_unit, 
                                data = mdf_new_grp_covs, 
                                family = binomial (link = "logit"), 
                                method = "REML")


grp_GI <- gam(cbind(Positive, Negative) ~ s(state, bs = "re", k=27) + s(time, bs = "tp", k = 10) + s(time, by = state, bs = "tp", m=1, k = 10) + age_cat + hiv_status + sex + health_unit, 
              data = mdf_new_grp_covs, 
              family = binomial (link = "logit"), 
              method = "REML")

grp_GS <- gam(cbind(Positive, Negative) ~ s(time, bs = "tp", m=2) + s(time, state, bs = "fs", m=2) + age_cat + hiv_status + sex + health_unit, 
              data = mdf_new_grp_covs, 
              family = binomial (link = "logit"), 
              method = "REML")

pred<- cbind(mdf_new_grp_covs, predict(grp_intercept_covs, mdf_new_grp_covs, se.fit = TRUE, type = "response"))





# looking at MG
print(mdf_new_grp %>% 
  filter(state_nm == "Mato Grosso") %>% 
  ungroup() %>% 
  group_by(time) %>% 
  summarize(cases = sum(cases), 
            tested = if_else(is.nan(sum(Negative + Positive)/sum(cases)), 0, sum(Negative + Positive)/sum(cases)), 
            positive = if_else(is.nan(sum(Positive)/sum(Negative + Positive)), 0, sum(Positive)/sum(Negative + Positive))), 
  n = 30)







# old code ----------------------------------------------------------------


# Model 1 - Smooth random intercept for state 
## Note:  Adding the 1 + is exactly the same thing as the random intercept
gam_mod_1 <- gam(cbind(Positive,Negative) ~ s(state, bs = "re"),
            family = binomial,
            data = mdf_new_grp, 
            method = "REML")

summary(gam_mod_1)
gam.vcomp(gam_mod_1) 


# Model 2 - Random effect/smooth time trend 
gam_mod_2 <- gam(cbind(Positive, Negative) ~ s(time, by = state, bs = "re"), 
            data = mdf_new_grp, 
            family = binomial,
            method = "REML")

summary(gam_mod_2)
gam.vcomp(gam_mod_2) 


# EDF is how much information in time we have after partial pooling down the 
## parameters (how much within effect we have after getting rid of the between effect?)
## Mato Gross has VERY few tests conducted, but a higher number of positives 

# Model 3 - Random intercept for state + smooth time trend 
gam_mod_3 <- gam(cbind(Positive, Negative) ~ 1 + s(state, bs = "re") + s(time, by = state, bs = "re"), 
                data = mdf_new_grp,
                family = binomial,
                method = "REML")

summary(gam_mod_3)


# Model 4 - Random effect/smooth time trend + random state intercept + linear covs (GROUP-LEVEL)
gam_mod_4 <- gam(cbind(Positive, Negative) ~ 1 + s(state, bs = "re") + s(time, by = state, bs = "re") + age_cat + hiv_status + sex, 
                  data = mdf_new_grp, 
                  family = binomial,
                  method = "REML")

save(gam_mod_4, file = "output/fits/gam_mod_4.rda")

summary(gam_mod_4)
mdf_new_grp$fitted <- fitted.values(gam_mod_4)



# Model 5 - Fit to individual estimates
gam_mod_5 <- gam(result ~ 1 + s(state, bs = "re") + s(time, by = state, bs = "re") + age_cat + hiv_status + sex, 
                 data = mdf_new_ind, 
                 family = binomial,
                 method = "REML", 
                 file = "output/fits/gam_mod_5.rda")

save(gam_mod_5, file = "output/fits/gam_mod_5.rda")
summary(gam_mod_5)

mdf_new_ind$fitted <- predict(gam_mod_5, newdata = mdf_new_ind, type = "response")



## Plot - Compare individual + group 
## Notes: They match, except for state 33
ggplot() +
  # Individual
  geom_line(data = mdf_new_ind %>% 
               filter(!is.na(fitted)) %>% 
               group_by(state, time) %>% 
               summarize(cases = n(), 
                         pred_pct = sum(fitted)/cases),
             aes(x = time, y = pred_pct)) + 
  # Group 
  geom_line(data = mdf_new_grp %>% 
               group_by(state, time) %>% 
               summarize(n = sum(cases), 
                         pred_pct = sum(fitted*cases)/n),
             aes(x = time, y = pred_pct, color = "blue")) + 
  facet_wrap(~state)









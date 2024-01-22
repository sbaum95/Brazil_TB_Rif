# Author: Sarah Baum
# Date Created: 2023-11-17
# Date Modified: 

# Description: Takes tt models (tt_gam_models-micro and tt_gam_models-mun) and creates plots 


source(here::here("code/dependencies.R"))

library(mgcv)
library(gratia)
library(itsadug)
library(tidygam)



# load sinan dataset 
load(here::here("data/sinan_xpert.Rdata"))


# load model input datasets 
load(here::here("data/mdf_new_ind.Rdata")) # New cases (individual)
load(here::here("data/mdf_mun_new_grp.Rdata")) # Municipality-level new cases (group)
load(here::here("data/mdf_mic_new_grp.Rdata")) # Micro-region-level new cases (group)

load(here::here("data/mdf_prev_ind.Rdata")) # Previous cases (individual)


# load models 
load(here::here("output/fits/mun_new_mod1a.rda")) # municipality-level new cases (2014-2019)
load(here::here("output/fits/mun_new_mod2a.rda")) # municipality-level new cases (2016-2019)

load(here::here("output/fits/mun_prev_mod1a.rda")) # municipality-level previous cases (2014-2019)
load(here::here("output/fits/mun_prev_mod2a.rda")) # municipality-level previous cases (2016-2019)

load(here::here("output/fits/mic_new_mod1a.Rda")) # microregion-level new cases (2014-2019)
load(here::here("output/fits/mic_new_mod2a.Rda")) # microregion-level new cases (2016-2019)

load(here::here("output/fits/mic_prev_mod1a.Rda")) # microregion-level previous cases (2014-2019)
load(here::here("output/fits/mic_prev_mod2a.Rda")) # microregion-level previous cases (2016-2019)





# predict from models 
pred_mun_new_mod1 <- fitted_values(mun_new_mod1a, data = mdf_mun_new_grp, scale = "response")
pred_mun_new_mod2 <- fitted_values(mun_new_mod2a, data = mdf_mun_new_grp %>% filter(time > 8), scale = "response")

pred_mun_prev_mod1 <- fitted_values(mun_prev_mod1a, data = mdf_prev_ind, scale = "response")
pred_mun_prev_mod2 <- fitted_values(mun_prev_mod2a, data = mdf_prev_ind %>% filter(time > 8), scale = "response")

pred_mic_new_mod1 <- fitted_values(mic_new_mod1a, data = mdf_mic_new_grp, scale = "response")
pred_mic_new_mod2 <- fitted_values(mic_new_mod2a, data = mdf_mic_new_grp %>% filter(time > 8), scale = "response")

pred_mic_prev_mod1 <- fitted_values(mic_prev_mod1a, data = mdf_prev_ind, scale = "response")
pred_mic_prev_mod2 <- fitted_values(mic_prev_mod2a, data = mdf_prev_ind %>% filter(time > 8), scale = "response")



# plot model output

# national-level estimates ------------------------------------------------
plot_national_all <- ggplot() +
  # new cases 
  # add pct tested and scale by number tested
  geom_point(
    data = mdf_mun_new_grp %>%
      group_by(time) %>%
      summarize(num_tested = sum(Positive + Negative),
                obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
    aes(x = time, y = obs_pct_pos*100, size = num_tested, color = "New"), alpha = 0.5) +
  
  # Municipality Model 1 (2014-2019) - fitted values
  geom_line(data = pred_mun_new_mod1 %>%
              filter(!is.na(fitted)) %>%
              group_by(time) %>%
              summarize(fitted_national = sum(fitted*cases)/sum(cases), 
                        cases = sum(cases)),
            aes(time, fitted_national*100, color = "New", linetype = "2014-2019")) +
  
  # Municipality Model 2 (2016-2019) - fitted values
  geom_line(data = pred_mun_new_mod2 %>%
              filter(!is.na(fitted)) %>%
              group_by(time) %>%
              summarize(fitted_national = sum(fitted*cases)/sum(cases)),
            aes(time, fitted_national*100, color = "New", linetype = "2016-2019")) +
  
  # previously treated 
  # add pct tested and scale by number tested
  geom_point(
    data = mdf_prev_ind %>%
      group_by(time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
    aes(x = time, y = obs_pct_pos*100, size = num_tested, color = "Previous"), alpha = 0.3) +
  
  # Municipality Model 1 (2014-2019) - fitted values
  geom_line(data = pred_mun_prev_mod1 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, color = "Previous", linetype = "2014-2019")) +
  
  # Municipality Model 2 (2016-2019) - fitted values
  geom_line(data = pred_mun_prev_mod2 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, color = "Previous", linetype = "2016-2019")) +
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("National-level estimates ") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 14)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Number of cases tested",
       color = "Case Type") +
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Case Type",
                     labels=c("New",
                              "Previous"),
                     values=c("black", "red")) + 
  scale_linetype_manual(name = "Model", 
                        labels = c("2014-2019", 
                                   "2016-2019"),
                        values=c(1, 2))
  
  
save(plot_national_all, file = "output/plots/plot_national_all")
ggsave(filename = "output/plots/plot_national_all.png", width = 14, height = 10)



## New cases (from municipality)
plot_national_new <- ggplot() +
  # add pct tested and scale by number tested
  geom_point(
    data = mdf_mun_new_grp %>%
      group_by(time) %>%
      summarize(num_tested = sum(Positive + Negative),
                obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
    aes(x = time, y = obs_pct_pos*100, size = num_tested), alpha = 0.5) +
  
  # Municipality Model 1 (2014-2019) - fitted values
  geom_line(data = pred_mun_new_mod1 %>%
              filter(!is.na(fitted)) %>%
              group_by(time) %>%
              summarize(fitted_national = sum(fitted*cases)/sum(cases), 
                        cases = sum(cases)),
            aes(time, fitted_national*100, color = "2014-2019")) +
  
  # Municipality Model 2 (2016-2019) - fitted values
  geom_line(data = pred_mun_new_mod2 %>%
              filter(!is.na(fitted)) %>%
              group_by(time) %>%
              summarize(fitted_national = sum(fitted*cases)/sum(cases)),
            aes(time, fitted_national*100, color = "2016-2019")) +
  
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("National: New Cases") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 14)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Number of cases tested",
       color = "Models") +
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Model",
                     labels=c("2014-2019",
                              "2016-2019"),
                     values=c("black", "red")) 



save(plot_national_new, file = "output/plots/plot_national_new")
ggsave("output/plots/plot_national_new.png", width = 14, height = 10)


## Previously-treated cases (from municipality)
plot_national_prev <- ggplot() +
  # add pct tested and scale by number tested
  geom_point(
    data = mdf_prev_ind %>%
      group_by(time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
    aes(x = time, y = obs_pct_pos*100, size = num_tested), alpha = 0.3) +
  
  # Municipality Model 1 (2014-2019) - fitted values
  geom_line(data = pred_mun_prev_mod1 %>% 
            filter(!is.na(fitted)) %>% 
            group_by(time) %>% 
            
            summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, color = "2014-2019")) +
  
  # Municipality Model 2 (2016-2019) - fitted values
  geom_line(data = pred_mun_prev_mod2 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, color = "2016-2019")) +
 
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("National: Previously Treated") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 14)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Number of cases tested",
       color = "Models") +
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Model",
                     labels=c("2014-2019",
                              "2016-2019"),
                     values=c("black", "red")) 


save(plot_national_prev, file = "output/plots/plot_national_prev")
ggsave("output/plots/plot_national_prev.png", width = 14, height = 10)





# Municipality-level ------------------------------------------------------


## new cases (2014-2019 and 2016-2019)
plot_mun_new <- ggplot() +

  # add pct tested and scale by number tested
  geom_point(
    data = mdf_mun_new_grp %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(Positive + Negative),
                obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
    aes(x = time, y = obs_pct_pos*100, group = state_nm, size = num_tested), alpha = 0.5) +
  
  # Add in indications of when no cases were tested with xpert in a given period
  geom_point(data = mdf_mun_new_grp  %>%
               group_by(state_nm, time) %>%
               summarize(num_tested = sum(Positive + Negative),
                         obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))) %>% 
               filter(num_tested == 0),
             aes(x = time, y = num_tested), colour="black", shape=4, size=3) + 
  
  # add fitted values - Model 1 (2014-2019)
  geom_line(data = pred_mun_new_mod1 %>%
              filter(!is.na(fitted)) %>%
              group_by(state_nm, time) %>%
              summarize(fitted = sum(fitted*cases)/sum(cases)),
            aes(time, fitted*100, group = state_nm, color = "2014-2019")) +

  # add fitted values - Model 2 (2016-2019)
  geom_line(data = pred_mun_new_mod2 %>%
              filter(!is.na(fitted)) %>%
              group_by(state_nm, time) %>%
              summarize(fitted = sum(fitted*cases)/sum(cases)),
            aes(time, fitted*100, group = state_nm, color = "2016-2019")) +


  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("Municipality-Level: New Cases") + 
  facet_wrap(~state_nm, scales = "free") +    
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(3, "pt"), 
        # legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 12)) +
  scale_size(
    range = c(0.5, 5)
  ) +
labs(size = "Number of cases tested",
     color = "Models") +
scale_fill_manual(values = c("Fitted")) +
scale_color_manual(name="Model",
                   labels=c("2014-2019",
                            "2016-2019"),
                   values=c("black", "red"))

save(plot_mun_new, file = "output/plots/plot_mun_new")
ggsave("output/plots/plot_mun_new.png", width = 18, height = 10)




## previous cases (2014-2019 and 2016-2019)
### Plot fitted model
plot_mun_prev <- ggplot() +
  # add pct tested and scale by number tested
  geom_point(
    data = mdf_prev_ind %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
    aes(x = time, y = obs_pct_pos*100, group = state_nm, size = num_tested), alpha = 0.3) +
  
  # Add in indications of when no cases were tested with xpert in a given period
  geom_point(
    data = mdf_prev_ind %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)) %>% 
      filter(num_tested == 0),
    aes(x = time, y = num_tested), colour="black", shape=4, size=3) + 
  
  # add fitted values - Model 1 (2014-2019)
  geom_line(data = pred_mun_prev_mod1 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(state_nm, time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, group = state_nm, color = "2014-2019")) +
  
  # add fitted values - Model 2 (2016-2019)
  geom_line(data = pred_mun_prev_mod2 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(state_nm, time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, group = state_nm, color = "2016-2019")) +
  
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("Municipality-level: Previously treated cases") + 
  facet_wrap(~state_nm, scales = "free") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(3, "pt"), 
        # legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 12)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Number of cases tested", 
       color = "")  + 
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Lines",
                     labels=c("2014-2019", 
                              "2016-2019"),
                     values=c("black", "red"))

save(plot_mun_prev, file = "output/plots/plot_mun_prev")
ggsave("output/plots/plot_mun_prev.png", width = 18, height = 10)





# Microregion-level -------------------------------------------------------

## new cases (2014-2019 and 2016-2019)
plot_mic_new <- ggplot() +
  
  # add pct tested and scale by number tested
  geom_point(
    data = mdf_mic_new_grp %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(Positive + Negative),
                obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))),
    aes(x = time, y = obs_pct_pos*100, group = state_nm, size = num_tested), alpha = 0.5) +
  
  # Add in indications of when no cases were tested with xpert in a given period
  geom_point(data = mdf_mic_new_grp  %>%
               group_by(state_nm, time) %>%
               summarize(num_tested = sum(Positive + Negative),
                         obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))) %>% 
               filter(num_tested == 0),
             aes(x = time, y = num_tested), colour="black", shape=4, size=3) + 
  
  # add fitted values - Model 1 (2014-2019)
  geom_line(data = pred_mic_new_mod1 %>%
              filter(!is.na(fitted)) %>%
              group_by(state_nm, time) %>%
              summarize(fitted = sum(fitted*cases)/sum(cases)),
            aes(time, fitted*100, group = state_nm, color = "2014-2019")) +
  
  # add fitted values - Model 2 (2016-2019)
  geom_line(data = pred_mic_new_mod2 %>%
              filter(!is.na(fitted)) %>%
              group_by(state_nm, time) %>%
              summarize(fitted = sum(fitted*cases)/sum(cases)),
            aes(time, fitted*100, group = state_nm, color = "2016-2019")) +
  
  
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("Microregion-Level: New Cases") + 
  facet_wrap(~state_nm, scales = "free") +    
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(3, "pt"), 
        # legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 12)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Number of cases tested",
       color = "Models") +
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Lines",
                     labels=c("2014-2019",
                              "2016-2019"),
                     values=c("black", "red"))


save(plot_mic_new, file = "output/plots/plot_mic_new")
ggsave("output/plots/plot_mic_new.png", width = 18, height = 10)




## Microregion-level previous cases (2014-2019 and 2016-2019)
### Plot fitted model
plot_mic_prev <- ggplot() +
  # # add pct tested and scale by number tested
  geom_point(
    data = mdf_prev_ind %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
    aes(x = time, y = obs_pct_pos*100, group = state_nm, size = num_tested), alpha = 0.3) +

  # # Add in indications of when no cases were tested with xpert in a given period
  geom_point(
    data = mdf_prev_ind %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)) %>%
      filter(num_tested == 0),
    aes(x = time, y = num_tested), colour="black", shape=4, size=3) +

  # add fitted values - Model 1 (2014-2019)
  geom_line(data = pred_mic_prev_mod1 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(state_nm, time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, group = state_nm, color = "2014-2019")) +
  
  # # add fitted values - Model 2 (2016-2019)
  geom_line(data = pred_mic_prev_mod2 %>%
              filter(!is.na(fitted)) %>%
              group_by(state_nm, time) %>%

              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, group = state_nm, color = "2016-2019"))+
  
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("Microregion-level: Previously treated cases") + 
  facet_wrap(~state_nm, scales = "free") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(3, "pt"), 
        # legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 12)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Number of cases tested", 
       color = "")  + 
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Lines",
                     labels=c("2014-2019", 
                              "2016-2019"
                     ),
                     values=c("black", "red"))

save(plot_mic_prev, file = "output/plots/plot_mic_prev")
ggsave("output/plots/plot_mic_prev.png", width = 18, height = 10)




## Mun and Microregion-level previous cases (2014-2019 and 2016-2019)
plot_mun.mic_prev <- ggplot() +
  # add pct tested and scale by number tested
  geom_point(
    data = mdf_prev_ind %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
    aes(x = time, y = obs_pct_pos*100, group = state_nm, size = num_tested), alpha = 0.3) +
  
  # Add in indications of when no cases were tested with xpert in a given period
  geom_point(
    data = mdf_prev_ind %>%
      group_by(state_nm, time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)) %>% 
      filter(num_tested == 0),
    aes(x = time, y = num_tested), colour="black", shape=4, size=3) + 
  
  # add fitted values - Model 1 (2014-2019)
  geom_line(data = pred_mic_prev_mod1 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(state_nm, time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, group = state_nm, color = "Micro-region", linetype = "2014-2019"))+
  
  # add fitted values - Model 2 (2016-2019)
  geom_line(data = pred_mic_prev_mod2 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(state_nm, time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, group = state_nm, color = "Micro-region", linetype = "2016-2019"))+
  # add fitted values - Model 1 (2014-2019)
  geom_line(data = pred_mun_prev_mod1 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(state_nm, time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, group = state_nm, color = "Municipality", linetype = "2014-2019")) +
  
  # add fitted values - Model 2 (2016-2019)
  geom_line(data = pred_mun_prev_mod2 %>% 
              filter(!is.na(fitted)) %>% 
              group_by(state_nm, time) %>% 
              
              summarize(fitted = sum(fitted)/n()), aes(time, fitted*100, group = state_nm, color = "Municipality", linetype = "2016-2019")) +
  
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("Municipality and Microregion-level: Previously treated cases") + 
  facet_wrap(~state_nm, scales = "free") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(3, "pt"), 
        # legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 12)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Number of cases tested", 
       color = "")  + 
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Geographic Region",
                     labels = c("Municipality", 
                                "Micro-region"),
                     values=c("black", "red")) + 
  scale_linetype_manual(name = "Model", 
                        labels = c("2014-2019", 
                                   "2016-2019"),
                        values=c(1, 2))

save(plot_mun.mic_prev, file = "output/plots/plot_mun.mic_prev")
ggsave("output/plots/plot_mun.mic_prev.png", width = 18, height = 10)





# Bootstrapped Results ----------------------------------------------------
load("output/fits/boot.mic_new_1.Rda")
load("output/fits/boot.mic_new_2.Rda")
load("output/fits/boot.mic_prev_1.Rda")
load("output/fits/boot.mic_prev_2.Rda")

load("output/fits/boot.mun_new_1.Rda")
load("output/fits/boot.mun_new_2.Rda")
load("output/fits/boot.mun_prev_1.Rda")
load("output/fits/boot.mun_prev_2.Rda")


## national-level ----------------------------------------------------------
plot_boot_nat_new <- ggplot() +
  geom_errorbar(data = boot.mic_new_2[["pred.int_nat"]], aes(time, ymin = lci*100, ymax = hci*100, color = "Micro-Region"), width = 0) +
  geom_point(data = boot.mic_new_2[["pred.int_nat"]], aes(time, mean*100, color = "Micro-Region")) +
  geom_line(data = boot.mic_new_2[["pred.int_nat"]], aes(time, mean*100, color = "Micro-Region")) +
  geom_errorbar(data = boot.mun_new_2[["pred.int_nat"]], aes(time, ymin = lci*100, ymax = hci*100, color ="Municipality"), width = 0) +
  geom_point(data = boot.mun_new_2[["pred.int_nat"]], aes(time, mean*100, color = "Municipality")) + 
  geom_line(data = boot.mun_new_2[["pred.int_nat"]], aes(time, mean*100, color = "Municipality")) + 
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("New (2016-2019)") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 11),
        panel.spacing = unit(3, "pt"), 
        legend.text = element_text(size = 14), 
        title = element_text(size = 18)) + 
  scale_color_manual(name="Level",
                     labels=c("Micro-Region",
                              "Municipality"),
                     values=c("black", "red"))

ggsave("output/plots/plot_boot_nat_new.png", width = 18, height = 10)


plot_boot_nat_prev <- ggplot() +
  # geom_errorbar(data = boot.mic_prev_2[["pred.int_nat"]], aes(time, ymin = lci*100, ymax = hci*100, color = "Micro-Region"), width = 0) +
  # geom_point(data = boot.mic_prev_2[["pred.int_nat"]], aes(time, mean*100, color = "Micro-Region")) +
  # geom_line(data = boot.mic_prev_2[["pred.int_nat"]], aes(time, mean*100, color = "Micro-Region")) +
  geom_errorbar(data = boot.mun_prev_2[["pred.int_nat"]], aes(time, ymin = lci*100, ymax = hci*100, color ="Municipality"), width = 0) +
  geom_point(data = boot.mun_prev_2[["pred.int_nat"]], aes(time, mean*100, color = "Municipality")) + 
  geom_line(data = boot.mun_prev_2[["pred.int_nat"]], aes(time, mean*100, color = "Municipality")) + 
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("Previously treated (2016-2019)") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 11),
        panel.spacing = unit(3, "pt"), 
        legend.text = element_text(size = 14), 
        title = element_text(size = 18)) + 
  scale_color_manual(name="Level",
                     labels=c("Micro-Region",
                              "Municipality"),
                     values=c("black", "red"))

ggsave("output/plots/plot_boot_nat_prev.png", width = 18, height = 10)

## state-level -------------------------------------------------------------


### new --------------------------------------------------------------------
plot_boot_new <- ggplot() +
  # geom_errorbar(data = boot.mic_new_1[["pred.int_state"]], aes(time, ymin = lci*100, ymax = hci*100, group = state_nm, color = "2014-2019")) + 
  # geom_point(data = boot.mic_new_1[["pred.int_state"]], aes(time, mean*100, group = state_nm, color = "2014-2019")) +
  geom_errorbar(data = boot.mic_new_2[["pred.int_state"]], aes(time, ymin = lci*100, ymax = hci*100, group = state_nm, color = "Micro-Region")) + 
  geom_point(data = boot.mic_new_2[["pred.int_state"]], aes(time, mean*100, group = state_nm, color = "Micro-Region")) +
  # geom_errorbar(data = boot.mun_new_1[["pred.int_state"]], aes(time, ymin = lci*100, ymax = hci*100, group = state_nm, color = "2014-2019")) + 
  # geom_point(data = boot.mun_new_1[["pred.int_state"]], aes(time, mean*100, group = state_nm, color = "2014-2019")) +
  geom_errorbar(data = boot.mun_new_2[["pred.int_state"]], aes(time, ymin = lci*100, ymax = hci*100, group = state_nm, color = "Municipality")) + 
  geom_point(data = boot.mun_new_2[["pred.int_state"]], aes(time, mean*100, group = state_nm, color = "Municipality")) +
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("95% Prediction Intervals: New (2016-2019)") + 
  theme_bw() + 
  facet_wrap(~state_nm) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(3, "pt"), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 12)) + 
  scale_color_manual(name="Level",
                     labels=c("Micro-Region",
                              "Municipality"),
                     values=c("black", "red"))

ggsave("output/plots/plot_boot_new.png", width = 18, height = 10)



### prev --------------------------------------------------------------------
# Note: Confirm that you've run model without 0-4 yos
plot_boot_prev <- ggplot() +
  # geom_errorbar(data = boot.mic_prev_1[["pred.int_state"]], aes(time, ymin = lci*100, ymax = hci*100, group = state_nm, color = "2014-2019")) +
  # geom_point(data = boot.mic_prev_1[["pred.int_state"]], aes(time, mean*100, group = state_nm, color = "2014-2019")) +
  # geom_errorbar(data = boot.mic_prev_2[["pred.int_state"]], aes(time, ymin = lci*100, ymax = hci*100, group = state_nm, color = "Micro-Region")) +
  # geom_point(data = boot.mic_prev_2[["pred.int_state"]], aes(time, mean*100, group = state_nm, color = "Micro-Region")) +
  geom_errorbar(data = boot.mun_prev_1[["pred.int_state"]], aes(time, ymin = lci*100, ymax = hci*100, group = state_nm, color = "Mun")) +
  geom_point(data = boot.mun_prev_1[["pred.int_state"]], aes(time, mean*100, group = state_nm, color = "Mun")) +
  geom_errorbar(data = boot.mun_prev_2[["pred.int_state"]], aes(time, ymin = lci*100, ymax = hci*100, group = state_nm, color = "Municipality")) +
  geom_point(data = boot.mun_prev_2[["pred.int_state"]], aes(time, mean*100, group = state_nm, color = "Municipality")) +
  xlab("Quarter") + 
  ylab("Percent positive") + 
  ggtitle("95% Prediction Intervals: Previously treated (2016-2019)") + 
  theme_bw() + 
  facet_wrap(~state_nm, scales = "free") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        panel.spacing = unit(3, "pt"), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 12)) + 
  scale_color_manual(name="Level",
                     labels=c("Micro-Region",
                              "Municipality"),
                     values=c("black", "red"))

ggsave("output/plots/plot_boot_prev_v2.png", width = 18, height = 10)


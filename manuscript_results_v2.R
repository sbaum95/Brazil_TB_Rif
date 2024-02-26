

source(here::here("code/dependencies.R"))
library(officer)
library(flextable)


# create empty word doc for results
result_output <- officer::read_docx()


############################################################################
############################################################################
###                                                                      ###
###                         LOAD DATA AND MODELS                         ###
###                                                                      ###
############################################################################
############################################################################

# load data ---------------------------------------------------------------
## New cases
load("data/mdf_mun_new_grp.Rdata")
load("data/mdf_mun_new_grp.miss.Rdata")
load("data/mdf_new_ind.Rdata")

## Previous cases
load("data/mdf_prev_ind.Rdata")
load("data/mdf_mun_prev_grp.miss.Rdata")



# load fitted models ------------------------------------------------------
source(here::here("code/time_trend_model/municip-level/get_fitted_ests.R"))


# Load projected cases from bootstrapped models 
## New cases 
load("output/fits/boot.mun_new_1.Rda")
# load("output/fits/boot.mun_new_2.Rda")
load("output/fits/boot.mun_new_3.Rda")
load("output/fits/boot.mun_new_4.Rda")

# load("output/fits/boot.mun_new_1.miss.Rda")
load("output/fits/boot.mun_new_3_miss.Rda")
load("output/fits/boot.mun_new_4_miss.Rda")

load("output/projected_nat_new.Rdata")
# load("output/projected_nat_new.yr.Rdata")
# load("output/projected_state_new.Rdata")
# load("output/projected_state_new.yr.Rdata")

## Previous cases
load("output/fits/boot.mun_prev_1.Rda")
# load("output/fits/boot.mun_prev_2.Rda")
load("output/fits/boot.mun_prev_3.Rda")
load("output/fits/boot.mun_prev_4.Rda")

# load("output/fits/boot.mun_prev_1.miss.Rda")
load("output/fits/boot.mun_prev_3.miss.Rda")
load("output/fits/boot.mun_prev_4.miss.Rda")

# load("output/projected_nat_prev.Rdata")
# load("output/projected_nat_prev.yr.Rdata")
# load("output/projected_state_prev.Rdata")
# load("output/projected_state_prev.yr.Rdata")


# functions to look at distributions
get_fitted_dist <- function(data, density_var, cat_var){

  data %>%
    ggplot(aes(x = !!sym(density_var), fill = !!sym(cat_var))) +
    geom_density(alpha = 0.5) +
    labs(title = paste0("Density Plot of RR-TB probability by ", cat_var),
         x = "Predicted Outcomes",
         y = "Density",
         fill = paste0(cat_var))

}
get_observed_dist <- function(data, outcome, cat_var){

  data %>%
    ggplot(aes(x = !!sym(cat_var), fill = factor(!!sym(outcome)))) +
    geom_bar(position = "dodge", stat = "count") +
    labs(title = paste0("Distribution of Binary Outcome by ", cat_var),
         x = paste0(cat_var),
         y = "Count",
         fill = paste0(outcome))
}

############################################################################
############################################################################
###                                                                      ###
###                           FIGURES & TABLES                           ###
###                                                                      ###
############################################################################
############################################################################



# Figure 1. Model Performance ---------------------------------------------
figA_national_all <- ggplot() +
  
  # 1. Plot observed and modeled data for new cases
  ## Observed data (complete cases)
  ## 1.1.1 Plot percent of confirmed cases with resistant test and number of confirmed Xpert tests performed
  geom_point(
    data = mdf_mun_new_grp %>%
      group_by(diag_qrt) %>%
      summarize(num_tested = sum(Positive + Negative),
                obs_pct_pos = if_else(is.nan(sum(Positive)/sum(Positive + Negative)), 0, sum(Positive)/sum(Positive + Negative))
      ), 
    aes(x = diag_qrt, y = obs_pct_pos*100, size = num_tested, color = "New"), alpha = 0.5) +
  
  ## 1.1.2. Plot percent of cases that get tested 
  geom_line(
    data = mdf_mun_new_grp %>%
      group_by(diag_qrt) %>%
      summarize(pct_tested = sum(Positive + Negative)/sum(Positive + Negative + Miss)),
    aes(x = diag_qrt, y = pct_tested*100,  color = "New"), alpha = 0.5) + 
  
  
  

  
  
  
  
  
  
  ## 1.2 Plot time trend in percent of projected cases/complete cases
  
  ### 1.2.1 Model 1 (2014-2019)
  #### Fitted values (Complete cases)
  # geom_line(
  #   data = pred_mun_new_mod1 %>%
  #     filter(!is.na(fitted)) %>%
  #     group_by(diag_qrt) %>%
  #     summarize(fitted_national = sum(fitted*cases)/sum(cases),
  #               cases = sum(cases)),
  #   aes(diag_qrt, fitted_national*100, color = "New", linetype = "2014-2019")) +
  # 
  #### Fitted values (Missing)
  geom_line(
    data = pred_mun_new_mod1.miss %>%
      filter(!is.na(fitted)) %>%
      group_by(diag_qrt) %>%
      summarize(fitted_national = sum(fitted*cases)/sum(cases),
                cases = sum(cases)),
    aes(diag_qrt, fitted_national*100, color = "New", linetype = "2014-2019")) +
  
  
  
  
  
  ### 1.2.2 Model 2 (2016-2019)
  #### Fitted values (Complete cases)
  # geom_line(data = pred_mun_new_mod2 %>%
  #             filter(!is.na(fitted)) %>%
  #             group_by(diag_qrt) %>%
  #             summarize(fitted_national = sum(fitted*cases)/sum(cases)),
  #           aes(diag_qrt, fitted_national*100, color = "New", linetype = "2016-2019")) +
  # 
  
  
  # ### 1.2.3 Model 3 (2017-2019) 
  # #### Fitted values (Complete cases)
  # geom_line(
  #   data = pred_mun_new_mod3 %>%
  #     filter(!is.na(fitted)) %>%
  #     group_by(diag_qrt) %>%
  #     summarize(fitted_national = sum(fitted*cases)/sum(cases),
  #               cases = sum(cases)),
  #   aes(diag_qrt, fitted_national*100, color = "New", linetype = "2017-2019")) +
  # 
  # #### Fitted values (Missing)
  # geom_line(
  #   data = pred_mun_new_mod3.miss %>%
  #     filter(!is.na(fitted)) %>%
  #     group_by(diag_qrt) %>%
  #     summarize(fitted_national = sum(fitted*cases)/sum(cases),
  #               cases = sum(cases)),
  #   aes(diag_qrt, fitted_national*100, color = "New", linetype = "2017-2019")) +

  
  ### 1.2.3 Model 4 (2015-2019) 
  #### Fitted values (Complete cases)
  geom_line(
    data = pred_mun_new_mod4 %>%
      filter(!is.na(fitted)) %>%
      group_by(diag_qrt) %>%
      summarize(fitted_national = sum(fitted*cases)/sum(cases),
                cases = sum(cases)),
    aes(diag_qrt, fitted_national*100, color = "New", linetype = "2015-2019 (excluding stock outs)")) +
  
  #### Fitted values (Missing)
  geom_line(
    data = pred_mun_new_mod4.miss %>%
      filter(!is.na(fitted)) %>%
      group_by(diag_qrt) %>%
      summarize(fitted_national = sum(fitted*cases)/sum(cases),
                cases = sum(cases)),
    aes(diag_qrt, fitted_national*100, color = "New", linetype = "2015-2019 (excluding quarter 12)")) +
  
  

  # 3. Plot observed and modeled data for previous cases
  ## Observed (complete cases)
  ## 3.1.1 Plot percent of confirmed cases with resistant test and number of confirmed Xpert tests performed
  geom_point(
    data = mdf_prev_ind %>%
      group_by(diag_qrt) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE),
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested)),
    aes(x = diag_qrt, y = obs_pct_pos*100, size = num_tested, color = "Previous"), alpha = 0.3) + 
  
  ## 3.1.2. Plot percent of cases that get tested 
  geom_line(
  data = mdf_prev_ind %>%
    group_by(diag_qrt) %>%
    summarize(pct_tested = sum(tested)/n()),
  aes(x = diag_qrt, y = pct_tested*100,  color = "Previous"), alpha = 0.5) +
  
  
  
  ggplot() + 
  ## 3.2 Plot time trend in percent of projected cases/complete cases
  ### 3.2.1 Model 1 (2014-2019)
  #### Fitted values (Complete cases)
  geom_line(data = pred_mun_prev_mod1 %>%
            filter(!is.na(fitted)) %>%
            group_by(diag_qrt) %>%
            summarize(fitted = sum(fitted)/n()),
          aes(diag_qrt, fitted*100, color = "Previous", linetype = "2014-2019")) +
  
  
  #### Fitted values (Missing)
  geom_line(data = pred_mun_prev_mod1.miss %>%
              filter(!is.na(fitted)) %>%
              group_by(diag_qrt) %>%
              summarize(fitted = sum(fitted)/n()),
            aes(diag_qrt, fitted*100, color = "Previous", linetype = "2014-2019")) +
  
  
  ### 3.2.2. Model 2 (2016-2019) 
  #### Fitted values (Complete cases)
  # geom_line(data = pred_mun_prev_mod2 %>%
  #             filter(!is.na(fitted)) %>%
  #             group_by(diag_qrt) %>%
  #             summarize(fitted = sum(fitted)/n()),
  #           aes(diag_qrt, fitted*100, color = "Previous", linetype = "2016-2019")) +
  # # 

  
  
  ### 3.2.3 Model 3 (2016-2019) 
  #### Fitted values (Complete cases)
  # geom_line(data = pred_mun_prev_mod3 %>%
  #             filter(!is.na(fitted)) %>%
  #             group_by(diag_qrt) %>%
  #             summarize(fitted = sum(fitted)/n()),
  #           aes(diag_qrt, fitted*100, color = "Previous", linetype = "2017-2019")) +
  # 
  # #### Fitted values (Missing)
  # geom_line(data = pred_mun_prev_mod3.miss %>%
  #             filter(!is.na(fitted)) %>%
  #             group_by(diag_qrt) %>%
  #             summarize(fitted = sum(fitted)/n()),
  #           aes(diag_qrt, fitted*100, color = "Previous", linetype = "2017-2019")) +
  

  ### 3.2.3 Model 4 (2015-2019) 
  #### Fitted values (Complete cases)
  # geom_line(data = pred_mun_prev_mod4 %>%
  #             filter(!is.na(fitted)) %>%
  #             group_by(diag_qrt) %>%
  #             summarize(fitted = sum(fitted)/n()),
  #           aes(diag_qrt, fitted*100, color = "Previous", linetype = "2015-2019 (excluding stock outs)")) +
  
  #### Fitted values (Missing)
  geom_line(data = pred_mun_prev_mod4.miss %>%
              filter(!is.na(fitted)) %>%
              group_by(diag_qrt) %>%
              summarize(fitted = sum(fitted)/n()),
            aes(diag_qrt, fitted*100, color = "Previous", linetype = "2015-2019 (excluding quarter 12)")) +
  
  
 
  
  
  # 4. Format figure: 
  ## Create secondary axis for percent tested 
  scale_y_continuous(limits = c(0, 45), 
                     sec.axis = sec_axis(~ .,
                                         breaks = seq(0, 100, by = 10),  # Specify breaks for the secondary axis
                                         name = "Percent of total TB cases with confirmed Xpert result", # Format labels as percentages
                                         labels = scales::label_percent(scale = 1)
                     )
  ) + 
  xlab("Quarter") + 
  ylab("Percent RR-TB positive") + 
  ggtitle("Trends in Xpert testing and RR-TB cases overtime") + 
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
  labs(size = "Number of cases with conclusive Xpert result",
       color = "Case Type") +
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Case Type",
                     labels=c("New",
                              "Previous"),
                     values=c("black", "red")) + 
  scale_linetype_manual(name = "Model", 
                        labels = c("2014-2019", 
                                   "2015-2019 (excluding quarter 12)",
                                   "2017-2019"
                                   ),
                        values=c(1, 2, 3))

ggsave(figA_national_all, filename = "manuscript/figA_national_all.png", width = 14, height = 6)





# Figure 1. Panel 2 -------------------------------------------------------
# load("output/projected_nat_new.Rdata")
load("output/projected_nat_prev.Rdata")
obs_new <- mdf_mun_new_grp %>%
  group_by(diag_qrt) %>%
  summarize(obs_RR = sum(Positive))

obs_prev <- mdf_prev_ind %>%
  group_by(diag_qrt) %>%
  summarize(obs_RR = sum(result == 1, na.rm = TRUE))
            
            


## Combined nat new and fitted values 
bias_new_mod4 <- merge(pred_mun_new_mod4, obs_new, by = "diag_qrt") %>% 
  filter(!is.na(fitted)) %>%
  group_by(diag_qrt, time) %>%
  summarize(proj_RR = sum(fitted*cases),
            obs_RR = obs_RR) %>% 
  unique() %>% 
  merge(., boot.mun_new_4[["pred.nat_qrt"]], by = "time")

bias_new_mod4.miss <- merge(pred_mun_new_mod4.miss, obs_new, by = "diag_qrt") %>% 
  filter(!is.na(fitted)) %>%
  group_by(diag_qrt, time) %>%
  summarize(proj_RR = sum(fitted*cases),
            obs_RR = obs_RR) %>% 
  unique() %>% 
  merge(., boot.mun_new_4_miss[["pred.nat_qrt"]], by = "time")



bias_prev <- merge(pred_mun_prev_mod4, obs_prev, by = "diag_qrt") %>% 
  filter(!is.na(fitted)) %>%
  group_by(diag_qrt, time) %>%
  summarize(proj_RR = sum(fitted),
            obs_RR = obs_RR) %>% 
  unique() %>% 
  merge(., boot.mun_prev_4[["pred.nat_qrt"]], by = "time")

bias_prev.miss <- merge(pred_mun_prev_mod4.miss, obs_prev, by = "diag_qrt") %>% 
  filter(!is.na(fitted)) %>%
  group_by(diag_qrt, time) %>%
  summarize(proj_RR = sum(fitted*cases),
            obs_RR = obs_RR) %>% 
  unique() %>% 
  merge(., boot.mun_prev_4.miss[["pred.nat_qrt"]], by = "time")


## Plot bias comparisons for each model and type - Observed RR-TB case count / Modeled RR-TB case count (Model 4)
figB_bias <- ggplot() + 
  # New cases 
  ## Point estimate - fitted results (Model 4)
  geom_line(
    data = bias_new,
    aes(diag_qrt, obs_RR/proj_RR, color = "New (CC)")) + 
  
  ## Error bar - Bootstrapped
  geom_errorbar(data = bias_new, 
                aes(x = diag_qrt, ymin = obs_RR/proj_lci, ymax = obs_RR/proj_hci, color = "New (CC)")) + 
  
  # New cases 
  ## Point estimate - fitted results (Model 4)
  geom_line(
    data = bias_new.miss,
    aes(diag_qrt, obs_RR/proj_RR, color = "New (Miss)")) + 
  
  ## Error bar - Bootstrapped
  geom_errorbar(data = bias_new.miss, 
                aes(x = diag_qrt, ymin = obs_RR/proj_lci, ymax = obs_RR/proj_hci, color = "New (Miss)")) + 
  
  
  
  
  # Previous cases
  ## Point estimate - fitted results (Model 4)
  geom_line(
    data = bias_prev,
    aes(diag_qrt, obs_RR/proj_RR, color = "Prev (CC)")) + 
  
  ## Error bar - Bootstrapped
  geom_errorbar(data = bias_prev, 
                aes(x = diag_qrt, ymin = obs_RR/proj_lci, ymax = obs_RR/proj_hci, color = "Prev (CC)")) + 
  
  # New cases 
  ## Point estimate - fitted results (Model 4)
  geom_line(
    data = bias_prev.miss,
    aes(diag_qrt, obs_RR/proj_RR, color = "Prev (Miss)")) + 
  
  ## Error bar - Bootstrapped
  geom_errorbar(data = bias_prev.miss, 
                aes(x = diag_qrt, ymin = obs_RR/proj_lci, ymax = obs_RR/proj_hci, color = "Prev (Miss)")) + 

    


  
  
  
  
  
  geom_line(data = nat_prev %>% filter(variable == "RR") %>% group_by(diag_qrt, model), aes(x = diag_qrt, y = obs_RR_cases/value, group = model, color = "Previous", linetype = model)) + 
  xlab("Quarter") + 
  ylab("Ratio of observed to projected RR-TB cases") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 14)) +
  scale_color_manual(name="Case Type",
                     labels=c("New",
                              "Previous"),
                     values=c("black", "red")) + 
  scale_linetype_manual(name = "Model", 
                        labels = c("2014-2019", 
                                   "2016-2019", 
                                   "2017-2019"),
                        values=c(1, 2, 3))
  
ggsave(figB_bias, filename = "manuscript/figB_bias.png", width = 14, height = 6)
  
  
  
  
  



# Figure 2. Model implications --------------------------------------------

## For 2017-2019; and 2015-2019
dst_new <- sinan_xpert %>% 
  filter(tratamento == 1) %>% 
  group_by(tratamento, diag_qrt) %>% 
  summarize(dst_count = sum(test_sensi %in% c(1, 2, 3, 4, 5))) # R only or I and R


# Area figure: 
figC_implications <- ggplot() + 
  # 1. Total number of cases
  # 1.1 Total number of cases with conclusive Xpert result
  geom_area(
    data = mdf_mun_new_grp %>%
      filter(time !=12 & time >= 5) %>%
      group_by(diag_qrt) %>%
      summarize(num_tested = sum(Positive + Negative)),
    aes(x = diag_qrt, y = num_tested, fill = "Cases with conclusive Xpert result"), alpha = 0.5) +
  
  ## 1.2 Total number of cases with conclusive DST result
  geom_area(
    data = dst_new %>%
      filter(diag_qrt != "2016-10-01" & diag_qrt >= "2015-01-01") %>%
      group_by(diag_qrt) %>%
      summarize(num_tested = dst_count),
    aes(x = diag_qrt, y = num_tested, fill = "Cases with conclusive DST result"), alpha = 0.5) +
  
  ## 1.3 Total # of modeled rif cases under CDR
  geom_area(
    data = pred_mun_new_mod4 %>% 
      filter(!is.na(fitted)) %>%
      group_by(diag_qrt) %>%
      ## add CDR for each year - Inflate observed cases by CDR, then calculate
      ## projected incidence
      ## Source: https://data.worldbank.org/indicator/SH.TBS.DTEC.ZS?locations=BR
      mutate(cdr = if_else(grepl(paste0("^", 2018, "-"), diag_qrt), 0.89, 
                           if_else(grepl(paste0("^", 2019, "-"), diag_qrt), 0.89, 0.87)), 
             cdr_cases = cases/cdr) %>% 
      summarize(fitted_cdr = sum(fitted*cdr_cases)), 
    aes(x = diag_qrt, y = fitted_cdr, fill = "Projected RR-TB cases in population"), alpha = 0.7) + 
  
  
  
  ## 1.3 Total # of modeled rif cases
  geom_area(
    data = pred_mun_new_mod4 %>% 
      filter(!is.na(fitted)) %>%
      group_by(diag_qrt) %>%
      summarize(fitted = sum(fitted*cases)), 
    aes(x = diag_qrt, y = fitted, fill = "Projected RR-TB cases seeking care"), alpha = 0.7) + 
  
  
  
  # 2. Incidence
  ## 2.1 Modeled incidence 
  geom_line(
    data = pred_mun_new_mod4 %>% 
      filter(!is.na(fitted)) %>%
      group_by(diag_qrt) %>%
      summarize(total_cases = sum(cases), 
                proj_incidence = (sum(fitted*cases)/total_cases)*100000), 
    aes(x = diag_qrt, y = proj_incidence, color = "Projected RR-TB incidence")) + 
  
  
  ## Create secondary axis for incidence
  scale_y_continuous(
    sec.axis = sec_axis(~ ./100,
                        name = "RR-TB incidence")
  ) + 
  
  xlab("Quarter") + 
  ylab("Case count") + 
  theme_bw() + 
  theme(
    axis.text.x  = element_text(size = 12), 
    axis.text.y  = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    title = element_text(size = 14)) +
  scale_fill_manual(name = "", 
                    labels = c("Cases with conclusive Xpert result",
                               "Cases with conclusive DST result",
                               "Projected RR-TB cases in population", 
                               "Projected RR-TB cases seeking care"), 
                    values = viridis(4)) +
  scale_color_manual(name="",
                     labels="Projected RR-TB incidence (per 1,000 TB cases)",
                     values="black")


ggsave(figC_implications, filename = "manuscript/figC_implications.png", width = 14, height = 6)
















############################################################################
############################################################################
###                                                                      ###
###           TAB 1 - NATIONAL TRENDS IN TESTING AND INCIDENCE           ###
###                                                                      ###
############################################################################
############################################################################
# tabA_nat_all <- nat_new.yr %>% 
#   # filter(model == "mod1") %>% 
#   group_by(diag_yr, model) %>% 
#   ## Note: Incidence is RR-TB cases per 1,000 TB cases (but perhaps denominator should be cases with complete covariates since that is who we are projecting back to)
#   summarize(pct_tested = (obs_tested/obs_TB_cases)*100, 
#             obs_incidence = (obs_RR_cases/obs_TB_cases)*1000, 
#             proj_incidence = (mod_RR_cases/obs_TB_complete_cases)*1000, 
#             incidence_lci = (mod_RR_lci/obs_TB_complete_cases)*1000,
#             incidence_hci = (mod_RR_hci/obs_TB_complete_cases)*1000) %>% 
#   ungroup() %>% 
#   group_by(model) %>% 
#   mutate(avg_pct_tested = mean(pct_tested[diag_yr %in% c("2017", "2018", "2019")]), 
#          avg_obs_inc = mean(obs_incidence[diag_yr %in% c("2017", "2018", "2019")]), 
#          avg_proj_inc = mean(proj_incidence[diag_yr %in% c("2017", "2018", "2019")])
#     ) %>% 
#   mutate(across(-model, ~ round(., digits = 2))) %>% 
#   flextable()
# 
# 
# tabB_nat_all <- nat_prev.yr %>% 
#   # filter(model == "mod1") %>% 
#   group_by(diag_yr, model) %>% 
#   ## Note: Incidence is RR-TB cases per 1,000 TB cases (but perhaps denominator should be cases with complete covariates since that is who we are projecting back to)
#   summarize(pct_tested = (obs_tested/obs_TB_cases)*100,
#             obs_incidence = (obs_RR_cases/obs_TB_cases)*1000, 
#             proj_incidence = (mod_RR_cases/obs_TB_complete_cases)*1000,
#             proj_incidence_lci = (mod_RR_lci/obs_TB_complete_cases)*1000,
#             proj_incidence_hci = (mod_RR_hci/obs_TB_complete_cases)*1000,
#             obs_RR_cases = obs_RR_cases, 
#             mod_RR_cases = mod_RR_cases) %>% 
#   ungroup() %>% 
#   group_by(model) %>% 
#   mutate(avg_pct_tested = mean(pct_tested[diag_yr %in% c("2017", "2018", "2019")]), 
#          avg_obs_cases = mean(obs_RR_cases[diag_yr %in% c("2017", "2018", "2019")]),
#          avg_mod_cases = mean(mod_RR_cases[diag_yr %in% c("2017", "2018", "2019")]),
#          avg_obs_inc = mean(obs_incidence[diag_yr %in% c("2017", "2018", "2019")]), 
#          avg_proj_inc = mean(proj_incidence[diag_yr %in% c("2017", "2018", "2019")])
#   ) %>% 
#   mutate(across(-model, ~ round(., digits = 2))) %>% 
#   flextable()











  
  
  
  




###########################################################################
###########################################################################
###                                                                     ###
###                    FIG 1 - NATIONAL-LEVEL TRENDS                    ###
###                                                                     ###
###########################################################################
###########################################################################

##################################################################
##                      Fig 1A - New cases                      ##
##################################################################


# A) New Cases
figA_nat_new <- ggplot() +
  
  # 1) Calculate annual observed incidence (among those who received Xpert)
  geom_point(
    data = mdf_mun_new_grp %>%
      group_by(diag_qrt) %>%
      summarize(num_tested = sum(Positive + Negative),
                
                ## Note: Incidence denominator is cases with confirmed test result, rather than all cases tested with xpert or all TB cases 
                incidence = (sum(Positive)/sum(Positive + Negative))*100
      ), 
    aes(x = diag_qrt, y = incidence, size = num_tested), alpha = 0.7) + 
  
  # 2) Calculate annual projected incidence from each model 
  geom_line(
    data = nat_new %>% 
      
      ## Note: Incidence is RR-TB cases per 1,000 TB cases (but perhaps denominator should be cases with complete covariates since that is who we are projecting back to)
      mutate(incidence = (mod_RR_cases/obs_TB_complete_cases)*100),  
    aes(x = diag_qrt, y = incidence, color = model)
  ) + 
  ggtitle("New Cases") +
  ylab("RR-TB incidence per 100 incident TB cases")  + 
  xlab("Quarter") + 
  
  # 3) Add second axis for percent of case
  geom_line(
    data = mdf_mun_new_grp %>%
      group_by(diag_qrt) %>%
      
      ## Calculate percent tested as share all all cases with a confirmed positive + negative test result 
      summarize(pct_tested = sum(Positive + Negative)/sum(Positive + Negative + Miss)),
    aes(x = diag_qrt, y = pct_tested*100, linetype = "Xpert coverage"), alpha = 0.5) + 
  
  # 4) Add secondary axis for number of TB cases with confirmed Xpert result 
  scale_y_continuous(limits = c(0, 40), 
                     sec.axis = sec_axis(~ .,
                                         breaks = seq(0, 100, by = 10),  # Specify breaks for the secondary axis
                                         name = "Percent of total TB cases with confirmed Xpert result", # Format labels as percentages
                                         labels = scales::label_percent(scale = 1)
                     )
  ) + 
  
  # 5) Edit plot 
  theme(axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 14)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Observed cases with conclusive Xpert result") +
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Model",
                     labels=c("Model 1 (2014-2019)",
                              "Model 2 (2016-2019)", 
                              "Model 3 (2017-2019)"
                     ),
                     values=c("black", "red", "blue")
  ) + 
  scale_linetype_manual(name = "", 
                        values=c(2)) + 
  theme_bw()



ggsave(filename = "manuscript/figA_nat_new.png", width = 14, height = 6)


#################################################################
##                     Fig 1B - Prev cases                     ##
#################################################################
figB_nat_prev <- ggplot() +
  
  # 1) Calculate annual observed incidence (among those who received Xpert)
  geom_point(
    data = mdf_prev_ind %>%
      group_by(time) %>%
      summarize(num_tested = sum(!is.na(result)),
                positive = sum(result == "1", na.rm = TRUE), 
                obs_pct_pos = if_else(is.na(sum(result == "1", na.rm = TRUE)/num_tested), 0, sum(result == "1", na.rm = TRUE)/num_tested), 
                cases = n(), 
                
                ## Note: Defined incidence as # RR-TB positive/ # previous cases 
                incidence = (positive/cases)*100),
    
    aes(x = time, y = incidence, size = num_tested), alpha = 0.7) + 
  
  # 2) Calculate annual projected incidence from each model 
  geom_line(
    data = nat_prev %>% 
      
      ## Note: Incidence is RR-TB cases per 1,000 TB cases (but perhaps denominator should be cases with complete covariates since that is who we are projecting back to)
      mutate(incidence = (mod_RR_cases/obs_TB_complete_cases)*100),  
    aes(x = time, y = incidence, color = model)
  ) + 
  ggtitle("Previous Cases") + 
  ylab("RR-TB incidence per 100 incident TB cases")  + 
  xlab("Quarter") + 
  
  # 3) Add second axis for percent of case
  geom_line(
    data = mdf_prev_ind %>%
      group_by(time) %>%
      
      ## Calculate percent tested as share all all cases with a confirmed positive + negative test result 
      summarize(pct_tested = sum(!is.na(result))/n()),
    aes(x = time, y = pct_tested*100, linetype = "Xpert coverage"), alpha = 0.5) + 
  
  # 4) Add secondary axis for number of TB cases with confirmed Xpert result 
  scale_y_continuous(limits = c(0, 40), 
                     sec.axis = sec_axis(~ .,
                                         breaks = seq(0, 100, by = 10),  # Specify breaks for the secondary axis
                                         name = "Percent of total TB cases with confirmed Xpert result", # Format labels as percentages
                                         labels = scales::label_percent(scale = 1)
                     )
  ) + 
  
  # 5) Edit plot 
  theme(axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 14)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Observed cases with conclusive Xpert result") +
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Model",
                     labels=c("Model 1 (2014-2019)",
                              "Model 2 (2016-2019)", 
                              "Model 3 (2017-2019)"
                     ),
                     values=c("black", "red", "blue")
  ) + 
  scale_linetype_manual(name = "", 
                        values=c(2)) + 
  theme_bw()

ggsave(filename = "manuscript/figB_nat_prev.png", width = 14, height = 6)




###########################################################################
###########################################################################
###                                                                     ###
###                  TAB 1 - STATE-LEVEL ANNUAL TRENDS                  ###
###                                                                     ###
###########################################################################
###########################################################################


#################################################################
##        Tab 1A - State-level annual trends, new cases        ##
#################################################################

## Create table with total RR-TB cases among new cases between 2017-2019
tabA_nat_new.mod3 <- nat_new.yr %>% 
  filter(model == "mod3") %>%
  group_by(diag_yr) %>% 
  mutate(obs_incidence = (obs_RR_cases/obs_TB_cases) * 1000,
         mod_incidence = (mod_RR_cases/obs_TB_complete_cases) * 1000) %>%
  ungroup() %>% 
  summarize(  
    # Average pct tested
    obs_pct_tested = (obs_tested[diag_yr == "2019"]/obs_TB_cases[diag_yr == "2019"])*100, 
    # 2019 incidence - Observed
    obs_inc_2019 = obs_incidence[diag_yr == "2019"], 
    # 2019 incidence - Projected
    mod_inc_2019 = mod_incidence[diag_yr == "2019"], 
    # 2017 incidence - Projected
    mod_inc_2017 = mod_incidence[diag_yr == "2017"], 
    # 2019 incidence - LCI
    mod_RR_lci = (mod_RR_lci[diag_yr == "2019"]/obs_TB_complete_cases[diag_yr == "2019"])*1000,
    # 2019 incidence - HCI
    mod_RR_hci = (mod_RR_hci[diag_yr == "2019"]/obs_TB_complete_cases[diag_yr == "2019"])*1000,
    # 2019 - Difference in projected vs. observed (absolute)
    diff_incidence = mod_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"], 
    # 2019 - Difference in projected vs. observed (%)
    diff_incidence_pct = (mod_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"])/obs_incidence[diag_yr == "2019"], 
    # 2019-2017 percent change - observed
    # obs_pct_chg = ((obs_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"]/obs_incidence[diag_yr == "2019"]) * 100)), 
    # 2019-2017 percent change - projected
    mod_pct_chg = ((mod_incidence[diag_yr == "2019"] - mod_incidence[diag_yr == "2017"])/mod_incidence[diag_yr == "2017"]) * 100, 
    avg_obs_inc = mean(obs_incidence[diag_yr %in% c("2017", "2018", "2019")]), 
    avg_proj_inc = mean(mod_incidence[diag_yr %in% c("2017", "2018", "2019")])
  )  %>% 
  round(., digits = 2) %>% 
  flextable()


nat_new.yr %>%
  filter(diag_yr %in% c("2017", "2018", "2019")) %>%
  group_by(diag_yr, model) %>%
  mutate(obs_incidence = sum(obs_RR_cases)/sum(obs_TB_cases) * 1000,
         mod_incidence = sum(mod_RR_cases)/sum(obs_TB_cases) * 1000) %>%
  group_by(model) %>%
  summarize(avg_obs_incidence = mean(obs_incidence),
            avg_obs_cases = mean(obs_RR_cases),
            avg_mod_incidence = mean(mod_incidence),
            avg_mod_cases = mean(mod_RR_cases),
            obs_pct_change = (obs_incidence[diag_yr == "2019"]-obs_incidence[diag_yr == "2017"])/obs_incidence[diag_yr == "2017"],
            mod_pct_change = (mod_incidence[diag_yr == "2019"]-mod_incidence[diag_yr == "2017"])/mod_incidence[diag_yr == "2017"])









## Make state-level table with 2019 incidence and testing, and change between 2017-2019
tabA_state_new.mod3 <- state_new.yr %>% 
  group_by(state_nm, diag_yr) %>% 
  filter(model == "mod3") %>% 
  mutate(obs_incidence = (obs_RR_cases/obs_TB_cases) * 1000, 
         mod_incidence = (mod_RR_cases/obs_TB_complete_cases) * 1000, 
         pct_tested = obs_tested/obs_TB_cases) %>% 
  # arrange(state_nm, diag_yr) %>% 
  ungroup() %>% 
  group_by(state_nm) %>% 
  summarize(
    # Average pct tested
    obs_pct_tested = mean(pct_tested[diag_yr == "2019"])*100, 
    # 2019 incidence - Observed
    obs_inc_2019 = obs_incidence[diag_yr == "2019"], 
    # 2019 incidence - Projected
    mod_inc_2019 = mod_incidence[diag_yr == "2019"], 
    # 2017 incidence - Projected
    mod_inc_2017 = mod_incidence[diag_yr == "2017"], 
    # 2019 incidence - LCI
    mod_RR_lci = (mod_RR_lci[diag_yr == "2019"]/obs_TB_complete_cases[diag_yr == "2019"])*1000,
    # 2019 incidence - HCI
    mod_RR_hci = (mod_RR_hci[diag_yr == "2019"]/obs_TB_complete_cases[diag_yr == "2019"])*1000,
    # 2019 - Difference in projected vs. observed (absolute)
    diff_incidence = mod_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"], 
    # 2019 - Difference in projected vs. observed (%)
    diff_incidence_pct = ((mod_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"])/obs_incidence[diag_yr == "2019"])*100, 
    # 2019-2017 percent change - observed
    # obs_pct_chg = ((obs_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"]/obs_incidence[diag_yr == "2019"]) * 100)), 
    # 2019-2017 percent change - projected
    mod_pct_chg = ((mod_incidence[diag_yr == "2019"] - mod_incidence[diag_yr == "2017"])/mod_incidence[diag_yr == "2017"]) * 100
  ) %>% 
  arrange(-obs_pct_tested) %>%   
  # round every column, except state name
  mutate(across(-state_nm, ~ round(., digits = 2))) %>% 
  flextable()



#################################################################
##        Tab 1 - State-level annual trends, prev cases        ##
#################################################################

# Get national estimates
tabB_nat_prev.mod3 <- nat_prev.yr %>% 
  filter(model == "mod3") %>%
  group_by(diag_yr) %>% 
  mutate(obs_incidence = (obs_RR_cases/obs_TB_cases) * 1000,
         mod_incidence = (mod_RR_cases/obs_TB_complete_cases) * 1000) %>%
  ungroup() %>% 
  summarize(  
    # Average pct tested
    obs_pct_tested = (obs_tested[diag_yr == "2019"]/obs_TB_cases[diag_yr == "2019"])*100, 
    # 2019 incidence - Observed
    obs_inc_2019 = obs_incidence[diag_yr == "2019"], 
    # 2019 incidence - Projected
    mod_inc_2019 = mod_incidence[diag_yr == "2019"], 
    # 2017 incidence - Projected
    mod_inc_2017 = mod_incidence[diag_yr == "2017"], 
    # 2019 incidence - LCI
    mod_RR_lci = (mod_RR_lci[diag_yr == "2019"]/obs_TB_complete_cases[diag_yr == "2019"])*1000,
    # 2019 incidence - HCI
    mod_RR_hci = (mod_RR_hci[diag_yr == "2019"]/obs_TB_complete_cases[diag_yr == "2019"])*1000,
    # 2019 - Difference in projected vs. observed
    diff_incidence = mod_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"], 
    # 2019-2017 percent change - observed
    # obs_pct_chg = ((obs_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"]/obs_incidence[diag_yr == "2019"]) * 100)), 
    # 2019-2017 percent change - projected
    mod_pct_chg = ((mod_incidence[diag_yr == "2019"] - mod_incidence[diag_yr == "2017"])/mod_incidence[diag_yr == "2017"]) * 100
  )  %>% 
  round(digits = 2) %>% 
  flextable()



## Make state-level table with 2019 incidence and testing, and change between 2017-2019
tabB_state_prev.mod3 <- state_prev.yr %>% 
  group_by(state_nm, diag_yr) %>% 
  filter(model == "mod3") %>% 
  mutate(obs_incidence = (obs_RR_cases/obs_TB_cases) * 1000, 
         mod_incidence = (mod_RR_cases/obs_TB_complete_cases) * 1000, 
         pct_tested = obs_tested/obs_TB_cases) %>% 
  # arrange(state_nm, diag_yr) %>% 
  ungroup() %>% 
  group_by(state_nm) %>% 
  summarize(
    # Average pct tested
    obs_pct_tested = mean(pct_tested[diag_yr == "2019"])*100, 
    # 2019 incidence - Observed
    obs_inc_2019 = obs_incidence[diag_yr == "2019"], 
    # 2019 incidence - Projected
    mod_inc_2019 = mod_incidence[diag_yr == "2019"], 
    # 2017 incidence - Projected
    mod_inc_2017 = mod_incidence[diag_yr == "2017"], 
    # 2019 incidence - LCI
    mod_RR_lci = (mod_RR_lci[diag_yr == "2019"]/obs_TB_complete_cases[diag_yr == "2019"])*1000,
    # 2019 incidence - HCI
    mod_RR_hci = (mod_RR_hci[diag_yr == "2019"]/obs_TB_complete_cases[diag_yr == "2019"])*1000,
    # 2019 - Difference in projected vs. observed
    diff_incidence = mod_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"], 
    # 2019-2017 percent change - observed
    # obs_pct_chg = ((obs_incidence[diag_yr == "2019"] - obs_incidence[diag_yr == "2019"]/obs_incidence[diag_yr == "2019"]) * 100)), 
    # 2019-2017 percent change - projected
    mod_pct_chg = ((mod_incidence[diag_yr == "2019"] - mod_incidence[diag_yr == "2017"])/mod_incidence[diag_yr == "2017"]) * 100
  ) %>% 
  arrange(-obs_pct_tested) %>%   
  # round every column, except state name
  mutate(across(-state_nm, ~ round(., digits = 2))) %>% 
  flextable()








############################################################################
############################################################################
###                                                                      ###
###                      FIG 2 - STATE-LEVEL TRENDS                      ###
###                                                                      ###
############################################################################
############################################################################


#################################################################
##        Fig 2A - State-level annual trends, new cases        ##
#################################################################

# Plot state-level trends in new cases
figA_state_new <- ggplot() + 
  # 1) Calculate pct tested 
  geom_line(data = state_new %>%  
              filter(model == "mod1" & diag_qrt >= "2017-01-01") %>% 
              mutate(pct_tested = (obs_tested/obs_TB_cases) * 100), 
            aes(x = diag_qrt, y = pct_tested, group = state_nm, linetype = "Percent Tested"), alpha = 0.6) + 
  
  
  # 3) Add line for projected incidence for each model (2017-2019)
  geom_line(data = state_new %>%  
              filter(diag_qrt >= "2017-01-01") %>% 
              group_by(model, diag_qrt, state_nm) %>% 
              mutate(mod_incidence = sum(mod_RR_cases)/sum(obs_TB_complete_cases) * 1000), 
            aes(x = diag_qrt, y = mod_incidence, color = model)) + 
  
  # 2) Add points for observed incidence
  geom_point(data = state_new %>%  
               filter(model == "mod1" & diag_qrt >= "2017-01-01") %>% 
               group_by(diag_qrt, state_nm) %>% 
               mutate(obs_incidence = (obs_RR_cases/obs_TB_cases) * 1000), 
             aes(x = diag_qrt, y = obs_incidence, group = state_nm, size = obs_TB_cases), alpha=0.7) + 
  
  # 4) Edit plot 
  facet_wrap(~state_nm, scales = "free") + 
  xlab("Quarter") + 
  ylab("RR-TB incidence per 100 incident TB cases") + 
  ggtitle("New cases") + 
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
  scale_fill_manual(label = "Observed", 
                    values = "black") + 
  scale_color_manual(name = "", 
                     labels = c("Model 1 (2014-2019)", 
                                "Model 2 (2016-2019)", 
                                "Model 3 (2017-2019)"),
                     values=c("blue","green", "red")) + 
  scale_linetype_manual(name = "", 
                        values=c(2))



ggsave(filename = "manuscript/figA_state_new.png", width = 18, height = 10)


##################################################################
##        Fig 2B - State-level annual trends, prev cases        ##
##################################################################

# Plot state-level trends in previous cases
figB_state_prev <- ggplot() + 
  # 1) Calculate pct tested 
  geom_line(data = state_prev %>%  
              filter(model == "mod1" & time > 12) %>% 
              mutate(pct_tested = (obs_tested/obs_TB_cases) * 100), 
            aes(x = time, y = pct_tested, group = state_nm, linetype = "Percent Tested"), alpha = 0.6) + 
  
  
  # 3) Add line for projected incidence for each model (2017-2019)
  geom_line(data = state_prev %>%  
              filter(time > 12) %>% 
              group_by(model, time, state_nm) %>% 
              mutate(mod_incidence = sum(mod_RR_cases)/sum(obs_TB_complete_cases) * 1000), 
            aes(x = time, y = mod_incidence, color = model)) + 
  
  # 2) Add line for observed incidence
  geom_line(data = state_prev %>%  
              filter(model == "mod1" & time > 12) %>% 
              group_by(time, state_nm) %>% 
              mutate(obs_incidence = (obs_RR_cases/obs_TB_cases) * 1000), 
            aes(x = time, y = obs_incidence, group = state_nm, color = "Observed")) + 
  
  # 4) Edit plot 
  facet_wrap(~state_nm, scales = "free") + 
  xlab("Quarter") + 
  ylab("RR-TB incidence per 100 incident TB cases") + 
  ggtitle("Previous cases") + 
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
  scale_color_manual(name = "", 
                     labels = c("Observed", 
                                "Model 1 (2014-2019)", 
                                "Model 2 (2016-2019)", 
                                "Model 3 (2017-2019)"),
                     values=c("black", "blue","green", "red")) + 
  scale_linetype_manual(name = "", 
                        values=c(2))


ggsave(filename = "manuscript/figB_state_prev.png", width = 18, height = 10)










##################################################################
##                         Save to .doc                         ##
##################################################################
result_output <- result_output %>% 
  body_add_par(value = "Table 1. National - all (2014-2019)", style = "heading 1") %>% 
  body_add_flextable(value = tabA_nat_all) %>% 
  body_add_flextable(value = tabB_nat_all) %>% 
  body_add_par(value = "Table 2A. National - New (2017-2019)", style = "heading 1") %>% 
  body_add_flextable(value = tabA_nat_new.mod3) %>% 
  body_add_par(value = "Table 2A. State-level - New (2017-2019)", style = "heading 1") %>% 
  body_add_flextable(value = tabA_state_new.mod3) %>% 
  # body_add_par(value = "Figure 1A. National - New (2017-2019)", style = "heading 1") %>% 
  # body_add_gg(value = fig1A_nat) %>% 
  # body_add_par(value = "Figure 1B. National - Previous (2017-2019)", style = "heading 1") %>% 
  # body_add_gg(value = fig1B_nat) %>% 
  body_add_par(value = "Table 1B. National - Previous (2017-2019)", style = "heading 1") %>% 
  body_add_flextable(value = tabB_nat_prev.mod3) %>% 
  body_add_par(value = "Table 1B. State-level - Previous (2017-2019)", style = "heading 1") %>% 
  body_add_flextable(value = tabB_state_prev.mod3) 
# %>%
# body_add_par(value = "Figure 2A. State-level - New (2017-2019)", style = "heading 1") %>% 
# body_add_gg(value = fig2A_state_new) %>% 
# body_add_par(value = "Figure 2B. State-level - Previous (2017-2019)", style = "heading 1") %>% 
# body_add_gg(value = fig2B_state_prev)


# output to file
result_output_file <- print(result_output, target = here::here("manuscript/result_output.docx"))










###########################################################################
###########################################################################
###                                                                     ###
###                            RESULT - TEXT                            ###
###                                                                     ###
###########################################################################
###########################################################################

## Pull coefficient tables 
mun_prev_mod1.xpert %>% 
  tidy() %>% 
  kable(digits = 2)




# Calculate N and percent of cases by diagnosis type
load("data/sinan_xpert.Rdata")

tabyl(sinan_xpert, tratamento)


# Calculate N among complete cases
## new 
mdf_new_mun_grp %>% 
  filter_at(vars(state, time, sex, hiv_status, age_cat, health_unit, mun_urban_cat, mun_fhs_cat, mun_has_prison, mun_bf_cat), 
            all_vars(!is.na(.))) %>% 
  ungroup() %>% 
  summarize(cases = sum(Positive + Negative))




# trends in who is getting tested 
## by sex
mdf_new_ind %>% 
  group_by(diag_yr, sex) %>% 
  mutate(pct_tested = sum(tested)/n()) %>% 
  ungroup() %>% 
  group_by(sex) %>% 
  summarize(mean_pct_tested = mean(pct_tested))

mdf_prev_ind %>% 
  group_by(diag_yr, sex) %>% 
  mutate(pct_tested = sum(tested)/n()) %>% 
  ungroup() %>% 
  group_by(sex) %>% 
  summarize(mean_pct_tested = mean(pct_tested))

## by age
mdf_new_ind %>% 
  group_by(diag_yr, age_cat) %>% 
  mutate(pct_tested = sum(tested)/n()) %>% 
  ungroup() %>% 
  group_by(age_cat) %>% 
  summarize(mean_pct_tested = mean(pct_tested))

mdf_prev_ind %>% 
  group_by(diag_yr, age_cat) %>% 
  mutate(pct_tested = sum(tested)/n()) %>% 
  ungroup() %>% 
  group_by(age_cat) %>% 
  summarize(mean_pct_tested = mean(pct_tested))


## by HIV status
mdf_new_ind %>% 
  group_by(diag_yr, hiv_status) %>% 
  summarize(pct_tested = sum(tested)/n()) %>% 
  ungroup() %>% 
  group_by(hiv_status) %>% 
  summarize(mean_pct_tested = mean(pct_tested))

mdf_prev_ind %>% 
  group_by(diag_yr, hiv_status) %>% 
  summarize(pct_tested = sum(tested)/n()) %>% 
  ungroup() %>% 
  group_by(hiv_status) %>% 
  summarize(mean_pct_tested = mean(pct_tested))


# Calculate trends in testing by state 
state_new.yr %>%
  filter(model == "mod1") %>%
  group_by(diag_yr) %>%
  summarize(avg_pct_test = mean(obs_tested/obs_TB_cases))



# Calculate median annual percent change in testing by state
print(state_new.yr %>%
        filter(model == "mod1") %>%
        group_by(state_nm) %>%
        mutate(pct_tested = obs_tested/obs_TB_cases) %>%
        arrange(state_nm, diag_yr) %>%
        mutate(pct_chg_tested = (pct_tested - lag(pct_tested))/lag(pct_tested) * 100) %>%
        summarize(avg_pct_chg = median((pct_tested - lag(pct_tested))/lag(pct_tested) * 100, na.rm = TRUE)) %>%
        arrange(avg_pct_chg), n=50)








############################################################################
############################################################################
###                                                                      ###
###                      APPENDIX: MODEL COMPARISON                      ###
###                                                                      ###
############################################################################
############################################################################




##################################################################
##                     New Cases - National                     ##
##################################################################

# Trends in new cases across models 
nat_new.yr %>%
  filter(diag_yr %in% c("2017", "2018", "2019")) %>%
  group_by(diag_yr, model) %>%
  mutate(obs_incidence = sum(obs_RR_cases)/sum(obs_TB_cases) * 1000,
         mod_incidence = sum(mod_RR_cases)/sum(obs_TB_cases) * 1000) %>%
  group_by(model) %>%
  summarize(avg_obs_incidence = mean(obs_incidence),
            avg_obs_cases = mean(obs_RR_cases),
            avg_mod_incidence = mean(mod_incidence),
            avg_mod_cases = mean(mod_RR_cases),
            obs_pct_change = (obs_incidence[diag_yr == "2019"]-obs_incidence[diag_yr == "2017"])/obs_incidence[diag_yr == "2017"],
            mod_pct_change = (mod_incidence[diag_yr == "2019"]-mod_incidence[diag_yr == "2017"])/mod_incidence[diag_yr == "2017"])







































#################################################################
##                    Prev Cases - National                    ##
#################################################################
nat_prev.yr %>% 
  filter(model == "mod3") %>%
  group_by(diag_yr) %>% 
  mutate(obs_incidence = sum(obs_RR_cases)/sum(obs_TB_cases) * 1000,
         mod_incidence = sum(mod_RR_cases)/sum(obs_TB_cases) * 1000, 
         bias = mod_RR_cases - obs_RR_cases) %>% 
  ungroup() %>% 
  summarize(avg_obs_incidence = mean(obs_incidence), 
            avg_obs_cases = mean(obs_RR_cases), 
            avg_mod_incidence = mean(mod_incidence), 
            avg_mod_cases = mean(mod_RR_cases), 
            obs_pct_change = (obs_incidence[diag_yr == "2019"]-obs_incidence[diag_yr == "2017"])/obs_incidence[diag_yr == "2017"], 
            mod_pct_change = (mod_incidence[diag_yr == "2019"]-mod_incidence[diag_yr == "2017"])/mod_incidence[diag_yr == "2017"])


# compare trends to other models
nat_prev.yr %>% 
  filter(diag_yr %in% c("2017", "2018", "2019")) %>% 
  group_by(diag_yr, model) %>% 
  mutate(obs_incidence = sum(obs_RR_cases)/sum(obs_TB_cases) * 1000,
         mod_incidence = sum(mod_RR_cases)/sum(obs_TB_cases) * 1000) %>% 
  group_by(model) %>% 
  summarize(avg_obs_incidence = mean(obs_incidence), 
            avg_obs_cases = mean(obs_RR_cases), 
            avg_mod_incidence = mean(mod_incidence), 
            avg_mod_cases = mean(mod_RR_cases), 
            obs_pct_change = (obs_incidence[diag_yr == "2019"]-obs_incidence[diag_yr == "2017"])/obs_incidence[diag_yr == "2017"], 
            mod_pct_change = (mod_incidence[diag_yr == "2019"]-mod_incidence[diag_yr == "2017"])/mod_incidence[diag_yr == "2017"])


ggplot() + 
  geom_point(data = nat_prev %>% filter(model == "mod1"), aes(x = time, y = obs_RR_cases, size = obs_tested)) + 
  geom_line(data = nat_prev, aes(x = time, y = mod_RR_cases, color = model))


#################################################################
##                  prev Cases - State (Year)                  ##
#################################################################

# quick viz of how trends in incidence compare across states by year (logged)
ggplot() + 
  geom_line(data = nat_prev.yr %>% filter(model == "mod1") %>% group_by(diag_yr) %>% mutate(obs_incidence = sum(obs_RR_cases)/sum(obs_TB_cases) * 1000), aes(x = diag_yr, y = log(obs_incidence))) +
  geom_line(data = state_prev.yr %>% filter(model == "mod1") %>% group_by(diag_yr, state_nm) %>% mutate(mod_incidence = sum(mod_RR_cases)/sum(obs_TB_cases) * 1000), aes(x = diag_yr, y = log(mod_incidence), color = state_nm)) + 
  geom_text(data = state_prev.yr %>% filter(model == "mod1") %>% group_by(diag_yr, state_nm) %>% mutate(mod_incidence = sum(mod_RR_cases)/sum(obs_TB_cases) * 1000),  aes(x = diag_yr, y = log(mod_incidence), label = state_nm), nudge_x = 0.2, nudge_y = 0.2, hjust = 0)

# quick viz of how trends in incidence are modeled for each model by state by year
ggplot() + 
  geom_line(data = state_prev.yr %>%  filter(model == "mod1" & diag_yr > 2016) %>% group_by(diag_yr, state_nm) %>% mutate(obs_incidence = sum(obs_RR_cases)/sum(obs_TB_cases) * 1000), aes(x = diag_yr, y = obs_incidence, group = state_nm)) + 
  geom_line(data = state_prev.yr %>%  filter(diag_yr > 2016) %>% group_by(model, diag_yr, state_nm) %>% mutate(mod_incidence = sum(mod_RR_cases)/sum(obs_TB_cases) * 1000), aes(x = diag_yr, y = mod_incidence, group = model, color = model)) + 
  facet_wrap(~state_nm, scales = "free")





## Look at trends in testing by state
test <- state_prev.yr %>% 
  filter(model == "mod1") %>% 
  group_by(diag_yr, state_nm) %>% 
  summarize(avg_pct_test = mean(obs_tested/obs_TB_cases))


print(state_prev.yr %>% 
        filter(model == "mod1") %>% 
        group_by(state_nm) %>% 
        mutate(pct_tested = obs_tested/obs_TB_cases) %>% 
        arrange(state_nm, diag_yr) %>% 
        # mutate(pct_chg_tested = (pct_tested - lag(pct_tested))/lag(pct_tested) * 100) %>% 
        summarize(avg_pct_chg = mean((pct_tested - lag(pct_tested))/lag(pct_tested) * 100, na.rm = TRUE)) %>% 
        arrange(avg_pct_chg), n=50)

# quick viz to look at testing rates by state
ggplot() + 
  geom_line(state_prev.yr, state_prev.yr aes(x = diag_yr, y = pct_tested, color = state_nm)) + 
  geom_text(state_prev.yr, aes(x = diag_yr, y = pct_tested, label = state_nm), nudge_x = 0.2, nudge_y = 0.2, hjust = 0)
# geom_line(aes(x = diag_yr, y = pct_chg_tested, group = state_nm)) + 
# facet_wrap(~state_nm)



#################################################################
##                 prev Cases - State (Quarter)               ##
#################################################################
ggplot() + 
  geom_point(data = state_prev %>%  filter(model == "mod1" & diag_qrt >= "2017-01-01"), 
             aes(x = diag_qrt, y = (obs_tested/obs_TB_cases) * 100, size = obs_TB_cases, group = state_nm)) + 
  geom_line(data = state_prev %>%  filter(model == "mod1" & diag_qrt >= "2017-01-01") %>% group_by(diag_qrt, state_nm) %>% mutate(obs_incidence = sum(obs_RR_cases)/sum(obs_TB_cases) * 1000), 
            aes(x = diag_qrt, y = obs_incidence, group = state_nm)) + 
  geom_line(data = state_prev %>%  filter(diag_qrt >= "2017-01-01") %>% group_by(model, diag_qrt, state_nm) %>% mutate(mod_incidence = sum(mod_RR_cases)/sum(obs_TB_cases) * 1000), 
            aes(x = diag_qrt, y = mod_incidence, group = model, color = model)) + 
  facet_wrap(~state_nm, scales = "free")

#################################################################
##                 prev Cases - State (Quarter)                 ##
#################################################################
ggplot() + 
  geom_point(data = state_prev %>%  filter(model == "mod1" & time > 12), 
             aes(x = time, y = (obs_tested/obs_TB_cases) * 100, size = obs_TB_cases, group = state_nm)) + 
  geom_line(data = state_prev %>%  filter(model == "mod1" & time > 12) %>% group_by(time, state_nm) %>% mutate(obs_incidence = sum(obs_RR_cases)/sum(obs_TB_cases) * 1000), 
            aes(x = time, y = obs_incidence, group = state_nm)) + 
  geom_line(data = state_prev %>%  filter(time > 12) %>% group_by(model, time, state_nm) %>% mutate(mod_incidence = sum(mod_RR_cases)/sum(obs_TB_cases) * 1000), 
            aes(x = time, y = mod_incidence, group = model, color = model)) + 
  facet_wrap(~state_nm, scales = "free")








##################################################################
##                           Appendix                           ##
##################################################################
print(state_new.yr %>%
        filter(model == "mod1") %>%
        group_by(state_nm) %>%
        mutate(pct_tested = obs_tested/obs_TB_cases) %>%
        arrange(state_nm, diag_yr) %>%
        # mutate(pct_chg_tested = (pct_tested - lag(pct_tested))/lag(pct_tested) * 100) %>%
        summarize(avg_pct_chg = mean((pct_tested - lag(pct_tested))/lag(pct_tested) * 100, na.rm = TRUE)) %>%
        arrange(avg_pct_chg), n=50)


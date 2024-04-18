library(RColorBrewer)
library(readr)

source("code/dependencies.R")

load("output/compiled_results.Rdata")

# Load SINAN 
# load("data/sinan_xpert.Rdata")
load("data/sinan_tmp.Rdata")

pop_2010 = 190732694

# Load WHO data -----------------------------------------------------------
who_mdr <- read_csv("data/MDR_RR_TB_burden_estimates_2024-04-03.csv") %>% 
  filter(country == "Brazil") %>% 
  filter(year >= 2015 & year < 2020) %>% 
  select(year, e_rr_pct_new, e_rr_pct_new_lo, e_rr_pct_new_hi, e_rr_pct_ret, e_rr_pct_ret_lo, e_rr_pct_ret_hi, e_inc_rr_num, e_inc_rr_num_lo, e_inc_rr_num_hi) %>% 
  pivot_longer(cols = c(e_rr_pct_new, e_rr_pct_ret), 
               names_to = c(".value", "case_type"), 
               names_sep = "_(?=[a-z]+$)") %>% 
  rename(group = case_type) %>% 
  pivot_longer(cols = c(e_rr_pct_new_lo, e_rr_pct_ret_lo), 
               names_to = c("lo", "case_type"),
               names_sep ="e_rr_pct_", 
               values_to = "pct_lo") %>% 
  mutate(case_type = if_else(case_type == "new_lo", "new", "ret")) %>% 
  filter(group == case_type) %>% 
  select(!case_type) %>% 
  pivot_longer(cols = c(e_rr_pct_new_hi, e_rr_pct_ret_hi), 
               names_to = c("hi", "case_type"),
               names_sep ="e_rr_pct_", 
               values_to = "pct_hi") %>% 
  mutate(case_type = if_else(case_type == "new_hi", "new", "ret")) %>% 
  filter(group == case_type) %>% 
  select(!c(group, lo, hi)) %>% 
  mutate(time = if_else(year == 2015, 6,
           if_else(year == 2016, 10, 
                        if_else(year == 2017, 14, 
                                if_else(year == 2018, 18, 22)))))



who_data_new <- compiled_results[["nat_yr"]] %>% 
  filter(model == "sp_2015-2019" & case_type == "new") %>% 
  select(year, total_TB_cases) %>% 
  left_join(., who_mdr %>% filter(case_type == "new"), by = "year") %>% 
  mutate(who_count = ((e_rr_pct/100)*total_TB_cases), 
         who_count_lo = ((pct_lo/100)*total_TB_cases),
         who_count_hi = ((pct_hi/100)*total_TB_cases),
         who_inc = (who_count/pop_2010)*100000, 
         who_inc_lo = (who_count_lo/pop_2010)*100000,
         who_inc_hi = (who_count_hi/pop_2010)*100000)

who_data_prev <- compiled_results[["nat_yr"]] %>% 
  filter(model == "sp_2015-2019" & case_type == "prev") %>% 
  select(year, total_TB_cases) %>% 
  left_join(., who_mdr %>% filter(case_type == "ret"), by = "year") %>% 
  mutate(who_count = ((e_rr_pct/100)*total_TB_cases), 
         who_count_lo = ((pct_lo/100)*total_TB_cases),
         who_count_hi = ((pct_hi/100)*total_TB_cases),
         who_inc = (who_count/pop_2010)*100000, 
         who_inc_lo = (who_count_lo/pop_2010)*100000,
         who_inc_hi = (who_count_hi/pop_2010)*100000)

who_data_total <- who_mdr %>% 
  select(time, year, e_inc_rr_num, e_inc_rr_num_lo, e_inc_rr_num_hi) %>% 
  unique() %>% 
  mutate(who_inc = (e_inc_rr_num/pop_2010)*100000, 
         who_inc_lo = (e_inc_rr_num_lo/pop_2010)*100000,
         who_inc_hi = (e_inc_rr_num_hi/pop_2010)*100000)



  
  
# Prepare data to plot ----------------------------------------------------
data_total <- data.frame(
  
  pop_2010 = pop_2010, 
  
  dst = sinan_tmp %>%
    filter(diag_yr >= "2015" & tratamento %in% c(1, 2, 3)) %>%
    mutate(test_sensi = as.factor(test_sensi),
           case_type = if_else(tratamento == "1", "new", "prev")) %>%
    group_by(diag_yr) %>%
    # summarize(tested = sum(test_sensi %in% c("1", "2", "3", "4", "5"))),
    summarize(cases = sum(test_sensi %in% c("2", "3"))),

  # xpert = compiled_results[["nat_yr"]] %>%
  #   filter(model == "sp_2015-2019") %>%
  #   ungroup() %>%
  #   group_by(year) %>%
  #   summarize(tested = sum(obs_num_tested)),
  
  observed = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(cases = sum(obs_RR)),
  
  projected = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(cases = sum(fitted_RR)),

  
  cdr = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    group_by(year) %>%
    summarize(cases = if_else(year <= "2017", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
    unique(), 
  
  tb = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    group_by(year) %>%
    summarize(cases = sum(total_TB_cases)), 
  case_type = "total"
  
) %>% 
  rename(diag_yr = projected.year) %>% 
  group_by(diag_yr) %>% 
  mutate(total.observed = sum(dst.cases + observed.cases)) %>% 
  select(diag_yr, total.observed, projected.cases, cdr.cases, tb.cases, pop_2010, case_type) %>% 
  pivot_longer(cols = c("projected.cases", "cdr.cases"), names_to = "var", values_to = "count") %>% 
  mutate(inc = (count/pop_2010)*100000)





data_type <- data.frame(
  
  pop_2010 = pop_2010, 
  
  dst = sinan_tmp %>%
    filter(diag_yr >= "2015" & tratamento %in% c(1, 2, 3)) %>%
    mutate(test_sensi = as.factor(test_sensi), 
           case_type = if_else(tratamento == "1", "new", "prev")) %>%
    group_by(diag_yr, case_type) %>%
    # summarize(tested = sum(test_sensi %in% c("1", "2", "3", "4", "5"))),
    summarize(cases = sum(test_sensi %in% c("2", "3"))),
  
  # xpert = compiled_results[["nat_yr"]] %>%
  #   filter(model == "sp_2015-2019") %>% 
  #   ungroup() %>%
  #   group_by(year, case_type) %>%
  #   summarize(tested = sum(obs_num_tested)),
  
  observed = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    ungroup() %>%
    group_by(year, case_type) %>%
    summarize(cases = sum(obs_RR)),
  
  projected = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    ungroup() %>%
    group_by(year, case_type) %>%
    summarize(cases = sum(fitted_RR), 
              pct = sum(fitted_RR)/sum(total_TB_cases)*100),
  
  cdr = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    group_by(year, case_type) %>%
    summarize(cases = if_else(year <= "2017", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
    unique(), 
  
  tb = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    group_by(year, case_type) %>%
    summarize(cases = sum(total_TB_cases))
  
)%>% 
  rename(diag_yr = projected.year, 
         case_type = projected.case_type) %>% 
  group_by(diag_yr, case_type) %>% 
  mutate(total.observed = sum(dst.cases + observed.cases)) %>% 
  select(diag_yr, total.observed, projected.cases, projected.pct, tb.cases, pop_2010, case_type) %>% 
  pivot_longer(cols = c("projected.pct"), names_to = "var", values_to = "pct")

# Make plot ---------------------------------------------------------------
pal <- ggsci::pal_npg("nrc", alpha = 0.8)(6)


# # Set order of stack 
# plot_data$var <- factor(plot_data$var, levels = c("total.observed", "xpert.tested", "cdr.cases", "projected.cases"))
# 

## Make plot
fig_total_inc <- ggplot() +
  # Plot new cases
  geom_area(data = data_type %>% 
              filter(case_type == "new"),
            aes(x = diag_yr, y = pct, fill = var),  position = "identity") + 
  geom_point(data = who_data_new, 
             aes(x = year, y = e_rr_pct, color = "black")) + 
  geom_errorbar(data = who_data_new, 
                aes(x = year, ymin = pct_lo, ymax = pct_hi, color = "black"), width = 0.3) + 
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25), 
                     labels = scales::label_percent(scale = 1)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(2015, 2016, 2017, 2018, 2019)) + 
  ggtitle("New") + 
  ylab("Proportion with RR-TB") + 
  xlab("") + 
  scale_fill_manual(name = "",
                    values = pal[3]) + 
  scale_color_manual(name = "", 
                     values = "black") + 
  theme_bw() + 
  theme(legend.position = c("none"),
        plot.title = element_text(hjust = 0.5)) + 
  
  
  
  # Plot previous cases
  ggplot() +
  geom_area(data = data_type %>% 
              filter(case_type == "prev"), 
            aes(x = diag_yr, y = pct, fill = var),  position = "identity") +  
  geom_point(data = who_data_prev, 
             aes(x = year, y = e_rr_pct, color = "black")) + 
  geom_errorbar(data = who_data_prev, 
                aes(x = year, ymin = pct_lo, ymax = pct_hi, color = "black"), width = 0.3) + 
  
  ggtitle("Previous") + 
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 25), 
                     labels = scales::label_percent(scale = 1)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(2015, 2016, 2017, 2018, 2019)) + 
  
  xlab("Year") + 
  ylab("") + 
  theme_bw() + 
  scale_fill_manual(name = "",
                    labels = c("projected.cases" = "Projected"), 
                    values = pal[3]) + 
  scale_color_manual(name = "", 
                     values = "black") + 
  theme(legend.position = c("none"), 
        plot.title = element_text(hjust = 0.5)) + 
  
  
  # Plot total cases 
  ggplot() +
  geom_line(data = data_total,
            aes(x = diag_yr, y = total.observed/300, linetype = "Total number of observed RR-TB cases")) + 
  geom_area(data = data_total,
            aes(x = diag_yr, y = inc, fill = var),  position = "identity") + 
  geom_point(data = who_data_total, 
             aes(x = year, y = who_inc, color = "black")) + 
  geom_errorbar(data = who_data_total, 
                aes(x = year, ymin = who_inc_lo, ymax = who_inc_hi, color = "black"), width = 0.3) + 
  
  ggtitle("Total") + 
  scale_y_continuous(name = "Incidence per 100,000 population",
    expand = c(0, 0), 
                     limits = c(0, 5), 
                     sec.axis = sec_axis (~ . * 300, 
                                          name = "Number of RR-TB cases")) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(2015, 2016, 2017, 2018, 2019)) + 
  xlab("") + 
  theme_bw() + 
  scale_linetype_manual(name = "", 
                        values = c(3)) + 
  scale_fill_manual(name = "",
                    labels = c("projected.cases" = "Projected RR-TB cases", 
                               "cdr.cases" = "CDR-inflated RR-TB cases"),
                    values = c(pal[4], pal[3])) + 
  scale_color_manual(name = "", 
                     labels = "WHO estimates",
                     values = "black")+
 
  theme(plot.title = element_text(hjust = 0.5))


# Save plot ---------------------------------------------------------------
ggsave(fig_total_inc, filename = "output/figures_and_tables/fig_total_inc.png", width = 14, height = 6)




# Appendix ----------------------------------------------------------------


    
  

  

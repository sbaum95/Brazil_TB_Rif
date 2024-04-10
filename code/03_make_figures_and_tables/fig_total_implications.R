library(RColorBrewer)
library(readr)

source("code/dependencies.R")

load("output/compiled_results.Rdata")

# Load SINAN 
load("data/sinan_xpert.Rdata")
load("data/sinan_xpert_tmp.Rdata")

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
  
  pop_2010 = 190732694, 
  
  dst = sinan_xpert %>%
    filter(diag_yr >= "2015" & tratamento %in% c(1, 2, 3)) %>%
    mutate(test_sensi = as.factor(test_sensi), 
           case_type = if_else(tratamento == "1", "new", "prev")) %>%
    group_by(diag_yr) %>%
    summarize(tested = sum(test_sensi %in% c("2", "3"))),
  
  xpert = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>% 
    ungroup() %>%
    group_by(year) %>%
    summarize(tested = sum(obs_num_tested)),
  
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
  rename(diag_yr = dst.diag_yr) %>% 
  group_by(diag_yr) %>% 
  mutate(total.tested = sum(dst.tested + xpert.tested)) %>% 
  select(diag_yr, total.tested, xpert.tested, projected.cases, cdr.cases, tb.cases, pop_2010, case_type)




data_type <- data.frame(
  
  pop_2010 = 190732694, 
  
  dst = sinan_xpert %>%
    filter(diag_yr >= "2015" & tratamento %in% c(1, 2, 3)) %>%
    mutate(test_sensi = as.factor(test_sensi), 
           case_type = if_else(tratamento == "1", "new", "prev")) %>%
    group_by(diag_yr, case_type) %>%
    summarize(tested = sum(test_sensi %in% c("2", "3"))),
  
  xpert = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>% 
    ungroup() %>%
    group_by(year, case_type) %>%
    summarize(tested = sum(obs_num_tested)),
  
  projected = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    ungroup() %>%
    group_by(year, case_type) %>%
    summarize(cases = sum(fitted_RR)),
  
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
  rename(diag_yr = dst.diag_yr, 
         case_type = dst.case_type) %>% 
  group_by(diag_yr, case_type) %>% 
  mutate(total.tested = sum(dst.tested + xpert.tested)) %>% 
  select(diag_yr, xpert.tested, total.tested, projected.cases, cdr.cases, tb.cases, pop_2010, case_type)


plot_data <- rbind(data_total, data_type) %>% 
  pivot_longer(cols = c("total.tested", "xpert.tested", "projected.cases", "cdr.cases"), names_to = "var", values_to = "count") %>% 
  mutate(inc = (count/pop_2010)*100000)




# Make plot ---------------------------------------------------------------

pal <- c("#00568D", "#0072BC", "#338EC9", "#66AAD7")


# Set order of stack 
plot_data$var <- factor(plot_data$var, levels = c("total.tested", "xpert.tested", "cdr.cases", "projected.cases"))


## Make plot
fig_total_inc <- ggplot() +
  # Plot new cases
  geom_area(data = plot_data %>% 
              filter(case_type == "new") %>% 
              filter(!is.na(var)),
            aes(x = diag_yr, y = inc, fill = var),  position = "identity") + 
  geom_point(data = who_data_new, 
             aes(x = year, y = who_inc)) + 
  geom_errorbar(data = who_data_new, 
                aes(x = year, ymin = who_inc_lo, ymax = who_inc_hi), width = 0.3) + 
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 15)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(2015, 2016, 2017, 2018, 2019)) + 
  ggtitle("New") + 
  ylab("Incidence per 100,000 population") + 
  xlab("") + 
  scale_fill_manual(name = "",
                    values = c(pal[4], pal[3], pal[2], pal[1])) + 
  theme_bw() + 
  theme(legend.position = c("none"),
        plot.title = element_text(hjust = 0.5)) + 
  
  
  
  # Plot previous cases
  ggplot() +
  geom_area(data = plot_data %>% 
              filter(case_type == "prev"),
            aes(x = diag_yr, y = inc, fill = var), position = "identity") + 
  geom_point(data = who_data_prev, 
             aes(x = year, y = who_inc)) + 
  geom_errorbar(data = who_data_prev, 
                aes(x = year, ymin = who_inc_lo, ymax = who_inc_hi), width = 0.3) + 
  
  ggtitle("Previous") + 
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 15)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(2015, 2016, 2017, 2018, 2019)) + 
  
  xlab("Year") + 
  ylab("") + 
  theme_bw() + 
  scale_fill_manual(name = "",
                    values = c(pal[4], pal[3], pal[2], pal[1])) + 
  theme(legend.position = c("none"), 
        plot.title = element_text(hjust = 0.5)) + 
  
  # Plot total cases 
  ggplot() +
  geom_area(data = plot_data %>% 
              filter(case_type == "total"),
            aes(x = diag_yr, y = inc, fill = var),  position = "identity") + 
  geom_point(data = who_data_total, 
             aes(x = year, y = who_inc)) + 
  geom_errorbar(data = who_data_total, 
                aes(x = year, ymin = who_inc_lo, ymax = who_inc_hi), width = 0.3) + 
  ggtitle("Total") + 
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 15)) + 
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(2015, 2016, 2017, 2018, 2019)) + 
  ylab("") + 
  xlab("") + 
  theme_bw() + 
  scale_fill_manual(name = "",
                    labels = c("xpert.tested" = "Cases tested with Xpert",
                               "total.tested" = "Cases tested with Xpert + DST",
                               "projected.cases" = "Projected RR-TB cases", 
                               "cdr.cases" = "CDR-inflated projected RR-TB cases"),
                    values = c(pal[4], pal[3], pal[2], pal[1]))+ 
  theme(plot.title = element_text(hjust = 0.5))


# Save plot ---------------------------------------------------------------
ggsave(fig_total_inc, filename = "output/figures_and_tables/fig_total_inc.png", width = 14, height = 6)




# Appendix ----------------------------------------------------------------
# Prepare data to plot ----------------------------------------------------

# data_total <- data.frame(
#   
#   pop_2010 = 190732694, 
#   
#   dst = sinan_xpert %>%
#     filter(diag_qrt >= "2015-01-01" & tratamento %in% c(1, 2, 3)) %>%
#     mutate(test_sensi = as.factor(test_sensi), 
#            case_type = if_else(tratamento == "1", "new", "prev")) %>%
#     group_by(diag_qrt) %>%
#     summarize(tested = sum(test_sensi %in% c("2", "3"))),
#   
#   xpert = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2015-2019") %>% 
#     ungroup() %>%
#     group_by(time) %>%
#     summarize(tested = sum(obs_num_tested)),
#   
#   projected = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2015-2019") %>%
#     ungroup() %>%
#     group_by(time) %>%
#     summarize(cases = sum(fitted_RR)),
#   
#   cdr = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2015-2019") %>%
#     group_by(time) %>%
#     summarize(cases = if_else(time <= "2017-01-01", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
#     unique(), 
#   tb = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2015-2019") %>%
#     group_by(time) %>%
#     summarize(cases = sum(total_TB_cases)), 
#   case_type = "total"
# ) %>% 
#   rename(time = xpert.time, 
#          diag_qrt = dst.diag_qrt) %>% 
#   select(diag_qrt, time, dst.tested, xpert.tested, projected.cases, cdr.cases, tb.cases, pop_2010, case_type)
# 
# 
# 
# 
# data_type <- data.frame(
#   
#   pop_2010 = 190732694, 
#   
#   dst = sinan_xpert %>%
#     filter(diag_qrt >= "2015-01-01" & tratamento %in% c(1, 2, 3)) %>%
#     mutate(test_sensi = as.factor(test_sensi), 
#            case_type = if_else(tratamento == "1", "new", "prev")) %>%
#     group_by(diag_qrt, case_type) %>%
#     summarize(tested = sum(test_sensi %in% c("2", "3"))),
#   
#   xpert = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2015-2019") %>% 
#     ungroup() %>%
#     group_by(time, case_type) %>%
#     summarize(tested = sum(obs_num_tested)),
#   
#   projected = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2015-2019") %>%
#     ungroup() %>%
#     group_by(time, case_type) %>%
#     summarize(cases = sum(fitted_RR)),
#   
#   cdr = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2015-2019") %>%
#     group_by(time, case_type) %>%
#     summarize(cases = if_else(time <= "2017-01-01", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
#     unique(), 
#   tb = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2015-2019") %>%
#     group_by(time, case_type) %>%
#     summarize(cases = sum(total_TB_cases))
# )%>% 
#   rename(time = xpert.time, 
#          diag_qrt = dst.diag_qrt, 
#          case_type = dst.case_type) %>% 
#   select(diag_qrt, time, dst.tested, xpert.tested, projected.cases, cdr.cases, tb.cases, pop_2010, case_type)
# 
# 
# plot_data <- rbind(data_total, data_type) %>% 
#   pivot_longer(cols = c("dst.tested", "xpert.tested", "projected.cases", "cdr.cases", "tb.cases"), names_to = "var", values_to = "count") %>% 
#   mutate(inc = (count/pop_2010)*100000)
# 

# 
# 
# pal <- c("#00568D", "#0072BC", "#338EC9", "#66AAD7", "#99C7E4")
# 
# 
# # Set order of stack 
# #plot_data$var <- factor(plot_data$var, levels = c("cdr.cases", "projected.cases", "dst.tested", "xpert.tested", "tb.cases"))
# plot_data$var <- factor(plot_data$var, levels = c("dst.tested", "xpert.tested", "projected.cases", "cdr.cases"))
# 
# 
# ## Make plot 
# fig_total_inc <- ggplot() +
#   geom_area(data = plot_data %>% 
#               filter(case_type == "new"),
#             aes(x = time, y = inc, fill = var)) + 
#   geom_point(data = who_data_new, 
#              aes(x = time, y = who_inc)) + 
#   geom_errorbar(data = who_data_new, 
#                 aes(x = time, ymin = who_inc_lo, ymax = who_inc_hi), width = 0.5) + 
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c(0, 20)) + 
#   scale_x_continuous(expand = c(0, 0), 
#                      breaks = c(9, 13, 17, 21),
#                      labels = c("Jan 2016", "Jan 2017" , "Jan 2018", "Jan 2019")) + 
#   ggtitle("New") + 
#   ylab("Incidence per 100,000 population") + 
#   xlab("") + 
#   scale_fill_manual(name = "",
#                     values = c(pal[5], pal[4], pal[3], pal[2], pal[1])) + 
#   theme_bw() + 
#   theme(legend.position = c("none"),
#         plot.title = element_text(hjust = 0.5)) + 
# 
# ggplot() +
#   geom_area(data = plot_data %>% 
#               filter(case_type == "prev"),
#             aes(x = time, y = inc, fill = var)) + 
#   geom_point(data = who_data_prev, 
#              aes(x = time, y = who_inc)) + 
#   geom_errorbar(data = who_data_prev, 
#                 aes(x = time, ymin = who_inc_lo, ymax = who_inc_hi), width = 0.5) + 
# 
#   ggtitle("Previous") + 
#   scale_x_continuous(expand = c(0, 0), 
#                      breaks = c(9, 13, 17, 21),
#                      labels = c("Jan 2016", "Jan 2017" , "Jan 2018", "Jan 2019")) + 
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c(0, 20)) + 
# 
#   xlab("Quarter") + 
#   ylab("") + 
#   theme_bw() + 
#   scale_fill_manual(name = "",
#                     values = c(pal[5], pal[4], pal[3], pal[2], pal[1])) + 
#   theme(legend.position = c("none"), 
#         plot.title = element_text(hjust = 0.5)) + 
#   
# 
# ggplot() +
#   geom_area(data = plot_data %>% 
#               filter(case_type == "total"),
#             aes(x = time, y = inc, fill = var)) + 
#   geom_point(data = who_data_total, 
#              aes(x = time, y = who_inc)) + 
#   geom_errorbar(data = who_data_total, 
#                 aes(x = time, ymin = who_inc_lo, ymax = who_inc_hi), width = 0.5) + 
#   ggtitle("Total") + 
#   scale_y_continuous(expand = c(0, 0), 
#                      limits = c(0, 20)) + 
#   scale_x_continuous(expand = c(0, 0), 
#                      breaks = c(9, 13, 17, 21),
#                      labels = c("Jan 2016", "Jan 2017" , "Jan 2018", "Jan 2019")) + 
#   ylab("") + 
#   xlab("") + 
#   theme_bw() + 
#   scale_fill_manual(name = "",
#                     labels = c("tb.cases" = "Notified TB cases",
#                                "xpert.tested" = "Cases tested with Xpert",
#                                "dst.tested" = "Cases tested with DST",
#                                "projected.cases" = "Projected RR-TB cases", 
#                                "cdr.cases" = "CDR-inflated projected RR-TB cases"),
#                     values = c(pal[5], pal[4], pal[3], pal[2], pal[1])) + 
#   theme(plot.title = element_text(hjust = 0.5))
# 
#   
# 









# 
# 
# fig_total_inc <- ggplot() +
#   geom_area(data = sinan_xpert %>%
#               filter(time >= 5) %>%
#               mutate(test_sensi = as.factor(test_sensi)) %>%
#               group_by(time) %>%
#               summarize(inc_tested = sum(test_sensi %in% c("2", "3"))),
#             aes(x = time, y = inc_tested, fill = "DST")) +
#   # Plot Xpert testing incidence 
#   geom_area(data = compiled_results[["state_qrt"]] %>%
#               filter(model == "sp_2015-2019") %>% 
#               group_by(time) %>%
#               summarize(total_xpert = sum(obs_num_tested)), 
#             aes(x = time, y = total_xpert, fill = "Xpert")) + 
#   
#   # Plot DST testing incidence
#   +
#   
# 
#   
#   
# 
#  
#   
#   # Plot CDR-inflated incidence
#   geom_area(data = compiled_results[["nat_qrt"]] %>%
#               filter(model == "sp_2015-2019") %>%
#               ungroup() %>%
#               group_by(time) %>%
#               summarize(total_RR = sum(fitted_RR),
#                         total_TB = sum(total_TB_cases)) %>%
#               mutate(proj_RR_inc_CDR = if_else(time <= "2017-01-01", ((total_RR/total_TB)*1000)/0.87, ((total_RR/total_TB)*1000)/0.89)),
#             aes(x = time, y = proj_RR_inc_CDR, fill = "CDR-inflated")) +
#   
#   
#   # Plot projected total incidence by case type 
#   geom_area(data = compiled_results[["nat_qrt"]] %>%
#               filter(model == "sp_2015-2019") %>%
#               ungroup() %>%
#               group_by(time) %>%
#               summarize(total_RR = sum(fitted_RR),
#                         total_TB = sum(total_TB_cases),
#                         proj_RR_inc = (total_RR/total_TB)*1000),
#             aes(x = time, y = proj_RR_inc, fill = "Projected")) +
#   
#   # Plot projected incidence by case type 
#   geom_line(data = compiled_results[["nat_qrt"]] %>%
#               filter(model == "sp_2015-2019") %>%
#               ungroup() %>%
#               group_by(time, case_type) %>%
#               summarize(total_RR = sum(fitted_RR),
#                         total_TB = sum(total_TB_cases),
#                         proj_RR_inc = (total_RR/total_TB)*1000),
#             aes(x = time, y = proj_RR_inc, linetype = case_type)) +
#   
# 
#   # Plot observed incidence
#   geom_area(data = compiled_results[["nat_qrt"]] %>%
#               ungroup() %>%
#               filter(model == "sp_2015-2019") %>%
#               group_by(time) %>%
#               summarize(total_obs_inc = (sum(obs_RR)/sum(obs_num_tested))*1000),
#             aes(x = time, y = total_obs_inc, fill = "Observed")) +
# 
# 
#   # Tested
#   annotate("text",
#            x = 24,
#            color = pal[1],
#            y = 180,
#            label = "Tested for resistance",
#            hjust=0,
#            size=3.5,
#            lineheight=.8,
#            fontface="bold") +
# 
#   # CDR
#   annotate("text",
#            x = 24,
#            color = pal[2],
#            y = 45,
#            label = "CDR-inflated incidence",
#            hjust=0,
#            size=3.5,
#            lineheight=.8,
#            fontface="bold") +
# 
#   # Projected
#   annotate("text",
#            x = 24,
#            color = pal[3],
#            y = 38,
#            label = "Projected",
#            hjust=0,
#            size=3.5,
#            lineheight=.8,
#            fontface="bold") +
# 
#   # Observed
#   annotate("text",
#            x = 24,
#            color = pal[4],
#            y = 17,
#            label = "Observed",
#            hjust=0,
#            size=3.5,
#            lineheight=.8,
#            fontface="bold") +
#   
#     scale_linetype_manual(name = "", 
#                           labels = c("new" = "New", 
#                                      "prev" = "Previous"),
#                           values=c(1, 2)) + 
#     
#     scale_fill_manual(name = "",
#                       # labels = c("DST and Xpert" = "DST and Xpert", 
#                       #            "Total projected incidence" = "Projected incidence - Total", 
#                       #            "Observed" = "Observed incidence - Total", 
#                       #            "CDR-inflated" = "Projected incidence - CDR"),
#                       values = c("DST and Xpert" = pal[1], 
#                                  "CDR-inflated" = pal[2], 
#                                  "Projected" = pal[3], 
#                                  "Observed" = pal[4])
#     ) +
#   xlab("") + 
#   scale_y_continuous(expand = c(0, 0)) + 
# 
#   scale_x_continuous(expand = c(0, 0), 
#                      limits = c(5, 26.5), 
#                      breaks=c(9,13,17,21),
#                      labels = c("Jan 2016", "Jan 2017" , "Jan 2018", "Jan 2019"))  + 
#   
# 
#   
# 
#   ylab("Incidence per 1,000 TB cases") +
#   theme_bw() +
#   # expand_limits(x = c(5, 27)) +
#   theme(
#     legend.position = c(0.05, 0.95),   # Adjust legend position
#     legend.justification = c(0, 1),     # Anchor legend to top-left corner
#     legend.box.just = "left",           # Align legend items in two columns
#     legend.spacing.y = unit(0.2, "cm"),
#     legend.text = element_text(size = 12),
#     
#     panel.border = element_blank(),
#     panel.grid = element_blank(),
#     axis.text.x = element_text(size=10, margin = margin(0,0,0,0)),
#     plot.margin = margin(10, 10, 10, 10)) + 
#     guides(fill = FALSE)
#   





    
  

  

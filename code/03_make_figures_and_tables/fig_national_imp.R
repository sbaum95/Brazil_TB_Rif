## National trends in testing: Create figure for national estimates of total
## incidence, and positivity for new and previously treated cases that show
## uncertainty intervals for each year

load("output/compiled_results.Rdata")

pop_2010 = 190732694
  
data <- compiled_results[["nat_qrt"]] %>% 
  filter(model == "sp_2015-2019") %>% 
  select(case_type, diag_qrt, fitted_RR, proj_lci, proj_hci, total_TB_cases)


pal <- ggsci::pal_npg("nrc", alpha = 0.8)(4)


fig_nat_pos <- ggplot() + 
  # Positivity among new cases 
  geom_line(data = data %>% 
              group_by(diag_qrt, case_type) %>%
              mutate(pct = (fitted_RR/total_TB_cases)*100), 
            aes(x = diag_qrt, y = pct, color = case_type)) + 
  geom_errorbar(data = data %>% 
                  group_by(diag_qrt, case_type) %>%
                  mutate(pct_lci = (proj_lci/total_TB_cases)*100, 
                          pct_hci = (proj_hci/total_TB_cases)*100), 
                aes(x = diag_qrt, ymin = pct_lci, ymax = pct_hci, color = case_type)) + 
    scale_y_continuous(expand = c(0,0), 
                       limits = c(0, 20), 
                       labels = scales::label_percent(scale = 1)) + 
    annotate("text", x = as.Date("2018-10-01"), y = 10, label = "Previous", color = "red", vjust = -3, size = 4)  + 
    annotate("text", x = as.Date("2016-04-01"), y = 1, label = "New", color = "black", vjust = -3, size = 4) + 
  
    xlab("Quarter") + 
    ylab("Projected proportion of cases with RR-TB") + 
    ggtitle("A) RR-TB positivity") + 
    scale_x_date(
      date_breaks = "1 year",  # Set breaks to 1 year
      date_labels = "%Y"  # Format labels as year
    ) + 
    theme_bw() + 
    scale_color_manual(
      name = "",
      labels = "",
      values = c("black",
               "red"
      )
    ) + 
    theme(legend.position = "none", 
          plot.margin= margin(0,0.5,0,0, "cm"))

fig_nat_pos
    
fig_nat_inc <- ggplot() + 
  geom_line(data = data %>% 
              group_by(diag_qrt) %>% 
              summarize(inc = (sum(fitted_RR)/pop_2010)*100000,
                        inc_lci = (sum(proj_lci)/pop_2010)*100000, 
                        inc_hci = (sum(proj_hci)/pop_2010)*100000), 
            aes(x = diag_qrt, y = inc)) + 
    geom_errorbar(data = data %>% 
                    group_by(diag_qrt) %>% 
                    summarize(inc = (sum(fitted_RR)/pop_2010)*100000,
                              inc_lci = (sum(proj_lci)/pop_2010)*100000, 
                              inc_hci = (sum(proj_hci)/pop_2010)*100000), 
                  aes(x = diag_qrt, ymin = inc_lci, ymax = inc_hci)) + 
  
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, 1.5)) + 
    scale_x_date(
      date_breaks = "1 year",  # Set breaks to 1 year
      date_labels = "%Y"  # Format labels as year
    ) + 
    ggtitle("B) Total incidence") + 
    xlab("Quarter") + 
    ylab("RR-TB cases per 100,000 population") + 
    theme_bw() + 
  theme(plot.margin= margin(0,0,0,0.5, "cm"))


fig_nat_imp <- fig_nat_pos | fig_nat_inc
  
ggsave(fig_nat_imp, filename = "output/figures_and_tables/fig_nat_imp.png", width = 8, height = 5)    
              
              
  
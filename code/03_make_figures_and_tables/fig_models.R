

# Observed ----------------------------------------------------------------
fig_observed_trends <-  ggplot() + 
  # Observed 
  geom_point(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019"), 
             aes(x = diag_qrt, y = (obs_RR/obs_num_tested)*1000, color = case_type, size = obs_num_tested), alpha = 0.5) + 
  
  # Percent tested
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019"), 
            aes(x = diag_qrt, y = (obs_pct_tested*100)*(300/40), color = case_type), alpha = 0.5) + 
  scale_y_continuous(expand = c(0, 0), # So X-axis set at 0
                     limits = c(0, 300),
                     sec.axis = sec_axis(~./(300/40), # Create secondary axis that is set based off of incidence axis
                                         name = "Percent of TB cases with confirmed Xpert result", 
                                         labels = scales::label_percent(scale = 1) # Format labels as percentages
                     )
  ) +  
  scale_x_date(
    date_breaks = "1 year",  # Set breaks to 1 year
    date_labels = "%b %Y"  # Format labels as year
  ) + 
  xlab("Time (Quarter)") + 
  ylab("Incidence per 1,000 TB cases") + 
  theme_bw() + 
  theme(
    axis.text.x  = element_text(size = 10), 
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 12), 
    title = element_text(size = 12)) +
  labs(
    size = "Number of cases with conclusive Xpert result",
    color = "Case Type"
  ) + 
  scale_size(
    range = c(0.5, 5)
  ) + 
  scale_color_manual(
    name = "Case Type",
    labels = c(
      "New",
      "Previous"
    ),
    values = c(
      "black",
             "red"
    )
  ) +
  ggtitle ("Observed trends in Xpert testing and RR-TB incidence")




# Model - TT --------------------------------------------------------------
fig_tt <-  ggplot() + 
  # Observed 
  geom_point(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019"), 
    aes(x = diag_qrt, y = (obs_RR/obs_num_tested)*1000, color = case_type, size = obs_num_tested), alpha = 0.5) + 
  
  # Percent tested
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019"), 
    aes(x = diag_qrt, y = (obs_pct_tested*100)*(300/40), color = case_type), alpha = 0.5) + 
  
  ## Model tt_2014-2019
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019"),
          aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "tt_2014-2019")) + 
  ## Model tt_2014-2019
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2015-2019"),
            aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "tt_2015-2019")) + 
  
  scale_y_continuous(expand = c(0, 0), # So X-axis set at 0
                     limits = c(0, 300),
                     sec.axis = sec_axis(~./(300/40), # Create secondary axis that is set based off of incidence axis
                                         name = "Percent of TB cases with confirmed Xpert result", 
                                         labels = scales::label_percent(scale = 1) # Format labels as percentages
                     )
  ) +  
  scale_x_date(
    date_breaks = "1 year",  # Set breaks to 1 year
    date_labels = "%b %Y"  # Format labels as year
  ) + 
  xlab("Time (Quarter)") + 
  ylab("Incidence per 1,000 TB cases") + 
  theme_bw() + 
  theme(
    axis.text.x  = element_text(size = 10), 
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 12), 
    title = element_text(size = 12)) +
  labs(
    size = "Number of cases with conclusive Xpert result",
    color = "Case Type"
  ) + 
  scale_size(
    range = c(0.5, 5)
  ) + 
  scale_color_manual(
    name = "Case Type",
    labels = c(
      "New",
      "Previous"
    ),
    values = c(
      "black",
             "red"
    )
  ) + 
  scale_linetype_manual(name = "Model", 
                        labels = c("tt_2014-2019" = "Time trend: 2014-2019", 
                                   "tt_2015-2019" = "Time trend: 2015-2019 (Q12 dropped)"),
                        values=c(1, 2, 3)) + 
  ggtitle ("Time trend models")





# Model: TT + SP ----------------------------------------------------------

fig_tt_sp <-  ggplot() + 
  
  # Observed 
  geom_point(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019"), 
             aes(x = diag_qrt, y = (obs_RR/obs_num_tested)*1000, color = case_type, size = obs_num_tested), alpha = 0.5) + 
  
  # Percent tested
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019"), 
            aes(x = diag_qrt, y = (obs_pct_tested*100)*(300/40), color = case_type), alpha = 0.5) + 
  
  # Add models 
  ## Model tt_2014-2019
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019"),
            aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "tt_2014-2019")) + 
  ## Model sp_2014-2019
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "sp_2014-2019"),
            aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "sp_2014-2019")) + 
  ## Model sp_2015-2019
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "sp_2015-2019"), 
            aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "sp_2015-2019")) + 

  scale_y_continuous(expand = c(0, 0), # So X-axis set at 0
                     limits = c(0, 300),
                     sec.axis = sec_axis(~./(300/40), # Create secondary axis that is set based off of incidence axis
                                         name = "Percent of TB cases with confirmed Xpert result", 
                                         labels = scales::label_percent(scale = 1) # Format labels as percentages
                     )
  ) +  
  scale_x_date(
    date_breaks = "1 year",  # Set breaks to 1 year
    date_labels = "%b %Y"  # Format labels as year
  ) + 
  xlab("Time (Quarter)") + 
  ylab("Incidence per 1,000 TB cases") + 
  theme_bw() + 
  theme(
    axis.text.x  = element_text(size = 10), 
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 12), 
    title = element_text(size = 12)) +
  labs(
    size = "Number of cases with conclusive Xpert result",
    color = "Case Type"
  ) + 
  scale_size(
    range = c(0.5, 5)
  ) + 
  scale_color_manual(
    name = "Case Type",
    labels = c(
      "New",
      "Previous"
    ),
    values = c(
      "black",
             "red"
    )
  ) + 
  scale_linetype_manual(name = "Model", 
                        labels = c("tt_2014-2019" = "Time trend: 2014-2019 (Ref)", 
                                   "sp_2014-2019" = "Spatial: 2014-2019", 
                                   "sp_2015-2019" = "Spatial: 2015-2019 (Q12 dropped)"),
                        values=c("tt_2014-2019" = "solid", 
                                 "sp_2014-2019" = "dashed",
                                 "sp_2015-2019" = "dotted"
                        )) + 
  ggtitle ("Spatial models")




# Sensitivity 1 -----------------------------------------------------------

fig_se1 <-  ggplot() + 
  
  # Observed 
  geom_point(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019") %>% filter(diag_qrt >= "2015-01-01"), 
             aes(x = diag_qrt, y = (obs_RR/obs_num_tested)*1000, color = case_type, size = obs_num_tested), alpha = 0.5) + 
  
  # Percent tested
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019") %>% filter(diag_qrt >= "2015-01-01"), 
            aes(x = diag_qrt, y = (obs_pct_tested*100)*(300/40), color = case_type), alpha = 0.5) + 
  
  # Add models 
  ## Model tt_2015-2019
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2015-2019"),
            aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "tt_2015-2019")) + 
  ## Model Se1 tt_2015-2019
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "se1_tt_2015-2019"),
            aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "se1_tt_2015-2019")) + 
  ## Model se1 Sp
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "se1_sp_2015-2019"), 
            aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "se1_sp_2015-2019")) + 
  
  scale_y_continuous(expand = c(0, 0), # So X-axis set at 0
                     limits = c(0, 300),
                     sec.axis = sec_axis(~./(300/40), # Create secondary axis that is set based off of incidence axis
                                         name = "Percent of TB cases with confirmed Xpert result", 
                                         labels = scales::label_percent(scale = 1) # Format labels as percentages
                     )
  ) +  
  scale_x_date(
    date_breaks = "1 year",  # Set breaks to 1 year
    date_labels = "%b %Y"  # Format labels as year
  ) + 
  xlab("Time (Quarter)") + 
  ylab("Incidence per 1,000 TB cases") + 
  theme_bw() + 
  theme(
    axis.text.x  = element_text(size = 10), 
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 12), 
    title = element_text(size = 12)) +
  labs(
    size = "Number of cases with conclusive Xpert result",
    color = "Case Type"
  ) + 
  scale_size(
    range = c(0.5, 5)
  ) + 
  scale_color_manual(
    name = "Case Type",
    labels = c(
      "New",
      "Previous"
    ),
    values = c(
      "black",
             "red"
    )
  ) + 
  scale_linetype_manual(name = "Model", 
                        labels = c("tt_2015-2019" = "Time trend (Ref)", 
                                   "se1_tt_2015-2019" = "Time trend (Sens: Patient covs)", 
                                   "se1_sp_2015-2019" = "Spatial (Sens: Patient covs)"),
                        values=c("tt_2015-2019" = "solid", 
                                 "se1_tt_2015-2019" = "dashed",
                                 "se1_sp_2015-2019" = "dotted"
                        )) + 
  ggtitle ("Sensitivity: Additional patient covariates")



# Sensitivity 2 -----------------------------------------------------------
fig_se2 <-  ggplot() + 
  
  # Observed 
  geom_point(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019") %>% filter(diag_qrt >= "2015-01-01"), 
             aes(x = diag_qrt, y = (obs_RR/obs_num_tested)*1000, color = case_type, size = obs_num_tested), alpha = 0.5) + 
  
  # Percent tested
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "tt_2014-2019") %>% filter(diag_qrt >= "2015-01-01"), 
            aes(x = diag_qrt, y = (obs_pct_tested*100)*(300/40), color = case_type), alpha = 0.5) + 
  
  # Add models 
  ## Model tt_2015-2019
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "sp_2015-2019"),
            aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "sp_2015-2019")) + 
  ## Model Se1 tt_2015-2019
  geom_line(data = compiled_results[["nat_qrt"]] %>% group_by(case_type) %>% filter(model == "se2_sp_2015-2019"),
            aes(x = diag_qrt, y = (fitted_RR/total_TB_cases)*1000, color = case_type, linetype = "se2_sp_2015-2019")) + 
  
  scale_y_continuous(expand = c(0, 0), # So X-axis set at 0
                     limits = c(0, 300),
                     sec.axis = sec_axis(~./(300/40), # Create secondary axis that is set based off of incidence axis
                                         name = "Percent of TB cases with confirmed Xpert result", 
                                         labels = scales::label_percent(scale = 1) # Format labels as percentages
                     )
  ) +  
  scale_x_date(
    date_breaks = "1 year",  # Set breaks to 1 year
    date_labels = "%b %Y"  # Format labels as year
  ) + 
  xlab("Time (Quarter)") + 
  ylab("Incidence per 1,000 TB cases") + 
  theme_bw() + 
  theme(
    axis.text.x  = element_text(size = 10), 
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 12), 
    title = element_text(size = 12)) +
  labs(
    size = "Number of cases with conclusive Xpert result",
    color = "Case Type"
  ) + 
  scale_size(
    range = c(0.5, 5)
  ) + 
  scale_color_manual(
    name = "Case Type",
    labels = c(
      "New",
      "Previous"
    ),
    values = c(
      "black",
             "red"
    )
  ) + 
  scale_linetype_manual(name = "Model", 
                        labels = c("sp_2015-2019" = "Spatial (Ref)", 
                                   "se2_sp_2015-2019" = "Spatial (Sens: Interaction)"),
                        values=c("sp_2015-2019" = "solid", 
                                 "se2_sp_2015-2019" = "dashed"
                        )) + 
  ggtitle ("Sensitivity: Time*Patient Covariates")

# Save plots --------------------------------------------------------------


ggsave(fig_observed_trends, filename = "output/figures_and_tables/fig_observed_trends.png", width = 14, height = 6)

ggsave(fig_tt, filename = "output/figures_and_tables/fig_tt.png", width = 14, height = 6)
ggsave(fig_tt_sp, filename = "output/figures_and_tables/fig_tt_sp.png", width = 14, height = 6)
ggsave(fig_se1, filename = "output/figures_and_tables/fig_se1.png", width = 14, height = 6)
ggsave(fig_se2, filename = "output/figures_and_tables/fig_se2.png", width = 14, height = 6)


                     
              
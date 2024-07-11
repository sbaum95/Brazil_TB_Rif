fig_bias <- ggplot(data = compiled_results[["nat_qrt"]] %>% 
                      filter(model == "sp_2017") %>% 
                      mutate(case_type = if_else(case_type == "new", "New", "Previously Treated")) %>% 
                      group_by(diag_qrt, case_type) %>% 
                      summarize(
                        # Calculate modeled prevalence (quarterly scaled to year)
                        prev_mod = (fitted_RR*4/pop_2010)*100000,
                        prev_mod_lci = (proj_lci*4/pop_2010)*100000,
                        prev_mod_hci = (proj_hci*4/pop_2010)*100000,
                        obs_pct_tested = obs_pct_tested, 
                        # Calculate naive prevalence scaled by fraction tested (quarterly scaled to year)
                        prev_nav = ((obs_RR*(1/obs_pct_tested)*4)/pop_2010)*100000, 
                        # Calculate bias
                        bias_prev = prev_mod/prev_nav, 
                        bias_lci = prev_mod_lci/prev_nav, 
                        bias_hci = prev_mod_hci/prev_nav)) +
  ## Plot point estimate 
  geom_line(aes(diag_qrt, bias_prev, color = case_type)) + 
  
  ## Plot error bar 
  geom_ribbon(aes(x = diag_qrt, ymin = bias_lci, ymax = bias_hci, group = case_type, fill = case_type), alpha = 0.2, color = NA) + 
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0.75,1.75)) + 
  facet_wrap(~ case_type) + 
  scale_x_date(
    date_breaks = "1 year",  # Set breaks to 1 year
    date_labels = "%Y"  # Format labels as year
  ) + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + 
  xlab("Quarter") + 
  ylab("Ratio of modeled to naïve prevalence") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12),
        axis.text.x  = element_text(size = 9), 
        axis.text.y  = element_text(size = 9), 
        legend.position = "none") +
  scale_color_manual(name="Case type",
                     labels=c("New",
                              "Previously Treated"),
                     values=c(new_color, prev_color)) + 
  scale_fill_manual(name="Case type",
                     labels=c("New",
                              "Previously Treated"),
                     values=c(new_color, prev_color))


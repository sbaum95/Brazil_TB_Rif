

fig_bias <- ggplot(data = compiled_results[["nat_qrt"]] %>% 
                     filter(model == "sp_2017") %>% 
                     mutate(case_type = if_else(case_type == "new", "New", "Previously Treated")) %>% 
                     group_by(diag_qrt, case_type) %>% 
                     summarize(
                       prev_mod = (fitted_RR*4/pop_2010)*100000,
                               prev_mod_lci = (proj_lci*4/pop_2010)*100000,
                               prev_mod_hci = (proj_hci*4/pop_2010)*100000,
                               obs_pct_tested = obs_pct_tested, 
                               prev_nav = ((obs_RR*(1/obs_pct_tested)*4)/pop_2010)*100000, 
                               bias_prev = prev_mod/prev_nav, 
                               bias_lci = prev_mod_lci/prev_nav, 
                               bias_hci = prev_mod_hci/prev_nav)) + 
  # Quantify bias for new cases
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





# Look at absolute numbers 
# ggplot() + 
#   geom_point(
#     data = compiled_results[["nat_qrt"]] %>% 
#       filter(model == "tt_2015-2019" & case_type == "new"),
#     aes(diag_qrt, fitted_RR, color = "New")) + 
#   geom_errorbar(data = compiled_results[["nat_qrt"]] %>% 
#                   filter(model == "tt_2015-2019" & case_type == "new"), 
#                 aes(x = diag_qrt, ymin = proj_lci, ymax = proj_hci, color = "New", width = 40)) + 
# 
#   geom_point(
#     data = compiled_results[["nat_qrt"]] %>% 
#       filter(model == "tt_2015-2019" & case_type == "new"),
#     aes(diag_qrt, obs_RR, color = "New")) + 
#   geom_line(
#     data = compiled_results[["nat_qrt"]] %>% 
#       filter(model == "tt_2014-2019" & case_type == "new" & time >= 5), 
#     aes(x = diag_qrt, y = obs_pct_tested*1000,  color = "New"), alpha = 0.5) + 
# 
# 
# ggplot() + 
#   geom_point(
#     data = compiled_results[["nat_qrt"]] %>% 
#       filter(model == "tt_2015-2019" & case_type == "prev"),
#     aes(diag_qrt, fitted_RR, color = "Prev")) + 
#   geom_errorbar(data = compiled_results[["nat_qrt"]] %>% 
#                   filter(model == "tt_2015-2019" & case_type == "prev"), 
#                 aes(x = diag_qrt, ymin = proj_lci, ymax = proj_hci, color = "Prev", width = 40)) + 
#   geom_point(
#     data = compiled_results[["nat_qrt"]] %>% 
#       filter(model == "tt_2015-2019" & case_type == "prev"),
#     aes(diag_qrt, obs_RR, color = "Prev")) 
  


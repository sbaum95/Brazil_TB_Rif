

fig_bias <- ggplot() + 
  # Quantify bias for new cases
  ## Plot point estimate 
  geom_line(
    data = compiled_results[["nat_qrt"]] %>% 
      filter(model == "sp_2015") %>% 
      group_by(case_type) %>% 
      mutate(bias = fitted_RR/obs_RR),
    aes(diag_qrt, bias, color = case_type)) + 
  
  ## Plot error bar 
  geom_errorbar(data = compiled_results[["nat_qrt"]] %>% 
                  filter(model == "sp_2015") %>% 
                  group_by(case_type) %>% 
                  mutate(bias_lci = proj_lci/obs_RR, 
                         bias_hci = proj_hci/obs_RR), 
                aes(x = diag_qrt, ymin = bias_lci, ymax = bias_hci, color = case_type), width = 30) + 
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0,15)) + 
  scale_x_date(
    date_breaks = "1 year",  # Set breaks to 1 year
    date_labels = "%b %Y"  # Format labels as year
  ) + 
  
  xlab("Quarter") + 
  ylab("Ratio of projected to observed") + 
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
                     values=c("black", "red"))





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
  


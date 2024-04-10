
source("code/dependencies.R")

load("output/compiled_results.Rdata")



fig_bias <- ggplot() + 
  # Quantify bias for new cases
  ## Plot point estimate 
  geom_line(
    data = compiled_results[["nat_qrt"]] %>% 
      filter(model == "sp_2015-2019" & case_type == "new"),
    aes(diag_qrt, fitted_RR/obs_RR, color = "New")) + 
  
  ## Plot error bar 
  geom_errorbar(data = compiled_results[["nat_qrt"]] %>% 
                  filter(model == "sp_2015-2019" & case_type == "new"), 
                aes(x = diag_qrt, ymin = proj_lci/obs_RR, ymax = proj_hci/obs_RR, color = "New", width = 40)) + 
  
  
  # quantify bias for previous cases
  geom_line(
    data = compiled_results[["nat_qrt"]] %>% 
      filter(model == "sp_2015-2019" & case_type == "prev"),
    aes(diag_qrt, fitted_RR/obs_RR, color = "Previous")) + 
  
  ## Plot error bar 
  geom_errorbar(data = compiled_results[["nat_qrt"]] %>% 
                  filter(model == "sp_2015-2019" & case_type == "prev"), 
                aes(x = diag_qrt, ymin = proj_lci/obs_RR, ymax = proj_hci/obs_RR, color = "Previous", width = 40)) + 
  
  scale_x_date(
    date_breaks = "1 year",  # Set breaks to 1 year
    date_labels = "%b %Y"  # Format labels as year
  ) + 
  
  xlab("Time (Quarter)") + 
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
                     values=c("black", "red")) + 
  ggtitle("Bias: Ratio of projected to observed RR-TB cases (Spatial model)")

ggsave(fig_bias, filename = "output/figures_and_tables/fig_bias.png", width = 14, height = 6)





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
  


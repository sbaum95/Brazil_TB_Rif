source("code/dependencies.R")

load("output/compiled_results.Rdata")


# Pull regions
state_codes <- read_excel("data/StateCodes.xlsx")

# Plot new cases by region 
fig_state_new_2019 <- compiled_results[["state_yr"]] %>% 
  filter(model == "tt_2015-2019", year == "2019", case_type == "new") %>% 
  left_join(., state_codes %>% 
              select(sg_uf, NAME_2) %>% 
              mutate(sg_uf = as.character(sg_uf)), by = c("state" = as.character("sg_uf"))) %>% 
  group_by(state_nm, year) %>% 
  summarize(region = NAME_2, 
            mod_incidence = (fitted_RR/total_TB_cases) * 1000, 
            inc_lci = (proj_lci/total_TB_cases) * 1000, 
            inc_hci = (proj_hci/total_TB_cases) * 1000) %>% 
  ggplot() + 
  geom_point(aes(x = reorder(state_nm, mod_incidence), y = mod_incidence, color = region)) +
  geom_errorbar(aes(x = state_nm, ymin = inc_lci, ymax = inc_hci, color = region), alpha = 0.5) + 
  xlab("State") + 
  ylab("Incidence per 1000 incident TB cases") + 
  ggtitle("RR-TB incidence among new cases in 2019") + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 9), 
    title = element_text(size = 10)) +
  labs(color = "Region")




# Plot previous cases by region 
fig_state_prev_2019 <- compiled_results[["state_yr"]] %>% 
  filter(model == "tt_2015-2019", year == "2019", case_type == "prev") %>% 
  left_join(., state_codes %>% 
              select(sg_uf, NAME_2) %>% 
              mutate(sg_uf = as.character(sg_uf)), by = c("state" = as.character("sg_uf"))) %>% 
  group_by(state_nm, year) %>% 
  summarize(region = NAME_2, 
            mod_incidence = (fitted_RR/total_TB_cases) * 1000, 
            inc_lci = (proj_lci/total_TB_cases) * 1000, 
            inc_hci = (proj_hci/total_TB_cases) * 1000) %>% 
  ggplot() + 
  geom_point(aes(x = reorder(state_nm, mod_incidence), y = mod_incidence, color = region)) +
  geom_errorbar(aes(x = state_nm, ymin = inc_lci, ymax = inc_hci, color = region), alpha = 0.5) + 
  xlab("State") + 
  ylab("Incidence per 1000 incident TB cases") + 
  ggtitle("RR-TB incidence among previous cases in 2019") + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 9), 
    title = element_text(size = 10)) +
  labs(color = "Region")



# Save plots --------------------------------------------------------------
ggsave(fig_state_new_2019, filename = "output/figures_and_tables/fig_state_new_2019.png", width = 10, height = 6)

ggsave(fig_state_prev_2019, filename = "output/figures_and_tables/fig_state_prev_2019.png", width = 10, height = 6)


 
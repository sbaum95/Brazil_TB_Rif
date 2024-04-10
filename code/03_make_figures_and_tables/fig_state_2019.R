source("code/dependencies.R")
library(ggsci)

load("output/compiled_results.Rdata")


# Pull regions
state_codes <- read_excel("data/StateCodes.xlsx")

# Plot new cases by region 
state_new_2019 <- compiled_results[["state_yr"]] %>% 
  filter(model == "sp_2015-2019", year == "2019", case_type == "new") %>% 
  left_join(., state_codes %>% 
              select(sg_uf, NAME_2) %>% 
              mutate(sg_uf = as.character(sg_uf)), by = c("state" = as.character("sg_uf"))) %>% 
  group_by(state_nm, year) %>% 
  summarize(region = NAME_2, 
            mod_pct = (fitted_RR/total_TB_cases) * 100, 
            pct_lci = (proj_lci/total_TB_cases) * 100, 
            pct_hci = (proj_hci/total_TB_cases) * 100) 


# Plot previous cases by region 
state_prev_2019 <- compiled_results[["state_yr"]] %>%
  filter(model == "sp_2015-2019", year == "2019", case_type == "prev") %>%
  left_join(., state_codes %>%
              select(sg_uf, NAME_2) %>%
              mutate(sg_uf = as.character(sg_uf)), by = c("state" = as.character("sg_uf"))) %>%
  group_by(state_nm, year) %>%
  summarize(region = NAME_2, 
            mod_pct = (fitted_RR/total_TB_cases) * 100, 
            pct_lci = (proj_lci/total_TB_cases) * 100, 
            pct_hci = (proj_hci/total_TB_cases) * 100) 



# Make plots --------------------------------------------------------------
pal <- pal_npg("nrc", alpha = 0.8)(6)

scales::show_col(pal)

fig_state_2019 <- 
# New cases
  ggplot(data = state_new_2019) + 
  geom_point(aes(x = reorder(state_nm, mod_pct), y = mod_pct, color = region)) +
  geom_errorbar(aes(x = state_nm, ymin = pct_lci, ymax = pct_hci, color = region), alpha = 0.5) + 
  xlab("State") + 
  ylab("Projected RR-TB positivity") + 
  ggtitle("New") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 9), 
    title = element_text(size = 10), 
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5)) + 
    scale_color_manual(name = "", 
                       values = pal) + 
  
# Previous cases
  ggplot(data = state_prev_2019) + 
  geom_point(aes(x = reorder(state_nm, mod_pct), y = mod_pct, color = region)) +
  geom_errorbar(aes(x = state_nm, ymin = pct_lci, ymax = pct_hci, color = region), alpha = 0.5) + 
  xlab("State") + 
  ylab("") + 
  ggtitle("Previous") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 9), 
    title = element_text(size = 10), 
    plot.title = element_text(hjust = 0.5)
    ) +
    scale_color_manual(name = "Region", 
                       values = pal)




# Save plots --------------------------------------------------------------
ggsave(fig_state_2019, filename = "output/figures_and_tables/fig_state_2019.png", width = 12, height = 5)


 
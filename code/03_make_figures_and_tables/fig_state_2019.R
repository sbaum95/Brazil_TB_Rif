source("code/dependencies.R")
library(ggsci)

load("output/compiled_results.Rdata")


# Pull regions
state_codes <- read_excel("data/StateCodes.xlsx")

# Load shapefile with states
shp_uf <- st_read("data/BR_UF_2021/BR_UF_2021.shp") %>% st_simplify(dTolerance = 20)
shp_uf <- shp_uf %>% st_set_crs(value = 5527) %>% st_transform(crs = 5875) # Change to EPSG:4674 SAD69, UTM Zone 18S



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

shp_new_2019 <- left_join(shp_uf, state_new_2019, by = c("NM_UF" = "state_nm"))


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

shp_prev_2019 <- left_join(shp_uf, state_prev_2019, by = c("NM_UF" = "state_nm"))

# Make maps ---------------------------------------------------------------

shp_centroids <- st_centroid(shp_uf)

shp_new <- ggplot() + 
  geom_sf(data = shp_new_2019, aes(fill = mod_pct)) + 
  geom_sf_text(data = shp_centroids, aes(label = SIGLA), size = 3) + 
  scale_fill_material(name = "Projected RR-TB positivity", 
                      "teal", 
                      labels = scales::percent_format(scale = 1)) + 
  theme_void() + 
  theme(legend.position = "bottom", 
        plot.margin = margin(0, 0, 0, 0, "cm"), 
        legend.margin = margin(t = -5, r = 0, b = 0, l = 0), 
        legend.text = element_text(size = 8), 
        legend.key.width = unit(1, "cm")) 


  

shp_prev <- ggplot(shp_prev_2019) + 
  geom_sf(aes(fill = mod_pct)) + 
  geom_sf_text(data = shp_centroids, aes(label = SIGLA), size = 3) + 
  scale_fill_material(name = "Projected RR-TB positivity", 
                      "teal", 
                      labels = scales::percent_format(scale = 1)) + 
  theme_void() + 
  theme(legend.position = "bottom", 
        plot.margin = margin(0, 0, 0, 0, "cm"), 
        legend.margin = margin(t = -5, r = 0, b = 0, l = 0), 
        legend.text = element_text(size = 8), 
        legend.key.width = unit(1, "cm")) 




# Make plots --------------------------------------------------------------
pal <- pal_npg("nrc", alpha = 0.8)(6)

# scales::show_col(pal)


# New cases
fig_new <-  ggplot(data = state_new_2019) + 
  geom_point(aes(x = reorder(state_nm, mod_pct), y = mod_pct, color = region)) +
  geom_errorbar(aes(x = state_nm, ymin = pct_lci, ymax = pct_hci, color = region), alpha = 0.5) + 
  xlab("State") + 
  ylab("Projected RR-TB positivity") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_color_manual(name = "Region", 
                     values = pal) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 9), 
    title = element_text(size = 10), 
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(1, 1, 1, 1, "cm"))
 
  
# Previous cases
fig_prev <- ggplot(data = state_prev_2019) + 
  geom_point(aes(x = reorder(state_nm, mod_pct), y = mod_pct, color = region)) +
  geom_errorbar(aes(x = state_nm, ymin = pct_lci, ymax = pct_hci, color = region), alpha = 0.5) + 
  xlab("State") + 
  ylab("") + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y  = element_text(size = 9), 
    legend.text = element_text(size = 9), 
    title = element_text(size = 10), 
    plot.title = element_text(hjust = 0.5), 
    plot.margin = margin(1, 1, 1, 1, "cm")
    ) +
    scale_color_manual(name = "Region", 
                       values = pal)




# Save plots --------------------------------------------------------------
ggsave(shp_new, filename = "output/figures_and_tables/fig_state_new_map.png", width = 5, height = 6, bg = "white")
ggsave(fig_new, filename = "output/figures_and_tables/fig_state_new_plot.png", width = 8, height = 6)

ggsave(shp_prev, filename = "output/figures_and_tables/fig_state_prev_map.png", width = 5, height = 6, bg = "white")
ggsave(fig_prev, filename = "output/figures_and_tables/fig_state_prev_plot.png", width = 8, height = 6)


 
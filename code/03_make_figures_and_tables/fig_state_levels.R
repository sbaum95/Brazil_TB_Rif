library(ggsci)

# Pull regions
state_codes <- read_excel("data/StateCodes.xlsx")
state_codes$uf_code <- paste("(", state_codes$uf_code, ")", sep = "")
state_codes$uf_name_code <- paste(state_codes$NAME_1, state_codes$uf_code)

# Load shapefile with states
shp_uf <- st_read("data/BR_UF_2021/BR_UF_2021.shp") %>% st_simplify(dTolerance = 20)
shp_uf <- shp_uf %>%
  st_set_crs(value = 5527) %>%
  st_transform(crs = 5875) # Change to EPSG:4674 SAD69, UTM Zone 18S

# Plot new cases by region
state_new_2023 <- compiled_results[["state_yr"]] %>%
  filter(model == "sp_2017", year == "2023", case_type == "new") %>%
  left_join(., state_codes %>%
    select(sg_uf, NAME_2, uf_name_code) %>%
    mutate(sg_uf = as.character(sg_uf)), by = c("state" = as.character("sg_uf"))) %>%
  group_by(state_nm, year) %>%
  summarize(
    region = NAME_2,
    uf_name_code = uf_name_code, 
    mod_pct = (fitted_RR / total_TB_cases) * 100,
    pct_lci = (proj_lci / total_TB_cases) * 100,
    pct_hci = (proj_hci / total_TB_cases) * 100
  )

shp_new_2023 <- left_join(shp_uf, state_new_2023, by = c("NM_UF" = "state_nm"))


# Plot previous cases by region
state_prev_2023 <- compiled_results[["state_yr"]] %>%
  filter(model == "sp_2017", year == "2023", case_type == "prev") %>%
  left_join(., state_codes %>%
    select(sg_uf, NAME_2, uf_name_code) %>%
    mutate(sg_uf = as.character(sg_uf)), by = c("state" = as.character("sg_uf"))) %>%
  group_by(state_nm, year) %>%
  summarize(
    uf_name_code = uf_name_code, 
    region = NAME_2,
    mod_pct = (fitted_RR / total_TB_cases) * 100,
    pct_lci = (proj_lci / total_TB_cases) * 100,
    pct_hci = (proj_hci / total_TB_cases) * 100
  )

shp_prev_2023 <- left_join(shp_uf, state_prev_2023, by = c("NM_UF" = "state_nm"))

# Make maps ---------------------------------------------------------------

shp_centroids <- st_centroid(shp_uf)

shp_new <- ggplot() +
  geom_sf(data = shp_new_2023, aes(fill = mod_pct)) +
  geom_sf_text(data = shp_centroids, aes(label = SIGLA), size = 3) +
  scale_fill_material(
    name = "Modeled prevalence",
    "teal",
    labels = scales::percent_format(scale = 1)
  ) +
  ggtitle("A) New") + 
  theme_void() +
  theme(
    title = element_text(size = 14),
    legend.title = element_text(size = 10), 
    legend.position = "bottom",
    plot.margin = margin(0, 0, 0, 0, "cm"),
    legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
    legend.text = element_text(size = 9),
    legend.key.width = unit(1, "cm")
  )

shp_prev <- ggplot(shp_prev_2023) +
  geom_sf(aes(fill = mod_pct)) +
  geom_sf_text(data = shp_centroids, aes(label = SIGLA), size = 3) +
  scale_fill_material(
    name = "Modeled prevalence",
    "teal",
    labels = scales::percent_format(scale = 1)
  ) +
  ggtitle("B) Previously Treated") + 
  theme_void() +
  theme(
    title = element_text(size = 14),
    legend.title = element_text(size = 10), 
    legend.position = "bottom",
    plot.margin = margin(0, 0, 0, 0, "cm"),
    legend.margin = margin(t = -5, r = 0, b = 0, l = 0),
    legend.text = element_text(size = 9),
    legend.key.width = unit(1, "cm")
  )




# Make plots --------------------------------------------------------------

# New cases
fig_new <- ggplot(data = state_new_2023) +
  geom_point(aes(x = reorder(uf_name_code, mod_pct), y = mod_pct, color = region)) +
  geom_errorbar(aes(x = uf_name_code, ymin = pct_lci, ymax = pct_hci, color = region), alpha = 0.5) +
  xlab("State") +
  ylab("Modeled prevalence") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(
    name = "Region",
    values = pal
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 0, 1, 1, "cm")
  )


# Previous cases
fig_prev <- ggplot(data = state_prev_2023) +
  geom_point(aes(x = reorder(uf_name_code, mod_pct), y = mod_pct, color = region)) +
  geom_errorbar(aes(x = uf_name_code, ymin = pct_lci, ymax = pct_hci, color = region), alpha = 0.5) +
  xlab("State") +
  ylab("Modeled prevalence") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 9),
    legend.text = element_text(size = 9),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(1, 0, 1, 1, "cm")
  ) +
  scale_color_manual(
    name = "Region",
    values = pal
  )


# Combine plots 
fig_combined_level <- gridExtra::grid.arrange(shp_new, fig_new, shp_prev, fig_prev, nrow = 2, ncol = 2, heights = c(1, 1), widths = c(1, 2))

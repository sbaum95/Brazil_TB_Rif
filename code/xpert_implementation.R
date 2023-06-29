# Author: Sarah Baum
# Created: 2023-06-26
# Updated: 2023-06-27
# Description: Spatial analysis of Xpert implementation 

source(here::here("code/dependencies.R"))

load("data/sinan_xpert.Rdata")



# add in Xpert implementation data
xpert_install <- read_excel("data/Equipamentos_RTR-TB_maio_2023.xlsx", sheet = "Equip por Estado",
                    col_types = c("text",
              "numeric", "text", "text", "text",
              "text", "numeric", "numeric", "date",
              "numeric", "text", "text", "text", "text")) %>%
  rename(id_municip = "Cód. Município", 
         dt_install = "Data da instalação", 
         dt_donation = "Ano da doação",
         id_unidade = "CNES", 
         facility = "Nome Instituição Destinada") %>% 
  mutate(dt_donation = ifelse(is.na(dt_donation), NA, paste(dt_donation, "01", "01", sep = "-")) %>% 
           as.Date(dt_donation, format = "%Y-%m-%d"),
         dt_install = as.Date(dt_install, format = "%Y-%m-%d"))

# Note: Where a single year of implementation was listed, I manually updated the
# implementation year to be Jan 01. Where implementation date was not listed,
# but a date of donation was listed, I use the date of donation.
xpert_install$dt_install <- if_else(is.na(xpert_install$dt_install), xpert_install$dt_donation, xpert_install$dt_install)

# Determing when Xpert was implemented in each municipality
start_imp_yr <- xpert_install %>% mutate(implementation_yr = floor_date(as_date(dt_install), "year")) %>% group_by(id_municip) %>% summarize(start_imp_yr = min(implementation_yr))




# create dataset ----------------------------------------------------------
xpert_implementation <- sinan_xpert %>% 
  left_join(start_imp_yr, by = "id_municip") %>% 
  group_by(diag_yr, id_municip) %>% 
  summarize(municip_name = municip_name,
            start_imp_yr = start_imp_yr, 
            diagnoses = n(), 
            number_performed = sum(if_else(test_molec %in% c("1", "2", "3"), 1, 0))/diagnoses,
            not_performed = sum(if_else(test_molec %in% c("5"), 1, 0))/diagnoses,
            missing = sum(if_else(is.na(test_molec) | test_molec == "", 1, 0))/diagnoses)




# Load shapefile  ---------------------------------------------------------
# Source: Shapefile - IBGE; Projection - https://epsg.io/5875
shapefile <- st_read("data/BR_Municipios_2020/BR_Municipios_2020.shp") %>% st_simplify(dTolerance = 20) 
shapefile <- shapefile %>% st_set_crs(value = 5527) %>% st_transform(crs = 5875) # Change to EPSG:4674 SAD69, UTM Zone 18S
sf::st_crs(shapefile)

shapefile$CD_MUN_merge <- as.numeric(substr(as.character(shapefile$CD_MUN), 1, nchar(shapefile$CD_MUN)-1)) # Transform ID (7-digit from IBGE to 6 in SINAN) to merge 



# Join shapefile and dataset ----------------------------------------------
shp_xpert <- left_join(shapefile %>% select(CD_MUN_merge, NM_MUN, geometry), xpert_implementation,  by = c("CD_MUN_merge" = "id_municip"), relationship = "many-to-many") %>% 
  unique() %>% 
  filter(!is.na(diag_yr))

# not_matched <- anti_join(xpert_implementation, shapefile, by = c("id_municip" = "CD_MUN_merge")) # see which municipalities didn't merge





# Plot Xpert roll-out -----------------------------------------------------
plot_xpert_by_year <- function(list_of_years) {
  
  for (year in list_of_years) {
    
  print(year)
    
  # filter data by year and where percentage of diagnoses receiving Xpert is non-zero
  year_to_plot <- shp_xpert %>% filter(diag_yr == year & number_performed > 0)
  
  # plot map 
  plot <- ggplot() +
    geom_sf(data = shapefile, fill = "darkgray", color = NA) + 
    # geom_sf(data = year_to_plot, aes(color = ifelse(start_imp_yr <= year, "red", NA)))+ 
    labs(color = "Municipality implementing Xpert") + 
    geom_sf(data = year_to_plot, aes(fill = number_performed), color = NA) + 
    labs(fill='Percent of diagnoses receiving Xpert (%)')+
    scale_fill_viridis_c() +
    ggtitle(paste("Xpert implementation in", year)) +
    theme_bw() + 
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(),
          legend.text = element_text(size = 7), 
          legend.title = element_text(size = 7),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          title = element_text(size = 8),
          legend.key.height = unit(0.5, "cm"), 
          legend.key.width = unit(0.3, "cm"))
 
  # save plot 
  ggsave(filename = paste0("figures/map_",year,".png"), plot)
  }
  
}

list_of_years <- c("2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01")

plot_xpert_by_year(list_of_years)
                     
                    
                     
                     









  
  

# # levels(sinan_xpert$test_molec)
# 
# muni <- sinan_xpert %>% 
#   filter(id_municip %in% muni_with_xpert) %>% 
#   filter(!test_molec %in% c("9")) %>% 
#   group_by(diag_yr, id_municip) %>% 
#   summarize(diagnoses = n(), 
#             number_performed = sum(if_else(test_molec %in% c("1", "2", "3"), 1, 0)),
#             not_performed = sum(if_else(test_molec %in% c("5"), 1, 0)),
#             missing = sum(if_else(is.na(test_molec) | test_molec == "", 1, 0)))
# 
# # Number of facilities in municipality
# # sinan_xpert %>% 
# #   filter(id_municip %in% muni_with_xpert) %>% 
# #   mutate(id_municip = as.factor(id_municip), 
# #          id_unidade = as.factor(id_unidade)) %>% 
# #   group_by(id_municip) %>% 
# #   summarize(facility_count = n_distinct(id_unidade))
# 
# # Number of facilities in municipality using Xpert
# # sinan_xpert %>% 
# #   filter(id_unidade %in% fac_with_xpert) %>% 
# #   mutate(id_municip = as.factor(id_municip), 
# #          id_unidade = as.factor(id_unidade)) %>% 
# #   group_by(id_municip) %>% 
# #   summarize(facility_count = n_distinct(id_unidade))
# 
# 
# sinan_xpert %>% 
#   filter(id_municip %in% muni_with_xpert_2017) %>% 
#   filter(!test_molec %in% c("9")) %>% 
#   group_by(diag_yr, id_municip) %>% 
#   summarize(diagnoses = n(), 
#             number_performed = sum(if_else(test_molec %in% c("1", "2", "3"), 1, 0))/diagnoses,
#             not_performed = sum(if_else(test_molec %in% c("5"), 1, 0))/diagnoses,
#             missing = sum(if_else(is.na(test_molec) | test_molec == "", 1, 0))/diagnoses) %>% 
#   ggplot(aes(x = diag_yr, y = number_performed, group = factor(id_municip))) + 
#   geom_line(aes(color = factor(id_municip)))
# 
# 
# 
# sinan_xpert %>% 
#   filter(id_municip == "260005") %>% 
#   filter(!test_molec %in% c("9")) %>% 
#   group_by(diag_yr, id_municip) %>% 
#   summarize(diagnoses = n(), 
#             number_performed = sum(if_else(test_molec %in% c("1", "2", "3"), 1, 0)),
#             not_performed = sum(if_else(test_molec %in% c("5"), 1, 0)),
#             missing = sum(if_else(is.na(test_molec) | test_molec == "", 1, 0)))
# 
# # pull out municipalities implementing Xpert
# # muni_with_xpert <- xpert %>% pull(id_municip) %>% unique()


# shp_xpert_1 <- left_join(xpert_implementation, shapefile %>% select(CD_MUN_merge, NM_MUN, geometry), by = c("id_municip" = "CD_MUN_merge"), relationship = "many-to-many") %>% unique()


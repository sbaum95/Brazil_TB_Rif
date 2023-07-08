# Author: Sarah Baum
# Created: 2023-06-26
# Updated: 2023-07-07
# Description: Loads and saves geometry data to sinan


source(here::here("code/dependencies.R"))

load("data/sinan_xpert.Rdata")


# create dataset ----------------------------------------------------------
xpert_implementation <- sinan_xpert %>% 
  mutate(start_imp_yr = floor_date(as_date(start_imp_yr), "year")) %>% 
  # left_join(start_imp_yr, by = "id_municip") %>% 
  group_by(diag_yr, id_municip) %>% 
  summarize(diagnoses = n(), 
            number_performed = sum(if_else(test_molec %in% c("1", "2", "3", "4"), 1, 0))/diagnoses,
            # not_performed = sum(if_else(test_molec %in% c("5"), 1, 0))/diagnoses,
            missing = sum(if_else(is.na(test_molec) | test_molec == "", 1, 0))/diagnoses,
            municip_name = first(municip_name),
            start_imp_yr = first(start_imp_yr))



# Load shapefile  ---------------------------------------------------------
# Source: Shapefile - IBGE; Projection - https://epsg.io/5875
shapefile <- st_read("data/BR_Municipios_2020/BR_Municipios_2020.shp") %>% st_simplify(dTolerance = 20) 
shapefile <- shapefile %>% st_set_crs(value = 5527) %>% st_transform(crs = 5875) # Change to EPSG:4674 SAD69, UTM Zone 18S
shapefile <- shapefile %>% st_transform(crs = 5875) # Change to EPSG:4674 SAD69, UTM Zone 18S

sf::st_crs(shapefile)

shapefile$CD_MUN_merge <- as.numeric(substr(as.character(shapefile$CD_MUN), 1, nchar(shapefile$CD_MUN)-1)) # Transform ID (7-digit from IBGE to 6 in SINAN) to merge 



# Join shapefile and dataset ----------------------------------------------
shapefile_xpert <- left_join(shapefile %>% select(CD_MUN_merge, NM_MUN, geometry, SIGLA_UF), xpert_implementation,  by = c("CD_MUN_merge" = "id_municip"), relationship = "many-to-many") %>% 
  unique() %>% 
  filter(!is.na(diag_yr))

# not_matched <- anti_join(xpert_implementation, shapefile, by = c("id_municip" = "CD_MUN_merge")) # see which municipalities didn't merge



save(shapefile_xpert, file = "data/shapefile_xpert.Rdata")




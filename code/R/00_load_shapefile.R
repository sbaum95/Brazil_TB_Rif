# Author: Sarah Baum
# Created: 2023-06-26
# Updated: 2023-07-07
# Description: Loads and saves geometry data to sinan



# create dataset ----------------------------------------------------------
xpert_implementation <- sinan_xpert %>% 
  mutate(start_imp_yr = floor_date(as_date(start_imp_yr), "year")) %>% 
  # left_join(start_imp_yr, by = "id_municip") %>% 
  group_by(diag_yr, id_municip, access_xpert, test_molec, start_imp_yr, municip_name) %>% 
  summarize(cases = n()) %>% 
  group_by(id_municip, diag_yr) %>% 
  summarize(# add in municipality name
    municip_name = first(municip_name),
    total_cases = sum(cases), 
    # Municipality has machine
    access_xpert = if_else(sum(access_xpert) > 0, 1, 0),
    # number of cases diagnosed with Xpert
    diag_with_xpert = sum(cases[test_molec %in% c("1", "2", "3", "4")]),
    # percent of cases diagnosed with Xpert
    pct_cases_xpert = if_else(is.nan(diag_with_xpert/total_cases), 0, (diag_with_xpert/total_cases)*100), 
    # not_performed = sum(if_else(test_molec %in% c("5"), 1, 0))/diagnoses,
    # missing = sum(if_else(is.na(test_molec) | test_molec == "", 1, 0))/total_cases,
    # number of Xpert cases that are RR+
    diag_RR_pos = sum(cases[test_molec == "2"]), 
    # percent of Xpert cases that are RR+
    pct_RR_pos = if_else(is.nan(diag_RR_pos/sum(cases[test_molec %in% c("1", "2")])), 0, (diag_RR_pos/sum(cases[test_molec %in% c("1", "2")]))*100),
    # add in year they received xpert machine (where applicable)
    start_imp_yr = first(start_imp_yr)
    ) %>% 
  mutate(never_use_xpert = if_else(all(diag_with_xpert == 0), 1, 0))






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




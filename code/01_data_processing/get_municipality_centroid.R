# Author: Sarah Baum
# Created: 2024-03-22
# Updated: 
# Description: Pulls municipality centroid from 2020 IBGE municipality centroid


get_municipality_centroid <- function() {
# load shapefile
# Source: Shapefile - IBGE; Projection - https://epsg.io/5875

shp_mun <- st_read("data/BR_Municipios_2020/BR_Municipios_2020.shp") %>% st_simplify(dTolerance = 20)

# shp_mun <- shp_mun %>% st_set_crs(value = 5527) 
# %>% st_transform(crs = 5875) # Change to EPSG:4674 SAD69, UTM Zone 18S

# Truncate municip ID (7-digit from IBGE to 6 in SINAN) to merge
shp_mun$CD_MUN_merge <- as.numeric(substr(as.character(shp_mun$CD_MUN), 1, nchar(shp_mun$CD_MUN) - 1)) %>% as.factor()

# Get the centroid of each municipality
centroid_mun <- sf::st_centroid(shp_mun)

# Extract latitude and longitude
centroid_mun$lat <- st_coordinates(centroid_mun)[, "Y"]
centroid_mun$lon <- st_coordinates(centroid_mun)[, "X"]

return(centroid_mun)
}


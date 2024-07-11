# Description: Pulls municipality centroid from 2020 IBGE municipality centroid


get_municipality_centroid <- function() {
  
# load shapefile
# Source: Shapefile - IBGE
shp_mun <- st_read("data/BR_Municipios_2020/BR_Municipios_2020.shp") %>% st_simplify(dTolerance = 20)

# Truncate municip ID (7-digit from IBGE to 6 in SINAN) to merge
shp_mun$CD_MUN_merge <- as.numeric(substr(as.character(shp_mun$CD_MUN), 1, nchar(shp_mun$CD_MUN) - 1)) %>% as.factor()

# Get the centroid of each municipality
centroid_mun <- sf::st_centroid(shp_mun)

# Extract latitude and longitude
centroid_mun$lat <- st_coordinates(centroid_mun)[, "Y"]
centroid_mun$lon <- st_coordinates(centroid_mun)[, "X"]

return(centroid_mun)
}


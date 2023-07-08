# Author: Sarah Baum
# Created: 2023-07-07
# Updated: 

# Description: Function to calculate distance (mi) from each municipality to nearest
# municipality with xpert installed


# function - calculate municipality distance to nearest municipality with xpert machine in a given year 
calculate_distance_to_xpert <- function (list_of_years) {
  
  # identify municipalities with access in a given year 
  shapefile_xpert$access <- if_else(is.na(shapefile_xpert$start_imp_yr), 0, 
                                    if_else(shapefile_xpert$start_imp_yr <= shapefile_xpert$diag_yr, 1, 0))
  
  result_list <- list()
  
  for (i in list_of_years) {
  print(i)
  
  # create centroids of all municipalities in given year, i 
  shapefile_xpert_sp <- sf::as_Spatial(shapefile_xpert %>%  filter(diag_yr == i))
  centroids <- sp::coordinates(shapefile_xpert_sp)
  
  # create centroids of municipalities with xpert in given year, i 
  mun_w_xpert_sp <- sf::as_Spatial(shapefile_xpert %>% filter(access == 1 & diag_yr == i))
  mun_w_xpert_points <- sp::coordinates(mun_w_xpert_sp)
  
  # calculate min. distance to municipality with xpert in miles 
  dm_xpert <- sp::spDists(centroids, mun_w_xpert_points)
  min_dist_xpert_test <- apply(dm_xpert, 1, min)/1609.34 # turn to KM 
  
  
  # Create a data frame for the current year's results
  year_results <- data.frame(
    diag_yr = i, 
    CD_MUN_merge = shapefile_xpert_sp$CD_MUN_merge,
    min_dist = min_dist_xpert_test)
  
  # Append the current year's results to the overall result data frame
  result_list[[i]] <- year_results
  }
  
  final_result <<- bind_rows(result_list)
}
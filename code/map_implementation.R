# Author: Sarah Baum
# Created: 2023-06-26
# Updated: 2023-06-27
# Description: Spatial analysis of Xpert implementation over time looking at spatial access to xpert and coverage
# 1. Plots access to Xpert by year
# 2. Plots coverage of Xpert by year 

source(here::here("code/dependencies.R"))


# map access over time  ---------------------------------------------------
plot_access_by_year <- function(list_of_years) {
  
  for (year in list_of_years) {
    
    print(year)
    
    # filter data by year and where percentage of diagnoses receiving Xpert is non-zero
    year_to_plot <- shp_xpert %>% 
      # filter to municipalities in the third quartile of diagnoses
      filter(diag_yr == year & diagnoses >= quantile(diagnoses, 0.75)) %>% 
      # create access variable that = 1 if municipality has access to xpert and
      # = 2 if it doesn't have a machine but is testing more than 10% of
      # patients with xpert
      mutate(xpert_access = if_else(is.na(start_imp_yr) & number_performed >= 0.1, 2,
                                    if_else(start_imp_yr > diag_yr & number_performed >= 0.1, 2, 
                                            if_else(start_imp_yr <= diag_yr, 1, 0)))) %>% 
      # remove those that are not actively using xpert
      filter(!xpert_access %in% c(0, NA))
    
    
    
    # plot map 
    plot <- ggplot() +
      geom_sf(data = shapefile, fill = "white", color = "lightgray") + 
      # geom_sf(data = year_to_plot, aes(color = ifelse(start_imp_yr <= year, "red", NA)))+
      # labs(color = "Municipality implementing Xpert") + 
      geom_sf(data = year_to_plot, aes(fill = factor(xpert_access)), color = NA) + 
      # labs(fill='Type of Access', 
      #      labels = c("Access", 
      #                 "No access, but >=10% of diagnoses receiving Xpert")) +
      scale_fill_discrete(name = "Municipality Type", labels = c("Access to machine", "No access, but >=10% of diagnoses receiving Xpert")) +
      ggtitle(paste("Xpert access in", year)) +
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
    ggsave(filename = paste0("figures/map_access_",year,".png"), plot)
  }
  
}

list_of_years <- c("2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01")


plot_access_by_year(list_of_years)






### calculate municipality centroids
# shapefile_sp <- sf::as_Spatial(shapefile)
# centroids <- sp::coordinates(shapefile_sp)
# 
# shp_xpert_sp <- sf::as_Spatial(shp_xpert)
# centroids <- sp::coordinates(shp_xpert_sp)
# 
# ### calculate distance to nearest municipality with machine in a given year 
# # for each year, identify which facilities have access and which don't
# # calculate distance from those without access to those with access
# # re-append to have distance in each year 
# 
# mun_w_xpert_sp <- sf::as_Spatial(shp_xpert %>% filter(access == 1))
# mun_w_xpert_points <- sp::coordinates(mun_w_xpert_sp)
# 
# dm_xpert <- sp::spDists(centroids, mun_w_xpert_points)
# shp_xpert$min_dist_xpert_test <- apply(dm_xpert, 1, min) # turn to KM 
# 






    

# Plot Xpert roll-out -----------------------------------------------------
plot_xpert_coverage_by_year <- function(list_of_years) {
  
  for (year in list_of_years) {
    
  print(year)
    
  # filter data by year and where percentage of diagnoses receiving Xpert is non-zero
  year_to_plot <- shp_xpert %>% filter(diag_yr == year & diagnoses >= quantile(diagnoses, 0.75))
  
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
  ggsave(filename = paste0("figures/map_coverage_",year,".png"), plot)
  }
  
}

list_of_years <- c("2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01")

plot_xpert_coverage_by_year(list_of_years)
                     
                    
    














  
  

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


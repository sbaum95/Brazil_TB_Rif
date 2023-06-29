# Author: Do Kyung Ryuk
# Updated: 2023-06-28
# Description: Merges health facility type to SINAN

# library(foreign) # to read dbf file 
# library(sf) # manipulate simple (spatial) features
# library(spdep) # spatial statistics
# library(dplyr) # data management
# library(ggplot2) # visualization and mapping
# library(ggsn) # separate package for ggplot() north symbol & scale bar
# library(viridis) # color palette
# library(egg) # for plotting multiple ggplots together
# library(rgeoda) # for bivariate moran's I and other functions
# library(RColorBrewer)
# library(sp)
# library(ggsn)
# library(grid)
# library(geobr)
# library(readr)
# library(mlogit)
# library(dfidx)
# library(LaplacesDemon)
# library(MASS)
# library(effects)
# library(ggeffects)
# library(scales)
# library(janitor)
# library(haven)
# #library(tidyverse)

# opening the data 
# data <- read.dbf("/Users/dokyungryuk/Documents/spring 2023/Brazil spring/data clean up/data_brazil.dbf", as.is = FALSE) # SINAN
# health_unit <- read_dta("/Users/dokyungryuk/Documents/spring 2023/Brazil spring/level health service_Brazil (1).dta")
health_unit_master <- read_dta("data/level health service_Brazil (1).dta")

colnames(sinan_xpert) <- toupper(colnames(sinan_xpert))

#filtering the years of your interest 
data <- filter(sinan_xpert, NU_ANO %in%  c("2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014")) 
health_unit <- filter(health_unit_master, year %in%  c("2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014")) 

#reclassification
data$ID_UNID_AT1 <- as.numeric(as.character(data$ID_UNID_AT))
health_unit$cnes1 <- as.numeric(as.character(health_unit$cnes))


## Add years 
my_list <- list()

years <- c("2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014")


for (i in years) {
  print(i)
  health_unit_in_year <- filter(health_unit, year == i) 
  sinan_in_year <- filter(data, NU_ANO == i)

  merged <- merge(sinan_in_year, health_unit_in_year, by.x = "ID_UNID_AT1", by.y = "cnes1", all.x = T)

  df_name <- paste0("dataset_", i)
  
  my_list[[df_name]] <- merged
} 

data_new <- bind_rows(my_list)


# 
# #2015
# health_unit2015 <- filter(health_unit, year %in%  c("2015")) 
# data2015test <- filter(data, NU_ANO %in%  c("2015"))
# data2015test_ <- merge(data2015test, health_unit2015, by.x = "ID_UNID_AT1", by.y = "cnes1", all.x = T)
# 
# #2016
# health_unit2016 <- filter(health_unit, year %in%  c("2016")) #should I exclude 2014..?
# data2016test <- filter(data, NU_ANO %in%  c("2016"))
# data2016test_ <- merge(data2016test, health_unit2016, by.x = "ID_UNID_AT1", by.y = "cnes1", all.x = T)
# 
# #2017
# health_unit2017 <- filter(health_unit, year %in%  c("2017")) #should I exclude 2014..?
# data2017test <- filter(data, NU_ANO %in%  c("2017"))
# data2017test_ <- merge(data2017test, health_unit2017, by.x = "ID_UNID_AT1", by.y = "cnes1", all.x = T)
# 
# #2018
# health_unit2018 <- filter(health_unit, year %in%  c("2018")) #should I exclude 2014..?
# data2018test <- filter(data, NU_ANO %in%  c("2018"))
# data2018test_ <- merge(data2018test, health_unit2018, by.x = "ID_UNID_AT1", by.y = "cnes1", all.x = T)
# 
# #binding rows
# data_new <- bind_rows(data2015test_, data2016test_) #85309+84846=170155
# data_new <- bind_rows(data_new, data2017test_) #170155+90456=260611
# data_new <- bind_rows(data_new, data2018test_) #260611+94239=354850

#cleaning unncessary dataframe 
# rm(health_unit2015)
# rm(health_unit2016)
# rm(health_unit2017)
# rm(health_unit2018)
# 
# rm(data2015test)
# rm(data2016test)
# rm(data2017test)
# rm(data2018test)
# 
# rm(data2015test_)
# rm(data2016test_)
# rm(data2017test_)
# rm(data2018test_)

#reclassified process needed for the new data
data_new <- data_new %>% 
  mutate(healthunit = case_when(
    first_level == 1  ~ "low complexity",
    second_level == 1 ~ "medium complexity",
    second_level_tb == 1 ~ "medium complexity",
    second_level == 1 & second_level_tb == 1  ~ "medium complexity", 
    third_level == 1 ~ "high complexity",
    third_level_tb == 1 ~ "high complexity",
    third_level == 1 & third_level_tb == 1 ~ "high complexity",
    second_level_tb == 1 & third_level_tb == 1 ~ "high complexity", 
    others == 1 ~ "other"))

data_new$healthunit <- factor(data_new$healthunit, levels = c("low complexity", "medium complexity", "high complexity", "other"))

merged_health_unit <- data_new 

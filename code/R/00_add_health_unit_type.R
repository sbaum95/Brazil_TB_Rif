# Author: Do Kyung Ryuk
# Updated: 2023-06-28
# Description: 
# -- Adapts Do's original code to merge health facility type to SINAN 

## Question: 
## - Is ID unit of treatment the same as the unit that made the notification? Which one would have diagnosed patient and used Xpert?
##  -- Check to see whether notification and treatment unit are the same 


# opening the data 
# data <- read.dbf("/Users/dokyungryuk/Documents/spring 2023/Brazil spring/data clean up/data_brazil.dbf", as.is = FALSE) # SINAN
# health_unit <- read_dta("/Users/dokyungryuk/Documents/spring 2023/Brazil spring/level health service_Brazil (1).dta")
health_unit <- read_dta("data/level health service_Brazil (1).dta")

# Count up number of health units in period of interest: 
# health_unit %>% 
#   filter(year >= 2014 & year < 2020) %>% 
#   select(cnes, uni_tp) %>% 
#   unique()

# colnames(sinan_xpert) <- toupper(colnames(sinan_xpert))

#filtering the years of your interest 
# data <- filter(sinan_xpert, NU_ANO %in%  c("2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014")) 
# health_unit <- filter(health_unit_master, year %in%  c("2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014")) 

# reclassification
# For treating health unit 
# sinan_xpert$ID_UNID_AT <- as.numeric(as.character(sinan_xpert$ID_UNID_AT)) 

# for notifying (that is ideally diagnosing health unit)
# sinan_xpert$ID_UNIDADE <- as.numeric(as.character(sinan_xpert$ID_UNIDADE)) 
sinan_tmp$id_unidade %<>%  as.numeric(as.character()) 

# health_unit$cnes <- as.numeric(as.character(health_unit$cnes))
health_unit$cnes %<>%  as.numeric(as.character()) 

## Add years 
my_list <- list()

years <- c("2019", "2018", "2017", "2016", "2015", "2014")
# sinan_xpert$DIAG_YR <- as.numeric(year(sinan_xpert$DIAG_YR)) # so it filters

for (i in years) {
  print(i)
  health_unit_in_year <- filter(health_unit, year == i) 
  # sinan_in_year <- filter(sinan_xpert, NU_ANO == i)
  sinan_in_year <- filter(sinan_tmp, diag_yr == i) # merge based on year of diag, rather than year of notif
  
  merged <- merge(sinan_in_year, health_unit_in_year, by.x = "id_unidade", by.y = "cnes", all.x = T)
  # merged <- merge(sinan_in_year, health_unit_in_year, by.x = "ID_UNID_AT1", by.y = "cnes1", all.x = T)

  df_name <- paste0("dataset.", i)
  
  my_list[[df_name]] <- merged
} 

merge <- bind_rows(my_list)

# check that it works! 
# health_unit_in_year %>% 
#   filter(cnes == "7468776")
# 
# sinan_in_year %>% 
#   filter(ID_UNID_AT1 == "7468776")
# 
# merged %>% 
#   filter(ID_UNID_AT1 == "7468776")

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
merged_health_unit <- merge  %>% 
  mutate(health_unit = case_when(
    first_level == 1  ~ "low complexity",
    second_level == 1 ~ "medium complexity",
    second_level_tb == 1 ~ "medium complexity",
    second_level == 1 & second_level_tb == 1  ~ "medium complexity", 
    third_level == 1 ~ "high complexity",
    third_level_tb == 1 ~ "high complexity",
    third_level == 1 & third_level_tb == 1 ~ "high complexity",
    second_level_tb == 1 & third_level_tb == 1 ~ "high complexity", 
    others == 1 ~ "other"))

merged_health_unit$health_unit <- factor(merged_health_unit$health_unit, levels = c("low complexity", "medium complexity", "high complexity", "other"))


rm(health_unit_in_year, sinan_in_year, merge, health_unit, my_list)













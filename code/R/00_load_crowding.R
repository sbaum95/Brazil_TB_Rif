# Author: Sarah Baum
# Created: 2023-09-06
# Updated: 
# Description: Ingests and cleans urbanicity data from 2010 census



# load and clean .csv file for Dec 2015
crowd <- read_excel("data/crowding/crowding.xlsx", 
                           sheet = "Worksheet") %>% 
  rename("municip" = "Territorialidades", 
         "pct_crowd" = "% da população que vive em domicílios com densidade superior a 2 pessoas por dormitório 2010") %>% 
  separate(municip, into = c("municip_nm", "state"), sep = "[()]") %>% 
  filter(!is.na(state))
  



# confirm proper number of municips per state
crowd %>% 
  group_by(state) %>% 
  summarize(n())



# add municip codes 
municip <- read_excel("data/MunicipalCodes.xlsx") %>% 
  rename("municip_nm" = "Município 2010")


# merge 
crowd$municip_nm <- trimws(crowd$municip_nm)
municip$municip_nm <- trimws(municip$municip_nm)

crowd_merge <- merge(crowd, municip, by = "municip_nm")


### Figure out why extra municipalities in crowd_merge

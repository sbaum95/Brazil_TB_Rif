# Author: Sarah Baum 
# Created: 2023-06-28

# Description: Add health unit type based on notifying health unit based on merge by NTP team 

load_health_unit_type <- function(sinan_tmp) {

  sinan_unit <- sinan_tmp %>%
    mutate(health_unit_not = case_when(
      first_level_not == 1 ~ "low complexity",
      second_level_not == 1 ~ "medium complexity",
      second_level_tb_not == 1 ~ "medium complexity",
      second_level_not == 1 & second_level_tb_not == 1 ~ "medium complexity",
      third_level_not == 1 ~ "high complexity",
      third_level_tb_not == 1 ~ "high complexity",
      third_level_not == 1 & third_level_tb_not == 1 ~ "high complexity",
      second_level_tb_not == 1 & third_level_tb_not == 1 ~ "high complexity",
      others_not == 1 ~ "other"
    ))

  sinan_unit$health_unit <- factor(sinan_unit$health_unit_not, levels = c("low complexity", "medium complexity", "high complexity", "other"))
  
  return(sinan_unit)
}
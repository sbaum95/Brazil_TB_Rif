# Author: Sarah Baum (From Do Kyung Ryuk)
# Created: 2023-06-28
# Updated: 2023-03-22
# Description:
# -- Adapts Do's original code to merge health facility type to SINAN

## Question:
## - Is ID unit of treatment the same as the unit that made the notification? Which one would have diagnosed patient and used Xpert?
##  -- Check to see whether notification and treatment unit are the same

load_health_unit_type <- function(years, sinan_tmp) {
  
  health_unit <- read_dta("data/health_service_level.dta")
  
  ## Bring in state codes to properly code sg_uf_unidade
  state_codes <- read_excel("data/StateCodes.xlsx") %>%
    select(sg_uf, uf_code)

  health_unit <- left_join(health_unit, state_codes, by = c("state" = "uf_code")) %>% 
    rename(sg_uf_unidade = sg_uf, 
           id_mn_unidade = municip_cod)

  sinan_tmp$id_unidade <- as.numeric(as.character(sinan_tmp$id_unidade))

  health_unit$cnes <- as.numeric(as.character(health_unit$cnes))

  sinan_with_unit <- list()

  for (i in years) {
    print(i)

    health_unit_in_year <- filter(health_unit, year == i)

    sinan_in_year <- filter(sinan_tmp, diag_yr == i) # merge based on year of diag, rather than year of notif

    merged <- merge(sinan_in_year, health_unit_in_year, by.x = "id_unidade", by.y = "cnes", all.x = T)

    df_name <- paste0("dataset.", i)

    sinan_with_unit[[df_name]] <- merged
  }


  sinan_unit <- bind_rows(sinan_with_unit)

  sinan_unit <- sinan_unit %>%
    mutate(health_unit = case_when(
      first_level == 1 ~ "low complexity",
      second_level == 1 ~ "medium complexity",
      second_level_tb == 1 ~ "medium complexity",
      second_level == 1 & second_level_tb == 1 ~ "medium complexity",
      third_level == 1 ~ "high complexity",
      third_level_tb == 1 ~ "high complexity",
      third_level == 1 & third_level_tb == 1 ~ "high complexity",
      second_level_tb == 1 & third_level_tb == 1 ~ "high complexity",
      others == 1 ~ "other"
    ))

  sinan_unit$health_unit <- factor(sinan_unit$health_unit, levels = c("low complexity", "medium complexity", "high complexity", "other"))
  

  sinan_unit <- sinan_unit %>% 
    select(-c("year", "first_level", "second_level_tb", "third_level_tb", "second_level", "third_level", "others")) %>% 
    mutate(sg_uf_unidade = if_else(sg_uf_unidade == "" | sg_uf_unidade == "9", NA, sg_uf_unidade) %>% as.factor(),
           id_mn_unidade = if_else(id_mn_unidade == "" | id_mn_unidade == "9", NA, id_mn_unidade) %>% as.factor())

  return(sinan_unit)
}


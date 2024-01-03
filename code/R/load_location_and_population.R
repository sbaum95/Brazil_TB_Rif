# Author: Sarah Baum
# Created: 2023-12-24
# Updated: 
# Description: 
# - Loads and cleans location ids (municipality and micro-region) and population 
# - Creates urbanicity variable by location



# load state name 
state_name <- read_xlsx("data/StateCodes.xlsx") %>% 
  select(sg_uf, NAME_1, NAME_2) %>% 
  rename(state_name = NAME_1, region.name = NAME_2) %>% 
  mutate(sg_uf = as.factor(sg_uf))



# load municipality names and population data
mun_pop <- read_excel("data/MunicipalCodes.xlsx") %>% 
  rename(id_mun = cod.6.2010, id_state = cod.sg.2010, name_mun = "Município 2010") %>% 
  mutate(
    id_mun = as.factor(id_mun), 
    id_state = as.factor(id_state),
    mun_pop_2010 = as.numeric(gsub("\\.", "", pop.tot.2010)), 
    mun_pct_urban = as.numeric(gsub("\\.", "", pop.urb.2010))/as.numeric(gsub("\\.", "", pop.tot.2010)))



# create municipality urbanicity variable 
mun_pop$mun_urban_cat <- cut(mun_pop$mun_pct_urban, 
                                    breaks = quantile(mun_pop$mun_pct_urban, probs = 0:5/5), 
                                    labels = FALSE, 
                                    include.lowest = TRUE)




# # load micro-region id
# mun_micro <- read.csv("data/micro_region/mun_micro_macro_2019.csv")
# 
# 
# 
# # aggregate population by micro-region 
# micro_pop <- left_join(mun_pop, mun_micro %>% 
#                                 mutate(codufmun = as.numeric(as.character(codufmun))), 
#                               by = c("id_municip" = "codufmun")) %>% 
#   group_by(cod_micro) %>% 
#   mutate(
#     mic_pop_2010 = sum(as.numeric(gsub("\\.", "", pop.tot.2010))), 
#     mic_pct_urban = sum(as.numeric(gsub("\\.", "", pop.urb.2010)))/sum(as.numeric(gsub("\\.", "", pop.tot.2010)))) %>% 
#   select(cod_micro, mic_pop_2010, mic_pct_urban) %>% 
#   unique()
# 
# 
# 
# # create micro-region urbanicity variable 
# micro_pop$mic_urban_cat <- cut(micro_pop$mic_pct_urban, breaks = quantile(micro_pop$mic_pct_urban, probs = 0:5/5), labels = FALSE, include.lowest = TRUE)
# 


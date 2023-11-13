# Author: Sarah Baum
# Created: 2023-06-15
# Updated: 2023-09-26
# Description: Loads sinan_xpert$ .dta files and cleans 
# -- Updated (09-26): To merged based on patient municipality of residence, not municipality of diagnosis

source(here::here("code/dependencies.R"))




# load sinan --------------------------------------------------------------
sinan_xpert <- read_dta("data/fim_abr_2023_no name.dta") %>% 
  mutate_at(c("nu_ano", "sg_uf_not", "id_municip", "id_regiona", "id_unidade", "nu_idade_n", "sg_uf", "id_mn_resi"), as.numeric) %>% 
  mutate_at(c("dt_notific", "dt_diag", "dt_encerra"), as.Date, format = "%Y-%m-%d") %>% 
  mutate(age = floor(as.numeric((dt_diag - dt_nasc)/365))) %>% # create age variable 
  mutate_at(c("cs_sexo", "cs_raca", "tratamento", "raiox_tora", "bacilosc_e", "cultura_es", 
              "situa_ence", "forma", "agravaids", "agravalcoo", "agravdiabe", "test_molec"), as.factor) %>% 
  mutate(
    # diag_yrmo = floor_date(as_date(dt_diag), "month"), 
         diag_yr = floor_date(as_date(dt_diag), "year"),
         diag_qrt = floor_date(as_date(dt_diag), "quarter"),
         # init_yr = floor_date(as_date(dt_inic_tr), "year"),
         # init_yrmo = floor_date(as_date(dt_inic_tr), "month"),
         # close_yrmo = floor_date(as_date(dt_encerra), "month")
         ) %>% 
  rename("id_municip_not" = "id_municip", 
         "id_municip" = "id_mn_resi") %>% 
  filter(diag_yr >= "2014-01-01" & diag_yr < "2020-01-01")

sinan_xpert$diag_yr <- as.numeric(year(sinan_xpert$diag_yr)) # so it filters


# create age cat 
sinan_xpert$age_cat <- case_when(sinan_xpert$age > 0 & sinan_xpert$age < 5 ~ "0-4",
                           sinan_xpert$age >= 5 & sinan_xpert$age < 15 ~ "5-14",
                           sinan_xpert$age >= 15 & sinan_xpert$age < 25 ~ "15-24",
                           sinan_xpert$age >= 25 & sinan_xpert$age < 35 ~ "25-34",
                           sinan_xpert$age >= 35 & sinan_xpert$age < 45 ~ "35-44",
                           sinan_xpert$age >= 45 & sinan_xpert$age < 55 ~ "45-54",
                           sinan_xpert$age >= 55 & sinan_xpert$age < 65 ~ "55-64", 
                           sinan_xpert$age >= 65 ~ "65+")

sinan_xpert$age_cat <- factor(sinan_xpert$age_cat, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"))



# organize closure codes 
sinan_xpert$situa_ence <- case_when(sinan_xpert$situa_ence %in% c(" 1", "01", "1") ~ "01", 
                              sinan_xpert$situa_ence %in% c(" 2", "02", "2", "-2") ~ "02",
                              sinan_xpert$situa_ence %in% c(" 3", "03", "3", "-3") ~ "03", 
                              sinan_xpert$situa_ence %in% c(" 4", "04", "4") ~ "04",
                              sinan_xpert$situa_ence %in% c(" 5", "05", "5") ~ "05",
                              sinan_xpert$situa_ence %in% c(" 6", "06", "6") ~ "06",
                              sinan_xpert$situa_ence %in% c(" 7", "07", "7") ~ "07",
                              sinan_xpert$situa_ence %in% c(" 8", "08", "8") ~ "08",
                              sinan_xpert$situa_ence %in% c(" 9", "09", "9") ~ "09", 
                              sinan_xpert$situa_ence %in% c("10") ~ "10", 
                              sinan_xpert$situa_ence %in% c("", "NA") ~ NA) 

sinan_xpert$situa_ence <- factor(sinan_xpert$situa_ence)





# municipality/micro-region characteristics -------------------------------

# add state name
state_name <- read_xlsx("data/StateCodes.xlsx")
sinan_xpert <- left_join(sinan_xpert, state_name %>% select(sg_uf, NAME_1, NAME_2), by = "sg_uf") %>% rename(state_nm = NAME_1, region_nm = NAME_2)



# add micro-region identifier 
mun_micro <- read.csv("data/micro_region/mun_micro_macro_2019.csv")
sinan_xpert <- left_join(sinan_xpert, mun_micro %>% select(codufmun, uf, cod_micro), by = c("id_municip" = "codufmun"))  %>% 
  rename(id_micro = cod_micro)



# add municipality names and population data
mun_population <- read_excel("data/MunicipalCodes.xlsx") %>% 
  rename(id_municip = cod.6.2010, id_state = cod.sg.2010, municip_name = "Município 2010") %>% 
  mutate(
    id_municip = as.numeric(id_municip), 
    id_state = as.numeric(id_state),
    mun_pop_2010 = as.numeric(gsub("\\.", "", pop.tot.2010)), 
    mun_pct_urban = as.numeric(gsub("\\.", "", pop.urb.2010))/as.numeric(gsub("\\.", "", pop.tot.2010)))

# make urbanicity categorical
mun_population$mun_urban_cat <- cut(mun_population$mun_pct_urban, 
                                            breaks = quantile(mun_population$mun_pct_urban, probs = 0:5/5), 
                                            labels = FALSE, 
                                            include.lowest = TRUE)

sinan_xpert <- left_join(sinan_xpert, mun_population %>% select(id_municip, mun_pop_2010, municip_name, mun_pct_urban, mun_urban_cat), by = c("id_municip"))

# clean municip name
sinan_xpert$municip_name <- tolower(sinan_xpert$municip_name)





# aggregate micro-region population 
micro_population <- left_join(mun_population, mun_micro %>% 
                                  mutate(codufmun = as.numeric(as.character(codufmun))), 
                                by = c("id_municip" = "codufmun")) %>% 
  group_by(cod_micro) %>% 
  mutate(
    mic_pop_2010 = sum(as.numeric(gsub("\\.", "", pop.tot.2010))), 
    mic_pct_urban = sum(as.numeric(gsub("\\.", "", pop.urb.2010)))/sum(as.numeric(gsub("\\.", "", pop.tot.2010)))) %>% 
  select(cod_micro, mic_pop_2010, mic_pct_urban) %>% 
  unique()



# make urbanicity categorical
micro_population$mic_urban_cat <- cut(micro_population$mic_pct_urban, 
                          breaks = quantile(micro_population$mic_pct_urban, probs = 0:5/5), 
                          labels = FALSE, 
                          include.lowest = TRUE)


sinan_xpert <- left_join(sinan_xpert, micro_population %>% select(cod_micro, mic_pop_2010, mic_pct_urban, mic_urban_cat), by = c("id_micro" = "cod_micro"))





# add health unit type data 
source(here::here("code/R/00_add_health_unit_type.R"))

sinan_xpert <- merged_health_unit %>% select(!c("state", "municip_cod", "uni_tp_cod", "uni_tp", "year", "first_level", "second_level_tb", "third_level_tb", "second_level", "third_level", "others"))

colnames(sinan_xpert) <- tolower(colnames(sinan_xpert))








# add FHS coverage (municipality and micro-region)
source(here::here("code/R/00_load_fhs_coverage.R"))

## add in data by municipality
sinan_xpert <- left_join(sinan_xpert, fhs %>% select(id_municip, year, mun_fhs_num_teams), by = c("id_municip", "diag_yr" = "year"))
  
sinan_xpert <- sinan_xpert %>% mutate(mun_fhs_per_cap = (mun_fhs_num_teams/mun_pop_2010)*4000)

sinan_xpert$mun_fhs_cat <- cut(sinan_xpert$mun_fhs_per_cap,
                               breaks = quantile(sinan_xpert$mun_fhs_per_cap, probs = 0:5/5, na.rm = TRUE), 
                               labels = FALSE, 
                               include.lowest = TRUE)


## calculate and add for micro-region
mic_fhs_cov <- sinan_xpert %>% 
  select(id_micro, id_municip, mun_fhs_num_teams, mic_pop_2010) %>% 
  unique() %>% 
  group_by(id_micro) %>% 
  mutate(mic_fhs_num_teams = mean(mun_fhs_num_teams, na.rm = TRUE)) %>% 
  select(-c(id_municip, mun_fhs_num_teams)) %>% 
  unique() %>% 
  mutate(mic_fhs_per_cap = (mic_fhs_num_teams/mic_pop_2010)*4000)


mic_fhs_cov$mic_fhs_cat <- cut(mic_fhs_cov$mic_fhs_per_cap,
                               breaks = quantile(mic_fhs_cov$mic_fhs_per_cap, probs = 0:5/5, na.rm = TRUE), 
                               labels = FALSE, 
                               include.lowest = TRUE)


sinan_xpert <- left_join(sinan_xpert, mic_fhs_cov %>% select(mic_fhs_per_cap, mic_fhs_cat), by = c("id_micro"))












# add prison (merge based on municip name)
source(here::here("code/R/00_add_prison.R"))

sinan_xpert <- left_join(sinan_xpert, prison_merge, by = c("municip_name" = "municip_nm", "diag_yr" = "year")) %>% 
  mutate(mun_has_prison = if_else(has_prison == 1, 1, 0)) %>% 
  select(-has_prison)


# from prison merge - 1125 municipalities have prisons 
# from sinan - 1114 municipalities in sinan are also among the list of municipalities from prison merge that have prisons
# filter out municipalities with prison from sinan and see whether or not has prison = 1: All of them do not have prisons, despite merge working well 

mic_has_prison <- sinan_xpert %>% 
  select(municip_name, id_micro, mun_has_prison) %>% 
  unique() %>% 
  group_by(id_micro) %>% 
  mutate(mic_has_prison = if_else(sum(mun_has_prison) > 0, 1, 0)) %>% 
  select(-c(municip_name, mun_has_prison)) %>% 
  unique() 

sinan_xpert <- left_join(sinan_xpert, mic_has_prison , by = c("id_micro")) 



# add BF coverage (avg per month in 2018)
## by municipality 
source(here::here("code/R/00_add_bf_coverage.R"))

sinan_xpert <- left_join(sinan_xpert, bf_merge %>% 
                           ungroup() %>% 
                           select(id_municip, mun_avg_persons_bf), 
                         by = c("id_municip")) %>% 
  mutate(mun_pct_bf = mun_avg_persons_bf/mun_pop_2010, 
         mun_bf_cat = cut(mun_pct_bf,
           breaks = quantile(mun_pct_bf, probs = 0:5/5, na.rm = TRUE), 
           labels = FALSE, 
           include.lowest = TRUE))


## by microregion 
mic_bf <- sinan_xpert %>% 
  select(id_micro, mun_avg_persons_bf, mic_pop_2010) %>% 
  unique() %>% 
  group_by(id_micro) %>% 
  mutate(mic_avg_person_bf = sum(mun_avg_persons_bf, na.rm = TRUE), 
         mic_pct_bf = mic_avg_person_bf/mic_pop_2010) %>% 
  select(-mun_avg_persons_bf) %>% 
  unique()


mic_bf$mic_bf_cat <- cut(mic_bf$mic_pct_bf,
                      breaks = quantile(mic_bf$mic_pct_bf, probs = 0:5/5, na.rm = TRUE), 
                      labels = FALSE, 
                      include.lowest = TRUE)


sinan_xpert <- left_join(sinan_xpert, mic_bf %>% select(id_micro, mic_bf_cat),  by = c("id_micro"))




# select vars of interest
sinan_xpert <- sinan_xpert %>% select(c(
  # Patient location identifiers
  "id_unidade", "nu_notific", "dt_notific", "nu_ano", "sg_uf", "sg_uf_not", "state_nm", "id_micro", "id_municip_not", "id_municip", "municip_name", 
  # Patient characteristics 
  "dt_diag", "diag_yr", "diag_qrt", "dt_inic_tr", "id_unid_at", "dt_nasc", "age","age_cat","health_unit", "cs_sexo", "tratamento", "agravaids", "hiv", "situa_ence", "test_molec",  
  # Municipality/Micro characteristics - Urbanicity
  "mun_pop_2010", "mic_pop_2010", "mun_pct_urban", "mic_pct_urban", "mun_urban_cat", "mic_urban_cat", 
  # Municipality/Micro characteristics - Primary health care coverage 
  "mun_fhs_num_teams", "mic_fhs_num_teams", "mun_fhs_per_cap", "mic_fhs_per_cap", "mic_fhs_cat", "mic_fhs_cat", 
  # Municipality/Micro characteristics - Poverty proxy
  "mun_pct_bf", "mic_pct_bf", "mun_bf_cat", "mic_bf_cat", 
  # Municipality/Micro characteristics - Prison 
  "mun_has_prison", "mic_has_prison"
  ))
                          
# write file --------------------------------------------------------------
save(sinan_xpert, file = "data/sinan_xpert.Rdata")


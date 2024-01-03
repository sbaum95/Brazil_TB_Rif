# Author: Sarah Baum
# Created: 2023-12-24
# Updated: 
# Description: Loads and merges sinan with covariate data and cleans
# -- Updated (09-26): Merge based on patient municipality of residence, not municipality of diagnosis



source(here::here("code/dependencies.R"))




# load and merge data -----------------------------------------------------

# load SINAN data
sinan <- read_dta("data/fim_abr_2023_no name.dta")
sinan_tmp <- sinan

# preliminary cleaning to merge covariate data and resitrict to 2014-2019
sinan_tmp %<>% 
  mutate(sg_uf = as.factor(sg_uf), 
         id_mn_resi = as.factor(id_mn_resi),
         diag_yr = floor_date(as_date(dt_diag), "year")) %>% 
  filter(diag_yr >= "2014-01-01" & diag_yr < "2020-01-01")

sinan_tmp$diag_yr <-  as.numeric(year(sinan_tmp$diag_yr))



# load and merge location id and population 
source(here::here("code/R/load_location_and_population.R"))

sinan_tmp <- left_join(sinan_tmp, state_name, by = "sg_uf")
sinan_tmp %<>% left_join(mun_pop %>% select(id_mun, mun_pop_2010, name_mun, mun_pct_urban, mun_urban_cat), by = c("id_mn_resi" = "id_mun"))





# load and merge health facility type
source(here::here("code/R/00_add_health_unit_type.R"))

sinan_tmp <- merged_health_unit %>% select(!c("state", "municip_cod", "uni_tp_cod", "uni_tp", "year", "first_level", "second_level_tb", "third_level_tb", "second_level", "third_level", "others"))



# load fhs coverage (number of FHS teams per 4,000 people)
source(here::here("code/R/00_load_fhs_coverage.R"))



# load prison data
source(here::here("code/R/00_add_prison.R"))



# load bolsa familia coverage
source(here::here("code/R/00_add_bf_coverage.R"))





# cleaning ----------------------------------------------------------------
sinan_tmp %<>%  
  mutate(name_mun = tolower(name_mun), 
         mun_urban_cat = as.factor(mun_urban_cat), 
         mun_bf_cat = as.factor(mun_bf_cat), 
         mun_has_prison = relevel(as.factor(mun_has_prison), ref = "1")) %>% 
  mutate_at(c("dt_notific", "dt_diag", "dt_encerra"), as.Date, format = "%Y-%m-%d") %>% 
  mutate(age = floor(as.numeric((dt_diag - dt_nasc)/365))) %>% 
  mutate_at(c("cs_sexo", "tratamento", "situa_ence", "agravaids", "agravalcoo", "test_molec"), as.factor) %>% 
  mutate(
    age = floor(as.numeric((dt_diag - dt_nasc)/365)), # needs to be nu_idade_n
    diag_yr = floor_date(as_date(dt_diag), "year"),
    diag_qrt = floor_date(as_date(dt_diag), "quarter"),
  ) 



# create age
age 
# if > 4000, nu_idade_n - 4000
# if < 4000, 0-5 
age <= 4004, "0-4"
age >= 4005, "5-14"


# remove NA or 3 digit codes
# check against dt_nasc 
# what share of dt_nasc that are kids have jobs? 



# create age cat 
sinan_tmp$age_cat %<>%  case_when(age > 0 & age < 5 ~ "0-4",
                                  age >= 5 & age < 15 ~ "5-14",
                                  age >= 15 & age < 25 ~ "15-24",
                                  age >= 25 & age < 35 ~ "25-34",
                                  age >= 35 & age < 45 ~ "35-44",
                                  age >= 45 & age < 55 ~ "45-54",
                                  age >= 55 & age < 65 ~ "55-64", 
                                  age >= 65 ~ "65+")

sinan_tmp$age_cat <- factor(sinan_tmp$age_cat, levels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"))


# clean closure variable
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










# select variables of interest 
sinan_tmp %>% select(c(
  # Patient location identifiers - Municipality
  "id_unidade", "nu_ano", "sg_uf", "state_nm", "id_municip_not", "id_municip", "municip_name", 
  # Patient location identifiers - Micro-region
  "id_micro", 
  # Patient characteristics 
  "dt_diag", "diag_yr", "diag_qrt", "id_unid_at", "dt_nasc", "age","age_cat","health_unit", "cs_sexo", "tratamento", "agravaids", "hiv", "situa_ence", "test_molec",  
  # Municipality characteristics 
  "mun_pop_2010", "mun_pct_urban", "mun_urban_cat", "mun_fhs_num_teams", "mun_fhs_per_cap", "mun_fhs_cat", "mun_pct_bf", "mun_bf_cat", "mun_has_prison", 
  # Micro-region characteristics 
  # "mic_pop_2010", "mic_pct_urban", "mic_urban_cat", "mic_fhs_num_teams", "mic_fhs_per_cap", "mic_fhs_cat", "mic_pct_bf", "mic_bf_cat", "mic_has_prison"
))





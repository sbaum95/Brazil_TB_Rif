# Author: Sarah Baum
# Created: 2023-06-15
# Updated: 
# Description: Loads sinan_xpert$ .dta files and cleans 

source(here::here("code/dependencies.R"))

# load data ---------------------------------------------------------------
sinan_xpert <- read_dta("data/fim_abr_2023_no name.dta") %>% 
  # select(dt_notific, nu_ano, sg_uf_not, id_municip, id_regiona, id_unidade, 
  #        dt_diag, dt_nasc, nu_idade_n, cs_sexo, cs_raca, sg_uf, id_mn_resi, 
  #        tratamento, raiox_tora, forma, agravaids, hiv, agravalcoo, agravdiabe,
  #        bacilosc_e, cultura_es, histopatol, dt_inic_tr, rifampicin, isoniazida, 
  #        estreptomi, etionamida, outras, sg_uf_at, id_munic_a, bacilosc_1, 
  #        bacilosc_2, bacilosc_3, bacilosc_4, bacilosc_5, bacilosc_6, situa_ence, 
  #        dt_encerra, test_molec
  #        # test_sensi, 
  #        # bac_apos_6, 
  #        # transf
  # ) %>% 
  mutate_at(c("nu_ano", "sg_uf_not", "id_municip", "id_regiona", "id_unidade", "nu_idade_n", "sg_uf", "id_mn_resi"), as.numeric) %>% 
  mutate_at(c("dt_notific", "dt_diag", "dt_encerra"), as.Date, format = "%Y-%m-%d") %>% 
  mutate(age = floor(as.numeric((dt_diag - dt_nasc)/365))) %>% # create age variable 
  mutate_at(c("cs_sexo", "cs_raca", "tratamento", "raiox_tora", "bacilosc_e", "cultura_es", 
              "situa_ence", "forma", "agravaids", "agravalcoo", "agravdiabe", "test_molec"), as.factor) %>% 
  mutate(diag_yrmo = floor_date(as_date(dt_diag), "month"), 
         diag_yr = floor_date(as_date(dt_diag), "year"),
         init_yr = floor_date(as_date(dt_inic_tr), "year"),
         init_yrmo = floor_date(as_date(dt_inic_tr), "month"),
         close_yrmo = floor_date(as_date(dt_encerra), "month")) %>% 
  filter(diag_yr >= "2014-01-01")

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


# create age cat 
sinan_xpert$age_cat <- case_when(sinan_xpert$age > 0 & sinan_xpert$age < 5 ~ "0-4",
                                 sinan_xpert$age >= 5 & sinan_xpert$age < 15 ~ "5-14",
                                 sinan_xpert$age >= 15 & sinan_xpert$age < 25 ~ "15-24",
                                 sinan_xpert$age >= 25 & sinan_xpert$age < 35 ~ "25-34",
                                 sinan_xpert$age >= 35 & sinan_xpert$age < 45 ~ "35-44",
                                 sinan_xpert$age >= 45 & sinan_xpert$age < 55 ~ "45-54",
                                 sinan_xpert$age >= 55 & sinan_xpert$age < 65 ~ "55-64", 
                                 sinan_xpert$age >= 65 ~ "65+")

sinan_xpert$age_cat <- factor(sinan_xpert$age_cat)

# add state names
state_name <- read_xlsx("data/StateCodes.xlsx")

sinan_xpert <- left_join(sinan_xpert, state_name %>% 
                        select(sg_uf, NAME_1, NAME_2), by = "sg_uf") %>% 
  rename(state_nm = NAME_1, region_nm = NAME_2)




# add population data 
population <- read_excel("data/MunicipalCodes.xlsx") %>% 
  select(cod.6.2010, cod.sg.2010, pop.tot.2010, "Município 2010") %>% 
  rename(id_municip = cod.6.2010, id_state = cod.sg.2010, municip_name = "Município 2010", pop_total_2010 = pop.tot.2010) %>% 
  mutate(id_municip = as.numeric(id_municip), 
         pop_total_2010 = as.numeric(gsub("\\.", "", pop_total_2010)),
         id_state = as.numeric(id_state)) %>% 
  group_by(id_state) %>% 
  summarize(state_pop = sum(pop_total_2010))

sinan_xpert <- left_join(sinan_xpert, population, by = c("sg_uf" = "id_state"))

# add municipality names 
population <- read_excel("data/MunicipalCodes.xlsx") %>% 
  select(cod.6.2010, cod.sg.2010, pop.tot.2010, "Município 2010") %>% 
  rename(id_municip = cod.6.2010, id_state = cod.sg.2010, municip_name = "Município 2010", pop_total_2010 = pop.tot.2010) %>% 
  mutate(id_municip = as.numeric(id_municip), 
         pop_total_2010 = as.numeric(gsub("\\.", "", pop_total_2010)),
         id_state = as.numeric(id_state)) %>% 
  select(id_municip, municip_name)

sinan_xpert <- left_join(sinan_xpert, population, by = "id_municip") 
# sinan_xpert$id_municip <- as.factor(sinan_xpert$id_municip)


# add health unit type data (Source: Do)
source(here::here("code/pull_health_unit.R"))




# write file --------------------------------------------------------------
save(sinan_xpert, file = "data/sinan_xpert.Rdata")

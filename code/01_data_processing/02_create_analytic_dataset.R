# Author: Sarah Baum
# Created: 2023-08-21
# Updated: 2024-03-24

# Description/Decisions: Takes sinan and prepare it for model analysis



source(here::here("code/dependencies.R"))

load("data/sinan_xpert.Rdata")




# load and clean individual-level observations -------------------------------------------
df <- sinan_xpert %>%
  mutate(
    state = as.factor(sg_uf),
    id_municip = as.factor(id_municip), 
    sex = if_else(cs_sexo == "M", "Male", 
                  if_else(cs_sexo == "F", "Female", NA)) %>% as.factor(), 
    # Defined by HIV test result; If NA, in progress or not performed, 
    # then by self-report AGRAVAIDS
    hiv_status = if_else(hiv == "1", "Positive",
                         if_else(hiv == "2", "Negative", 
                                 if_else(agravaids == "1", "Positive", 
                                         if_else(agravaids == "2", "Negative", "Missing")))) %>% as.factor(),
    age_cat = if_else(is.na(age_cat), "Missing", age_cat) %>% as.factor(), 
    health_unit = if_else(is.na(health_unit), "Missing", health_unit) %>% as.factor(), 
    tested = if_else(test_molec %in% c("1", "2"), "Tested", "Not Tested") %>% as.factor(),
    result = if_else(test_molec %in% c("2"), "Positive", 
                     if_else(test_molec %in% c("1"), "Negative", "Missing")) %>% as.factor(),
    mun_urban_cat = if_else(is.na(mun_urban_cat), "Missing", mun_urban_cat) %>% as.factor(), 
    mun_bf_cat = if_else(is.na(mun_bf_cat), "Missing", mun_bf_cat) %>% as.factor(),  
    mun_fhs_cat = if_else(is.na(mun_fhs_cat), "Missing", mun_fhs_cat) %>% as.factor(),
    
    # Homeless
    pop_rua = if_else(pop_rua == 1, "Yes", 
                      if_else(pop_rua == 2, "No", "Missing")) %>% as.factor(),
    
    # Incarcerated
    pop_liber = if_else(pop_liber == 1, "Yes", 
                        if_else(pop_liber == 2, "No", "Missing")) %>% as.factor(),
    
    # Smoking
    agravtabac = if_else(agravtabac == 1, "Yes", 
                         if_else(agravtabac == 2, "No", "Missing")) %>% as.factor(),
    # Alcohol
    agravalcoo = if_else(agravalcoo == 1, "Yes", 
                         if_else(agravalcoo == 2, "No", "Missing")) %>% as.factor(),
    # Ilicit drug use
    agravdroga = if_else(agravdroga == 1, "Yes", 
                         if_else(agravdroga == 2, "No", "Missing")) %>% as.factor(),
    # Diabetes
    agravdiabe = if_else(agravdiabe == 1, "Yes", 
                         if_else(agravdiabe == 2, "No", "Missing")) %>% as.factor()
    
    # Education (add in)
    # Immigration state (add in)
  ) %>% 
  filter(diag_qrt < "2020-01-01") %>% 
  select(state, state_nm, id_municip, diag_qrt, diag_yr, tested, result, age, age_cat, sex, tratamento, hiv_status, health_unit, 
         lat, lon, border, 
         mun_urban_cat, mun_has_prison, mun_bf_cat, mun_fhs_cat, pop_imig, 
         pop_rua, pop_liber, agravtabac, agravalcoo, agravdroga, agravdiabe, cs_escol_n) %>% 
  filter(diag_qrt < "2020-01-01") %>% 
  filter(!is.na(state)) %>% 
  filter(!is.na(sex))




## re-level categorical reference categories

df$age_cat <- relevel(as.factor(df$age_cat), ref = "25-34") # 25-34 (largest age cat)
df$sex <- relevel(df$sex, ref = "Female") # female
df$hiv_status <- relevel(df$hiv_status, ref = "Negative")

df$mun_urban_cat <- relevel(as.factor(df$mun_urban_cat), ref = "5") # most urban
df$mun_bf_cat <- relevel(as.factor(df$mun_bf_cat), ref = "1") # lowest bf coverage
df$mun_fhs_cat <- relevel(as.factor(df$mun_fhs_cat), ref = "1") # lowest fhs coverage

df$pop_rua <- relevel(as.factor(df$pop_rua), ref = "No") 
df$pop_liber <- relevel(as.factor(df$pop_liber), ref = "No") 
df$agravtabac <- relevel(as.factor(df$agravtabac), ref = "No") 
df$agravalcoo <- relevel(as.factor(df$agravalcoo), ref = "No") 
df$agravdroga <- relevel(as.factor(df$agravdroga), ref = "No") 
df$agravdiabe <- relevel(as.factor(df$agravdiabe), ref = "No") 


### Order the dates in ascending order
sorted_dates <- sort(unique(df$diag_qrt))

### Create a named vector to store the rank for each date
time <- setNames(1:length(sorted_dates), format(sorted_dates, "%Y-%m-%d"))
dates <- cbind(as.data.frame(sorted_dates), time)
df <- left_join(df, dates, by = c("diag_qrt" = "sorted_dates"))
df$time <- as.numeric(as.character(df$time))




# create individual-level datasets
mdf_new_ind <- df %>% filter(tratamento == "1")

mdf_prev_ind <- df %>% 
  filter(tratamento %in% c("2", "3")) %>% 
  mutate(tratamento = if_else(tratamento == "2", 0, 1) %>% as.factor())







# Save analytic datasets --------------------------------------------------
save(mdf_new_ind, file = "data/mdf_new_ind.Rdata")
save(mdf_prev_ind, file = "data/mdf_prev_ind.Rdata")

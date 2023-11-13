# Author: Sarah Baum
# Created: 2023-08-21
# Updated: 
# Description/Decisions: 
# -- Combined learn_GAM.R and draft_stan.R scripts to create clear analytic 
#    datasets for models


source(here::here("code/dependencies.R"))

load("data/sinan_xpert.Rdata")
# Cleaning to dos: 
## -- Remove duplicates
## -- Ages - Remove (age >=0 & age <=100)


# individual-level observations -------------------------------------------
df <- sinan_xpert %>% 
  mutate(
    state = as.factor(sg_uf), 
    
    # define sex 
    sex = if_else(cs_sexo == "M", 1, 
                  if_else(cs_sexo == "F", 0, NA)) %>% as.factor(), 
    
    # define HIV status 
    # First, defined by HIV test result; If NA, in progress or not performed, 
    # then by self-report AGRAVAIDS
    hiv_status = if_else(hiv == "1", 1,
                         if_else(hiv == "2", 0, 
                                 if_else(agravaids == "1", 1, 
                                         if_else(agravaids == "2", 0, NA)))) %>% as.factor(),
    
    # Old definition
    # hiv = if_else(agravaids == 1, 1, 
    #               if_else(agravaids == 2, 0, NA)) %>% as.factor(),
    
    # define Xpert test status
    tested = if_else(test_molec %in% c("1", "2", "3", "4"), 1, 0),
    
    # define RR-TB status
    result = if_else(test_molec %in% c("2"), 1, 
                     if_else(test_molec %in% c("1", "3", "4"), 0, NA)) %>% as.factor()
  ) %>% 
  filter(diag_qrt < "2020-01-01") %>% 
  select(state, state_nm, id_municip, diag_qrt, age, age_cat, sex, tratamento, hiv_status, result, health_unit, 
         # pct_fhs_cov, 
         urban_cat, has_prison, bf_cat) %>% 
  filter(!is.na(state))




## update reference category for age cat to be 25-34 (largest age cat)
df$age_cat <- relevel(df$age_cat, ref = "25-34")

## update reference category for sex cat to be female 
df$sex <- relevel(df$sex, ref = "0")

## update reference category for sex cat to be most rural
df$urban_cat <- relevel(as.factor(df$urban_cat), ref = "1")

## update reference category for sex cat to be lowest bf coverage
df$bf_cat <- relevel(as.factor(df$bf_cat), ref = "1")



## Create time variable (resource: https://nicholasrjenkins.science/post/tidy_time_series/tts_r/)
### Order the dates in ascending order
sorted_dates <- sort(unique(df$diag_qrt))

### Create a named vector to store the rank for each date
time <- setNames(1:length(sorted_dates), format(sorted_dates, "%Y-%m-%d"))
dates <- cbind(as.data.frame(sorted_dates), time)
df <- left_join(df, dates, by = c("diag_qrt" = "sorted_dates"))
df$time <- as.numeric(as.character(df$time)) # make is

## create individual-level datasets
mdf_new_ind <- df %>% filter(tratamento == "1")
mdf_relapse_ind <- df %>% filter(tratamento == "2") 
mdf_reentry_ind <- df %>% filter(tratamento == "3") 

save(mdf_new_ind, file = "data/mdf_new_ind.Rdata")
save(mdf_relapse_ind, file = "data/mdf_relapse_ind.Rdata")
save(mdf_reentry_ind, file = "data/mdf_reentry_ind.Rdata")







# create group-level dataset (hiv, sex, age_cat) ---------------------------------
## new cases 
load("data/mdf_new_ind.Rdata")

mdf_new_grp <- mdf_new_ind %>% 
  # filter(!is.na(hiv_status) & !is.na(age) &  !is.na(sex) & !is.na(state) & !is.na(age_cat)) %>%
  select(-c(diag_qrt, age, tratamento)) %>% 
  group_by(state, state_nm, id_municip, time, age_cat, sex, hiv_status, result, health_unit, 
           # pct_fhs_cov, 
           urban_cat, has_prison, bf_cat) %>% 
  count() %>% 
  pivot_wider(names_from = "result", values_from = "n") %>%
  rename(neg = "0",
         pos = "1",
         miss = `NA`) %>%
  mutate(Negative = if_else(is.na(neg), 0, neg),
         Positive = if_else(is.na(pos), 0, pos),
         Miss = if_else(is.na(miss), 0, miss)) %>% 
  mutate(cases = Negative + Positive + Miss, 
         pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
         obs_pct_positive = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive))
         ) %>% 
  select(-c(miss, neg, pos))


# mdf_new_grp_covs <- mdf_new_ind %>% 
#   # filter(!is.na(hiv_status) & !is.na(age) &  !is.na(sex) & !is.na(state) & !is.na(age_cat)) %>%
#   select(-c(diag_qrt, age, tratamento)) %>% 
#   group_by(state, state_nm, time, age_cat, sex, hiv_status, result, health_unit) %>% 
#   count() %>% 
#   pivot_wider(names_from = "result", values_from = "n") %>%
#   rename(neg = "0",
#          pos = "1",
#          miss = `NA`) %>%
#   mutate(Negative = if_else(is.na(neg), 0, neg),
#          Positive = if_else(is.na(pos), 0, pos),
#          Miss = if_else(is.na(miss), 0, miss)) %>% 
#   mutate(cases = Negative + Positive + Miss, 
#          pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
#          obs_pct_positive = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive))
#   ) %>% 
#   select(-c(miss, neg, pos))





mdf_new_grp %>% ungroup() %>% summarize(sum(cases)) # check number of cases 

save(mdf_new_grp, file = "data/mdf_new_grp.Rdata")





## relapse
mdf_relapse_grp <- mdf_relapse_ind %>% 
  filter(!is.na(hiv_status) & !is.na(age) &  !is.na(sex) & !is.na(state) & !is.na(age_cat)) %>%
  select(-c(diag_qrt, age, tratamento)) %>% 
  group_by(state, time, age_cat, sex, hiv, result) %>% 
  count() %>% 
  pivot_wider(names_from = "result", values_from = "n") %>%
  rename(neg = "0",
         pos = "1",
         miss = `NA`) %>%
  mutate(Negative = if_else(is.na(neg), 0, neg),
         Positive = if_else(is.na(pos), 0, pos),
         Miss = if_else(is.na(miss), 0, miss)) %>% 
  mutate(pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
         pct_pos = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive)),
         cases = Negative + Positive + Miss)

mdf_relapse_grp %>% ungroup() %>% summarize(sum(cases)) # check number of cases 

save(mdf_relapse_grp, file = "data/mdf_relapse_grp.Rdata")




## re-entry
mdf_reentry_grp <- mdf_reentry_ind %>% 
  filter(!is.na(hiv_status) & !is.na(age) &  !is.na(sex) & !is.na(state) & !is.na(age_cat)) %>%
  select(-c(diag_qrt, age, tratamento)) %>% 
  group_by(state, time, age_cat, sex, hiv, result) %>% 
  count() %>% 
  pivot_wider(names_from = "result", values_from = "n") %>%
  rename(neg = "0",
         pos = "1",
         miss = `NA`) %>%
  mutate(Negative = if_else(is.na(neg), 0, neg),
         Positive = if_else(is.na(pos), 0, pos),
         Miss = if_else(is.na(miss), 0, miss)) %>% 
  mutate(pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
         pct_pos = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive)),
         cases = Negative + Positive + Miss)

mdf_reentry_grp %>% ungroup() %>% summarize(sum(cases)) # check number of cases 

save(mdf_reentry_grp, file = "data/mdf_reentry_grp.Rdata")









# Author: Sarah Baum
# Created: 2023-08-21
# Updated: 2023-11-13
# Description/Decisions: 
# -- Combined learn_GAM.R and draft_stan.R scripts to create clear analytic 
#    datasets for models
# -- Merged re-entry and relapsed datasets into "previous"
# -- Added in FHS cat



source(here::here("code/dependencies.R"))

load("data/sinan_xpert.Rdata")
# Cleaning to dos: 
## -- Remove duplicates
## -- Ages - Remove (age >=0 & age <=100)



# load and clean individual-level observations -------------------------------------------
df <- sinan_xpert %>% 
  mutate(
    state = as.factor(sg_uf), 
    
    # define sex 
    sex = if_else(cs_sexo == "M", 1, 
                  if_else(cs_sexo == "F", 0, NA)) %>% as.factor(), 
    
    # define HIV status 
    # Defined by HIV test result; If NA, in progress or not performed, 
    # then by self-report AGRAVAIDS
    hiv_status = if_else(hiv == "1", 1,
                         if_else(hiv == "2", 0, 
                                 if_else(agravaids == "1", 1, 
                                         if_else(agravaids == "2", 0, NA)))) %>% as.factor(),
    
    # define Xpert test status
    tested = if_else(test_molec %in% c("1", "2", "3", "4"), 1, 0),
    
    # define RR-TB status - Confirm: Should be res = 2, sens = 1 (should not include )
    result = if_else(test_molec %in% c("2"), 1, 
                     if_else(test_molec %in% c("1", "3", "4"), 0, NA)) %>% as.factor()
  ) %>% 
  filter(diag_qrt < "2020-01-01") %>% 
  dplyr::select(state, state_nm, id_municip, id_micro, diag_qrt, diag_yr, tested, result, age, age_cat, sex, tratamento, hiv_status, health_unit, 
         mun_urban_cat, mun_has_prison, mun_bf_cat, mun_fhs_cat, 
         mic_urban_cat, mic_has_prison, mic_bf_cat, mic_fhs_cat) %>% 
  filter(!is.na(state))





## re-level categorical reference categories
df$age_cat <- relevel(df$age_cat, ref = "25-34") # 25-34 (largest age cat)
df$sex <- relevel(df$sex, ref = "0") # female

df$mun_urban_cat <- relevel(as.factor(df$mun_urban_cat), ref = "5") # most urban
df$mun_bf_cat <- relevel(as.factor(df$mun_bf_cat), ref = "1") # lowest bf coverage
df$mun_fhs_cat <- relevel(as.factor(df$mun_fhs_cat), ref = "1") # lowest fhs coverage


df$mic_urban_cat <- relevel(as.factor(df$mic_urban_cat), ref = "5") # most urban
df$mic_bf_cat <- relevel(as.factor(df$mic_bf_cat), ref = "1") # lowest bf coverage
df$mic_fhs_cat <- relevel(as.factor(df$mic_fhs_cat), ref = "1") # lowest fhs coverage






## Create time variable (resource: https://nicholasrjenkins.science/post/tidy_time_series/tts_r/)
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

  

save(mdf_new_ind, file = "data/mdf_new_ind.Rdata")
save(mdf_prev_ind, file = "data/mdf_prev_ind.Rdata")





# create group-level datasets (hiv, sex, age_cat) ---------------------------------
## new cases 
load("data/mdf_new_ind.Rdata")

mdf_mun_new_grp <- mdf_new_ind %>% 
  # filter(!is.na(hiv_status) & !is.na(age) &  !is.na(sex) & !is.na(state) & !is.na(age_cat)) %>%
  select(-c(diag_qrt, age, tratamento)) %>% 
  group_by(state, state_nm, id_municip, time, age_cat, sex, hiv_status, result, health_unit, 
           mun_urban_cat, mun_has_prison, mun_bf_cat, mun_fhs_cat) %>% 
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

# cross-check number of cases 
mdf_mun_new_grp %>% ungroup() %>% summarize(sum(cases)) 

# save df 
save(mdf_mun_new_grp, file = "data/mdf_mun_new_grp.Rdata")




# create groups at the micro-region level
mdf_mic_new_grp <- mdf_new_ind %>% 
  # filter(!is.na(hiv_status) & !is.na(age) &  !is.na(sex) & !is.na(state) & !is.na(age_cat)) %>%
  select(-c(diag_qrt, age, tratamento)) %>% 
  group_by(state, state_nm, id_micro, time, age_cat, sex, hiv_status, result, health_unit, 
           mic_urban_cat, mic_has_prison, mic_bf_cat, mic_fhs_cat) %>% 
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

# cross-check number of cases 
mdf_mic_new_grp %>% ungroup() %>% summarize(sum(cases)) 

# save df 
save(mdf_mic_new_grp, file = "data/mdf_mic_new_grp.Rdata")





# ## previous 
# ## municipality
# mdf_mun_prev_grp <- mdf_prev_ind %>% 
#   filter(!is.na(hiv_status) & !is.na(age) &  !is.na(sex) & !is.na(state) & !is.na(age_cat)) %>%
#   select(-c(diag_qrt, age, tratamento)) %>% 
#   group_by(state, time, age_cat, sex, hiv, result, mun_urban_cat, mun_has_prison, mun_bf_cat, mun_fhs_cat) %>% 
#   count() %>% 
#   pivot_wider(names_from = "result", values_from = "n") %>%
#   rename(neg = "0",
#          pos = "1",
#          miss = `NA`) %>%
#   mutate(Negative = if_else(is.na(neg), 0, neg),
#          Positive = if_else(is.na(pos), 0, pos),
#          Miss = if_else(is.na(miss), 0, miss)) %>% 
#   mutate(pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
#          pct_pos = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive)),
#          cases = Negative + Positive + Miss)
# 
# # cross-check number of cases 
# mdf_mun_prev_grp %>% ungroup() %>% summarize(sum(cases)) # check number of cases 
# 
# # save df 
# save(mdf_mun_prev_grp, file = "data/mdf_mun_prev_grp.Rdata")
# 
# 
# 
# ## micro-region 
# mdf_mic_prev_grp <- mdf_prev_ind %>% 
#   filter(!is.na(hiv_status) & !is.na(age) &  !is.na(sex) & !is.na(state) & !is.na(age_cat)) %>%
#   select(-c(diag_qrt, age, tratamento)) %>% 
#   group_by(state, time, age_cat, sex, hiv, result, mic_urban_cat, mic_has_prison, mic_bf_cat, mic_fhs_cat) %>% 
#   count() %>% 
#   pivot_wider(names_from = "result", values_from = "n") %>%
#   rename(neg = "0",
#          pos = "1",
#          miss = `NA`) %>%
#   mutate(Negative = if_else(is.na(neg), 0, neg),
#          Positive = if_else(is.na(pos), 0, pos),
#          Miss = if_else(is.na(miss), 0, miss)) %>% 
#   mutate(pct_tested = (Negative + Positive)/(Negative + Positive + Miss), 
#          pct_pos = if_else(is.nan(Positive/(Negative + Positive)), 0, Positive/(Negative + Positive)),
#          cases = Negative + Positive + Miss)
# 
# # cross-check number of cases 
# mdf_mic_prev_grp %>% ungroup() %>% summarize(sum(cases)) # check number of cases 
# 
# # save df 
# save(mdf_mic_prev_grp, file = "data/mdf_mic_prev_grp.Rdata")








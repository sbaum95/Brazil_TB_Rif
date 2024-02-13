source(here::here("code/dependencies.R"))

###########################################################################
###########################################################################
###                                                                     ###
###                              NEW CASES                              ###
###                                                                     ###
###########################################################################
###########################################################################

### load bootstrapped results 
load("output/fits/boot.mun_new_1.Rda")
load("output/fits/boot.mun_new_2.Rda")
load("output/fits/boot.mun_new_3.Rda")

### load data 
load("data/mdf_new_ind.Rdata")

##################################################################
##                        New - National                        ##
##################################################################

### Take the mean for each year and multiply it by the number of new TB cases for that year 
obs_nat_new <- mdf_new_ind %>% 
  group_by(time) %>% 
  summarize(obs_TB_cases = n(), 
            TB_complete = mdf_new_ind %>% 
              group_by(time) %>% 
              filter_at(vars(state, time, sex, hiv_status,
                          age_cat, health_unit, mun_urban_cat,
                          mun_fhs_cat, mun_has_prison, mun_bf_cat),
                     all_vars(!is.na(.))) %>% 
              summarize(TB_complete = n()), 
           obs_RR_cases = sum(result == 1, na.rm = TRUE),
           obs_tested = sum(tested == 1, na.rm = TRUE)) %>% 
  filter(time == TB_complete$time) %>% 
  mutate(obs_TB_complete_cases = TB_complete[[2]]) %>% 
  select(c(time, obs_TB_cases, obs_TB_complete_cases, obs_RR_cases, obs_tested))




### pull estimates from each model 
mod1_nat_new <- boot.mun_new_1[["pred.int_nat"]] %>% 
  rename(mod1_mean = mean, 
         mod1_lci = lci, 
         mod1_hci = hci)
mod2_nat_new <- boot.mun_new_2[["pred.int_nat"]] %>% 
  rename(mod2_mean = mean, 
       mod2_lci = lci, 
       mod2_hci = hci) 

mod3_nat_new <- boot.mun_new_3[["pred.int_nat"]] %>% 
  rename(mod3_mean = mean, 
         mod3_lci = lci, 
         mod3_hci = hci)


## prep modeled data 
mod_nat_new <- left_join(mod1_nat_new %>% select(time, diag_qrt, mod1_mean, mod1_lci, mod1_hci), 
                           mod2_nat_new %>% select(time, mod2_mean, mod2_lci, mod2_hci),
                           by = "time") %>% 
  left_join(., mod3_nat_new %>% select(time, mod3_mean, mod3_lci, mod3_hci), by = "time") %>% 
  pivot_longer(cols = c(contains("mean"), contains("lci"), contains("hci")),
               names_to = c("model", ".value"), 
               names_sep = "_") %>% 
  filter(!is.na(mean)) %>% 
  rename(mod_mean = mean,
         mod_hci = hci, 
         mod_lci = lci)
  

## combine with observed data 
nat_new <- left_join(obs_nat_new, mod_nat_new, by = "time") %>% 
  mutate(mod_RR_cases = obs_TB_cases * mod_mean, 
         mod_RR_lci = obs_TB_cases * mod_lci, 
         mod_RR_hci = obs_TB_cases * mod_hci)


save(nat_new, file = "output/projected_nat_new.Rdata")



ggplot() + 
  geom_line(data = nat_new %>% filter(model == "mod1"), aes(x = diag_qrt, y = obs_RR_cases)) + 
  geom_line(data = nat_new, aes(x = diag_qrt, y = mod_RR_cases, group = model))




#################################################################
##                    New - National (Year)                    ##
#################################################################
### Take the mean for each year and multiply it by the number of new TB cases for that year 
obs_nat_new.yr <- mdf_new_ind %>% 
  group_by(diag_yr) %>% 
  summarize(obs_TB_cases = n(), 
            TB_complete = mdf_new_ind %>% 
              group_by(diag_yr) %>% 
              filter_at(vars(state, diag_yr, sex, hiv_status,
                             age_cat, health_unit, mun_urban_cat,
                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                        all_vars(!is.na(.))) %>% 
              summarize(TB_complete = n()), 
            obs_RR_cases = sum(result == 1, na.rm = TRUE),
            obs_tested = sum(tested == 1, na.rm = TRUE)) %>% 
  filter(diag_yr == TB_complete$diag_yr) %>% 
  mutate(obs_TB_complete_cases = TB_complete[[2]]) %>% 
  select(c(diag_yr, obs_TB_cases, obs_TB_complete_cases, obs_RR_cases, obs_tested))

### pull estimates from each model 
mod1_nat_new.yr <- boot.mun_new_1[["pred.int_nat_year"]] %>% 
  rename(mod1_mean = mean, 
         mod1_lci = lci, 
         mod1_hci = hci)

mod2_nat_new.yr <- boot.mun_new_2[["pred.int_nat_year"]] %>% 
  rename(mod2_mean = mean, 
         mod2_lci = lci, 
         mod2_hci = hci) 

mod3_nat_new.yr <- boot.mun_new_3[["pred.int_nat_year"]] %>% 
  rename(mod3_mean = mean, 
         mod3_lci = lci, 
         mod3_hci = hci)


## prep modeled data 
mod_nat_new.yr <- left_join(mod1_nat_new.yr %>% select(year, mod1_mean, mod1_lci, mod1_hci), 
                         mod2_nat_new.yr %>% select(year, mod2_mean, mod2_lci, mod2_hci),
                         by = "year") %>% 
  left_join(., mod3_nat_new.yr %>% select(year, mod3_mean, mod3_lci, mod3_hci), by = "year") %>% 
  pivot_longer(cols = c(contains("mean"), contains("lci"), contains("hci")),
               names_to = c("model", ".value"), 
               names_sep = "_") %>% 
  filter(!is.na(mean)) %>% 
  rename(mod_mean = mean,
         mod_hci = hci, 
         mod_lci = lci)


## combine with observed data 
nat_new.yr <- left_join(obs_nat_new.yr, mod_nat_new.yr, by = c("diag_yr"="year")) %>% 
  mutate(mod_RR_cases = obs_TB_cases * mod_mean, 
         mod_RR_lci = obs_TB_cases * mod_lci, 
         mod_RR_hci = obs_TB_cases * mod_hci)


save(nat_new.yr, file = "output/projected_nat_new.yr.Rdata")



ggplot() + 
  geom_line(data = nat_new %>% filter(model == "mod1"), aes(x = diag_qrt, y = obs_RR_cases)) + 
  geom_line(data = nat_new, aes(x = diag_qrt, y = mod_RR_cases, group = model))






#################################################################
##                         New - State                         ##
#################################################################

obs_state_new <- mdf_new_ind %>% 
group_by(time, state, state_nm, diag_qrt) %>% 
  summarize(obs_TB_cases = n(), 
            TB_complete = mdf_new_ind %>% 
              group_by(time, state) %>% 
              filter_at(vars(state, time, sex, hiv_status,
                             age_cat, health_unit, mun_urban_cat,
                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                        all_vars(!is.na(.))) %>% 
              summarize(TB_complete = n()), 
            obs_RR_cases = sum(result == 1, na.rm = TRUE),
            obs_tested = sum(tested == 1, na.rm = TRUE)) %>% 
  filter(time == TB_complete[[1]] & state == TB_complete[[2]]) %>% 
  mutate(obs_TB_complete_cases = TB_complete[[3]]) %>% 
  select(c(time, state, state_nm, diag_qrt, obs_TB_cases, obs_TB_complete_cases, obs_RR_cases, obs_tested))


### pull estimates from each model 
mod1_state_new <- boot.mun_new_1[["pred.int_state"]] %>% 
  rename(mod1_mean = mean, 
         mod1_lci = lci, 
         mod1_hci = hci)
mod2_state_new <- boot.mun_new_2[["pred.int_state"]] %>% 
  rename(mod2_mean = mean, 
         mod2_lci = lci, 
         mod2_hci = hci) 

mod3_state_new <- boot.mun_new_3[["pred.int_state"]] %>% 
  rename(mod3_mean = mean, 
         mod3_lci = lci, 
         mod3_hci = hci)


## prep modeled data 
mod_state_new <- left_join(mod1_state_new %>% select(time, state, mod1_mean, mod1_lci, mod1_hci), 
                         mod2_state_new %>% select(time, state, mod2_mean, mod2_lci, mod2_hci),
                         by = c("time", "state")) %>% 
  left_join(., mod3_state_new %>% select(time, state, mod3_mean, mod3_lci, mod3_hci), by = c("time", "state")) %>% 
  pivot_longer(cols = c(contains("mean"), contains("lci"), contains("hci")),
               names_to = c("model", ".value"), 
               names_sep = "_") %>% 
  filter(!is.na(mean)) %>% 
  rename(mod_mean = mean,
         mod_hci = hci, 
         mod_lci = lci)


## combine with observed data 
state_new <- left_join(obs_state_new, mod_state_new, by = c("time", "state")) %>% 
  mutate(mod_RR_cases = obs_TB_cases * mod_mean, 
         mod_RR_lci = obs_TB_cases * mod_lci, 
         mod_RR_hci = obs_TB_cases * mod_hci)
                          

## save
save(state_new, file = "output/projected_state_new.Rdata")



##################################################################
##                   New Cases - State (Year)                   ##
##################################################################
obs_state_new.yr <- mdf_new_ind %>% 
  group_by(state, state_nm, diag_yr) %>% 
  summarize(obs_TB_cases = n(), 
            TB_complete = mdf_new_ind %>% 
              group_by(state, state_nm, diag_yr) %>% 
              filter_at(vars(state, time, sex, hiv_status,
                             age_cat, health_unit, mun_urban_cat,
                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                        all_vars(!is.na(.))) %>% 
              summarize(TB_complete = n()), 
            obs_RR_cases = sum(result == 1, na.rm = TRUE),
            obs_tested = sum(tested == 1, na.rm = TRUE)) %>% 
  filter(diag_yr == TB_complete[[3]] & state == TB_complete[[1]]) %>% 
  mutate(obs_TB_complete_cases = TB_complete[[4]]) %>% 
  select(c(state, state_nm, diag_yr, obs_TB_cases, obs_RR_cases, obs_tested, obs_TB_complete_cases))


### pull estimates from each model 
mod1_state_new.yr <- boot.mun_new_1[["pred.int_state_year"]] %>% 
  rename(mod1_mean = mean, 
         mod1_lci = lci, 
         mod1_hci = hci)

mod2_state_new.yr <- boot.mun_new_2[["pred.int_state_year"]] %>% 
  rename(mod2_mean = mean, 
         mod2_lci = lci, 
         mod2_hci = hci) 

mod3_state_new.yr <- boot.mun_new_3[["pred.int_state_year"]] %>% 
  rename(mod3_mean = mean, 
         mod3_lci = lci, 
         mod3_hci = hci)


## prep modeled data 
mod_state_new.yr <- left_join(mod1_state_new.yr %>% select(year, state, mod1_mean, mod1_lci, mod1_hci), 
                           mod2_state_new.yr %>% select(year, state, mod2_mean, mod2_lci, mod2_hci),
                           by = c("year", "state")) %>% 
  left_join(., mod3_state_new.yr %>% select(year, state, mod3_mean, mod3_lci, mod3_hci), by = c("year", "state")) %>% 
  pivot_longer(cols = c(contains("mean"), contains("lci"), contains("hci")),
               names_to = c("model", ".value"), 
               names_sep = "_") %>% 
  filter(!is.na(mean)) %>% 
  rename(mod_mean = mean,
         mod_hci = hci, 
         mod_lci = lci)


## combine with observed data 
state_new.yr <- left_join(obs_state_new.yr, mod_state_new.yr, by = c("diag_yr" = "year", "state")) %>% 
  mutate(mod_RR_cases = obs_TB_cases * mod_mean, 
         mod_RR_lci = obs_TB_cases * mod_lci, 
         mod_RR_hci = obs_TB_cases * mod_hci)


## save
save(state_new.yr, file = "output/projected_state_new.yr.Rdata")








############################################################################
############################################################################
###                                                                      ###
###                            PREVIOUS CASES                            ###
###                                                                      ###
############################################################################
############################################################################

### load bootstrapped results 
load("output/fits/boot.mun_prev_1.Rda")
load("output/fits/boot.mun_prev_2.Rda")
load("output/fits/boot.mun_prev_3.Rda")

### load data 
load("data/mdf_prev_ind.Rdata")

#################################################################
##                       Prev - National                       ##
#################################################################

### Take the mean for each year and multiply it by the number of prev TB cases for that year 
obs_nat_prev <- mdf_prev_ind %>% 
  group_by(time) %>% 
  summarize(obs_TB_cases = n(), 
            TB_complete = mdf_prev_ind %>% 
              group_by(time) %>% 
              filter_at(vars(state, time, sex, hiv_status, tratamento, 
                             age_cat, health_unit, mun_urban_cat,
                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                        all_vars(!is.na(.))) %>% 
              summarize(TB_complete = n()), 
            obs_RR_cases = sum(result == 1, na.rm = TRUE),
            obs_tested = sum(tested == 1, na.rm = TRUE)) %>% 
  filter(time == TB_complete$time) %>% 
  mutate(obs_TB_complete_cases = TB_complete[[2]]) %>% 
  select(c(time, obs_TB_cases, obs_TB_complete_cases, obs_RR_cases, obs_tested))


### pull estimates from each model 
mod1_nat_prev <- boot.mun_prev_1[["pred.int_nat"]] %>% 
  rename(mod1_mean = mean, 
         mod1_lci = lci, 
         mod1_hci = hci)

mod2_nat_prev <- boot.mun_prev_2[["pred.int_nat"]] %>% 
  rename(mod2_mean = mean, 
         mod2_lci = lci, 
         mod2_hci = hci) 

mod3_nat_prev <- boot.mun_prev_3[["pred.int_nat"]] %>% 
  rename(mod3_mean = mean, 
         mod3_lci = lci, 
         mod3_hci = hci)


## prep modeled data 
mod_nat_prev <- left_join(mod1_nat_prev %>% select(time, mod1_mean, mod1_lci, mod1_hci), 
                         mod2_nat_prev %>% select(time, mod2_mean, mod2_lci, mod2_hci),
                         by = "time") %>% 
  left_join(., mod3_nat_prev %>% select(time, mod3_mean, mod3_lci, mod3_hci), by = "time") %>% 
  pivot_longer(cols = c(contains("mean"), contains("lci"), contains("hci")),
               names_to = c("model", ".value"), 
               names_sep = "_") %>% 
  filter(!is.na(mean)) %>% 
  rename(mod_mean = mean,
         mod_hci = hci, 
         mod_lci = lci)


## combine with observed data 
nat_prev <- left_join(obs_nat_prev, mod_nat_prev, by = "time") %>% 
  mutate(mod_RR_cases = obs_TB_cases * mod_mean, 
         mod_RR_lci = obs_TB_cases * mod_lci, 
         mod_RR_hci = obs_TB_cases * mod_hci)


save(nat_prev, file = "output/projected_nat_prev.Rdata")


##################################################################
##                    Prev - National (Year)                    ##
##################################################################
### Take the mean for each year and multiply it by the number of new TB cases for that year 
obs_nat_prev.yr <- mdf_prev_ind %>% 
  group_by(diag_yr) %>% 
  summarize(obs_TB_cases = n(), 
            TB_complete = mdf_prev_ind %>% 
              group_by(diag_yr) %>% 
              filter_at(vars(state, diag_yr, sex, hiv_status, tratamento, 
                             age_cat, health_unit, mun_urban_cat,
                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                        all_vars(!is.na(.))) %>% 
              summarize(TB_complete = n()), 
            obs_RR_cases = sum(result == 1, na.rm = TRUE),
            obs_tested = sum(tested == 1, na.rm = TRUE)) %>% 
  filter(diag_yr == TB_complete$diag_yr) %>% 
  mutate(obs_TB_complete_cases = TB_complete[[2]]) %>% 
  select(c(diag_yr, obs_TB_cases, obs_TB_complete_cases, obs_RR_cases, obs_tested))


### pull estimates from each model 
mod1_nat_prev.yr <- boot.mun_prev_1[["pred.int_nat_year"]] %>% 
  rename(mod1_mean = mean, 
         mod1_lci = lci, 
         mod1_hci = hci)

mod2_nat_prev.yr <- boot.mun_prev_2[["pred.int_nat_year"]] %>% 
  rename(mod2_mean = mean, 
         mod2_lci = lci, 
         mod2_hci = hci) 

mod3_nat_prev.yr <- boot.mun_prev_3[["pred.int_nat_year"]] %>% 
  rename(mod3_mean = mean, 
         mod3_lci = lci, 
         mod3_hci = hci)


## prep modeled data 
mod_nat_prev.yr <- left_join(mod1_nat_prev.yr %>% select(year, mod1_mean, mod1_lci, mod1_hci), 
                            mod2_nat_prev.yr %>% select(year, mod2_mean, mod2_lci, mod2_hci),
                            by = "year") %>% 
  left_join(., mod3_nat_prev.yr %>% select(year, mod3_mean, mod3_lci, mod3_hci), by = "year") %>% 
  pivot_longer(cols = c(contains("mean"), contains("lci"), contains("hci")),
               names_to = c("model", ".value"), 
               names_sep = "_") %>% 
  filter(!is.na(mean)) %>% 
  rename(mod_mean = mean,
         mod_hci = hci, 
         mod_lci = lci)


## combine with observed data 
nat_prev.yr <- left_join(obs_nat_prev.yr, mod_nat_prev.yr, by = c("diag_yr"="year")) %>% 
  mutate(mod_RR_cases = obs_TB_cases * mod_mean, 
         mod_RR_lci = obs_TB_cases * mod_lci, 
         mod_RR_hci = obs_TB_cases * mod_hci)


save(nat_prev.yr, file = "output/projected_nat_prev.yr.Rdata")






##################################################################
##                         Prev - State                         ##
##################################################################
obs_state_prev <- mdf_prev_ind %>% 
  group_by(time, state, state_nm) %>% 
  summarize(obs_TB_cases = n(), 
            TB_complete = mdf_prev_ind %>% 
              group_by(time, state) %>% 
              filter_at(vars(state, time, sex, hiv_status, tratamento, 
                             age_cat, health_unit, mun_urban_cat,
                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                        all_vars(!is.na(.))) %>% 
              summarize(TB_complete = n()), 
            obs_RR_cases = sum(result == 1, na.rm = TRUE),
            obs_tested = sum(tested == 1, na.rm = TRUE)) %>% 
  filter(time == TB_complete[[1]] & state == TB_complete[[2]]) %>% 
  mutate(obs_TB_complete_cases = TB_complete[[3]]) %>% 
  select(c(time, state, state_nm, obs_TB_cases, obs_TB_complete_cases, obs_RR_cases, obs_tested))





### pull estimates from each model 
mod1_state_prev <- boot.mun_prev_1[["pred.int_state"]] %>% 
  rename(mod1_mean = mean, 
         mod1_lci = lci, 
         mod1_hci = hci)
mod2_state_prev <- boot.mun_prev_2[["pred.int_state"]] %>% 
  rename(mod2_mean = mean, 
         mod2_lci = lci, 
         mod2_hci = hci) 

mod3_state_prev <- boot.mun_prev_3[["pred.int_state"]] %>% 
  rename(mod3_mean = mean, 
         mod3_lci = lci, 
         mod3_hci = hci)


## prep modeled data 
mod_state_prev <- left_join(mod1_state_prev %>% select(time, state, mod1_mean, mod1_lci, mod1_hci), 
                           mod2_state_prev %>% select(time, state, mod2_mean, mod2_lci, mod2_hci),
                           by = c("time", "state")) %>% 
  left_join(., mod3_state_prev %>% select(time, state, mod3_mean, mod3_lci, mod3_hci), by = c("time", "state")) %>% 
  pivot_longer(cols = c(contains("mean"), contains("lci"), contains("hci")),
               names_to = c("model", ".value"), 
               names_sep = "_") %>% 
  filter(!is.na(mean)) %>% 
  rename(mod_mean = mean,
         mod_hci = hci, 
         mod_lci = lci)


## combine with observed data 
state_prev <- left_join(obs_state_prev, mod_state_prev, by = c("time", "state")) %>% 
  mutate(mod_RR_cases = obs_TB_cases * mod_mean, 
         mod_RR_lci = obs_TB_cases * mod_lci, 
         mod_RR_hci = obs_TB_cases * mod_hci)


## save
save(state_prev, file = "output/projected_state_prev.Rdata")





##################################################################
##                   Prev Cases - State (Year)                  ##
##################################################################
obs_state_prev.yr <- mdf_prev_ind %>% 
  group_by(state, state_nm, diag_yr) %>% 
  summarize(obs_TB_cases = n(), 
            TB_complete = mdf_prev_ind %>% 
              group_by(state, state_nm, diag_yr) %>% 
              filter_at(vars(state, time, sex, hiv_status, tratamento, 
                             age_cat, health_unit, mun_urban_cat,
                             mun_fhs_cat, mun_has_prison, mun_bf_cat),
                        all_vars(!is.na(.))) %>% 
              summarize(TB_complete = n()), 
            obs_RR_cases = sum(result == 1, na.rm = TRUE),
            obs_tested = sum(tested == 1, na.rm = TRUE)) %>% 
  filter(diag_yr == TB_complete[[3]] & state == TB_complete[[1]]) %>% 
  mutate(obs_TB_complete_cases = TB_complete[[4]]) %>% 
  select(c(state, state_nm, diag_yr, obs_TB_cases, obs_RR_cases, obs_tested, obs_TB_complete_cases))


### pull estimates from each model 
mod1_state_prev.yr <- boot.mun_prev_1[["pred.int_state_year"]] %>% 
  rename(mod1_mean = mean, 
         mod1_lci = lci, 
         mod1_hci = hci)

mod2_state_prev.yr <- boot.mun_prev_2[["pred.int_state_year"]] %>% 
  rename(mod2_mean = mean, 
         mod2_lci = lci, 
         mod2_hci = hci) 

mod3_state_prev.yr <- boot.mun_prev_3[["pred.int_state_year"]] %>% 
  rename(mod3_mean = mean, 
         mod3_lci = lci, 
         mod3_hci = hci)


## prep modeled data 
mod_state_prev.yr <- left_join(mod1_state_prev.yr %>% select(year, state, mod1_mean, mod1_lci, mod1_hci), 
                              mod2_state_prev.yr %>% select(year, state, mod2_mean, mod2_lci, mod2_hci),
                              by = c("year", "state")) %>% 
  left_join(., mod3_state_prev.yr %>% select(year, state, mod3_mean, mod3_lci, mod3_hci), by = c("year", "state")) %>% 
  pivot_longer(cols = c(contains("mean"), contains("lci"), contains("hci")),
               names_to = c("model", ".value"), 
               names_sep = "_") %>% 
  filter(!is.na(mean)) %>% 
  rename(mod_mean = mean,
         mod_hci = hci, 
         mod_lci = lci)


## combine with observed data 
state_prev.yr <- left_join(obs_state_prev.yr, mod_state_prev.yr, by = c("diag_yr" = "year", "state")) %>% 
  mutate(mod_RR_cases = obs_TB_cases * mod_mean, 
         mod_RR_lci = obs_TB_cases * mod_lci, 
         mod_RR_hci = obs_TB_cases * mod_hci)


## save
save(state_prev.yr, file = "output/projected_state_prev.yr.Rdata")


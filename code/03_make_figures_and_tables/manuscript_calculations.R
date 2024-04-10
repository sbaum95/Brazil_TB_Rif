# Paper calculations
source("code/dependencies.R")

load("output/compiled_results.Rdata")


# Methods ---------------------------------------------------------------
load("data/sinan_xpert_tmp.Rdata")

tabyl(sinan_tmp, tratamento)

## Missing sex
tabyl(sinan_tmp, sex)

## Missing municipality (even after imputation)
sinan_tmp %>% filter(is.na(lat)) %>% count()

## Share of xpert test results
## Share with conclusive result
data.frame(conclusive = (tabyl(sinan_tmp, test_molec)[[2, 3]] + tabyl(sinan_tmp, test_molec)[[3, 3]])*100, 
           not_detectable = tabyl(sinan_tmp, test_molec)[[4, 3]]*100, 
           inconclusive =  tabyl(sinan_tmp, test_molec)[[5, 3]]*100, 
           not_tested = (tabyl(sinan_tmp, test_molec)[[1, 3]] + tabyl(sinan_tmp, test_molec)[[6, 3]] + tabyl(sinan_tmp, test_molec)[[7, 3]])*100)



## Share not tested
(tabyl(sinan_tmp, test_molec)[[5, 3]] + tabyl(sinan_tmp, test_molec)[[9, 3]] )*100

## Share with inclusive 






# National --------------------------------------------------------
## Year --------------------------------------------------------------------

total <- data.frame(
  pop_2010 = 190732694, 
  projected = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(cases = sum(fitted_RR), 
              lci = sum(proj_lci),
              hci = sum(proj_hci)),
  cdr = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    group_by(year) %>%
    summarize(cases = if_else(year <= "2017-01-01", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
    unique()) %>% 
  rename(year = projected.year) %>% 
  select(year, projected.cases, projected.lci, projected.hci, cdr.cases, pop_2010) %>% 
  pivot_longer(cols = c("projected.cases", "cdr.cases", "projected.lci", "projected.hci"), names_to = "var", values_to = "count") %>% 
  mutate(inc = (count/pop_2010)*100000)

total %>% 
  filter(var %in% c("projected.cases", "cdr.cases")) %>% 
  group_by(var) %>% 
  mutate(proj_inc_chg = if_else(!is.na(lag(inc)), (inc - lag(inc))/lag(inc), NA)) %>% 
  summarize(mean(proj_inc_chg, na.rm = TRUE))



## Quarter -----------------------------------------------------

total <- data.frame(
  pop_2010 = 190732694, 
  projected = compiled_results[["nat_qrt"]] %>%
    filter(model == "sp_2015-2019") %>%
    ungroup() %>%
    group_by(diag_qrt) %>%
    summarize(cases = sum(fitted_RR), 
              lci = sum(proj_lci),
              hci = sum(proj_hci)),
  cdr = compiled_results[["nat_qrt"]] %>%
    filter(model == "sp_2015-2019") %>%
    group_by(diag_qrt) %>%
    summarize(cases = if_else(diag_qrt <= "2017-01-01", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
    unique()) %>% 
  rename(diag_qrt = projected.diag_qrt) %>% 
  select(diag_qrt, projected.cases, projected.lci, projected.hci, cdr.cases, pop_2010) %>% 
  pivot_longer(cols = c("projected.cases", "cdr.cases", "projected.lci", "projected.hci"), names_to = "var", values_to = "count") %>% 
  mutate(inc = (count/pop_2010)*100000)

total %>% 
  filter(var == "projected.cases") %>% 
  mutate(proj_inc_chg = if_else(!is.na(lag(inc)), (inc - lag(inc))/lag(inc), NA)) %>% 
  summarize(mean(proj_inc_chg, na.rm = TRUE))

         
         


## By case type ------------------------------------------------------------

case_type <- data.frame(
  pop_2010 = 190732694, 
  projected = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    ungroup() %>%
    group_by(year, case_type) %>%
    summarize(cases = sum(fitted_RR), 
              lci = sum(proj_lci),
              hci = sum(proj_hci), 
              total_TB = sum(total_TB_cases)),
  cdr = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2015-2019") %>%
    group_by(year, case_type) %>%
    summarize(cases = if_else(year <= "2017-01-01", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
    unique()) %>% 
  rename(year = projected.year, 
         case_type = cdr.case_type, 
         total_tb = projected.total_TB) %>% 
  select(year, projected.cases, projected.lci, projected.hci, cdr.cases, pop_2010, case_type, total_tb) %>% 
  mutate(pct = (projected.cases/total_tb)*100, 
         pct_lci = (projected.lci/total_tb)*100, 
         pct_hci = (projected.hci/total_tb)*100)











# State -------------------------------------------------------------------
## New cases ---------------------------------------------------------------


new_st_2019 <- compiled_results[["state_yr"]] %>% 
  left_join(., pop_UF, by = c("state_nm" = "state")) %>% 
  filter(model == "sp_2015-2019", case_type == "new", year == 2019) %>% 
  group_by(state_nm) %>% 
  summarize(TB_inc = (total_TB_cases/pop_2010)*100000,
            pct = (fitted_RR/total_TB_cases)*100, 
            lci_pct = (proj_lci/total_TB_cases)*100,
            hci_pct = (proj_hci/total_TB_cases)*100, 
            inc = (fitted_RR/pop_2010)*100000, 
            lci_inc = (proj_lci/pop_2010)*100000,
            hci_inc = (proj_hci/pop_2010)*100000)


new_st_2019 %>% 
  arrange(desc(TB_inc)) %>% 
  head(10) %>% 
  arrange(desc(inc))


new_st_yr <- compiled_results[["state_yr"]] %>% 
  left_join(., pop_UF, by = c("state_nm" = "state")) %>% 
  filter(model == "sp_2015-2019", case_type == "new") %>% 
  group_by(state_nm) %>% 
  summarize(year = year, 
    pct = (fitted_RR/total_TB_cases)*100, 
            lci_pct = (proj_lci/total_TB_cases)*100,
            hci_pct = (proj_hci/total_TB_cases)*100, 
            inc = (fitted_RR/pop_2010)*100000, 
            lci_inc = (proj_lci/pop_2010)*100000,
            hci_inc = (proj_hci/pop_2010)*100000,
            proj_inc_chg = if_else(!is.na(lag(inc)), (inc - lag(inc))/lag(inc)*100, NA))
  






# Previous ---------------------------------------------------------------------
prev_st_2019 <- compiled_results[["state_yr"]] %>% 
  left_join(., pop_UF, by = c("state_nm" = "state")) %>% 
  filter(model == "sp_2015-2019", case_type == "prev", year == 2019) %>% 
  group_by(state_nm) %>% 
  summarize(TB_inc = (total_TB_cases/pop_2010)*100000,
            pct = (fitted_RR/total_TB_cases)*100, 
            lci_pct = (proj_lci/total_TB_cases)*100,
            hci_pct = (proj_hci/total_TB_cases)*100, 
            inc = (fitted_RR/pop_2010)*100000, 
            lci_inc = (proj_lci/pop_2010)*100000,
            hci_inc = (proj_hci/pop_2010)*100000)


prev_st_2019 %>% 
  arrange(desc(TB_inc)) %>% 
  head(10) %>% 
  arrange(desc(inc))


prev_st_yr <- compiled_results[["state_yr"]] %>% 
  left_join(., pop_UF, by = c("state_nm" = "state")) %>% 
  filter(model == "sp_2015-2019", case_type == "prev") %>% 
  group_by(state_nm) %>% 
  summarize(year = year, 
            pct = (fitted_RR/total_TB_cases)*100, 
            lci_pct = (proj_lci/total_TB_cases)*100,
            hci_pct = (proj_hci/total_TB_cases)*100, 
            inc = (fitted_RR/pop_2010)*100000, 
            lci_inc = (proj_lci/pop_2010)*100000,
            hci_inc = (proj_hci/pop_2010)*100000,
            proj_inc_chg = if_else(!is.na(lag(inc)), (inc - lag(inc))/lag(inc)*100, NA))


prev_st_yr %>% 
  ungroup() %>% 
  group_by(state_nm) %>% 
  summarize(
    inc = mean(inc), 
    mean(proj_inc_chg, na.rm = TRUE)) %>% 
  arrange(desc(inc)) %>% 
  head(10)



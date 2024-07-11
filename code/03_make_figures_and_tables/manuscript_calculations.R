# Paper calculations
source("code/dependencies.R")

# Load national population ------------------------------------------------

pop_UF <- read_excel("data/Brazil_population_UF.xls", skip = 6) %>%
  rename(
    pop_2010 = "...2",
    state = "...1"
  ) %>%
  select(state, pop_2010) %>%
  filter(!is.na(state)) %>%
  filter(!state %in% c("Brasil", "Sudeste", "Centro-Oeste", "Norte", "Nordeste", "Sul"))

pop_2010 = sum(pop_UF$pop_2010)


state_codes <- read_excel("data/StateCodes.xlsx")


# Abstract ----------------------------------------------------------------
# Range of estimated prevalence by case type
compiled_results[["nat_yr"]] %>% 
  filter(model == "sp_2017") %>% 
  group_by(year, case_type) %>% 
  summarize(mod_prev = fitted_RR/total_TB_cases, 
            naive_prev = obs_RR/obs_num_tested, 
            pct_difference = (mod_prev-naive_prev)/naive_prev *100) %>% 
  group_by(case_type) %>% 
  summarize(min = min(pct_difference), 
            max = max(pct_difference))



# Methods ---------------------------------------------------------------

# Exclusion criteria
tabyl(sinan_tmp, tratamento)
tabyl(sinan_tmp, test_molec)

sinan_tmp %>% 
  summarize(
    new = sum(tratamento == 1),
    previous = sum(tratamento %in% c("2", "3")),
    relapse = sum(tratamento == "2"),
    retreat = sum(tratamento == "3"),
    unknown = sum(tratamento == "4"), 
    unknown_pct = round((sum(tratamento == "4")/n())*100, 2),
    transfer = sum(tratamento == "5"), 
    transfer_pct = round((sum(tratamento == "5")/n())*100, 2),
    postmortem = sum(tratamento == "6"), 
    postmortem_pct = round((sum(tratamento == "6")/n())*100, 2),
    misdiag = sum(situa_ence == "06", na.rm = TRUE), 
    misdiag_pct = round((sum(situa_ence == "06", na.rm = TRUE)/n())*100, 2))


# Final sample size
sinan_tmp %>% 
  filter(tratamento %in% c("1", "2", "3") & situa_ence != "06") %>% 
  summarize(
    total = n(), 
    new = sum(tratamento == 1),
    previous = sum(tratamento %in% c("2", "3")),
    relapse = sum(tratamento == "2"),
    retreat = sum(tratamento == "3"))


# Municipality/state imputation
## Share of patients with missing municipality of residence
sinan_tmp %>% 
  filter(is.na(id_mn_resi) & tratamento %in% c("1", "2", "3") & situa_ence != "06") %>% 
  summarize(count = round(n(), 2), pct = round((n()/count(sinan_tmp))*100, 2))

## Share of imputed id_mn_resi from notifying health facility 
tabyl(sinan_tmp %>% filter(tratamento %in% c("1", "2", "3") & situa_ence != "06"), mn_to_merge_flag)

## Share of patients with missing state of residence
sinan_tmp %>% filter(is.na(sg_uf) & tratamento %in% c("1", "2", "3") & situa_ence != "06") %>%  summarize(count = round(n(), 2), pct = round((n()/count(sinan_tmp))*100, 2))

## Share of imputed sg_uf coming from notifying health facility
tabyl(sinan_tmp %>% filter(tratamento %in% c("1", "2", "3") & situa_ence != "06"), sg_uf_clean_flag)

## Share of patients where imputed municipality of residence differs from listed state of residence
sinan_tmp %>% 
  filter(mn_to_merge_flag == "id_mn_not" & tratamento %in% c("1", "2", "3") & sg_uf !=sg_uf_not)


# Age
tabyl(sinan_tmp, age_flag)


# Share of xpert test results (share of patients)
data.frame(conclusive = (tabyl(sinan_tmp %>% filter(tratamento %in% c("1", "2", "3") & situa_ence != "06"), test_molec)[[1, 3]] + tabyl(sinan_tmp %>% filter(tratamento %in% c("1", "2", "3")), test_molec)[[2, 3]])*100, 
           not_detectable = tabyl(sinan_tmp %>% filter(tratamento %in% c("1", "2", "3") & situa_ence != "06"), test_molec)[[3, 3]]*100, 
           inconclusive =  tabyl(sinan_tmp %>% filter(tratamento %in% c("1", "2", "3") & situa_ence != "06"), test_molec)[[4, 3]]*100, 
           not_tested = (tabyl(sinan_tmp %>% filter(tratamento %in% c("1", "2", "3") & situa_ence != "06"), test_molec)[[5, 3]] + tabyl(sinan_tmp %>% filter(tratamento %in% c("1", "2", "3")), test_molec)[[6, 3]] + tabyl( sinan_tmp %>% filter(tratamento %in% c("1", "2", "3")), test_molec)[[7, 3]])*100)







# Results -----------------------------------------------------------------

# Figure 1A ---------------------------------------------------------------

# Test coverage of all cases in 2014
round(((subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year == "2014")[["obs_num_tested"]] + 
  subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year == "2014")[["obs_num_tested"]])/
  (subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year == "2014")[["total_TB_cases"]] + 
     subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year == "2014")[["total_TB_cases"]]))*100, 2)

# Test coverage of ALL cases at end of 2023
round(((subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year == "2023")[["obs_num_tested"]] +
          subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year == "2023")[["obs_num_tested"]])/
         (subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year == "2023")[["total_TB_cases"]] +
            subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year == "2023")[["total_TB_cases"]]))*100, 2)

# Average difference in coverage between new and prev
compiled_results[["nat_yr"]] %>% 
  filter(model == "sp_2014") %>% 
  group_by(case_type, year) %>% 
  mutate(cov = obs_num_tested/total_TB_cases) %>% 
  group_by(year) %>% 
  summarize(cov_diff = cov[case_type == "prev"] - cov[case_type == "new"])


# # Test coverage of NEW cases at end of 2014
# subset(compiled_results[["nat_qrt"]], model == "sp_2014" & case_type == "new" & diag_qrt == "2014-10-01")[["obs_pct_tested"]]
# 
# # Test coverage of PREV cases at end of 2014
# subset(compiled_results[["nat_qrt"]], model == "sp_2014" & case_type == "prev" & diag_qrt == "2014-10-01")[["obs_pct_tested"]]


# Total observed RR-TB cases between 2014-2016
compiled_results[["nat_yr"]] %>% 
  filter(model == "sp_2014" & year <= 2016) %>% 
  group_by(case_type) %>% 
  summarize(avg_pos = round(mean(obs_RR/obs_num_tested)*100, 2))

# Positivity in 2023
compiled_results[["nat_yr"]] %>% 
  filter(model == "sp_2017" & year >2016) %>% 
  group_by(case_type) %>% 
  summarize(avg_pos = round(mean(obs_RR/obs_num_tested)*100, 2))



# Figure 1B ---------------------------------------------------------------

## Get total incidence from WHO figure

# ## Obs positivity in 2014 - New cases
# sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["obs_RR"]])/sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["obs_num_tested"]]) 
# 
# ## Obs positivity in 2014 - Prev cases
# sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year %in% c("2014"))[["obs_RR"]])/sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year %in% c("2014"))[["obs_num_tested"]]) 
# 
# ## Projected positivity in 2014 - New Cases (pt, UI)
# sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["fitted_RR"]])/sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["total_TB_cases"]]) 
# sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["proj_lci"]])/sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["total_TB_cases"]]) 
# sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["proj_hci"]])/sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["total_TB_cases"]]) 

# Figure 2 ----------------------------------------------------------------

## Calculate average bias between 2017-2023 for new cases
compiled_results[["nat_yr"]] %>% 
  filter(model == "sp_2017") %>% 
  group_by(case_type, year) %>% 
  summarize(bias = mean(fitted_RR/obs_RR), 
            fitted = mean(fitted_RR), 
            obs = mean(obs_RR))

compiled_results[["nat_qrt"]] %>% 
  filter(model == "sp_2017") %>% 
  group_by(case_type) %>% 
  mutate(
    prev_mod = (fitted_RR*4/pop_2010)*100000,
    prev_mod_lci = (proj_lci*4/pop_2010)*100000,
    prev_mod_hci = (proj_hci*4/pop_2010)*100000) %>% 
  summarize(
    prev_mod = mean(fitted_RR*4/pop_2010)*100000,
    prev_nav = mean((obs_RR*(1/obs_pct_tested)*4)/pop_2010)*100000, 
    bias_prev = mean(prev_mod/prev_nav), 
    bias_lci = mean(prev_mod_lci/prev_nav), 
    bias_hci = mean(prev_mod_hci/prev_nav))

## Calculate average trend in bias
compiled_results[["nat_yr"]] %>% 
  filter(model == "sp_2017") %>% 
  mutate(case_type = if_else(case_type == "new", "New", "Previously Treated")) %>%
  group_by(case_type) %>% 
  mutate(
    prev_mod = (fitted_RR/pop_2010),
    prev_nav = ((obs_RR*(1/obs_pct_tested))/pop_2010), 
    bias_prev = prev_mod/prev_nav) %>% 
  summarize(
    bias_ann_chg = (bias_prev - lag(bias_prev)), 
    avg = mean(bias_ann_chg, na.rm = TRUE),
    bias_prev_chg = ((bias_prev[year == "2023"] - bias_prev[year == "2017"])/bias_prev[year == "2017"])*100)


# Figure 3 ----------------------------------------------------------------
total <- data.frame(
  pop_2010 = pop_2010, 
  projected = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2017") %>%
    ungroup() %>%
    group_by(year) %>%
    summarize(cases = sum(fitted_RR), 
              lci = sum(proj_lci),
              hci = sum(proj_hci)),
  cdr = compiled_results[["nat_yr"]] %>%
    filter(model == "sp_2017") %>%
    group_by(year) %>%
    # summarize(cases = if_else(year <= "2017-01-01", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
    summarize(cases = if_else(year <= "2017-01-01", sum(fitted_RR) / 0.87,
                              if_else(year > "2017-01-01" & year < "2020-01-01", sum(fitted_RR) / 0.89,
                                      if_else(year >= "2020-01-01" & year < "2021-01-01", sum(fitted_RR) / 0.78,
                                              if_else(year >= "2021-01-01" & year < "2022-01-01", sum(fitted_RR) / 0.76,
                                                      if_else(year >= "2022-01-01", sum(fitted_RR) / 0.83, NA)
                                              )
                                      )
                              )
    )
    ) %>% 
    unique()) %>% 
  rename(year = projected.year) %>% 
  select(year, projected.cases, projected.lci, projected.hci, cdr.cases, pop_2010) %>% 
  pivot_longer(cols = c("projected.cases", "cdr.cases", "projected.lci", "projected.hci"), names_to = "var", values_to = "count") %>% 
  mutate(inc = (count/pop_2010)*100000)

# Total RR-TB incidence among notified TB cases in 2023 (Point estimate and UI)
round(subset(total, year == "2023" & var == "projected.cases")[["inc"]], 2)
round(subset(total, year == "2023" & var == "projected.lci")[["inc"]], 2)
round(subset(total, year == "2023" & var == "projected.hci")[["inc"]], 2)

# Total RR-TB incidence accounting for CDR cases in 2023 (Point estimate and UI)
round(subset(total, year == "2023" & var == "cdr.cases")[["inc"]], 2)

# Positivity among new cases in 2023 (Point estimate and UI)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "new")[["fitted_RR"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "new")[["total_TB_cases"]])*100, 2)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "new")[["proj_lci"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "new")[["total_TB_cases"]])*100, 2)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "new")[["proj_hci"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "new")[["total_TB_cases"]])*100, 2)

# Positivity among prev cases in 2023 (Point estimate and UI)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "prev")[["fitted_RR"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "prev")[["total_TB_cases"]])*100, 2)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "prev")[["proj_lci"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "prev")[["total_TB_cases"]])*100, 2)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "prev")[["proj_hci"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "prev")[["total_TB_cases"]])*100, 2)

# percent change 2017-2023 new
round(((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "new")[["fitted_RR"]] - subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2017" & case_type == "new")[["fitted_RR"]])/(subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2017" & case_type == "new")[["fitted_RR"]]))*100, 2)

# percent change 2017-2023 prev
round(((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2023" & case_type == "prev")[["fitted_RR"]] - subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2017" & case_type == "prev")[["fitted_RR"]])/(subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2017" & case_type == "prev")[["fitted_RR"]]))*100, 2)

# compiled_results[["nat_yr"]] %>% 
#   filter(model == "sp_2017" & case_type == "prev") %>% 
#   mutate(pct_chg = if_else(!is.na(lag(fitted_RR/total_TB_cases)), (fitted_RR/total_TB_cases - lag(fitted_RR/total_TB_cases))/lag(fitted_RR/total_TB_cases)*100, NA)) %>% 
#   summarize(mean = mean(pct_chg, na.rm = TRUE))


# WHO estimates
source("code/03_make_figures_and_tables/fig_results_national_WHO.R")

# WHO positivity among new cases in 2022
who_data_new$e_rr_pct[who_data_new$year == 2022]
who_data_new$pct_lo[who_data_new$year == 2022]
who_data_new$pct_hi[who_data_new$year == 2022]

# WHO positivity among prev cases in 2022
who_data_prev$e_rr_pct[who_data_prev$year == 2022]
who_data_prev$pct_lo[who_data_prev$year == 2022]
who_data_prev$pct_hi[who_data_prev$year == 2022]

# Positivity among new cases in 2022 (Point estimate and UI)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "new")[["fitted_RR"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "new")[["total_TB_cases"]])*100, 2)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "new")[["proj_lci"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "new")[["total_TB_cases"]])*100, 2)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "new")[["proj_hci"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "new")[["total_TB_cases"]])*100, 2)


# Positivity among prev cases in 2022 (Point estimate and UI)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "prev")[["fitted_RR"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "prev")[["total_TB_cases"]])*100, 2)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "prev")[["proj_lci"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "prev")[["total_TB_cases"]])*100, 2)
round((subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "prev")[["proj_hci"]]/subset(compiled_results[["nat_yr"]], model == "sp_2017" & year == "2022" & case_type == "prev")[["total_TB_cases"]])*100, 2)


# Figure 4 ----------------------------------------------------------------

## States with highest RR-TB incidence among notified new cases in 2023
compiled_results[["state_yr"]] %>% 
  filter(model == "sp_2017", year == "2023") %>% 
  group_by(case_type, state_nm) %>% 
  summarize(pct = round((fitted_RR/total_TB_cases)*100, 2), 
            lci_pct = round((proj_lci/total_TB_cases)*100, 2),
            hci_pct = round((proj_hci/total_TB_cases)*100, 2)) %>% 
  filter(pct == max(pct) | pct == min(pct))



# Figure 5 ----------------------------------------------------------------
## Panel A - Time periods

# Projected positivity in 2014
compiled_results[["nat_yr"]] %>% 
  filter(model %in% c("sp_2017", "sp_2014") & year == 2014) %>% 
  group_by(case_type, model) %>% 
  summarize(pct = (fitted_RR/total_TB_cases)*100, 
            pct_lci = (proj_lci/total_TB_cases)*100, 
            pct_hci = (proj_hci/total_TB_cases)*100)



## Panel B - Specs
test <- compiled_results[["nat_yr"]] %>% 
  filter(model %in% c("sp_2017", "sens_1", "sens_2")) %>% 
  group_by(year, case_type, model) %>% 
  summarize(pct = (fitted_RR/total_TB_cases)*100) %>% 
  mutate(diff_sens_1 = (pct[model == "sp_2017"] - pct[model == "sens_1"]), 
         diff_sens_2 = (pct[model == "sp_2017"] - pct[model == "sens_2"]))

# Percentage point difference 
mean(test$diff_sens_1[test$case_type == "new"])
mean(test$diff_sens_1[test$case_type == "prev"])

mean(test$diff_sens_2[test$case_type == "new"])
mean(test$diff_sens_2[test$case_type == "prev"])









## New cases ---------------------------------------------------------------

# 
# ## Quarter -----------------------------------------------------
# 
# total <- data.frame(
#   pop_2010 = 190732694, 
#   projected = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2017") %>%
#     ungroup() %>%
#     group_by(diag_qrt) %>%
#     summarize(cases = sum(fitted_RR), 
#               lci = sum(proj_lci),
#               hci = sum(proj_hci)),
#   cdr = compiled_results[["nat_qrt"]] %>%
#     filter(model == "sp_2017") %>%
#     group_by(diag_qrt) %>%
#     summarize(cases = if_else(diag_qrt <= "2017-01-01", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
#     unique()) %>% 
#   rename(diag_qrt = projected.diag_qrt) %>% 
#   select(diag_qrt, projected.cases, projected.lci, projected.hci, cdr.cases, pop_2010) %>% 
#   pivot_longer(cols = c("projected.cases", "cdr.cases", "projected.lci", "projected.hci"), names_to = "var", values_to = "count") %>% 
#   mutate(inc = (count/pop_2010)*100000)
# 
# total %>% 
#   filter(var == "projected.cases") %>% 
#   mutate(proj_inc_chg = if_else(!is.na(lag(inc)), (inc - lag(inc))/lag(inc), NA)) %>% 
#   summarize(mean(proj_inc_chg, na.rm = TRUE))
# 
# 
# 
# 
# 
# ## By case type ------------------------------------------------------------
# 
# case_type <- data.frame(
#   pop_2010 = 190732694, 
#   projected = compiled_results[["nat_yr"]] %>%
#     filter(model == "sp_2017") %>%
#     ungroup() %>%
#     group_by(year, case_type) %>%
#     summarize(cases = sum(fitted_RR), 
#               lci = sum(proj_lci),
#               hci = sum(proj_hci), 
#               total_TB = sum(total_TB_cases)),
#   cdr = compiled_results[["nat_yr"]] %>%
#     filter(model == "sp_2017") %>%
#     group_by(year, case_type) %>%
#     summarize(cases = if_else(year <= "2017-01-01", sum(fitted_RR)/0.87, sum(fitted_RR)/0.89)) %>% 
#     unique()) %>% 
#   rename(year = projected.year, 
#          case_type = cdr.case_type, 
#          total_tb = projected.total_TB) %>% 
#   select(year, projected.cases, projected.lci, projected.hci, cdr.cases, pop_2010, case_type, total_tb) %>% 
#   mutate(pct = (projected.cases/total_tb)*100, 
#          pct_lci = (projected.lci/total_tb)*100, 
#          pct_hci = (projected.hci/total_tb)*100)
# 
# 
# 
# 
# 


# ## Projected RR-TB cases among new patients
# round(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["fitted_RR"]], 0)
# round(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["proj_lci"]], 0)
# round(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "new" & year %in% c("2014"))[["proj_hci"]], 0)
# 
# ## Projected positivity in 2014 - Prev Cases
# sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year %in% c("2014"))[["fitted_RR"]])/sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year %in% c("2014"))[["total_TB_cases"]]) 
# 
# ## Projected RR-TB cases among prev patients
# round(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year %in% c("2014"))[["fitted_RR"]], 0)
# sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year %in% c("2014"))[["proj_lci"]])/sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year %in% c("2014"))[["total_TB_cases"]]) 
# sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year %in% c("2014"))[["proj_hci"]])/sum(subset(compiled_results[["nat_yr"]], model == "sp_2014" & case_type == "prev" & year %in% c("2014"))[["total_TB_cases"]]) 
# 
# # 
# ## Percet of total cases tested during stock out in 2016
# sum(subset(compiled_results[["nat_qrt"]],  model == "sp_2014" & diag_qrt == "2016-10-01")[["obs_num_tested"]])/sum(subset(compiled_results[["nat_qrt"]],  model == "sp_2014" & diag_qrt == "2016-10-01")[["total_TB_cases"]])
# 
# # positivity new cases during stock outs
# round((subset(compiled_results[["nat_qrt"]], model == "sp_2014" & case_type == "new" & diag_qrt == "2016-10-01")[["fitted_RR"]]/subset(compiled_results[["nat_qrt"]],  model == "sp_2014" & diag_qrt == "2016-10-01" & case_type == "new")[["total_TB_cases"]])*100, 2)
# 
# round((subset(compiled_results[["nat_qrt"]], model == "sp_2014" & case_type == "new" & diag_qrt == "2016-10-01")[["proj_lci"]]/subset(compiled_results[["nat_qrt"]],  model == "sp_2014" & diag_qrt == "2016-10-01" & case_type == "new")[["total_TB_cases"]])*100, 2)
# round((subset(compiled_results[["nat_qrt"]], model == "sp_2014" & case_type == "new" & diag_qrt == "2016-10-01")[["proj_hci"]]/subset(compiled_results[["nat_qrt"]],  model == "sp_2014" & diag_qrt == "2016-10-01" & case_type == "new")[["total_TB_cases"]])*100, 2)
# 
# compiled_results[["nat_qrt"]] %>% 
#   filter(model == "sp_2014") %>% 
#   group_by(diag_qrt, case_type) %>% 
#   summarize(pct = fitted_RR/total_TB_cases, 
#             lci = proj_lci/total_TB_cases, 
#             hci = proj_hci/total_TB_cases)






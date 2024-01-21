# Author: Sarah Baum
# Date Created: 2024-01-18
# Date Modified: 2024-01-21

# Description: 
## -- Creates Table X for RR-TB manuscript




source(here::here("code/dependencies.R"))

# load sinan
load(here::here("data/sinan_xpert.Rdata"))


# load bootstrapped results 
load("output/fits/boot.mun_new_1.Rda")
load("output/fits/boot.mun_new_2.Rda")
load("output/fits/boot.mun_prev_1.Rda")
load("output/fits/boot.mun_prev_2.Rda")



# pull number tested from cases (easier to do it this way with fewer manipulations to previous script)
obs_test <- sinan_xpert %>%
  dplyr::select(sg_uf, diag_qrt, diag_yr, state_nm, id_municip, test_molec, tratamento) %>%
  # restrict to new and previous cases; cases with conclusive test result
  filter(tratamento %in% c(1, 2, 3) & test_molec %in% c(1,2)) %>% 
  # define case type: new = 1, prev = 2
  mutate(type = if_else(tratamento == 1, "New", "Prev")) %>% 
  group_by(sg_uf, diag_yr, type) %>%
  mutate(state_tested = n(), 
         state_RR = sum(test_molec == 2), 
         state_pctRR = state_RR/state_tested) %>%
  dplyr::select(-c(test_molec, id_municip, tratamento, test_molec, tratamento, diag_qrt)) %>%
  unique()

# add time variable in sinan to merge to modeled results
# ### Order the dates in ascending order
# sorted_dates <- sort(unique(obs_test$diag_yr))
# 
# ### Create a named vector to store the rank for each date
# year <- setNames(1:length(sorted_dates), format(sorted_dates, "%Y"))
# dates <- cbind(as.data.frame(sorted_dates), year)
# obs_test <- left_join(obs_test, dates, by = c("year" = "sorted_dates"))
# obs_test$diag_yr <- as.numeric(as.character(obs_test$diag_yr)) 






# make table for new cases
make_table_new <- boot.mun_new_2[["pred.int_state_year"]] %>% 
   # add number tested by state, quarter
  left_join(obs_test %>% filter(type == "New"), by = c("state" = "sg_uf", "year" = "diag_yr")) %>% 
  dplyr::select(-c(type)) %>% 
  mutate_at(c("state_tested", "state_RR", "state_pctRR"), ~ifelse(is.na(.), 0, .)) %>% 
  group_by(state_nm) %>% 
  summarize(
    # observed percent of new TB cases tested with Xpert that are RR-TB positive in 2019
    obs_2019 = state_pctRR[year==2019],
    # observed percent change (2019-2016)
    obs_pctchg = (state_pctRR[year==2019] - state_pctRR[year==2016])/state_pctRR[year==2016], 
    # mod 2019
    mod_2019 = mean[year==2019], 
    # modeled percent change (2019-2016)
    mod_pct_chg = (mean[year==2019]-mean[year==2016])/mean[year==2016])

















# output table
# library(officer)
# 
# output_table_1 <- read_docx()
# # existing_doc <- read_docx("path/to/existing_document.docx")
# 
# output_table_1 <- output_table_1  %>%
#   officer::add_table(make_table_new, style = "table")
# 
# # Save the Word document
# write_docx(output_table_1, "output/output_table_1.docx")





### Old


# tested defined in mdf_new_ind includes everyone who was tested even if they
# had an inconclusive or undetectable result
# new_test <- mdf_new_ind %>%
#   dplyr::select(state, tested, time, result) %>% 
#   group_by(state, time) %>% 
#   summarize(
#     # get the number of TB cases tested with Xpert each quarter
#     obs_new_tested = sum(tested == 1), 
#     # get the number of positive Xpert tests
#     obs_new_RR = sum(result == 1, na.rm = TRUE), 
#     # calculate positivity
#     obs_new_pctRR = obs_new_RR/obs_new_tested
#     ) 


# tested defined in mdf_new_ind includes everyone who was tested even if they
# had an inconclusive or undetectable result
# prev_test <- mdf_prev_ind %>%
#   dplyr::select(state, tested, time, result) %>% 
#   group_by(state, time) %>% 
#   summarize(
#     # get the number of TB cases tested with Xpert each quarter
#     obs_prev_tested = sum(tested == 1), 
#     # get the number of positive Xpert tests
#     obs_prev_RR = sum(result == 1, na.rm = TRUE), 
#     # calculate positivity
#     obs_prev_pctRR = obs_prev_RR/obs_prev_tested
#   ) 






  
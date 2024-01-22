# Author: Sarah Baum
# Created: 2024-01-21
# Updated: 

# Description/Decisions: 
# -- Creates maps comparing raw vs. modeled estimates in 2019
# -- Test out municipality + state-level maps 


# Plan: 
-- Write map script (states for now)

- Create four separate maps: 


source(here::here("code/dependencies.R"))



# load shapefile (From IBGE: https://www.ibge.gov.br/en/geosciences/territorial-organization/territorial-meshes/2786-np-municipal-mesh/18890-municipal-mesh.html)
# municipality
# Source: Shapefile - IBGE; Projection - https://epsg.io/5875
shapefile <- st_read("data/BR_Municipios_2020/BR_Municipios_2020.shp") %>% st_simplify(dTolerance = 20) 
shapefile <- shapefile %>% st_set_crs(value = 5527) %>% st_transform(crs = 5875) # Change to EPSG:4674 SAD69, UTM Zone 18S
shapefile <- shapefile %>% st_transform(crs = 5875) # Change to EPSG:4674 SAD69, UTM Zone 18S

sf::st_crs(shapefile)

# truncate municip ID (7-digit from IBGE to 6 in SINAN) to merge 
shapefile$CD_MUN_merge <- as.numeric(substr(as.character(shapefile$CD_MUN), 1, nchar(shapefile$CD_MUN)-1))  %>% as.factor()






# load sinan
load(here::here("data/sinan_xpert.Rdata"))


# load bootstrapped results 
load("output/fits/boot.mun_new_1.Rda")
load("output/fits/boot.mun_new_2.Rda")
load("output/fits/boot.mun_new_2b.Rda")
load("output/fits/boot.mun_prev_1.Rda")
load("output/fits/boot.mun_prev_2.Rda")



# get raw estimates
## turn this into a function eventually 
obs_state <- sinan_xpert %>%
  dplyr::select(sg_uf, diag_qrt, diag_yr, state_nm, id_municip, test_molec, tratamento) %>%
  # restrict to new and previous cases; cases with conclusive test result
  filter(tratamento %in% c(1, 2, 3) & test_molec %in% c(1,2)) %>% 
  # define case type: new = 1, prev = 2
  mutate(type = if_else(tratamento == 1, "New", "Prev")) %>% 
  group_by(sg_uf, diag_yr, type) %>%
  mutate(obs_tested = n(), 
         obs_RR = sum(test_molec == 2), 
         obs_pctRR = obs_RR/obs_tested) %>%
  # need to add in 0 observations where they aren't being calculated for states to make percent change work 
  # obs_pctchng = (obs_pctRR[diag_yr==2019]-obs_pctRR[diag_yr==2016])/obs_pctRR[diag_yr==2016])
  dplyr::select(-c(id_municip, test_molec, tratamento, test_molec, tratamento, diag_qrt, state_nm)) %>%
  unique() 


# municipality 
obs_mun <- sinan_xpert %>%
  dplyr::select(sg_uf, diag_qrt, diag_yr, state_nm, id_municip, test_molec, tratamento) %>%
  # restrict to new and previous cases; cases with conclusive test result
  filter(tratamento %in% c(1, 2, 3) & test_molec %in% c(1,2)) %>% 
  # define case type: new = 1, prev = 2
  mutate(type = if_else(tratamento == 1, "New", "Prev")) %>% 
  group_by(id_municip, diag_yr, type) %>%
  mutate(obs_tested = n(), 
         obs_RR = sum(test_molec == 2), 
         obs_pctRR = obs_RR/obs_tested) %>%
  # need to add in 0 observations where they aren't being calculated for states to make percent change work 
  # obs_pctchng = (obs_pctRR[diag_yr==2019]-obs_pctRR[diag_yr==2016])/obs_pctRR[diag_yr==2016])
  dplyr::select(-c(test_molec, tratamento, test_molec, tratamento, diag_qrt, state_nm)) %>%
  unique() 




###########################################################################
###########################################################################
###                                                                     ###
###                              NEW CASES                              ###
###                                                                     ###
###########################################################################
###########################################################################
combined_new_state <- boot.mun_new_2b[["pred.int_state_year"]] %>% # note need to change to int_state_year
  # add number tested by state, quarter
  left_join(obs_state %>% filter(type == "New"), by = c("state" = "sg_uf", "year" = "diag_yr")) %>% 
  dplyr::select(-c(type)) %>% 
  mutate_at(c("obs_tested", "obs_RR", "obs_pctRR"), ~ifelse(is.na(.), 0, .)) %>% 
  group_by(state_nm) %>% 
  summarize(
    # observed percent of new TB cases tested with Xpert that are RR-TB positive in 2019
    obs_2019 = obs_pctRR[year==2019],
    # observed percent change (2019-2016)
    obs_pctchg = ((obs_pctRR[year==2019] - obs_pctRR[year==2016])/obs_pctRR[year==2016])*100, 
    # mod 2019
    mod_2019 = mean[year==2019], 
    # modeled percent change (2019-2016)
    mod_pct_chg = ((mean[year==2019]-mean[year==2016])/mean[year==2016])*100
  )


combined_new_mun <- boot.mun_new_2b[["pred.int_mun"]] %>% # note need to change to int_state_year
  # add number tested by state, quarter
  left_join(obs_mun %>% filter(type == "New"), by = c("id_municip", "year" = "diag_yr")) %>% 
  dplyr::select(-c(type)) %>% 
  mutate_at(c("obs_tested", "obs_RR", "obs_pctRR"), ~ifelse(is.na(.), 0, .)) %>% 
  group_by(id_municip) %>% 
  summarize(
    # observed percent of new TB cases tested with Xpert that are RR-TB positive in 2019
    obs_2019 = obs_pctRR[year==2019],
    # observed percent change (2019-2016)
    # obs_pctchg = ((obs_pctRR[year==2019] - obs_pctRR[year==2016])/obs_pctRR[year==2016])*100,
    # mod 2019
    mod_2019 = mean[year==2019], 
    # modeled percent change (2019-2016)
    mod_pct_chg = ((mean[year==2019]-mean[year==2016])/mean[year==2016])*100
    )




############################################################################
############################################################################
###                                                                      ###
###                            PREVIOUS CASES                            ###
###                                                                      ###
############################################################################
############################################################################
combined_prev_mun <- boot.mun_prev_2b[["pred.int_mun"]] %>% # note need to change to int_state_year
  # add number tested by state, quarter
  left_join(obs_mun %>% filter(type == "Prev"), by = c("id_municip", "year" = "diag_yr")) %>% 
  dplyr::select(-c(type)) %>% 
  mutate_at(c("obs_tested", "obs_RR", "obs_pctRR"), ~ifelse(is.na(.), 0, .)) %>% 
  group_by(id_municip) %>% 
  summarize(
    # observed percent of new TB cases tested with Xpert that are RR-TB positive in 2019
    obs_2019 = obs_pctRR[year==2019],
    # observed percent change (2019-2016)
    # obs_pctchg = ((obs_pctRR[year==2019] - obs_pctRR[year==2016])/obs_pctRR[year==2016])*100,
    # mod 2019
    mod_2019 = mean[year==2019], 
    # modeled percent change (2019-2016)
    mod_pct_chg = ((mean[year==2019]-mean[year==2016])/mean[year==2016])*100
  )



combined_prev_state <- boot.mun_prev_2b[["pred.int_state_year"]] %>% # note need to change to int_state_year
  # add number tested by state, quarter
  left_join(obs_state %>% filter(type == "Prev"), by = c("state" = "sg_uf", "year" = "diag_yr")) %>% 
  dplyr::select(-c(type)) %>% 
  mutate_at(c("obs_tested", "obs_RR", "obs_pctRR"), ~ifelse(is.na(.), 0, .)) %>% 
  group_by(state_nm) %>% 
  summarize(
    # observed percent of new TB cases tested with Xpert that are RR-TB positive in 2019
    obs_2019 = obs_pctRR[year==2019],
    # observed percent change (2019-2016)
    obs_pctchg = ((obs_pctRR[year==2019] - obs_pctRR[year==2016])/obs_pctRR[year==2016])*100, 
    # mod 2019
    mod_2019 = mean[year==2019], 
    # modeled percent change (2019-2016)
    mod_pct_chg = ((mean[year==2019]-mean[year==2016])/mean[year==2016])*100
  )







# Join shapefile and dataset ----------------------------------------------
shapefile_new_mun <- left_join(shapefile %>% dplyr::select(CD_MUN_merge, NM_MUN, geometry, SIGLA_UF), combined_new_mun,  by = c("CD_MUN_merge" = "id_municip"), relationship = "many-to-many") %>% 
  unique()



# need to figure out how to merge by state 
# shapefile_new_state <- left_join(shapefile %>% select(CD_MUN_merge, NM_MUN, geometry, SIGLA_UF), combined_new_state,  by = c("CD_MUN_merge" = "id_municip"), relationship = "many-to-many") %>% 
#   unique() %>% 
#   filter(!is.na(diag_yr))




# Maps: # add data by municipality and also by state 
- new cases (observed 2019, modeled 2019, pct change, difference)
- prev cases (observed 2019, modeled 2019, pct change, difference)


# plot map 
ggplot() +
  geom_sf(data = shapefile_new_mun, fill = "white", color = "lightgray") + 
  # geom_sf(data = year_to_plot, aes(color = ifelse(start_imp_yr <= year, "red", NA)))+
  # labs(color = "Municipality implementing Xpert") + 
  # geom_sf(data = year_to_plot, aes(fill = factor(xpert_access)), color = NA) + 
  # labs(fill='Type of Access', 
  #      labels = c("Access", 
  #                 "No access, but >=10% of diagnoses receiving Xpert")) +
  scale_fill_discrete(name = "Municipality Type", labels = c("Access to machine", "No access, but >=10% of diagnoses receiving Xpert")) +
  ggtitle(paste("Xpert access in", year)) +
  theme_bw() + 
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        title = element_text(size = 8),
        legend.key.height = unit(0.5, "cm"), 
        legend.key.width = unit(0.3, "cm"))




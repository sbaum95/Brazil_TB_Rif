# Author: Sarah Baum
# Created: 2023-06-26
# Updated: 2023-06-27
# Description: 
# - Loads and cleans Xpert machine installation data



# add in Xpert implementation data
xpert_install <- read_excel("data/Equipamentos_RTR-TB_maio_2023.xlsx", sheet = "Equip por Estado",
                            col_types = c("text",
                                          "numeric", "text", "text", "text",
                                          "text", "numeric", "numeric", "date",
                                          "numeric", "text", "text", "text", "text")) %>%
  rename(id_municip = "Cód. Município", 
         dt_install = "Data da instalação", 
         dt_donation = "Ano da doação",
         id_unidade = "CNES", 
         facility = "Nome Instituição Destinada") %>% 
  mutate(dt_donation = ifelse(is.na(dt_donation), NA, paste(dt_donation, "01", "01", sep = "-")) %>% 
           as.Date(dt_donation, format = "%Y-%m-%d"),
         dt_install = as.Date(dt_install, format = "%Y-%m-%d"))

# Note: Where a single year of implementation was listed, I manually updated the
# implementation year to be Jan 01. Where implementation date was not listed,
# but a date of donation was listed, I use the date of donation.
xpert_install$dt_install <- if_else(is.na(xpert_install$dt_install), xpert_install$dt_donation, xpert_install$dt_install)

# Determing when Xpert was implemented in each municipality
start_imp_yr <- xpert_install %>% mutate(implementation_yr = floor_date(as_date(dt_install), "quarter")) %>% group_by(id_municip) %>% summarize(start_imp_yr = min(implementation_yr))



# Author: Sarah Baum
# Created: 2023-09-06
# Updated: 2023-11-11

# Description: 
# Ingests and cleans FHS coverage data (number of family health teams per 4,000 based)

# Source: http://tabnet.datasus.gov.br/cgi/tabcgi.exe?cnes/cnv/equipebr.def
# Period: Jan 2014-Dec 2019
# By: Municipality and Micro-region (aggregated based on aggregated municip populations)
# Team type: Select all teams with "ESF" in the title
# Background on team types in DATASUS: https://www.oecd-ilibrary.org/sites/e6920110-en/index.html?itemId=/content/component/e6920110-en



# old code 
# load and clean .csv file for Dec 2015
# fhs_cov <- read_delim("data/fhs_coverage/siab_cnv_SIABFBR22050998_216_48_63.csv", 
#                       delim = ";", col_names = FALSE, trim_ws = TRUE, 
#                       skip = 4
#                       )
# 
# 
# fhs_cov_clean <- separate(fhs_cov, X1, into = c("id_municip", "municip_nm"), sep = "(?<=\\d)\\s") %>% 
#   filter(!is.na(municip_nm)) %>% 
#   rename("people_with_fhs_2015" = "X2")
# 
# 
# count(fhs_cov_clean) # Only have FHS coverage for 3758 municipalities 
# 
# unique_df <- fhs_cov_clean[duplicated(fhs_cov_clean$id_municip), ] # no duplicates
# 
# fhs_cov_clean$id_municip <- as.numeric(fhs_cov_clean$id_municip)
#

# load and clean csv with number of family health teams per month
fhs_cov <- read_delim("data/fhs_coverage/health_teams/cnes_cnv_equipebr23112798_216_48_63.csv",
                      delim = ";", escape_double = FALSE, col_names = FALSE, 
                      trim_ws = TRUE, skip = 4
                      ) %>% 
  slice_head(n = -8)

# drop cell
fhs_cov[1,1] <- "municip"

# make second row column names
colnames(fhs_cov) <- unlist(fhs_cov[1, ])
colnames(fhs_cov) <- gsub("/", "_", colnames(fhs_cov)) 
colnames(fhs_cov) <- tolower(colnames(fhs_cov))



# filter to mid-year count (July) - Based on Andrade et al. (2018)
fhs <- separate(fhs_cov, municip, into = c("id_municip", "name_mun"), sep = "(?<=\\d)\\s") %>%
  mutate_all(~na_if(., "-")) %>% 
  filter(
    !is.na(name_mun)
    ) %>% 
  select(
    1:2, matches("jun|jul|ago|mai|set|abr|out|mar|nov|fev|dez")) %>% 
  #   ) %>% 
  # rename_with(
  #   ~gsub("_jul$", "", .), .cols = matches("jul|jun")
  #   ) %>% 
#   rename_with(
#   ~gsub("_jun$", "", .) %>% 
#   gsub("_jul$", "", .) %>% 
#   gsub("_ago$", "", .) %>% 
#   gsub("_mai$", "", .),
# .cols = matches("jun|jul|ago|mai")) %>% 
  # pivot_longer(
  #   cols = 3:8, names_to = "year", values_to = "mun_fhs_num_teams"
  #   )
  pivot_longer(
  cols = -c(1:2),  # Assuming the first two columns are identifiers (e.g., ID, Name)
  names_to = c("year", "month"),
  names_pattern = "^(\\d{4})_(\\w{3})$",
  values_to = "value") %>% 
  filter(year == 2017) %>% # filter to 2017
  pivot_wider(names_from = month, 
              values_from = value) %>% 
  group_by(id_municip) %>% 
  mutate(mun_fhs_num_teams = coalesce(jul, jun, mai, ago, mai, set)) %>% 
  select(id_municip, name_mun, mun_fhs_num_teams)


# to make merging to sinan work
fhs$id_municip <- as.factor(fhs$id_municip)
# fhs$year <- as.numeric(fhs$year)
fhs$mun_fhs_num_teams <- as.numeric(fhs$mun_fhs_num_teams)



## add in data by municipality
sinan_tmp <- left_join(sinan_tmp, fhs %>% select(id_municip, mun_fhs_num_teams), by = c("id_mn_resi"= "id_municip"))

# check missing post-merge (~3511 with missing values)
# sinan_tmp %>%
#   filter(is.na(mun_fhs_num_teams)) %>%
#   select(id_mn_resi) %>%
#   unique()


# calculate number of teams per 4000
sinan_tmp <- sinan_tmp %>% mutate(mun_fhs_per_cap = (mun_fhs_num_teams/mun_pop_2010)*4000)


# create categorical var 
sinan_tmp$mun_fhs_cat <- cut(sinan_tmp$mun_fhs_per_cap,
                               breaks = quantile(sinan_tmp$mun_fhs_per_cap, probs = 0:5/5, na.rm = TRUE), 
                               labels = FALSE, 
                               include.lowest = TRUE)




## calculate and add for micro-region
# mic_fhs_cov <- sinan_xpert %>% 
#   select(id_micro, id_municip, mun_fhs_num_teams, mic_pop_2010) %>% 
#   unique() %>% 
#   group_by(id_micro) %>% 
#   mutate(mic_fhs_num_teams = mean(mun_fhs_num_teams, na.rm = TRUE)) %>% 
#   select(-c(id_municip, mun_fhs_num_teams)) %>% 
#   unique() %>% 
#   mutate(mic_fhs_per_cap = (mic_fhs_num_teams/mic_pop_2010)*4000)
# 
# 
# mic_fhs_cov$mic_fhs_cat <- cut(mic_fhs_cov$mic_fhs_per_cap,
#                                breaks = quantile(mic_fhs_cov$mic_fhs_per_cap, probs = 0:5/5, na.rm = TRUE), 
#                                labels = FALSE, 
#                                include.lowest = TRUE)
# 
# 
# sinan_xpert <- left_join(sinan_xpert, mic_fhs_cov %>% select(mic_fhs_per_cap, mic_fhs_cat), by = c("id_micro"))



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



# Note: This only includes data for 3361 municipalities
source(here::here("code/dependencies.R"))

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
                      )

# drop cell
fhs_cov[1,1] <- "municip"

# make second row column names
colnames(fhs_cov) <- unlist(fhs_cov[1, ])
colnames(fhs_cov) <- gsub("/", "_", colnames(fhs_cov)) 
colnames(fhs_cov) <- tolower(colnames(fhs_cov))



# filter to mid-year count (July) - Based on Andrade et al. (2018)
fhs <- separate(fhs_cov, municip, into = c("id_municip", "municip_nm"), sep = "(?<=\\d)\\s") %>%
  filter(
    !is.na(municip_nm)
    ) %>% 
  select(
    1:2, matches("jul")
    ) %>% 
  rename_with(
    ~gsub("_jul$", "", .), .cols = matches("jul")
    ) %>% 
  pivot_longer(
    cols = 3:8, names_to = "year", values_to = "mun_fhs_num_teams"
    )



# to make merging to sinan work
fhs$id_municip <- as.numeric(fhs$id_municip)
fhs$year <- as.numeric(fhs$year)
fhs$mun_fhs_num_teams <- as.numeric(fhs$mun_fhs_num_teams)



# save
save(fhs, file = "data/fhs_coverage.Rdata")

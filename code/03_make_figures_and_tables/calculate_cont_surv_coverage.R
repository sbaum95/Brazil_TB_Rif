source("code/dependencies.R")

# Load WHO TB data
who_rr <- read_csv("data/MDR_RR_TB_burden_estimates_2024-04-03.csv") %>%
  filter(year == 2022) %>%
  select(country, iso3, source_rr_new) %>% 
  unique()

who_ds <- read_csv("data/TB_burden_countries_2024-07-03.csv") %>%
  filter(year == 2022) %>%
  select(iso3, e_pop_num, e_inc_num) %>% 
  unique()

# Load WB poopulation data - Remove since included in who_ds dataset
# pop <- read_csv("data/world_bank_pop_2022.csv", skip = 3) %>% 
#   select("Country Code", "2022")
# 
# names(pop)[1] <- "iso3"
# names(pop)[2] <- "pop_2022"

# Combine data to get testing coverage
coverage <- left_join(who_rr, who_ds, by = "iso3")
names(coverage)[4] <- "pop_2022"

# Coverage by total global population
coverage %>% 
  mutate(total_pop = sum(pop_2022, na.rm = TRUE)) %>% 
  group_by(source_rr_new) %>% 
  summarize(countries = n(), 
            pop = sum(pop_2022, na.rm = TRUE), 
            pct = sum(pop_2022, na.rm = TRUE)/total_pop) %>% 
  unique()


# Coverage by TB incidence
coverage %>% 
  mutate(total_inc = sum(e_inc_num, na.rm = TRUE)) %>% 
  group_by(source_rr_new) %>% 
  summarize(countries = n(), 
            pop = sum(e_inc_num, na.rm = TRUE), 
            pct = sum(e_inc_num, na.rm = TRUE)/total_inc) %>% 
  unique()


# 
# # Identify coverage in are high MDR-TB countries
# mdr_countries <- c("China", "Democratic Republic of the Congo", "India", "Indonesia", "Mozambique", "Myanmar", 
#                    "Nigeria", "Philippines", "South Africa", "Zambia", 
#                    "Angola", "Bangladesh", "Democratic People's Republic of Korea", "Mongolia", "Pakistan", 
#                    "Papua New Guinea", "Viet Nam", 
#                    "Azerbaijan", "Belarus", "Kazakhstan", "Kyrgyzstan", "Nepal", 
#                    "Peru", "Republic of Moldova", "Russian Federation", "Somalia", "Tajikistan", 
#                    "Ukraine", "Uzbekistan", "Zimbabwe")
# 
# coverage %>% 
#   filter(country %in% mdr_countries) %>% 
#   mutate(total_pop = sum(pop_2022, na.rm = TRUE)) %>% 
#   group_by(source_rr_new) %>% 
#   summarize(countries = n(), 
#             pop = sum(pop_2022, na.rm = TRUE), 
#             pct = sum(pop_2022, na.rm = TRUE)/total_pop) %>% 
#   unique()
# 





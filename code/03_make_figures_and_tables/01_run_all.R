# Author: Sarah Baum
# Created: 2024-03-23
# Updated: 2024-03-25

# Description: Executes files to create all figures  

source("code/dependencies.R")

pop_UF <- read_excel("data/Brazil_population_UF.xls", skip = 6) %>%
  rename(
    pop_2010 = "...2",
    state = "...1"
  ) %>%
  select(state, pop_2010) %>%
  filter(!is.na(state)) %>%
  filter(!state %in% c("Brasil", "Sudeste", "Centro-Oeste", "Norte", "Nordeste", "Sul"))

pop_2010 = sum(pop_UF$pop_2010)

# Figure - Create national plot with trends in testing and RR-TB positivity
source("code/03_make_figures_and_tables/fig_model_output_and_testing_trends.R")

# Figure - Bias between model and observed
source("code/03_make_figures_and_tables/fig_bias.R")

# Figure - Trends in RR-TB incidence by state
source("code/03_make_figures_and_tables/fig_state_trends.R")

# Table 1 
source("code/03_make_figures_and_tables/tabl1_covs.R")

# Table - Associations between covariates and Xpert testing and RR-TB positivity, conditional on getting tested
# To make 
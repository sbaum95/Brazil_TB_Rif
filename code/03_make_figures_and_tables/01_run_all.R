# Author: Sarah Baum
# Created: 2024-03-23
# Updated: 2024-03-25

# Description: Executes files to create all figures  

source("code/dependencies.R")

# Create file output params
file_version <- "20240530"

# Load relevant data  ------------------------------------------------
load(paste0("output/compiled_results_", file_version, ".Rdata"))
load(paste0("data/sinan_tmp_", file_version, ".Rdata"))
load(paste0("data/mdf_new_ind_tmp_", file_version, ".Rdata"))
load(paste0("data/mdf_prev_ind_tmp_", file_version, ".Rdata"))

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



# Figures  ------------------------------------------------

# Set palette
pal <- ggsci::pal_npg("nrc", alpha = 0.8)(6)

# Figure 1 - National plot with trends in testing and RR-TB positivity (And sensitivity analyses)
source("code/03_make_figures_and_tables/fig_model_performance.R")

ggsave(fig_model_performance, filename = paste0("output/figures_and_tables/fig_", file_version, "_model_performance.png"), width = 14, height = 5)
ggsave(fig_sens, filename = paste0("output/figures_and_tables/fig_", file_version, "_sens.png"), width = 14, height = 5)

# Figure 2 - Bias between model and observed
source("code/03_make_figures_and_tables/fig_bias.R")

ggsave(fig_bias, filename = paste0("output/figures_and_tables/fig_", file_version, "_bias.png"), width = 9, height = 4)

# Figure 3 - National incidence + WHO comparisons
source("code/03_make_figures_and_tables/fig_WHO.R")

ggsave(fig_total_inc, filename = paste0("output/figures_and_tables/fig_", file_version, "_total_inc.png"), width = 14, height = 6)

# Figure 3 - National incidence 
# source("code/03_make_figures_and_tables/fig_national_results.R")
# 
# ggsave(fig_nat_imp, filename = paste0("output/figures_and_tables/fig_", file_version, "_nat_imp.png"), width = 10, height = 5)    
# 

# Figure 4 - RR-TB incidence by state in 2023
source("code/03_make_figures_and_tables/fig_state_levels.R")

ggsave(shp_new, filename = paste0("output/figures_and_tables/fig_", file_version, "_state_new_map.png"), width = 5, height = 6, bg = "white")
ggsave(fig_new, filename = paste0("output/figures_and_tables/fig_", file_version, "_state_new_plot.png"), width = 8, height = 6)

ggsave(shp_prev, filename = paste0("output/figures_and_tables/fig_", file_version, "_state_prev_map.png"), width = 5, height = 6, bg = "white")
ggsave(fig_prev, filename = paste0("output/figures_and_tables/fig_", file_version, "_state_prev_plot.png"), width = 8, height = 6)


# Figure 5 - Trends by state 
source("code/03_make_figures_and_tables/fig_state_trends.R")

ggsave(fig_state_new_sp, filename = paste0("output/figures_and_tables/fig_", file_version, "_state_new_sp.png"), width = 10, height = 6)
ggsave(fig_state_prev_sp, filename = paste0("output/figures_and_tables/fig_", file_version, "_state_prev_sp.png"), width = 10, height = 6)




# Tables  ------------------------------------------------

# Table 1 
source("code/03_make_figures_and_tables/tab1_covs.R")

stargazer2(new_model_list)
stargazer2(prev_model_list)

# Missing count for Table 1 notes (there are no missing for health unit)
tabyl(mdf_new_ind, sex)[[3, 2]] + tabyl(mdf_prev_ind, sex)[[3, 2]]
tabyl(mdf_new_ind, hiv_status)[[2, 2]] + tabyl(mdf_prev_ind, hiv_status)[[2, 2]]
tabyl(mdf_new_ind, age_cat)[[9, 2]] + tabyl(mdf_prev_ind, age_cat)[[9, 2]]

# Appendix ------------------------------------------------


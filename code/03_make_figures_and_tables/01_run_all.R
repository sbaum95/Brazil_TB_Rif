# Description: Executes files to create all figures and tables

source("code/dependencies.R")

# Create file output params
file_version_load <- "20240711"
file_version_save <- "20240711"

# Load SINAN and model results  ------------------------------------------------
load(paste0("output/compiled_results_", file_version_load, ".Rdata"))
load(paste0("data/sinan_tmp_", file_version_load, ".Rdata"))
load(paste0("data/mdf_new_ind_tmp_", file_version_load, ".Rdata"))
load(paste0("data/mdf_prev_ind_tmp_", file_version_load, ".Rdata"))


# Load population data ------------------------------------------------
pop_UF <- read_excel("data/Brazil_population_UF.xls", skip = 6) %>%
  rename(
    pop_2010 = "...2",
    state = "...1"
  ) %>%
  select(state, pop_2010) %>%
  filter(!is.na(state)) %>%
  filter(!state %in% c("Brasil", "Sudeste", "Centro-Oeste", "Norte", "Nordeste", "Sul"))

pop_2010 = sum(pop_UF$pop_2010)



# Create figures  ------------------------------------------------

# Set palette
pal <- ggsci::pal_npg("nrc", alpha = 0.8)(6)

new_color = pal[3]
prev_color = pal[1]
projected_color = pal[1]
cdr_color = pal[4]
observed_all_color = pal[3]
observed_xpert_color = pal[2]
who_color = "black"


# Figs 1 and 6 - Results for main model and alternative model specifications
source("code/03_make_figures_and_tables/fig_model_results.R")
ggsave(fig_main_results, filename = paste0("output/figures_and_tables/fig1_", file_version_save, "_main_results.png"), width = 14, height = 5)
ggsave(fig_sens_results, filename = paste0("output/figures_and_tables/fig6_", file_version_save, "_sens_results.png"), width = 14, height = 5)

# Fig 2 - Bias between model and observed
source("code/03_make_figures_and_tables/fig_bias.R")
ggsave(fig_bias, filename = paste0("output/figures_and_tables/fig2_", file_version_save, "_bias.png"), width = 10, height = 4)

# Fig 3 - National total incidence and WHO comparisons
source("code/03_make_figures_and_tables/fig_total_inc_WHO.R")
ggsave(fig_total_inc_WHO, filename = paste0("output/figures_and_tables/fig3_", file_version_save, "_total_inc_WHO.png"), width = 14, height = 6)

# Fig 4 - Levels by state (2023)
source("code/03_make_figures_and_tables/fig_state_levels.R")
ggsave(fig_state_level, filename = paste0("output/figures_and_tables/fig4_", file_version_save, "_state_level.png"), width = 16, height = 12)

# Fig 5 - Trends by state (2017-2023)
source("code/03_make_figures_and_tables/fig_state_trends.R")
ggsave(fig_state_trends, filename = paste0("output/figures_and_tables/fig5_", file_version_save, "_state_trends.png"), width = 14, height = 6)




# Tables  ------------------------------------------------

# Table 1 - Descriptive characteristics of notified TB cases (2017-2023)
source("code/03_make_figures_and_tables/tab1_descriptive.R")
write_xlsx(dat_reordered, paste0("output/figures_and_tables/tab1_", file_version_save, "_descriptive.xlsx"))



# Appendix ------------------------------------------------

# Table A1 - Associations between covariates and testing and resistance
source("code/03_make_figures_and_tables/tab1_covs.R")

save_new <- stargazer2(new_model_list)
new_matrix <- read.table(textConnection(save_new), header = FALSE, sep = "\t")
write.xlsx(new_matrix, paste0("output/figures_and_tables/tabA1_new", file_version_save, "_reg.xlsx"), colNames = FALSE, rowNames = FALSE)

save_prev <- stargazer2(prev_model_list)
prev_matrix <- read.table(textConnection(save_prev), header = FALSE, sep = "\t")
write.xlsx(prev_matrix, paste0("output/figures_and_tables/tabA1_prev", file_version_save, "_reg.xlsx"), colNames = FALSE, rowNames = FALSE)

# Missing count for Table 1 notes (there are no missing for health unit)
# tabyl(mdf_new_ind, sex)[[3, 2]] + tabyl(mdf_prev_ind, sex)[[3, 2]]
# tabyl(mdf_new_ind, hiv_status)[[2, 2]] + tabyl(mdf_prev_ind, hiv_status)[[2, 2]]
# tabyl(mdf_new_ind, age_cat)[[9, 2]] + tabyl(mdf_prev_ind, age_cat)[[9, 2]]
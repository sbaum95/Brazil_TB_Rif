



# Load data and results ---------------------------------------------------
source("code/dependencies.R")

load("output/compiled_results.Rdata")



# Create function to get plots by state-quarter ---------------------------
fig_state <- function(state_list, model_name, case_type1 = NULL, case_type2 = NULL) {
  
  data <- compiled_results[["state_qrt"]] %>% 
    left_join(., pop_UF, by = c("state_nm" = "state")) %>% 
    mutate(tb_inc = (total_TB_cases/pop_2010)*100000) %>% 
    filter(state_nm %in% state_list) %>% 
    filter(model == model_name)
    
  
  # Determine whether plot both case types or only one
  if (!is.null(case_type1) & !is.null(case_type2)) {
    
    data_plot <- data %>% filter(case_type %in% c(case_type1, case_type2))
    
  } else if (!is.null(case_type1)) {
    
    data_plot <- data %>% filter(case_type == case_type1)
    
  } else {
    data_plot <- data %>% filter(case_type == case_type2)
  }
  
  # transform_func <- function(x) {
  #   x * (500/165)  # Mapping secondary axis range (0-50) to primary axis range (0-165)
  # }
  # 
  # Plot
    
    ggplot() + 
    
    # Add line for projected incidence for each model (2017-2019)
    geom_line(data = data_plot %>%
                mutate(obs_incidence = ((obs_RR/obs_num_tested)*total_TB_cases)/pop_2010 * 100000),
              aes(x = diag_qrt, y = obs_incidence, color = "Observed")) +
    
  # Add line for projected incidence for each model (2017-2019)
  geom_line(data = data_plot %>%
              mutate(mod_incidence = (fitted_RR/pop_2010) * 100000),
            aes(x = diag_qrt, y = mod_incidence, color = "Projected")) +

  geom_errorbar(data = data_plot %>%
              mutate(inc_lci = (proj_lci/pop_2010) * 100000,
                     inc_hci = (proj_hci/pop_2010) * 100000),
            aes(x = diag_qrt, ymin = inc_lci, ymax = inc_hci, color = "Projected"), alpha = 0.5) +

  facet_wrap(~state_nm, scales = "free")
  
}


set_base_aes_specs <- function(by_state) {
  
  by_state + 
    
    scale_x_date(
      date_breaks = "1 year",  # Set breaks to 1 year
      date_labels = "%b %Y"  # Format labels as year
    ) + 
    xlab("Quarter") + 
    ylab("Incidence per 100,000 population") + 
    theme_bw() + 
    theme(
      # axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.x = element_text(size = 7),
      axis.text.y  = element_text(size = 9), 
      legend.text = element_text(size = 9), 
      title = element_text(size = 10)) +
    scale_size(
      range = c(0.5, 5)
    ) + 
    scale_color_manual(name = "", 
      labels = c(
        "Observed",
        "Projected"
      ),
      values = c("red", "black"
                      )
                
      )
  
} 



# Execute function --------------------------------------------------------
state_list_new = compiled_results[["state_yr"]] %>% 
  left_join(., pop_UF, by = c("state_nm" = "state")) %>% 
  filter(year == 2019 & case_type == "new") %>% 
  mutate(tb_inc = (total_TB_cases/pop_2010)*100000, 
         inc = (fitted_RR/pop_2010)*100000) %>% 
  arrange(desc(inc)) %>% 
  select(state_nm) %>% 
  unique() %>% 
  head(10) %>% 
  pull()

fig_state_new_sp <- fig_state(state_list = state_list_new, model_name = "sp_2015-2019", case_type1 = "new") %>% 
  set_base_aes_specs() + 
  ggtitle("RR-TB incidence among new cases")


state_list_prev = compiled_results[["state_yr"]] %>% 
  left_join(., pop_UF, by = c("state_nm" = "state")) %>% 
  mutate(tb_inc = (total_TB_cases/pop_2010)*100000) %>% 
  filter(year == 2019 & case_type == "prev") %>% 
  arrange(desc(total_TB_cases)) %>% 
  select(state_nm) %>% 
  unique() %>% 
  head(10) %>% 
  pull()


fig_state_prev_sp <- fig_state(state_list = state_list_prev, model_name = "sp_2015-2019", case_type1 = "prev") %>% 
  set_base_aes_specs() + 
  ggtitle("RR-TB incidence among previous cases")




# Compare all models ------------------------------------------------------
# model_list <- c("tt_2014-2019","tt_2015-2019", "sp_2014-2019", "sp_2015-2019", "se1_sp_2015-2019", "se2_sp_2015-2019")
#   
# compiled_results[["state_qrt"]] %>% 
#   filter(state_nm %in% state_list, model %in% model_list, case_type == "new") %>% 
#   group_by(state_nm, diag_qrt, model) %>% 
#   summarize(mod_incidence = (fitted_RR/total_TB_cases) * 1000) %>% 
#   ggplot() + 
#   
#   # Add line for projected incidence for each model (2017-2019)
#   geom_line(aes(x = diag_qrt, y = mod_incidence, color = model)) +
#   
#   facet_wrap(~state_nm, scales = "free")



# Save output -------------------------------------------------------------
ggsave(fig_state_new_sp, filename = "output/figures_and_tables/fig_state_new_sp.png", width = 14, height = 6)
ggsave(fig_state_prev_sp, filename = "output/figures_and_tables/fig_state_prev_sp.png", width = 14, height = 6)


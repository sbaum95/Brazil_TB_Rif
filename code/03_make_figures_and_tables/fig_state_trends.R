



# Load data and results ---------------------------------------------------
source("code/dependencies.R")
load("output/compiled_results.Rdata")



# Create function to get plots by state-quarter ---------------------------
fig_state <- function(agg_level, case, model_name) {
  
  data <- compiled_results[[agg_level]] %>% filter(model == model_name) %>% filter(case_type == case & state %in% state_list)
  
  by_state <- ggplot() + 
    
  # 1) Calculate pct tested 
  geom_line(data = data %>% 
              mutate(obs_incidence = (obs_RR/obs_num_tested) * 1000),
            aes(x = diag_qrt, y = obs_incidence, group = state), alpha = 0.6) + 
  
  # 3) Add line for projected incidence for each model (2017-2019)
  geom_point(data = data %>% 
              mutate(mod_incidence = (fitted_RR/total_TB_cases) * 1000), 
            aes(x = diag_qrt, y = mod_incidence, color = model, group = state)) + 
  
  geom_errorbar(data = data %>% 
              mutate(inc_lci = (proj_lci/total_TB_cases) * 1000, 
                     inc_hci = (proj_hci/total_TB_cases) * 1000), 
            aes(x = diag_qrt, ymin = inc_lci, ymax = inc_hci, group = state)) + 
    
  facet_wrap(~state) + 

  xlab("Time (Quarter)") + 
  ylab("RR-TB incidence per 100 incident TB cases") + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 14)) +
    scale_x_date(
      date_breaks = "1 year",  # Set breaks to 1 year
      date_labels = "%Y"  # Format labels as year
    ) +  
  scale_size(
    range = c(0.5, 5)
  )
  
  return(by_state)
  
}



# Execute function --------------------------------------------------------
state_list <- c("São Paulo", "Rio de Janeiro", "Rio Grande do Sul", "Bahia")

state_list <- "São Paulo"

fig_state_new <- fig_state(agg_level = "state_qrt", case = "new", model_name = "sp_2015-2019")

fig_state_prev <- fig_state(agg_level = "state_qrt", case = "prev", model_name = "sp_2014-2019")




# Save output -------------------------------------------------------------
ggsave(fig_state_new)



state_yr <- compiled_results[["state_yr"]] %>% filter(model == "sp_2014-2019")


state_qrt <- compiled_results[["state_qrt"]] %>% filter(model == "sp_2015-2019") %>% filter(grepl("Paulo", state))
state_qrt_int <- intervals[["sp_2015-2019_new"]][["proj_state_qrt"]] %>% filter(state == 35)

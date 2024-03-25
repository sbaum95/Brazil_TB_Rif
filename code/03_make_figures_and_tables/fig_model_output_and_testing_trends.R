source("code/dependencies.R")
library(RColorBrewer)

# Load data and results ---------------------------------------------------
load("data/mdf_mun_new_grp.Rdata")
load("data/mdf_mun_prev_grp.Rdata")
load("output/compiled_results.Rdata")



# Separate plotting into functions ----------------------------------------

plot_observed <- function () {

  observed <- ggplot() + 
  # Observed RR-TB positivity
  geom_point(
    data = compiled_results[["nat_qrt"]] %>% 
      filter(model == "tt_2014-2019" & case_type == "new"), 
    aes(x = diag_qrt, y = obs_pct_pos*100, size = obs_num_tested, color = "New"), alpha = 0.5) +
    
    ## Percent with conclusive Xpert result
    geom_line(
      data = compiled_results[["nat_qrt"]] %>% 
        filter(model == "tt_2014-2019" & case_type == "new"), 
      aes(x = diag_qrt, y = obs_pct_tested*100,  color = "New"), alpha = 0.5) + 
    
    geom_point(
      data = compiled_results[["nat_qrt"]] %>% 
        filter(model == "tt_2014-2019" & case_type == "prev"), 
      aes(x = diag_qrt, y = obs_pct_pos*100, size = obs_num_tested, color = "prev"), alpha = 0.5) +
    
    ## Percent with conclusive Xpert result
    geom_line(
      data = compiled_results[["nat_qrt"]] %>% 
        filter(model == "tt_2014-2019" & case_type == "prev"), 
      aes(x = diag_qrt, y = obs_pct_tested*100,  color = "prev"), alpha = 0.5)
  
  return(observed)
  
}

plot_models <- function(agg_level, model, case_type){
  
  ggplot() + 
  geom_line(
    data = compiled_results[[agg_level]] %>% 
      filter(model == model & case_type == case_type),
    aes(diag_qrt, (fitted_RR/total_TB_cases)*100, color = case_type, linetype = model))
    
  
}


set_base_aes_specs <- function(plot) {
  plot + 
  scale_y_continuous(limits = c(0, 40), 
                     sec.axis = sec_axis(~ .,
                                         breaks = seq(0, 50, by = 10),  # Specify breaks for the secondary axis
                                         name = "Percent of TB cases with confirmed Xpert result", # Format labels as percentages
                                         labels = scales::label_percent(scale = 1)
                     )
  ) + 
    scale_x_date(
      date_breaks = "1 year",  # Set breaks to 1 year
      date_labels = "%b %Y"  # Format labels as year
    ) + 
    xlab("Time (Quarter)") + 
    ylab("Percent RR-TB positive") + 
    theme_bw() + 
    theme(
      axis.text.x  = element_text(size = 12), 
      axis.text.y  = element_text(size = 12), 
      legend.text = element_text(size = 12), 
      title = element_text(size = 12)) +
    scale_size(
      range = c(0.5, 5)
    )
  
}


ggplot() + 
  plot_models(agg_level = "nat_qrt", model = "tt_2014-2019", case_type = "new")


# Plot in ggplot ----------------------------------------------------------

  
plot_TT_SP <- function(plot) {

## New cases ---------------------------------------------------------------

  plot + 
  # Modeled results:
  ## TT: 2014-2019
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "tt_2014-2019" & case_type == "new"),
    aes(diag_qrt, (fitted_RR/total_TB_cases)*100, color = "New", linetype = "2014-2019 (TT)")) +

  ### TT: 2015-2019
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "tt_2015-2019" & case_type == "new"),
    aes(diag_qrt, (fitted_RR/total_TB_cases)*100, color = "New", linetype = "2015-2019 (TT)")) +

  ### SP: 2014-2019
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "sp_2014-2019" & case_type == "new"),
    aes(diag_qrt, (fitted_RR/total_TB_cases)*100, color = "New", linetype = "2014-2019 (SP)")) +

  ### SP: 2015-2019
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "sp_2015-2019" & case_type == "new"),
    aes(diag_qrt, (fitted_RR/total_TB_cases)*100, color = "New", linetype = "2015-2019 (SP)")) +
  
  

## Previous cases ----------------------------------------------------------
  
  ## Modeled results:
  ### TT: 2014-2019
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "tt_2014-2019" & case_type == "prev"),
    aes(diag_qrt, (fitted_RR/total_TB_cases)*100, color = "prev", linetype = "2014-2019 (TT)")) +

  ### TT: 2015-2019
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "tt_2015-2019" & case_type == "prev"),
    aes(diag_qrt, (fitted_RR/total_TB_cases)*100, color = "prev", linetype = "2015-2019 (TT)")) +
  
  ### SP: 2014-2019
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "sp_2014-2019" & case_type == "prev"),
    aes(diag_qrt, (fitted_RR/total_TB_cases)*100, color = "prev", linetype = "2014-2019 (SP)")) +

  ### SP: 2015-2019
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "sp_2015-2019" & case_type == "prev"),
    aes(diag_qrt, (fitted_RR/total_TB_cases)*100, color = "prev", linetype = "2015-2019 (SP)")) +

  
  

## Format figure  ----------------------------------------------------------
  ## Create secondary axis for percent tested 
  scale_y_continuous(limits = c(0, 40), 
                     sec.axis = sec_axis(~ .,
                                         breaks = seq(0, 50, by = 10),  # Specify breaks for the secondary axis
                                         name = "Percent of total TB cases with confirmed Xpert result", # Format labels as percentages
                                         labels = scales::label_percent(scale = 1)
                     )
  ) + 
  scale_x_date(
    date_breaks = "1 year",  # Set breaks to 1 year
    date_labels = "%b %Y"  # Format labels as year
  ) + 
  xlab("Time (Quarter)") + 
  ylab("Percent RR-TB positive") + 
  theme_bw() + 
  theme(
        axis.text.x  = element_text(size = 12), 
        axis.text.y  = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        title = element_text(size = 12)) +
  scale_size(
    range = c(0.5, 5)
  ) +
  labs(size = "Number of cases with conclusive Xpert result",
       color = "Case Type") +
  scale_fill_manual(values = c("Fitted")) +
  scale_color_manual(name="Case Type",
                     labels=c("New",
                              "Previous"),
                     values=c("black", 
                                     "red")) + 
  scale_linetype_manual(name = "Model", 
                        labels = c("2014-2019 (TT)", 
                                   "2014-2019 (SP)", 
                                   "2015-2019 (TT)", 
                                   "2015-2019 (SP)" 
                        ),
                        values=c(1, 2, 3, 4))

}


# Save output -------------------------------------------------------------
fig_observed_trends <- plot_observed() %>% 
  set_base_aes_specs() + 
  labs(size = "Number of cases with conclusive Xpert result",
       color = "Case Type") +
  scale_color_manual(name="Case Type",
                     labels=c("New",
                              "Previous"),
                     values=c("black", 
                                     "red"))
                                     

fig_national_trends_TT_SP <- plot_observed() %>% plot_TT_SP()
  
  

ggsave(fig_observed_trends, filename = "output/figures_and_tables/fig_observed_trends.png", width = 14, height = 6)
ggsave(fig_national_trends_TT, filename = "output/figures_and_tables/fig_national_trends_TT.png", width = 14, height = 6)
ggsave(fig_national_trends_TT_SP, filename = "output/figures_and_tables/fig_national_trends_TT_SP.png", width = 14, height = 6)










# Base R Attempt... -------------------------------------------------------




png("output/figures_and_tables/fig_national_trends_combined.png", width = 8000, height = 6000, units = "px", res = 1000) 

# png("output/figures_and_tables/fig_observed_trends.png", width = 8000, height = 6000, units = "px", res = 1000) 


plot_observed_data <- function () {
  par(mar = c(5, 4, 4, 6) + 0.1)
  
  ## New cases ---------------------------------------------------------------
  data <- compiled_results[["nat_qrt"]] %>% filter(model == "tt_2014-2019" & case_type == "new")
  
  
  ## Plot observed cases that are RR-TB positive
  
  plot(data$diag_qrt, (data$obs_pct_pos) * 100,
       pch = 19,
       cex = 0.6,
       axes = FALSE,
       ylim = c(0, 35),
       xlab = "",
       ylab = "", 
       col = "black"
  )
  
  axis(2, ylim = c(0, 35), col = "black", las = 1)
  mtext("Percent RR-TB positive", side = 2, line = 2.5)
  box()
  
  
  
  ## Add line for percent of new cases being tested with Xpert
  par(new = TRUE)
  
  plot(data$diag_qrt, (data$obs_pct_tested) * 100,
       xlab = "", 
       ylab = "", 
       ylim = c(0, 50),
       axes = FALSE, 
       col = "black", 
       type = "l"
  )
  
  axis(4, ylim = c(0, 50), col = "black", col.axis = "black", las = 1)
  mtext("Percent tested with Xpert", side = 4, col = "black", line = 4)
  
  
  
  ## Add X-axis
  axis(1, data$diag_qrt, format(data$diag_qrt, "%b %Y"))
  mtext("Time (Quarters)", side = 1, col = "black", line = 2.5)
  
  
  
  
  
  
  # Previous cases ----------------------------------------------------------
  data <- compiled_results[["nat_qrt"]] %>% filter(model == "tt_2014-2019" & case_type == "prev")
  
  
  ## Plot observed cases that are RR-TB positive
  par(new = TRUE)
  
  plot(data$diag_qrt, (data$obs_pct_pos) * 100,
       pch = 19,
       cex = 0.6,
       axes = FALSE,
       ylim = c(0, 35),
       xlab = "",
       ylab = "",
       col = "#A50F15"
  )
  
  ## Plot share of previous cases being tested
  par(new = TRUE)
  
  plot(data$diag_qrt, (data$obs_pct_tested) * 100,
       pch = 15, 
       xlab = "", 
       ylab = "", 
       ylim = c(0, 50),
       axes = FALSE, 
       col = "#A50F15", 
       type = "l"
  )
  
  
  
}


plot_new_ests <- function() {
  
  
  ## Model: TT 2014-2019
  par(new = TRUE)
  
  plot(data$diag_qrt, (data$fitted_RR / data$total_TB_cases) * 100,
       pch = 15, xlab = "", ylab = "", ylim = c(0, 35),
       axes = FALSE, col = "blue", type = "l"
  )
  
  ## Model: TT 2015-2019
  par(new = TRUE)
  
  data <- compiled_results[["nat_qrt"]] %>% filter(model == "tt_2015-2019" & case_type == "new")
  
  plot(data$diag_qrt, (data$fitted_RR / data$total_TB_cases) * 100,
       pch = 15, xlab = "", ylab = "", ylim = c(0, 35), xlim = as.Date(c("2015-01-01", "2019-10-01")),
       axes = FALSE, col = "blue", type = "l", lty = 2
  )
  
  ## Model: SP 2014-2019
  par(new = TRUE)
  
  data <- compiled_results[["nat_qrt"]] %>% filter(model == "sp_2014-2019" & case_type == "new")
  
  plot(data$diag_qrt, (data$fitted_RR / data$total_TB_cases) * 100,
       pch = 15, xlab = "", ylab = "", ylim = c(0, 35),
       axes = FALSE, col = "purple", type = "l"
  )
  
  ## Model: SP 2015-2019
  par(new = TRUE)
  
  data <- compiled_results[["nat_qrt"]] %>% filter(model == "sp_2015-2019" & case_type == "new")
  
  plot(data$diag_qrt, (data$fitted_RR / data$total_TB_cases) * 100,
       pch = 15, xlab = "", ylab = "", ylim = c(0, 35), xlim = as.Date(c("2015-01-01", "2019-10-01")),
       axes = FALSE, col = "purple", type = "l", lty = 2
  )
  
  
}






# Plot observed data only 
png("output/figures_and_tables/fig_observed_trends.png", width = 8000, height = 6000, units = "px", res = 1000) 
plot_observed_data()
dev.off()




# Plot observed data and modeled data 
png("output/figures_and_tables/fig_national_trends_combined.png", width = 8000, height = 6000, units = "px", res = 1000) 
plot_observed_data()
plot_new_ests()
dev.off()






## Plot model estimates for new cases --------------------------------------


dev.off()


## Plot observed data for previous cases -----------------------------------



## Plot model estimates for previous cases ---------------------------------

## Model: TT 2014-2019
par(new = TRUE)

plot(data$diag_qrt, (data$fitted_RR / data$total_TB_cases) * 100,
     pch = 15, xlab = "", ylab = "", ylim = c(0, 35),
     axes = FALSE, col = "blue", type = "l"
)


## Model: TT 2015-2019
par(new = TRUE)

data <- compiled_results[["nat_qrt"]] %>% filter(model == "tt_2015-2019" & case_type == "prev")

plot(data$diag_qrt, (data$fitted_RR / data$total_TB_cases) * 100,
     pch = 15, xlab = "", ylab = "", ylim = c(0, 35), xlim = as.Date(c("2015-01-01", "2019-10-01")),
     axes = FALSE, col = "blue", type = "l", lty = 2
)


## Model: SP 2014-2019
data <- compiled_results[["nat_qrt"]] %>% filter(model == "sp_2014-2019" & case_type == "prev")

par(new = TRUE)

plot(data$diag_qrt, (data$fitted_RR / data$total_TB_cases) * 100,
     pch = 15, xlab = "", ylab = "", ylim = c(0, 35),
     axes = FALSE, col = "purple", type = "l"
)


## Model: SP 2015-2019
data <- compiled_results[["nat_qrt"]] %>% filter(model == "sp_2014-2019" & case_type == "prev")

par(new = TRUE)

plot(data$diag_qrt, (data$fitted_RR / data$total_TB_cases) * 100,
     pch = 15, xlab = "", ylab = "", ylim = c(0, 35), xlim = as.Date(c("2015-01-01", "2019-10-01")),
     axes = FALSE, col = "purple", type = "l", lty = 2
)





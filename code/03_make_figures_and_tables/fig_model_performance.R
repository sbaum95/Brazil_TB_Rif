# Separate plotting into functions ----------------------------------------

plot_observed_tested <- function(quarter, case_type1 = NULL, case_type2 = NULL) {
  data <- compiled_results[["nat_qrt"]] %>%
    group_by(case_type) %>%
    filter(model == "sp_2014") %>%
    filter(diag_qrt >= quarter)


  # Determine whether plot both case types or only one
  if (!is.null(case_type1) & !is.null(case_type2)) {
    data_plot <- data %>% filter(case_type %in% c(case_type1, case_type2))
  } else if (!is.null(case_type1)) {
    data_plot <- data %>% filter(case_type == case_type1)
  } else {
    data_plot <- data %>% filter(case_type == case_type2)
  }

  # set max for y axis
  # y_max <- max((data_plot$obs_RR/data_plot$obs_num_tested)*1000 + 5, (data_plot$fitted_RR/data_plot$total_TB_cases)*1000 + 5) %>% round()
  #
  # make plot
  data_plot %>%
    ggplot() +

    # Percent tested among observed
    geom_line(aes(x = diag_qrt, y = (obs_pct_tested * 100), color = case_type))
}

plot_observed_rr <- function(quarter, case_type1 = NULL, case_type2 = NULL) {
  data <- compiled_results[["nat_qrt"]] %>%
    group_by(case_type) %>%
    filter(model == "sp_2014") %>%
    filter(diag_qrt >= quarter)


  # Determine whether plot both case types or only one
  if (!is.null(case_type1) & !is.null(case_type2)) {
    data_plot <- data %>% filter(case_type %in% c(case_type1, case_type2))
  } else if (!is.null(case_type1)) {
    data_plot <- data %>% filter(case_type == case_type1)
  } else {
    data_plot <- data %>% filter(case_type == case_type2)
  }

  # set max for y axis
  # y_max <- max((data_plot$obs_RR/data_plot$obs_num_tested)*1000 + 5, (data_plot$fitted_RR/data_plot$total_TB_cases)*1000 + 5) %>% round()
  #
  # make plot
  data_plot %>%
    ggplot() +

    # Observed incidence
    geom_point(aes(x = diag_qrt, y = (obs_RR / obs_num_tested) * 100, color = case_type, size = obs_num_tested), alpha = 0.5)
}

plot_model <- function(plot, agg_level, model_name, case_type) {
  plot +
    geom_line(
      data = compiled_results[[agg_level]] %>%
        filter(model == model_name & case_type == case_type),
      aes(x = diag_qrt, y = (fitted_RR / total_TB_cases) * 100, color = case_type, linetype = model)
    )
}


# Plot in ggplot ----------------------------------------------------------
fig_obs <- plot_observed_tested(quarter = "2014-01-01", "new", "prev") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 50),
    labels = scales::label_percent(scale = 1),
    name = "Percent with conclusive result"
  ) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y" 
  ) +
  xlab("Quarter") +
  ggtitle("A) Proportion tested with Xpert MTB/RIF") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 9),
    legend.text = element_text(size = 12),
    title = element_text(size = 12),
    plot.margin = margin(1, 1, 0, 1, "cm")
  ) +
  scale_color_manual(
    name = "",
    labels = c(
      "",
      ""
    ),
    values = c(
      new_color,
      prev_color
    )
  ) +
  theme(legend.position = "none")




fig_sp <- plot_observed_rr(quarter = "2017-01-01", "new", "prev") +
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "sp_2017" & case_type == case_type),
    aes(x = diag_qrt, y = (fitted_RR / total_TB_cases) * 100, color = case_type)
  ) +
  scale_y_continuous(
    expand = c(0, 0), # So X-axis set at 0
    limits = c(0, 10.5),
    labels = scales::label_percent(scale = 1)
  ) +
  scale_x_date(
    date_breaks = "1 year", # Set breaks to 1 year
    date_labels = "%Y" # Format labels as year
  ) +
  xlab("Quarter") +
  ylab("Percent with RR-TB") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 12),
    title = element_text(size = 12),
    plot.margin = margin(1, 0, 0, 0.5, "cm")
  ) +
  scale_color_manual(
    name = "Case type",
    labels = c(
      "New",
      "Previous"
    ),
    values = c(
      new_color,
      prev_color
    )
  ) +
  scale_size(
    name = "Observed number of cases tested with Xpert",
    range = c(0.5, 5)
  ) +
  ggtitle("B) Proportion with RR-TB (Comparing naive and modeled)") +
  scale_linetype_manual(
    name = "Projected",
    labels = c("2017-2023"),
    values = c("sp_2017" = "solid")
  )


# Sensitivity analyses
# Sensitivity 1: Time periods 
fig_sens_1 <- plot_observed_rr(quarter = "2014-01-01", "new", "prev") +
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "sp_2014" & case_type == case_type),
    aes(x = diag_qrt, y = (fitted_RR / total_TB_cases) * 100, color = case_type, linetype = model)
  ) +
  geom_line(
    data = compiled_results[["nat_qrt"]] %>%
      filter(model == "sp_2017" & case_type == case_type),
    aes(x = diag_qrt, y = (fitted_RR / total_TB_cases) * 100, color = case_type, linetype = model)
  ) +
  scale_y_continuous(
    expand = c(0, 0), # So X-axis set at 0
    limits = c(0, 35),
    labels = scales::label_percent(scale = 1)
  ) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y" 
  ) +
  xlab("Quarter") +
  ylab("Percent with RR-TB") +
  ggtitle("A) Comparing time periods") + 
  scale_color_manual(
    name = "Case type",
    labels = c(
      "New",
      "Previous"
    ),
    values = c(
      new_color,
      prev_color
    )
  ) +
  scale_size(
    name = "Observed number of cases tested with Xpert",
    range = c(0.5, 5) 
  )  +
  scale_linetype_manual(
    name = "Panel A: Time period",
    labels = c(
      "sp_2014" = "2014-2023",
      "sp_2017" = "Reference (2017-2023)"
    ),
    values = c(
      "sp_2014" = "dashed",
      "sp_2017" = "solid"
    )
  ) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 12),
    title = element_text(size = 12),
    plot.margin = margin(1, 0, 0, 0.5, "cm")
  )


## Sensitivity 2: Specifications
fig_sens_2 <- plot_observed_rr(quarter = "2017-01-01", "new", "prev") %>%
  plot_model(agg_level = "nat_qrt", model_name = "sp_2017") %>%
  plot_model(agg_level = "nat_qrt", model_name = "sens_1") %>%
  plot_model(agg_level = "nat_qrt", model_name = "sens_2") +
  scale_y_continuous(
    expand = c(0, 0), 
    limits = c(0, 10.5),
    labels = scales::label_percent(scale = 1)
  ) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y" 
  ) +
  xlab("Quarter") +
  ylab("") +
  ggtitle("B) Comparing specifications") +
  scale_color_manual(
    name = "Case type",
    labels = c(
      "New",
      "Previous"
    ),
    values = c(
      new_color,
      prev_color
    ), 
    guide = FALSE
  ) +
  scale_size(
    name = "Observed number of cases tested with Xpert",
    range = c(0.5, 5), 
    guide = FALSE
  ) +
  scale_linetype_manual(
    name = "Panel B: Specification",
    labels = c(
      "sp_2017" = "Reference",
      "sens_1" = "Additional patient covariates",
      "sens_2" = "Time-varying selection"
    ),
    values = c(
      "sp_2017" = "solid",
      "sens_1" = "dashed",
      "sens_2" = "dotted"
    )
  ) + 
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 12),
    title = element_text(size = 12),
    plot.margin = margin(1, 1, 0, 0.5, "cm")
  )



# Save plots --------------------------------------------------------------
fig_model_performance <- fig_obs | fig_sp
fig_sens <- fig_sens_1 + fig_sens_2 + plot_layout(nrow = 1, guides = "collect")

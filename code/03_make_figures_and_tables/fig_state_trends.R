# Create function to get plots by state-quarter ---------------------------
fig_state <- function(state_list, model_name, case_type1 = NULL, case_type2 = NULL) {
  data <- compiled_results[["state_qrt"]] %>%
    # left_join(., pop_UF, by = c("state_nm" = "state")) %>%
    mutate(tb_inc = (total_TB_cases / pop_2010) * 4 * 100000) %>%
    filter(state_nm %in% state_list) %>%
    filter(model == model_name) %>%
    group_by(diag_qrt, state_nm) %>%
    mutate(
      obs_inc = if_else(obs_RR != 0, (((obs_RR / obs_num_tested) * total_TB_cases) / pop_2010) * 4 * 100000, 0),
      mod_inc = (fitted_RR / pop_2010) * 4 * 100000,
      inc_lci = (proj_lci / pop_2010) * 4 * 100000,
      inc_hci = (proj_hci / pop_2010) * 4 * 100000
    )

  data$facet_state <- factor(data$state_nm, levels = state_list)


  # Determine whether plot both case types or only one
  if (!is.null(case_type1) & !is.null(case_type2)) {
    data_plot <- data %>% filter(case_type %in% c(case_type1, case_type2))
  } else if (!is.null(case_type1)) {
    data_plot <- data %>% filter(case_type == case_type1)
  } else {
    data_plot <- data %>% filter(case_type == case_type2)
  }


  ggplot(data_plot) +
    # Add line for projected incidence for each model (2017-2019)
    geom_line(aes(x = diag_qrt, y = mod_inc, color = "Projected")) +
    geom_ribbon(aes(x = diag_qrt, ymin = inc_lci, ymax = inc_hci, color = "Projected"), alpha = 0.5, fill = "lightgray", color = NA) +
    # Add line for observed incidence for each model (2017-2019)
    geom_line(aes(x = diag_qrt, y = obs_inc, color = "Observed (Xpert Only)")) +
    scale_y_continuous(
      expand = c(0, 0),
      breaks = scales::pretty_breaks(n = 5)
    ) +
    facet_wrap(~facet_state)
}


set_base_aes_specs <- function(by_state, case_type1) {
  if (case_type1 == "new") {
    by_state +
      scale_x_date(
        date_breaks = "1 year", # Set breaks to 1 year
        date_labels = "%Y" # Format labels as year
      ) +
      xlab("Year") +
      ylab("RR-TB cases per 100,000 person-years") +
      ggtitle("A) New Cases") +
      scale_size(
        range = c(0.5, 5)
      ) +
      scale_color_manual(
        name = "",
        values = c(pal[1], pal[3])
      ) +
      theme_bw() +
      theme(
        legend.position = c("none"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
        axis.text.y = element_text(size = 9),
        legend.text = element_text(size = 9),
        title = element_text(size = 10),
        strip.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)
      )
  } else {
    by_state +
      scale_x_date(
        date_breaks = "1 year", # Set breaks to 1 year
        date_labels = "%Y" # Format labels as year
      ) +
      xlab("Year") +
      ylab("RR-TB cases per 100,000 person-years") +
      ggtitle("B) Previous Cases") +

      scale_size(
        range = c(0.5, 5)
      ) +
      scale_color_manual(
        name = "",
        labels = c(
          "Observed (Xpert Only)",
          "Projected"
        ),
        values = c(pal[1], pal[3])
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
        axis.text.y = element_text(size = 9),
        legend.text = element_text(size = 9),
        title = element_text(size = 10),
        strip.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)
      )
  }
}



# Execute function --------------------------------------------------------
# New cases
state_list_new <- compiled_results[["state_yr"]] %>%
  filter(year == 2023 & case_type == "new" & model == "sp_2017" & obs_pct_tested > 0.3) %>%
  mutate(
    tb_inc = (total_TB_cases / pop_2010) * 100000,
    rr_inc = (fitted_RR / pop_2010) * 100000
  ) %>%
  arrange(desc(rr_inc)) %>%
  select(state_nm) %>%
  unique() %>%
  head(8) %>%
  pull()


fig_state_new_sp <- fig_state(state_list = state_list_new, model_name = "sp_2017", case_type1 = "new", case_type2 = NULL) %>%
  set_base_aes_specs(case_type1 = "new")

# Previous cases
state_list_prev <- compiled_results[["state_yr"]] %>%
  mutate(tb_inc = (total_TB_cases / pop_2010) * 100000) %>%
  filter(year == 2023 & case_type == "prev" & model == "sp_2017" & obs_pct_tested > 0.3) %>%
  mutate(
    tb_inc = (total_TB_cases / pop_2010) * 100000,
    rr_inc = (fitted_RR / pop_2010) * 100000
  ) %>%
  arrange(desc(rr_inc)) %>%
  select(state_nm) %>%
  unique() %>%
  head(8) %>%
  pull()


fig_state_prev_sp <- fig_state(state_list = state_list_prev, model_name = "sp_2017", case_type1 = "prev") %>%
  set_base_aes_specs(case_type1 = "prev")

# Combine
fig_state_trends <- fig_state_new_sp + fig_state_prev_sp

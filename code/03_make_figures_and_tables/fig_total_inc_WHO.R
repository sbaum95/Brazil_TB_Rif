
# Load WHO data -----------------------------------------------------------
who_mdr <- read_csv("data/MDR_RR_TB_burden_estimates_2024-04-03.csv") %>%
  filter(country == "Brazil") %>%
  filter(year >= 2017 & year <= 2023) %>%
  select(year, e_rr_pct_new, e_rr_pct_new_lo, e_rr_pct_new_hi, e_rr_pct_ret, e_rr_pct_ret_lo, e_rr_pct_ret_hi, e_inc_rr_num, e_inc_rr_num_lo, e_inc_rr_num_hi) %>%
  pivot_longer(
    cols = c(e_rr_pct_new, e_rr_pct_ret),
    names_to = c(".value", "case_type"),
    names_sep = "_(?=[a-z]+$)"
  ) %>%
  rename(group = case_type) %>%
  pivot_longer(
    cols = c(e_rr_pct_new_lo, e_rr_pct_ret_lo),
    names_to = c("lo", "case_type"),
    names_sep = "e_rr_pct_",
    values_to = "pct_lo"
  ) %>%
  mutate(case_type = if_else(case_type == "new_lo", "new", "ret")) %>%
  filter(group == case_type) %>%
  select(!case_type) %>%
  pivot_longer(
    cols = c(e_rr_pct_new_hi, e_rr_pct_ret_hi),
    names_to = c("hi", "case_type"),
    names_sep = "e_rr_pct_",
    values_to = "pct_hi"
  ) %>%
  mutate(case_type = if_else(case_type == "new_hi", "new", "ret")) %>%
  filter(group == case_type) %>%
  select(!c(group, lo, hi)) %>%
  mutate(time = if_else(year == 2017, 14,
    if_else(year == 2018, 18,
      if_else(year == 2019, 22,
        if_else(year == 2020, 26,
          if_else(year == 2021, 30, 34)
        )
      )
    )
  ))

# Repeat 2022 estimates for 2023 
repeat_2022_mdr <- subset(who_mdr, year == 2022)
repeat_2022_mdr$year <- 2023

who_mdr <- rbind(who_mdr, repeat_2022_mdr)




who_data_new <- compiled_results[["nat_yr"]] %>%
  filter(model == "sp_2017" & case_type == "new") %>%
  select(year, total_TB_cases) %>%
  left_join(., who_mdr %>% filter(case_type == "new"), by = "year") %>%
  mutate(
    who_count = ((e_rr_pct / 100) * total_TB_cases),
    who_count_lo = ((pct_lo / 100) * total_TB_cases),
    who_count_hi = ((pct_hi / 100) * total_TB_cases),
    who_inc = (who_count / pop_2010) * 100000,
    who_inc_lo = (who_count_lo / pop_2010) * 100000,
    who_inc_hi = (who_count_hi / pop_2010) * 100000
  ) %>%
  filter(year <= 2023) %>%
  mutate(
    diag_yr = as.Date(as.character(year), format = "%Y"),
    diag_qrt = floor_date(as_date(diag_yr), "year")
  )

# Repeat 2022 estimates for 2023 
repeat_2022_new <- subset(who_data_new, year == 2022)
repeat_2022_new$year <- 2023
who_data_new <- rbind(who_data_new, repeat_2022_new)



who_data_prev <- compiled_results[["nat_yr"]] %>%
  filter(model == "sp_2017" & case_type == "prev") %>%
  select(year, total_TB_cases) %>%
  left_join(., who_mdr %>% filter(case_type == "ret"), by = "year") %>%
  mutate(
    who_count = ((e_rr_pct / 100) * total_TB_cases),
    who_count_lo = ((pct_lo / 100) * total_TB_cases),
    who_count_hi = ((pct_hi / 100) * total_TB_cases),
    who_inc = (who_count / pop_2010) * 100000,
    who_inc_lo = (who_count_lo / pop_2010) * 100000,
    who_inc_hi = (who_count_hi / pop_2010) * 100000
  ) %>%
  filter(year <= 2023) %>%
  mutate(
    diag_yr = as.Date(as.character(year), format = "%Y"),
    diag_qrt = floor_date(as_date(diag_yr), "year")
  )

# Repeat 2022 estimates for 2023 
repeat_2022_prev <- subset(who_data_prev, year == 2022)
repeat_2022_prev$year <- 2023
who_data_prev <- rbind(who_data_prev, repeat_2022_prev)


who_data_total <- who_mdr %>%
  select(time, year, e_inc_rr_num, e_inc_rr_num_lo, e_inc_rr_num_hi) %>%
  unique() %>%
  mutate(
    who_inc = (e_inc_rr_num / pop_2010) * 100000,
    who_inc_lo = (e_inc_rr_num_lo / pop_2010) * 100000,
    who_inc_hi = (e_inc_rr_num_hi / pop_2010) * 100000
  ) %>%
  mutate(
    diag_yr = as.Date(as.character(year), format = "%Y"),
    diag_qrt = floor_date(as_date(diag_yr), "year")
  )


# Prepare modeled data 
data_type <- data.frame(
  pop_2010 = pop_2010,
  dst = sinan_tmp %>%
    filter(diag_qrt >= "2017-01-01" & tratamento %in% c(1, 2, 3) & situa_ence != "06") %>%
    mutate(
      test_sensi = as.factor(test_sensi),
      case_type = if_else(tratamento == "1", "new", "prev")
    ) %>%
    group_by(diag_qrt) %>%
    summarize(tested = sum(test_sensi %in% c("1", "2", "3", "4", "5")), 
              cases = sum(test_sensi %in% c("2", "3")), # At least RR+  
              pct = (cases / tested) * 100
    ), 
  observed = compiled_results[["nat_qrt"]] %>%
    filter(model == "sp_2017") %>%
    ungroup() %>%
    group_by(diag_qrt, case_type) %>%
    summarize(
      cases = sum(obs_RR),
      pct = sum(obs_RR) / sum(obs_num_tested) * 100
    ),
  projected = compiled_results[["nat_qrt"]] %>%
    filter(model == "sp_2017") %>%
    ungroup() %>%
    group_by(diag_qrt, case_type) %>%
    summarize(
      cases = sum(fitted_RR),
      pct = sum(fitted_RR) / sum(total_TB_cases) * 100,
      pct_lci = sum(proj_lci) / sum(total_TB_cases) * 100,
      pct_hci = sum(proj_hci) / sum(total_TB_cases) * 100
    ),
  tb = compiled_results[["nat_qrt"]] %>%
    filter(model == "sp_2017") %>%
    group_by(diag_qrt, case_type) %>%
    summarize(cases = sum(total_TB_cases)
  )
) %>%
  rename(
    diag_qrt = projected.diag_qrt,
    case_type = projected.case_type
  ) %>%
  group_by(diag_qrt, case_type) %>%
  mutate(total.observed = sum(observed.cases + dst.cases)) %>%
  select(diag_qrt, case_type, total.observed, tb.cases, pop_2010, dst.cases, dst.pct, projected.cases, projected.pct, projected.pct_lci, projected.pct_hci)


data_total <- data.frame(
  pop_2010 = pop_2010,
  dst = sinan_tmp %>%
    filter(diag_qrt >= "2017-01-01" & tratamento %in% c(1, 2, 3) & situa_ence != "06") %>%
    mutate(
      test_sensi = as.factor(test_sensi),
      case_type = if_else(tratamento == "1", "new", "prev")
    ) %>%
    group_by(diag_qrt) %>%
    summarize(tested = sum(test_sensi %in% c("1", "2", "3", "4", "5")), 
              cases = sum(test_sensi %in% c("2", "3"))  # At least RR+  
    ),
  observed = compiled_results[["nat_qrt"]] %>%
    filter(model == "sp_2017") %>%
    ungroup() %>%
    group_by(diag_qrt) %>%
    summarize(cases = sum(obs_RR)
    ),
  projected = compiled_results[["nat_qrt"]] %>%
    filter(model == "sp_2017") %>%
    ungroup() %>%
    group_by(diag_qrt) %>%
    summarize(cases = sum(fitted_RR), 
              lci = sum(proj_lci), 
              hci = sum(proj_hci)
    ),
  cdr = compiled_results[["nat_qrt"]] %>%
    filter(model == "sp_2017") %>%
    group_by(diag_qrt) %>%
    summarize(cases = if_else(diag_qrt <= "2017-01-01", sum(fitted_RR) / 0.87,
                              if_else(diag_qrt > "2017-01-01" & diag_qrt < "2020-01-01", sum(fitted_RR) / 0.89,
                                      if_else(diag_qrt >= "2020-01-01" & diag_qrt < "2021-01-01", sum(fitted_RR) / 0.78,
                                              if_else(diag_qrt >= "2021-01-01" & diag_qrt < "2022-01-01", sum(fitted_RR) / 0.76,
                                                      if_else(diag_qrt >= "2022-01-01", sum(fitted_RR) / 0.83, NA)
                                              )
                                      )
                              )
    )
    ) %>%
    unique(),
  tb = compiled_results[["nat_qrt"]] %>%
    filter(model == "sp_2017") %>%
    group_by(diag_qrt) %>%
    summarize(cases = sum(total_TB_cases)),
  case_type = "total"
) %>%
  rename(diag_qrt = projected.diag_qrt) %>%
  group_by(diag_qrt) %>%
  mutate(total.observed = sum(observed.cases + dst.cases)) %>%
  select(diag_qrt, total.observed, observed.cases, projected.cases, projected.lci, projected.hci, cdr.cases, tb.cases, pop_2010) %>%
  group_by(diag_qrt) %>% 
  # Get person-years from person-quarters
  summarize(observed_xpert_inc = (observed.cases/pop_2010)*(4) * 100000, 
            observed_total_inc = (total.observed/pop_2010)*(4) * 100000, 
            projected_inc = (projected.cases/pop_2010)*(4) * 100000, 
            projected_inc_lci = (projected.lci/pop_2010)*(4) * 100000, 
            projected_inc_hci = (projected.hci/pop_2010)*(4) * 100000,
            cdr_inc = (cdr.cases/pop_2010)*(4) * 100000)
  

# Make plot ---------------------------------------------------------------
fig_total_inc_WHO <- ggplot() +
  # 1. Plot new cases
  geom_line(
    data = data_type %>%
      filter(case_type == "new"),
    aes(x = diag_qrt, y = projected.pct, color = "Modeled")
  ) +
  geom_ribbon(
    data = data_type %>%
      filter(case_type == "new"),
    aes(x = diag_qrt, ymin = projected.pct_lci, ymax = projected.pct_hci, color = "Modeled"), alpha = 0.5, fill = "lightgray", color = NA
  ) +
  geom_point(
    data = who_data_new,
    aes(x = diag_qrt, y = e_rr_pct, color = "WHO"),
  ) +
  geom_errorbar(
    data = who_data_new,
    aes(x = diag_qrt, ymin = pct_lo, ymax = pct_hi, color = "WHO"), width = 75, alpha = 0.6
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 25),
    labels = scales::label_percent(scale = 1)
  ) +
  scale_x_date(
    date_breaks = "1 year", # Set breaks to 1 year
    date_labels = "%Y" # Format labels as year
  ) +
  ggtitle("A) New") +
  ylab("Proportion of notified TB cases with RR-TB") +
  xlab("") +
  scale_color_manual(
    name = "",
    values = c(projected_color, who_color)
  ) +
  theme_bw() +
  theme(
    legend.position = c("none"),
    plot.title = element_text(hjust = 0.5)
  ) +

  # 2. Plot previous cases
  ggplot() +
  geom_line(
    data = data_type %>%
      filter(case_type == "prev"),
    aes(x = diag_qrt, y = projected.pct, color = "Modeled")
  ) +
  geom_ribbon(
    data = data_type %>%
      filter(case_type == "prev"),
    aes(x = diag_qrt, ymin = projected.pct_lci, ymax = projected.pct_hci, color = "Modeled"), alpha = 0.5, fill = "lightgray", color=NA
  ) +
  geom_point(
    data = who_data_prev,
    aes(x = diag_qrt, y = e_rr_pct, color = "WHO")
  ) +
  geom_errorbar(
    data = who_data_prev,
    aes(x = diag_qrt, ymin = pct_lo, ymax = pct_hi, color = "WHO"), width = 75, alpha = 0.6
  ) +
  ggtitle("B) Previously Treated") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 25),
    labels = scales::label_percent(scale = 1)
  ) +
  scale_x_date(
    date_breaks = "1 year", # Set breaks to 1 year
    date_labels = "%Y" # Format labels as year
  ) +
  xlab("Year") +
  ylab("") +
  theme_bw() +
  scale_color_manual(
    name = "",
    values = c(projected_color, who_color)
  ) +
  theme(
    legend.position = c("none"),
    plot.title = element_text(hjust = 0.5)
  ) +


  # Plot total incidence per 100,000 PY
  ggplot() +
  # Observed - Xpert + DST
  geom_line(
    data = data_total,
    aes(x = diag_qrt, y = observed_total_inc, color = "C Naïve total")
  ) +
  geom_line(
    data = data_total,
    aes(x = diag_qrt, y = observed_xpert_inc, color = "D Naïve xpert")
  ) + 
  # Projected
  geom_line(
    data = data_total,
    aes(x = diag_qrt, y = projected_inc, color = "B Modeled")
  ) +
  geom_ribbon(
    data = data_total,
    aes(x = diag_qrt, ymin = projected_inc_lci, ymax = projected_inc_hci, color = "B Modeled"), alpha = 0.5, fill = "lightgray", color=NA
  ) +
  # CDR
  geom_line(
    data = data_total,
    aes(x = diag_qrt, y = cdr_inc, color = "A CDR-inflated")
  ) +
  # WHO
  geom_point(
    data = who_data_total,
    aes(x = diag_qrt, y = who_inc, color = "E WHO")
  ) +
  geom_errorbar(
    data = who_data_total,
    aes(x = diag_qrt, ymin = who_inc_lo, ymax = who_inc_hi, color = "E WHO"), width = 75, alpha = 0.6
  ) +
  ggtitle("C) Total") +
  scale_y_continuous(
    name = "RR-TB cases per 100,000 person-years",
    expand = c(0, 0),
    limits = c(0, 5)
  ) +
  scale_x_date(
    date_breaks = "1 year", # Set breaks to 1 year
    date_labels = "%Y" # Format labels as year
  ) +
  xlab("") +
  theme_bw() +
  scale_color_manual(
    name = "",
    labels = c(
      "A CDR-inflated" = "CDR-inflated", 
      "B Modeled" = "Modeled",
      "C Naïve total" = "Naïve (Xpert + DST)",
      "D Naïve xpert" = "Naïve (Xpert)",
      "E WHO" = "WHO"
    ),
    values = c(cdr_color, projected_color, observed_all_color, observed_xpert_color, who_color)
  ) +
  theme(plot.title = element_text(hjust = 0.5))

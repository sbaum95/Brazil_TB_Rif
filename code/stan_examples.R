source("code/dependencies.R")

library(rstan)
options(digits=4)
options(scipen = 999)

load("output/compiled_results_20240604.Rdata")
load("output/fitted_values_20240604.Rdata")

# Get re-entry and relapse cases
fitted_prev <- fitted_values[["sp_2017_prev"]] %>%
  filter(!is.na(fitted)) %>%
  mutate(year = case_when(
    time <= 4 ~ 2014,
    time > 4 & time <= 8 ~ 2015,
    time > 8 & time <= 12 ~ 2016,
    time > 12 & time <= 16 ~ 2017,
    time > 16 & time <= 20 ~ 2018,
    time > 20 & time <= 24 ~ 2019,
    time > 24 & time <= 28 ~ 2020,
    time > 28 & time <= 32 ~ 2021,
    time > 32 & time <= 36 ~ 2022,
    time > 36 & time <= 40 ~ 2023
  )) %>%
  group_by(time, tratamento) %>%
  summarize(fitted_RR = sum((fitted * (cases - (negative + positive))) + (negative * 0) + positive)) %>% 
  pivot_wider(names_from = "tratamento", 
              values_from = c(fitted_RR))



stan_data <- compiled_results[["nat_qrt"]] %>% 
  filter(model == "sp_2017") %>% 
  select(time, case_type, fitted_RR, total_TB_cases, obs_pct_tested) %>% 
  pivot_wider(names_from = "case_type", 
              values_from = c(fitted_RR, total_TB_cases, obs_pct_tested)) %>% 
  merge(fitted_prev, by = "time")

pop_UF <- read_excel("data/Brazil_population_UF.xls", skip = 6) %>%
  rename(
    pop_2010 = "...2",
    state = "...1"
  ) %>%
  select(state, pop_2010) %>%
  filter(!is.na(state)) %>%
  filter(!state %in% c("Brasil", "Sudeste", "Centro-Oeste", "Norte", "Nordeste", "Sul"))

pop_2010 = sum(pop_UF$pop_2010)

# Create data for model
model1dat <- list(
  n_quarters = nrow(stan_data), 
  x = stan_data$time, 
  pr_tested = stan_data$obs_pct_tested_new, 
  notif_rr = round(stan_data$fitted_RR_new, 0), 
  reentry_rr = round(stan_data$reentry, 0), 
  relapse_rr = round(stan_data$relapse, 0), 
  pop = rep(as.integer(pop_2010), 28)
)


# Run stan examples
model1stan <- stan_model(file = "code/mech_model.stan")
model1mcmc <- sampling(model1stan, data = model1dat, chains = 4, iter = 5000, seed = 123456)
model1mcmc
traceplot(model1mcmc)
  
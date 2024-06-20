source("code/dependencies.R")

library(rstan)
options(digits=4)
options(scipen = 999)

stan_data <- compiled_results[["nat_qrt"]] %>% 
  filter(case_type == "new" & model == "sp_2017") %>% 
  select(time, fitted_RR, total_TB_cases) %>% 
  mutate(notif_ds = total_TB_cases - fitted_RR)

model1dat <- list(
  n_quarters = nrow(stan_data), 
  x = stan_data$time, 
  notif_rr = round(stan_data$fitted_RR, 0), 
  notif_ds = round(stan_data$notif_ds, 0), 
  pop = rep(as.integer(pop_2010), 28)
)


# Run stan examples
model1stan <- stan_model(file = "code/mech_model.stan")
model1mcmc <- sampling(model1stan, data = model1dat, chains = 4, iter = 1000)
model1mcmc
traceplot(model1mcmc)
  
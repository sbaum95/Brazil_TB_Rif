//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//



// Define variables in the data: 
data {
  int<lower=0> n_quarters_tot; // number of time periods 
  int < lower=0 > notif_rr[n_quarters_tot]; // number of notified cases per quarter
  int < lower=0 > pop[n_quarters_tot]; // country population per quarter
}


// Define parameters to be estimated
parameters {

  //real<lower=0> active_rr_cases; 
  real<lower=0> beta_rr; 
  //real<lower=0> pr_dev_act;
  real<lower=0> pr_notified[n_quarters_tot];
  //real<lower=0> rr_inc[n_quarters_tot]; 
  real<lower=0> never_infected_pop[n_quarters_tot]; 
  //real<lower=0> rr_inc; 
  
}

transformed parameters  {
  vector[n_quarters_tot]latent_rr_cases; 
    
  real rr_inc[n_quarters_tot];
  
  for (i in 1:n_quarters_tot) {
    // Mean
    rr_inc[i] = beta_rr * (latent_rr_cases[i-1]/pop[i-1]);
    
    //never_infected_pop[i] = pop - sum(latent_rr_cases[+1:i]; 
    }
  }



// The model to be estimated. 
model {
  
  // Prior
  beta_rr ~ normal(2,1);   // Same across states and across time
  //pr_dev_act ~ binomial(0,1);
  pr_notified ~ normal(0.5,1);
  
  // Likelihood 
  // Latent cases - keep as a loop for now and then vectorize once you understand (can also use targets)
  for(i in 1:n_quarters_tot) {
    //active_rr_cases[i] = pr_dev_act  *  latent_rr[i-1]; 
    
    latent_rr_cases[i] ~ poisson_log(rr_inc[i]); 
    
    //rr_inc[i] ~ exp([]); 
  
    notif_rr[i] = latent_rr_cases[i-1] * pr_notified

  }
  
  
  
  
  
  
  
  
  // Likelihood
  
  // INFECTIONS
  // New RR-TB Infections: Number of new RR-TB infections (latent state) per quarter
  // latent_rr[t] = random_intercept[0,j] + slope[t+1,j] 
  latent_rr_cases[t] = latent_rr[0] + latent_rr[t-1]
  
  
  // latent_rr[t+1] = [beta_rr * prevalence (active_rr[t]/N)] * never-infected_pop[t]
  latent_rr_cases[t] = [beta_rr * (active_rr[t-1]/N)] * never_infected_pop[t]

  // never-infected_pop[t] = N - (sum(latent_rr[t-1]) + sum(latent_ds[t-1]) # Just sum up all folks that have ever been in the latent state
  // active_rr[t] = pr(latent_rr -> active_rr) * latent_rr[t-1]
  
  
  // New DS-TB Infections: Number of new DS-TB infections (latent state) per quarter, state
  // latent_ds[t] = random_intercept[0,j] + slope[t+1, j]
  // latent_ds[t+1] = [beta_ds * prevalence (active_ds[t]/N)] * never-infected_pop[t]
  // beta_ds 
  // never-infected_pop[t] = N - (sum(latent_rr[t-1]) + sum(latent_ds[t-1])
  // active_ds[t] = pr(latent_ds -> active_ds) * latent_ds[t-1]
  
  
  // NOTIFICATIONS
  // notif_rr[t] = active_rr[t-1] * pr(seek care) * pr(diagnosed with rr | has rr)
  // notif_ds[t] = active_ds[t-1] * pr(seek care)
  // pr(seek care) = Probability differs by case type since RR-TB is more severe, but for now assume it's the same and that the fraction seeking care is the same overtime
  // pr(diagnosed with rr | tested + Has rr) = Maybe ignore for now. Just assume everyone who has RR is appropriately diagnosed 
  
  

  
  



  
  RR_cases[i] ~ poisson(rr_inc*pop[i]*notified);
  rr_inc ~ exp(beta_r*(Ir/N))
  
}


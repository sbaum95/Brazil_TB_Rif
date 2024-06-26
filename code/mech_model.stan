// To do: 
// - Lit review for priors
// - Figure out how to run without specifying parameters in period 1


data {
  int<lower=0>           n_quarters; // Number of time periods (quarters)
  int                    notif_rr[n_quarters]; // New RR-TB notifications (modeled from previous paper)
  int                    reentry_rr[n_quarters]; // Reentry RR-TB notifications (modeled from previous paper)
  int                    relapse_rr[n_quarters]; // Relapse RR-TB notifications 
  
  vector[n_quarters]     pop; // Population (2010 Census)
  real                   pr_tested[n_quarters]; // Fraction of cases tested with Xpert
}


parameters {
  real<lower=0>           beta_rr; // Transmissibility of RR-TB
  real<lower=0, upper=1>  pr_lat_to_act; 
  real<lower=0, upper=1>  pr_notified;
  real<lower=0, upper=1>  pr_treatment_default;
}



transformed parameters{
  matrix<lower=0>[n_quarters, 1] pop_suscep;
  
  matrix<lower=0>[n_quarters, 1] rr_inc_rate;  // RR-TB inc rate (constant in every quarter) 
  matrix<lower=0>[n_quarters, 1] rr_lat_inc;  // Number of incident infections in quarter n
  matrix<lower=0>[n_quarters, 1] rr_act_inc; // Number of latent infections that have become active in quarter n
  
  matrix<lower=0>[n_quarters, 1] rr_first_line;
  matrix<lower=0>[n_quarters, 1] rr_second_line;
  
  // Set initial parameters
    rr_act_inc[1,1] = notif_rr[1] * (1/pr_notified); 
    
    rr_lat_inc[1,1] = rr_act_inc[1,1] * (1/pr_lat_to_act); // rr_act_inc[1,1] * (1/pr_lat_to_act)
  
    rr_inc_rate[1,1] = beta_rr * ((rr_lat_inc[1,1] +  rr_act_inc[1,1])/pop[1]); // Prevalence in 2017
    
    pop_suscep[1,1] = pop[1] - rr_lat_inc[1,1];
  
  
  // Incidence rate = transmissibility(beta_rr) * I/N :
  for (n in 2:n_quarters) { 
    rr_inc_rate[n,1] = beta_rr * ((sum(rr_lat_inc[1:n-1,1]) - sum(notif_rr[1:n-1])) / pop[n-1]); // RR-TB Incidence rate - Right 
    
    rr_lat_inc[n,1] = rr_inc_rate[n,1] * pop_suscep[n-1,1]; // Num. Latent infections = incidence rate * susceptible pop
    
    pop_suscep[n,1] = pop[n] - sum(rr_lat_inc[1:n-1]); // Never infected = Total pop - all latent infections
  }


  // Incidence of active RR-TB per quarter = the share of incidence cases in last quarter that progress to active disease 
  rr_act_inc[1,1] = rr_lat_inc[1,1] * pr_lat_to_act;
  
  for (n in 2:n_quarters){
    rr_act_inc[n,1] = rr_lat_inc[n-1,1] * pr_lat_to_act; 
    }
    
 // Treatment assignment among RR-TB cases
  for (n in 1:n_quarters){
    rr_first_line[n,1] = notif_rr[n] * pr_treatment_default;
    rr_second_line[n,1] = notif_rr[n] * pr_treatment_default;
    }

          
          
          
          
          
          
}

// The model to be estimated
model {
  
  //beta_rr ~ ;
  pr_lat_to_act ~ normal(0.9, 0.05);
  pr_notified ~ normal(0.9, 0.05);
  
  // Model new RR-TB notifications
  for (n in 1:n_quarters){
    notif_rr[n] ~ poisson(rr_act_inc[n,1] * pr_notified); // Number of fitted cases
    }
  
  // Model previous RR-TB cases 
  for (n in 2:n_quarters){
    reentry_rr[n] ~ poisson((rr_second_line[n-1,1] * pr_treatment_default) + (rr_first_line[n-1,1] * pr_treatment_default));
    relapse_rr[n] ~ poisson(((rr_second_line[n-1,1] * (1-pr_treatment_default)) + (rr_first_line[n-1,1] * (1-pr_treatment_default))) * rr_inc_rate[n-1,1]);
    }
}




generated quantities {
  vector[n_quarters] latent_rr_cases; // Number of new infections per quarter
  vector[n_quarters] failure_second_line;
  vector[n_quarters] init_rr_treated_ds;
  vector[n_quarters] reinfection_rr;
  
  for (n in 1:n_quarters){
    latent_rr_cases[n] = rr_lat_inc[n,1]; 
    failure_second_line[n] = rr_second_line[n,1] * pr_treatment_default;
    init_rr_treated_ds[n] = rr_first_line[n,1] * pr_treatment_default;
    reinfection_rr[n] = relapse_rr[n];
  }


}

  // for (n in 1:n_quarters) {
  //   rr_inc_rate[n] = alpha_rr + beta_rr[n] * x;
  //   //log_ds_inc_rate[n] = alpha_ds + beta_ds[n];
  // }
  // 
  // // Comput latent infections each quarter
  // for (n in 1:n_quarters) {
  // 
  //   //log_rr_inc[n] = log_pop[n] + log_rr_inc_rate[n];  // Update based on log_pop and log_rr_inc_rate
  //   rr_lat_inc[n] = exp(rr_inc_rate*(1-(sum infected/N))*
  //   
  //   log_rr_inc[n]);  // Calculate incidence of latent RR-TB per quarter
  //   

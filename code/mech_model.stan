// To do: 
// - Lit review for priors
// - Figure out how to run without specifying parameters in period 1


data {
  int<lower=0> n_quarters; // Number of time periods (quarters)
  int notif_rr[n_quarters]; // Expected RR-TB notifications (modeled from previus paper)
  vector[n_quarters] pop; // Log population (2010 Census)
}


parameters {
  real<lower=0>           beta_rr; // Transmissibility of RR-TB
  real<lower=0>           beta_ds; // Transmissibility of RR-TB
  real<lower=0, upper=1>  pr_lat_to_act; 
  real<lower=0, upper=1>  pr_notified;
}



transformed parameters{
  matrix<lower=0>[n_quarters, 1] pop_suscep;
  matrix<lower=0>[n_quarters, 1] rr_inc_rate;  // RR-TB inc rate (constant in every quarter) --> Needs to be a matrix when adding state-random effects
  matrix<lower=0>[n_quarters, 1] rr_lat_inc;  // Number of incident infections in quarter n
  matrix<lower=0>[n_quarters, 1] rr_act_inc; // Number of latent infections that have become active in quarter n
  
  matrix<lower=0>[n_quarters, 1] ds_inc_rate;  // RR-TB inc rate (constant in every quarter) --> Needs to be a matrix when adding state-random effects
  matrix<lower=0>[n_quarters, 1] ds_lat_inc;  // Number of incident infections in quarter n
  matrix<lower=0>[n_quarters, 1] ds_act_inc;
  
  
  
  // Set initial parameters
    rr_act_inc[1,1] = 955;
    
    rr_lat_inc[1,1] = (rr_act_inc[1,1]/pr_lat_to_act) * (1/pr_notified); // Should be notified RR
  
    rr_inc_rate[1,1] = beta_rr * (rr_lat_inc[1,1] +  rr_act_inc[1,1]); // Prevalence in 2017
    
    pop_suscep[1,1] = 190755799;
  
    // Calculate incidence rate = transmissibility(beta_rr) * I/N :
  for (n in 2:n_quarters) { 
    rr_inc_rate[n,1] = beta_rr * (sum(rr_lat_inc[1:n-1,1]) / pop[n-1]); // RR-TB Incidence rate = RR-TB transmissibility * (All latent infections/Pop)
    rr_lat_inc[n,1] = rr_inc_rate[n,1] * pop_suscep[n-1,1]; // Num. Latent infections = incidence rate * susceptible pop
    pop_suscep[n,1] = pop[n] - sum(rr_lat_inc[1:n-1]); // Never infected = Total pop - all latent infections
  }


  // Incidence of active RR-TB per quarter = the share of incidence cases in last quarter that progress to active disease 
  rr_act_inc[1,1] = rr_lat_inc[1,1] * pr_lat_to_act;
  
  for (n in 2:n_quarters){
          rr_act_inc[n,1] = rr_lat_inc[n-1,1] * pr_lat_to_act; 
          }
          
          
          
          
          
          
}

// The model to be estimated
model {
  
  //beta_rr ~ ;
  //pr_lat_to_act ~ ; 
  //pr_notified ~ ; Use CDR?
  
  // Model RR-TB notifications 
  for (n in 1:n_quarters){
    
    notif_rr[n] ~ poisson(rr_act_inc[n,1] * pr_notified); // Number of fitted cases
    }
}




// generated quantities {
//   
// // What outputs do we want? - What share of total new active RR-TB cases (Notified or underlying?) are from each pathway 
//   vector[n_quarters] latent_rr_cases; // Number of new infections per quarter 
//   
//   for (n in 1:n_quarters){
//     latent_rr_cases[n] = (rr_lat_inc[n]/exp(log_pop[n]))*100000; // Number of fitted cases
//   }
//   
//   
// }

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

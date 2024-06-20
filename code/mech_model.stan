// To do: 
// - Figure out how to model incidence (it's on the log scale...but that doesn't really make sense for how beta is being read in)
// - Figure out how to update population each time so it's only the never infecteds being infected
// - Lit review for priors 


// Not sampling because rr_lat_inc is < 0 (when it's bounded by 0) -- something funky going on with the alpha_rr values that I think is resulting in a susceptible pop that is less than 0

data {
  int<lower=0> n_quarters; // Number of time periods (quarters)
  int x [n_quarters]; // Time periods
  int notif_rr[n_quarters]; // Expected RR-TB notifications (modeled from previus paper)
  int notif_ds[n_quarters];
  int<lower=0> pop[n_quarters]; // Log population (2010 Census)
}


parameters {
  real<lower=0>  alpha_rr; // intercept
  real beta_rr;
  real<lower=0>  alpha_ds; // intercept
  real beta_ds[n_quarters];
  
  real<lower=0>  pr_lat_to_act;
  real<lower=0>  pr_notified;
}



transformed parameters{
  real rr_inc_rate[n_quarters];  // RR-TB inc rate (constant in every quarter)
  //real log_ds_inc_rate[n_quarters]; 
  
  //real log_ds_inc[n_quarters];  // Log of RR-TB incidence per quarter
  
  real pop_suscep[n_quarters];
  
  real<lower=0>  rr_lat_inc[n_quarters];  
  real<lower=0>  rr_act_inc[n_quarters];
  
  //real<lower=0>  ds_lat_inc[n_quarters];  
  //real<lower=0>  ds_act_inc[n_quarters];
  
  // Incidence Rate = transmissibility(beta_rr) * I/N : Constant for now
  
  for (n in 1:n_quarters) {
    
    if (n > 1){
      
    rr_inc_rate[n] = beta_rr * sum(rr_lat_inc[1:n-1]) / pop[n-1]; // Every 
    
    } else {
      
    rr_inc_rate[n] = alpha_rr; // Random intercept (mean)
  }

}
  
  for (n in 1:n_quarters) {  
    if (n < 2){
     pop_suscep[n] = pop[n] - (alpha_rr*pop[n]); // Never infected = 
     rr_lat_inc[n] = rr_inc_rate[n] * pop_suscep[n];
    
    } else {
     pop_suscep[n] = pop[n] - sum(rr_lat_inc[1:n-1]); // Never infected = 
     rr_lat_inc[n] = rr_inc_rate[n] * pop_suscep[n-1];
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
    
    
    
    

    //log_ds_inc[n] = log_pop[n] + log_ds_inc_rate[n];  // Update based on log_pop and log_rr_inc_rate
    //ds_lat_inc[n] = exp(log_ds_inc[n]);  // Calculate incidence of latent RR-TB per quarter
  
  // Incidence of active RR-TB per quarter = the share of incidence cases in last quarter that progress to active disease 
  for (n in 1:n_quarters){
    
    if (n > 1){
          rr_act_inc[n] = rr_lat_inc[n-1] * pr_lat_to_act; 
          //ds_act_inc[n] = ds_lat_inc[n-1] * pr_lat_to_act; 
          
          } else{
            
          rr_act_inc[n] = rr_lat_inc[1] * pr_lat_to_act;
          //ds_act_inc[n] = ds_lat_inc[1] * pr_lat_to_act;
          }
  }
}

// The model to be estimated
model {
  beta_rr ~ gamma(0.001,0.001); // Random prior on beta - Picked an uninformative prior, gamma distribution is bounded by 0
  //beta_ds ~ cauchy(0,2); // Random prior on beta
  
  alpha_rr ~ gamma(0.001,0.001); // Random prior on intercept - perhaps this can be incidence from national prev survey? 
  //alpha_ds ~ cauchy(0,2); // Random prior on intercept 
  pr_notified ~ normal(0.8,0.1);

  
  // Model RR-TB notifications 
  for (n in 2:n_quarters){
    notif_rr ~ poisson(rr_act_inc[n-1] * pr_notified); // Number of fitted cases
    //notif_ds ~ poisson(ds_act_inc[n-1] * pr_notified);
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



//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=1> X; // number of age categories 
  int<lower=1> R; // reported deaths 
  int<lower=2> S; // number of sexes
  int E_x[S,X]; // exposures by age group and sex 
  vector[X] E_age; // exposures by age group 
  matrix[S, X] pi_x_hat; // sex-specific means age-distributions - this model assumes they are inputs, but we can code a model for the age groups 
  matrix[S,X] pi_sd; // lower bounds for the age distribution estimates 
  matrix[S,X] mu_x_noc; //baseline mortality from the WPP 
  vector[X] mu_age_noc; // age specific baseline mortality
  int D_baseline[S,X]; 
}

transformed data {
row_vector[S] v_ones = rep_row_vector(1, S);

}


// The parameters accepted by the model
parameters {
  matrix[S,X] theta_x; 
  real<lower=0, upper=1> pr_raw; // overreporting or underreporting rate 
  
}

transformed parameters {
 // matrix[S, X] mu_x; // mortality rate in each age-sex group 
  matrix[S,X] pi_x; 
  matrix[S,X] R_x; 
  row_vector[X] R_age; 
  vector[X] mu_age; 
  vector[X] mu_age_total;
  matrix[S,X] D_baseline_tmp; // life expectancy
  real tmp;
  matrix[S, X] mu_x; // mortality rate in each age-sex group 
  matrix[S,X] log_mu_x; 
  real pr;
  matrix[S,X] mu_x_total; 
  row_vector[X] D_baseline_age;
  
  pr = pr_raw*0.14 + 0.52; //shifted beta distribution so that the mean is around 0.8 instead of 0.5 

  tmp = sum(exp(theta_x));
  for(s in 1:S){
    for(x in 1:X){
      pi_x[s,x] = exp(theta_x[s,x])/tmp; 
    //  D_baseline_tmp[s,x] = D_baseline[s,x];
    //  print(pi_x[s,x]);
      }
  }

  //print(tmp);
  for(s in 1:S){
    for(x in 1:X){
      R_x[s,x] =pi_x[s,x]*R; // generating each R_x from the age distributions 
      log_mu_x[s,x] = log(R_x[s,x]/E_x[s,x]) -log(pr); //compute the age-specific mortality rates 
      mu_x[s,x] = exp(log_mu_x[s,x]);  //exponentiate the mortality 
      mu_x_total[s,x] =  mu_x[s,x] + mu_x_noc[s,x]; 
     }
  }
  
  R_age = v_ones *R_x; // get reported deaths aggregated over sex 
 // D_baseline_age = v_ones*D_baseline_tmp; 
  for(x in 1:X){
    mu_age[x] = log(R_age[x]/E_age[x]) -log(pr); 
    mu_age_total[x] = exp(mu_age[x]) + mu_age_noc[x]; //mortality over sexes 
  }
   
}

model {
 pr_raw ~ beta(2,2); // reporting error multiplier 
  
  for(s in 1:S){
      for(x in 1:X){
        theta_x[s,x] ~ normal(pi_x_hat[s,x], pi_sd[s,x]); // priors on the age distributions - the model works without this, but this could also help w/ identifiability (see Schmertmann 2018)
        D_baseline[s,x] ~ poisson(E_x[s,x]*mu_x_noc[s,x]); 
      }
  }

}

generated quantities{
  int R_x_sim[S,X]; // generated reported deaths - sanity check to make sure our model produces reasonable estimates 
  int D_x_sim[S,X]; //estimated sex-specific deaths in each age group
//  real D_x_total[S,X];
  
  // generate reported deaths based on our priors for mortality and reporting rates 
  for(s in 1:S){
    for(x in 1:X){
        D_x_sim[s,x] = poisson_rng(E_x[s,x]*mu_x[s,x]); // compute age-adjusted deaths from our estimated mortalities 
        R_x_sim[s,x] =to_int(pi_x[s,x]*R); // generating each R_x from the age distributions 
      //  D_x_total[s,x] = D_x_sim[s,x] + D_x_noc[s,x];
    }
  }
  
}


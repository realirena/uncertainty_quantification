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
  int<lower=1> rep_cat; // number of reporting rate age groups 
  matrix[S,X] E_x; // exposures by age group and sex 
  vector[X] E_age; // exposures by age group 
  matrix[S, X] pi_x_hat; // sex-specific means age-distributions - this model assumes they are inputs, but we can code a model for the age groups 
  matrix[S,X] pi_sd; // lower bounds for the age distribution estimates 
  matrix[S,X] mu_x_noc; //baseline mortality from the WPP 
  vector[X] mu_age_noc; // age specific baseline mortality
  matrix[S, rep_cat] rep_ll; // lower bound for the reporting rate 
  matrix[S, rep_cat] rep_int; //interval length for the reporting rate
  int rep_cat_ind[X]; // list of length X indicating which reporting rate each age group should take 
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
  vector[X] mu_age_total;
//  matrix[S,X] D_baseline_tmp; // life expectancy
  real tmp;
  matrix[S, X] mu_x; // mortality rate in each age-sex group 
  matrix[S,X] log_mu_x; 
  matrix[S, rep_cat] pr;
  matrix[S,X] mu_x_total; 
  matrix[S,X] D_x_gen; 
  row_vector[X] D_age; 
 // row_vector[X] D_baseline_age;
  
  for(s in 1:S){ 
    for(r in 1:rep_cat){
        pr[s,r] = pr_raw*rep_int[s,r] + rep_ll[s,r]; //shifted according to each age group 
    }
  }


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
      D_x_gen[s,x] = (1/pr[s,rep_cat_ind[x]])*R_x[s,x]; // get D_x from  multiplying 1/pr * R_x 
      log_mu_x[s,x] = log(R_x[s,x]/E_x[s,x]) -log(pr[s, rep_cat_ind[x]]); //compute the age-specific mortality rates 
      mu_x[s,x] = exp(log_mu_x[s,x]);  //exponentiate the mortality 
      mu_x_total[s,x] =  mu_x[s,x] + mu_x_noc[s,x]; 
     }
  }
  
  R_age = v_ones *R_x; // get reported deaths aggregated over sex 
  D_age = v_ones *D_x_gen; // get true deaths aggregated over sex
 // D_baseline_age = v_ones*D_baseline_tmp; 
  for(x in 1:X){
    mu_age_total[x] =  D_age[x]/E_age[x] + mu_age_noc[x]; //mortality over sexes 
  }
   
}

model {
 pr_raw ~ beta(2,2); // reporting error multiplier 
  
  for(s in 1:S){
      for(x in 1:X){
        theta_x[s,x] ~ normal(pi_x_hat[s,x], pi_sd[s,x]); // priors on the age distributions - the model works without this, but this could also help w/ identifiability (see Schmertmann 2018)
       //  D_baseline[s,x] ~ poisson(E_x[s,x]*mu_x_noc[s,x]); 
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


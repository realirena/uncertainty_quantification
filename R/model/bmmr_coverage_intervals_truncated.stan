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
functions {
  matrix sd_vals(matrix pi_x, matrix a_x, matrix b_x, int S, int X) {
    matrix[S, X] sds;
    real sd1 = 0; 
    real sd2 = -1;
    real tmp; 
    for (s in 1: S) {
      for (x in 1:X) {
        sd1 = (b_x[s,x] - pi_x[s,x])/2; 
        sd2 = (pi_x[s,x] - a_x[s,x])/2; 
        if(sd1==sd2){
          sds[s, x] = tmp;
        }
        else { 
          reject("confidence intervals for pi_x not symmetric", sd1, sd2);
       }
      }
    }
    return sds; 
  }
  
  // vector get_diff_x(vector x){
  //   M = size(x);
  //   vector[m] diff_x; 
  //   for(m in 2:M){
  //     diff_x[m-1] = x[m] - x[m-1];
  //   }
  //   diff_x[m] = -10000; 
  //   return(diff_x);
  // }
  // 
  // 
  // vector get_le_estimates(vector x, vector mx, int sex){
  //   M = size(x);
  //   vector qx; 
  //   vector lx = 
  //   vector ax = rep_vector(0, M); 
  //   n = get_diff_x(x); 
  //   if(x[1]!=0 | x[2] != 1){
  //     for(m in 1:(M-1)){
  //       ax[m] = n[m]*0.5;
  //     }
  //     ax[M] = 1/mx[M]; 
  //   } else {
  //     if(sex==1){
  //       if(mx[1] >= 0.107){
  //         ax[1] = 0.350;
  //       } else {
  //         ax[1] = 0.053 + 2.800*mx[1];
  //       }
  //     }
  //     if(sex==2){
  //       if(mx[1] >= 0.107){
  //         ax[1] = 0.330;
  //       } else {
  //         ax[1] = 0.045 + 2.684*mx[1]; 
  //       }
  //     }
  //     for(m in 2:(M-1)){
  //       ax[m] = n[m]*0.5;
  //     }
  //     ax[M] = 1/mx[M]
  //   }
  //   
  //   }
  
  //  matrix get_theta_means(matrix pi_x, matrix sd_pi_x, int S, int X) {
  //   matrix[S, X] mean_theta;
  //   for (s in 1: S) {
  //     for (x in 1:X) {
  //       mean_theta[s,x] = log(pi_x[s,x]) - log((sd_pi_x[s,x]/(pi_x[s,x])^2 + 1)/2)
  //     }
  //   }
  //   return mean_theta; 
  // }
  
  // matrix get_theta_sds(matrix pi_x, matrix sd_pi_x, S, X) {
  //   matrix[S, X] mean_theta;
  //   for (s in 1: S) {
  //     for (x in 1:X) {
  //       mean_theta[s,x] = log(sd_pi_x[s,x]/(pi_x[s,x])^2 + 1)
  //     }
  //   }
  //   return mean_theta; 
  // }
}


data {
  int<lower=1> X; // number of age categories 
  int<lower=1> R; // reported deaths 
  int<lower=2> S; // number of sexes
  matrix[S,X] E_x; // exposures by age group and sex 
  vector[X] E_age; // exposures by age group 
  matrix[S, X] pi_x_hat; // sex-specific means age-distributions - this model assumes they are inputs, but we can code a model for the age groups 
  matrix[S,X] pi_sd; // lower bounds for the age distribution estimates 
  matrix[S,X] mu_x_wpp; //baseline mortality from the WPP 
  vector[X] mu_age_wpp; // age specific baseline mortality
  matrix [S,X] U; 
  matrix[S,X] L; 
}

transformed data {
row_vector[S] v_ones = rep_row_vector(1, S);

}


// The parameters accepted by the model
parameters {
  matrix<lower= L, upper=U>[S,X] theta_x; 
  real<lower=0, upper=1> pr_raw; // overreporting or underreporting rate 
  
}

transformed parameters {
 // matrix[S, X] mu_x; // mortality rate in each age-sex group 
  matrix[S,X] pi_x; 
  matrix[S,X] R_x; 
  row_vector[X] R_age; 
  vector[X] mu_age; 
  vector[X] mu_age_total; 
  // matrix[S,X] q_x; // life expectancy
  real tmp;
  matrix[S, X] mu_x; // mortality rate in each age-sex group 
  matrix[S,X] log_mu_x; 
  real pr;
  matrix[S,X] mu_x_total; 
 
  pr = pr_raw*0.14 + 0.52; //shifted beta distribution so that the mean is around 0.8 instead of 0.5 

  tmp = sum(exp(theta_x));
  for(s in 1:S){
    for(x in 1:X){
      pi_x[s,x] = exp(theta_x[s,x])/tmp; 
    //  print(pi_x[s,x]);
      }
  }

  //print(tmp);
  for(s in 1:S){
    for(x in 1:X){
      R_x[s,x] =pi_x[s,x]*R; // generating each R_x from the age distributions 
      log_mu_x[s,x] = log(R_x[s,x]/E_x[s,x]) -log(pr); //compute the age-specific mortality rates 
      mu_x[s,x] = exp(log_mu_x[s,x]);  //exponentiate the mortality 
      mu_x_total[s,x] =  mu_x[s,x] + mu_x_wpp[s,x]; //add the baseline mortality from WPP 
     }
  }
  
  R_age = v_ones *R_x; // get reported deaths aggregated over sex 
  for(x in 1:X){
    mu_age[x] = log(R_age[x]/E_age[x]) -log(pr); 
    mu_age_total[x] = exp(mu_age[x]) + mu_age_wpp[x];
  }
   
}

model {
 pr_raw ~ beta(2,2); // reporting error multiplier 
  
  for(s in 1:S){
      for(x in 1:X){
        theta_x[s,x] ~ normal(pi_x_hat[s,x], pi_sd[s,x]) T[L[s,x], U[s,x]]; // priors on the age distributions - the model works without this, but this could also help w/ identifiability (see Schmertmann 2018)
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
      //  D_x_total[s,x] = D_x_sim[s,x] + D_x_wpp[s,x];
    }
  }
  
}


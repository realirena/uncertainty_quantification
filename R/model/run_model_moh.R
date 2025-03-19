
rm(list=ls())
library(tidyverse)
library(haven)
library(rstan)
library(reshape2)
seed = 12125
options(mc.cores = parallel::detectCores(logical= FALSE))
setwd("U:/Documents/repos/uncertainty_quantification/")
## load the functions to calculate life expectancy 
source("R/0_setup.R")
## set the working directory
model_dir <- paste0(getwd(),"/R/model/")
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2022/2024/gaza/")
## load the 2024 moh age distributions (as an example)
pi_x_moh <- readRDS("data/pi_x_moh_2024.rds")
## get the sex-specific age distributions 
pi_x_moh <- pi_x_moh[pi_x_moh$sex!="t",]

## reshape the age distributions for the shape that we need for the model 
pi_x= spread(pi_x_moh[,c("sex", "age", "pi_x_mean")], key=age, value=pi_x_mean)
pi_sds= spread(pi_x_moh[,c("sex", "age", "pi_x_sd")], key=age, value=pi_x_sd)
pi_ul = spread(pi_x_moh[,c("sex", "age", "pi_x_ul")], key=age, value=pi_x_ul)
pi_ll = spread(pi_x_moh[,c("sex", "age", "pi_x_ll")], key=age, value=pi_x_ll)

## we want E(log(theta)) and sd(log(theta)), so apply Delta method to the means and sds: 
pi_mu = log(pi_x[,-1])
pi_sd = pi_sds[,-1]/pi_x[,-1]


##-------------------------------
## read in the exposure and the forecasted baseline mortality 
##-------------------------------
## read in exposure data:
master_forecast_dt <- readRDS("R/lc/data_plus_forecasts_v2.rds")
pcbs_exp  <- master_forecast_dt[master_forecast_dt$region=="Gaza Strip"&master_forecast_dt$year==2024&master_forecast_dt$sex%in%c("m", "f")&master_forecast_dt$source=="pcbs",]
## number of exposures by age
E_x = spread(pcbs_exp[,c("sex", "age","pop")], key=age, value=pop)
## exposures by age 
E_age =colSums(E_x[,-1])
## get total exposures 
E = sum(rowSums(E_x[,-1]))


## reshape the forecasted baseline mortality as well 
pcbs_mx<-  master_forecast_dt[master_forecast_dt$region=="Gaza Strip"&master_forecast_dt$year==2024&master_forecast_dt$sex%in%c("m", "f")&master_forecast_dt$source=="lc_pcbs_2022",]
D_x_pcbs= spread(pcbs_mx[,c("sex", "age","mx_noc")], key=age, value=mx_noc)

## age-sex specific mortality rates 
mu_x_pcbs <-  (D_x_pcbs[,-1])/E_x[,-1] 
## age specific mortality 
mu_age_pcbs <- colSums(D_x_pcbs[,-1])/E_age

### set the reported death toll (Gaza, 2024)### get reported cumulative death count (Palestine 2023: 22286, 2024: 24213)
### WB: 2023: 308, 2024: 494 
## Gaza Strip: 2023: 21978,  2024: 23719
R =23719
## total number of sexes 
S = nrow(mu_x_pcbs)
## total number of age groups 
X = ncol(mu_x_pcbs)

## age groups 
x <- as.numeric(colnames(mu_x_pcbs))

##-------------------------------
## setting up and running the Bayesian model 
##-------------------------------

## compile the model 
compiled_model <- stan_model(paste0(model_dir, "bmmr_coverage_intervals.stan"))

model_out <- sampling(compiled_model,
                      # include = TRUE,
                      sample_file=paste0(results_dir, 'moh_samples.csv'), #writes the samples to CSV file
                      iter =2000,
                      warmup=1000, #BURN IN
                      chains =4,
                      seed = seed,
                      control = list(max_treedepth = 60,
                                     adapt_delta=0.85),
                      data = list(
                        mu_x_noc = mu_x_pcbs, ##  baseline mortality
                        mu_age_noc = mu_age_pcbs, # WPP age baseline 
                        E_x = E_x[,-1],
                        E_age = E_age,
                        pi_x_hat = pi_mu,
                        pi_sd = pi_sd, 
                        R = R,
                        S = S,
                        X= X))
## check for convergence
# 
# rstan::traceplot(model_out, pars=c("mu_age_total[1]", "pi_x[1,1]", "pi_x[1,3]", "pr"))
rstan::traceplot(model_out, pars=c("pr", "pi_x[1,1]", "pi_x[2,1]"))
pairs(model_out, pars=c("pi_x[2,1]", "pi_x[1,1]","mu_x_total[1,1]", "mu_age_total[1]", "lp__"))
other_pars <- data.frame(summary(model_out, pars=c("pi_x"))$summary)


### extract the model-generated mortality distributions (incl WPP deaths)
mu_x_total = rstan::extract(model_out, pars=c("mu_x_total"))$mu_x_total

## mortality over sexes (only age specific )
mu_age_total =  rstan::extract(model_out, pars=c("mu_age_total"))$mu_age_total

## this is a 8000 (iterations) x 2 (sex) x 17 (age groups) array 
## we can get 8000 le0 estimates using the following code: 
lifetable_m  <- list()
lifetable_f  <- list()
lifetable_t <- list()
x <- as.numeric(colnames(mu_x_hat)) ## age groups 

num_iter <- dim(mu_x_total)[1] ## grab the number of MCMC iterations 
## alternative: if we don't want to use all of the samples, we can just use a random sample of iterations 
set.seed(233)
ran_iter <- sample(c(1:num_iter), 1000, replace=FALSE)

## this should now be an array with dimension ran_iter x 2 x 17 
mu_x_subset <- mu_x_total[ran_iter,,]
mu_age_subset <- mu_age_total[ran_iter,]
## get estimated life expectancies from the life table calculations 

for(i in 1:1000){
  lifetable_f[[i]] <- lifetable.mx(x, t(mu_x_subset[i, 1,]), sex="f")  # get life expectancy for women 
  lifetable_m[[i]] <- lifetable.mx(x, t(mu_x_subset[i,2,]))# get life expectancy for men 
  lifetable_t[[i]] <- lifetable.mx(x, t(mu_age_subset[i,]))
  
}

## get life expectancy tables into a dataset (this will be a large dataset!)
all_lifetable_f <- Reduce(rbind,lifetable_f)
all_lifetable_m <- Reduce(rbind,lifetable_m)
all_lifetable_t <- Reduce(rbind,lifetable_t)

get_le0_dt <- function(lifetable, sex, year){ 
  lifetable_age0 <- lifetable[lifetable$x==0,]
  lifetable_age0$year <- year
  lifetable_age0$sex <- sex
  lifetable_age0$scenario <-  "GMoH"
  if(sex=="Females"){
    lifetable_age0$bmmr_lss <- 76.86086 -  lifetable_age0$ex   ## LSS for women
  } else if(sex=="Males"){
    lifetable_age0$bmmr_lss <- 72.08611 - lifetable_age0$ex     ## LSS for men 
  } else{
    lifetable_age0$bmmr_lss <- 	74.46358 - lifetable_age0$ex  ## total LSS
  }
  
  return(lifetable_age0)
}

lifetable_f_age0 <- get_le0_dt(all_lifetable_f, "Females", 2024)
lifetable_m_age0 <- get_le0_dt(all_lifetable_m, "Males", 2024)
lifetable_t_age0 <- get_le0_dt(all_lifetable_t, "Total", 2024)
### histograms of the estimated life expectancy at 0 
hist(lifetable_f_age0$ex)
hist(lifetable_m_age0$ex)
hist(lifetable_t_age0$ex)


write.csv(lifetable_m_age0, paste0(results_dir, "le_estimates/cumulative_2024/moh_2024_m_le0.csv"), row.names = FALSE)
write.csv(lifetable_f_age0, paste0(results_dir, "le_estimates/cumulative_2024/moh_2024_f_le0.csv"), row.names = FALSE)
write.csv(lifetable_t_age0, paste0(results_dir, "le_estimates/cumulative_2024/moh_2024_t_le0.csv"), row.names = FALSE)

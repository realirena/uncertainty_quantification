
rm(list=ls())
library(tidyverse)
library(haven)
library(rstan)
library(stringr)
seed = 823
options(mc.cores = parallel::detectCores(logical= FALSE))

## set the working directory
setwd("U:/Documents/repos/Life_expectancy_Palestine")
model_dir <- paste0(getwd(),"/R/bmmr/")
results_dir <- paste0(getwd(),"/R/bmmr/samples/")

## read in age distributions
pi_x_oct26 <- read.csv(paste0(model_dir, "data/moh_2024_var_sims.csv"))
#### for the august 24 distributions, we have some age categories that do not have verified deaths: 
## so we make 75+ to be the last age group (for men and women)

## reshape the age distributions 
pi_x= spread(pi_x_oct26[,c("sex", "age5", "pi_x_mean")], key=age5, value=pi_x_mean)
pi_sds= spread(pi_x_oct26[,c("sex", "age5", "pi_x_sd")], key=age5, value=pi_x_sd)
pi_sd = pi_sds[,-1]/pi_x[,-1]

## read in exposure data: 
wpp_5yr_22 <- readRDS("data_inter/wpp2024_exposures_region.rds")
wpp_5yr_22 <- readRDS("data_inter/wpp2024_populations_5y_ages.rds")
## get palestine 2024 exposures by sex 
wpp_pse <- wpp_5yr_22[wpp_5yr_22$Year==2024&wpp_5yr_22$Sex%in%c("m", "f"),]

### since we grouped 75+ into its own age group, need to reshape the exposures to match 
## number of exposures by age
E_x = spread(wpp_pse[,c("sex", "age","pop")], key=age, value=pop)

## get total exposures 
# E = sum(rowSums(E_x_grp[,-1]))
E = sum(rowSums(E_x[,-1]))
## exposures by age 
E_age =colSums(E_x[,-1])

## read in baseline mortality data from WPP: 
wpp_deaths <-read.csv("R/bmmr/data/WPP_deaths_palestine_agegrp_2024.csv")

wpp_deaths_grp <- wpp_deaths |> 
  group_by(sex, age_grp) |> 
  summarise(deaths = sum(deaths))

D_x_wpp = spread(wpp_deaths_grp[,c("sex", "age_grp","deaths")], key=age_grp, value=deaths)


mu_x_wpp <-  (D_x_wpp[,-1])/E_x[,-1] 
mu_age_wpp <- colSums(D_x_wpp[,-1])/E_age

#pi_sds = matrix(rep(0.05, 34), nrow=2, ncol=17)

### get reported cumulative death count (for 2024: this is OCHA day 452)
R =45541
### multiply R by the age distribution to get R_x 
R_x = pi_x[,-1]*R

### crude mortality 
mu_x_hat = R_x/E_x[,-1]

## round to the nearest integer (since R_x needs to be integer valued for modeling as a Poisson)
R_x = round(R_x)
## can't have 0s in the counts so replace them with 1 -> for 2024, the reported death count stays the same 
R_x[R_x==0] <- 1

S = nrow(R_x)
X = ncol(R_x)
## age groups 
x <- as.numeric(colnames(R_x))

## baseline deaths
## read in baseline mortality data from WPP: 
wpp_deaths <-readRDS("data_inter/data_plus_forecasts.rds")

wpp_deaths_2024 <- wpp_deaths[wpp_deaths$region=="Palestine"&wpp_deaths$year==2024&wpp_deaths$sex%in%c("f", "m")&wpp_deaths$source=="lc_wpp2024", ]

mu_x_wpp = spread(wpp_deaths_2024[,c("sex", "age","mx_noc")], key=age, value=mx_noc)
mu_age_wpp <- colSums(mu_x_wpp[,-1])

## compile the model (uncomment these lines if you wish to run the model again)
compiled_model <- stan_model(paste0(model_dir, "bmmr_coverage_intervals.stan"))
# # ## run the model 

#file_names <- c("aug24_samples_multinom_sds_1", "aug24_samples_multinom_sds_2",  "aug24_samples_multinom_sds_3", "aug24_samples_multinom_sds_4")
#model_out <- read_stan_csv(paste0("G:/irena/bmmr/", file_names,".csv"))

model_out <- sampling(compiled_model,
                    # sample_file=paste0('G:/irena/bmmr/oct26_samples.csv'), #writes the samples to CSV file
                      iter =2000,
                      warmup=1000, #BURN IN
                      chains =4,
                      seed = seed,
                      control = list(max_treedepth = 60,
                                     adapt_delta=0.8),
                      data = list(
                        mu_x_wpp = mu_x_wpp, ## WPP baseline mortality
                        mu_age_wpp = mu_age_wpp, ## WPP age baseline 
                        E_x = E_x[,-1],
                        E_age = E_age,
                        pi_x_hat = log(pi_x[,-1]), ##means of the age distributions
                        pi_sd =pi_sd,
                        R = R,
                        S = S,
                        X= X)
                      )

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

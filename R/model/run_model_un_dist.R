
rm(list=ls())
library(tidyverse)
library(haven)
library(rstan)
library(bayesplot)
# library(ggdist)
library(stringr)
library(reshape2)
seed = 12125
options(mc.cores = parallel::detectCores(logical= FALSE))

## load the functions to calculate life expectancy 
#source("R/0_setup.R")
## set the working directory
## set the working directory
setwd("U:/Documents/repos/uncertainty_quantification/")
model_dir <- paste0(getwd(),"/R/model/")
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2022/2023/gaza/")

## read in age distributions (UN)
pi_x_un <- readRDS("data_inter/pi_x_un_2023.rds")
pi_x_un <- pi_x_un[pi_x_un$sex!="t",]

pi_x_un_conflict <- pi_x_un[pi_x_un$scenario=="conflict",]
pi_x_un_geno <- pi_x_un[pi_x_un$scenario=="genocide",]
pi_x_un_earth <- pi_x_un[pi_x_un$scenario=="earthquake",]


pi_x= spread(pi_x_un_conflict[,c("sex", "age", "pi_x_mean")], key=age, value=pi_x_mean)
pi_sds= spread(pi_x_un_conflict[,c("sex", "age", "pi_x_sd")], key=age, value=pi_x_sd)
pi_ul = spread(pi_x_un_conflict[,c("sex", "age", "pi_x_ul")], key=age, value=pi_x_ul)
pi_ll = spread(pi_x_un_conflict[,c("sex", "age", "pi_x_ll")], key=age, value=pi_x_ll)
## parameters for the age distribution priors 
## Delta method for E(log(theta))
pi_mu = log(pi_x[,-1])
## Delta method for sd(log(theta))
pi_sd = pi_sds[,-1]/pi_x[,-1]

## read in exposure data:
master_forecast_dt <- readRDS("R/lc/data_plus_forecasts_v2.rds")
pcbs_exp  <- master_forecast_dt[master_forecast_dt$region=="Gaza Strip"&master_forecast_dt$year==2023&master_forecast_dt$sex%in%c("m", "f")&master_forecast_dt$source=="pcbs",]
## number of exposures by age
E_x = spread(pcbs_exp[,c("sex", "age","pop")], key=age, value=pop)

# E = sum(rowSums(E_x_grp[,-1]))
E = sum(rowSums(E_x[,-1]))
## exposures by age 
E_age =colSums(E_x[,-1])

## reshape the forecasted baseline mortality as well 
pcbs_mx<-  master_forecast_dt[master_forecast_dt$region=="Gaza Strip"&master_forecast_dt$year==2023&master_forecast_dt$sex%in%c("m", "f")&master_forecast_dt$source=="lc_pcbs_2022",]
D_x_pcbs= spread(pcbs_mx[,c("sex", "age","mx_noc")], key=age, value=mx_noc)

mu_x_pcbs <-  (D_x_pcbs[,-1])/E_x[,-1] 
mu_age_pcbs <- colSums(D_x_pcbs[,-1])/E_age


### get reported cumulative death count (Palestine 2023: 22286, 2024: 24213)
### WB: 2023: 308, 2024: 494 
## Gaza Strip: 2023: 21978,  2024: 23719
R = 21978
### multiply R by the age distribution to get R_x 
R_x = pi_x[,-1]*R

### crude mortality 
mu_x_hat = R_x/E_x[,-1]
## round to the nearest integer (since R_x needs to be integer valued for modeling as a Poisson)
R_x = round(R_x)
S = nrow(R_x)
X = ncol(R_x)

## compile the model (uncomment these lines if you wish to run the model again)
compiled_model <- stan_model(paste0(model_dir, "bmmr_coverage_intervals_truncated.stan"))

model_out <- sampling(compiled_model,
                    # include = TRUE,
                  sample_file=paste0(results_dir, 'un_dist_conflict.csv'), #writes the samples to CSV file
                    iter =2000,
                    warmup=1000, #BURN IN
                    chains =4,
                    seed = seed,
                    control = list(max_treedepth = 60,
                                   adapt_delta=0.85),
                  data = list(
                    mu_x_noc = mu_x_pcbs, ## WPP baseline mortality
                    mu_age_noc = mu_age_pcbs, ## WPP age baseline 
                    E_x = E_x[,-1],
                    E_age = E_age,
                    pi_x_hat = pi_mu, ##means of the age distributions
                    pi_sd = pi_sd, 
                    U = log(pi_ul[,-1]),
                    L = log(pi_ll[,-1]), 
                    R = R,
                    S = S,
                    X= X))


rm(list=ls())
library(tidyverse)
library(haven)
library(reshape2)
library(stringr)
library(rstan)
seed = 1234
## set the working directory
setwd("U:/Documents/repos/uncertainty_quantification/")
options(mc.cores = parallel::detectCores(logical= FALSE))
## load the functions to calculate life expectancy 
source("R/0_setup.R")

## set up model + results directory
model_dir <- paste0(getwd(),"/R/model/diff_reporting/")
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/gaza/pcbs_2022/")

## load the 2024 moh age distributions (as an example)
## read in age distributions (btselem data)
pi_x_selem <- readRDS("data/pi_x_btselem_2024.rds")
pi_x_selem <- pi_x_selem[pi_x_selem$sex!="t",]
## reshape the age distributions 
pi_x= spread(pi_x_selem[,c("sex", "age", "pi_x_mean")], key=age, value=pi_x_mean)
pi_sds= spread(pi_x_selem[,c("sex", "age", "pi_x_sd")], key=age, value=pi_x_sd)
pi_ul = spread(pi_x_selem[,c("sex", "age", "pi_x_ul")], key=age, value=pi_x_ul)
pi_ll = spread(pi_x_selem[,c("sex", "age", "pi_x_ll")], key=age, value=pi_x_ll)
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
## exposures by age 
E_age =colSums(E_x[,-1])
## get total exposures 
E = sum(rowSums(E_x[,-1]))

## reshape the forecasted baseline mortality as well 
pcbs_mx<-  master_forecast_dt[master_forecast_dt$region=="Gaza Strip"&master_forecast_dt$year==2023&master_forecast_dt$sex%in%c("m", "f")&master_forecast_dt$source=="lc_pcbs_2019",]

pcbs_mx_mean <- pcbs_mx |> 
  select(year, sex, age, mx_noc) |>
  left_join(pcbs_exp |> select(year, sex, age, pop), by = c("year", "sex", "age")) |>
  mutate(Dx_noc = mx_noc*pop/1E5) |>
  group_by(sex, age) |>
  summarise(mean_Dx_noc = mean(Dx_noc))

D_x_pcbs= spread(pcbs_mx_mean[,c("sex", "age","mean_Dx_noc")], key=age, value=mean_Dx_noc)

### 2023 only: combatants

# age-sex specific mortality rates (for 2023 ONLY - add combatants)
# mu_x_pcbs <-  (D_x_pcbs[,-1] + Dx_cmb_spread[,-1])/E_x[,-1]
# mu_age_pcbs <- colSums(D_x_pcbs[,-1] + Dx_cmb_spread[,-1])/E_age

Dx_cmb <- readRDS("data/Dx_cmb.rds")
Dx_cmb_spread <- spread(Dx_cmb, key=age, value=Dx_cmb_mean)

## age-sex specific mortality rates (for 2023 ONLY - add combatants)
mu_x_pcbs <-  (D_x_pcbs[,-1] + Dx_cmb_spread[,-1])/E_x[,-1] 
# mu_x_pcbs <-  (D_x_pcbs[,-1])/E_x[,-1]
mu_age_pcbs <- colSums(D_x_pcbs[,-1] + Dx_cmb_spread[,-1])/E_age
# mu_age_pcbs <- colSums(D_x_pcbs[,-1] )/E_age

##2024: 
mu_x_pcbs <-  (D_x_pcbs[,-1])/E_x[,-1]
mu_age_pcbs <- colSums(D_x_pcbs[,-1])/E_age
### get reported cumulative death count (Palestine 2023: 22130, 2024: 24217)
### WB: 2023: 308, 2024: 498 
## Gaza Strip: 2023: 21822,  2024: 23719
R = 23719
### multiply R by the age distribution to get R_x 
R_x = pi_x[,-1]*R

### crude mortality 
mu_x_hat = R_x/E_x[,-1]

## round to the nearest integer (since R_x needs to be integer valued for modeling as a Poisson)
R_x = round(R_x)
S = nrow(R_x)
X = ncol(R_x)

### set different reporting rates for each age group (note that pr_ul and pr_ll are flipped and ul = lower bound):
rep_rate_grp <- readRDS("data/pr_age.rds")
rep_rate_grp <- rep_rate_grp[rep_rate_grp$sex%in%c("Female", "Male"),]
rep_rate_grp$int <- rep_rate_grp$pr_ll - rep_rate_grp$pr_ul
rep_ll <-  spread(rep_rate_grp[,c("sex", "agegrp", "pr_ul")], key=agegrp, value=pr_ul)
rep_int <-  spread(rep_rate_grp[,c("sex", "agegrp", "int")], key=agegrp, value=int)

### indicator for the reporting rates 
rep_cat_ind  <- c(rep(1, 4), rep(2,3), rep(3, 3), rep(4, 3), rep(5 ,5))
rep_cat <- ncol(rep_ll) - 1 
##-------------------------------
# compiled_model <- stan_model(paste0(model_dir, "bmmr_change_prior_trunc.stan"))
compiled_model <- stan_model(paste0(model_dir, "bmmr_trunc.stan"))

compiled_model <- stan_model(paste0(model_dir, "bmmr_change_prior_trunc.stan"))
#compiled_model <- stan_model(paste0(model_dir, "bmmr_coverage_intervals_truncated.stan"))

model_out <- sampling(compiled_model,
                     sample_file=paste0(results_dir, 'bts_samples.csv'), #writes the samples to CSV file
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
                        X= X,
                        rep_cat = rep_cat,
                        rep_ll = rep_ll[,-1],
                        rep_int = rep_int[,-1],
                        rep_cat_ind = rep_cat_ind)
)


rstan::traceplot(model_out, pars=c("pr[1,1]"))


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
model_dir <- paste0(getwd(),"/R/model/diff_reporting/")
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/palestine/")
# results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2023/palestine_bu/")
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
pcbs_exp  <- master_forecast_dt[master_forecast_dt$region=="Palestine"&master_forecast_dt$year==2024&master_forecast_dt$sex%in%c("m", "f")&master_forecast_dt$source=="pcbs",]
## number of exposures by age
E_x = spread(pcbs_exp[,c("sex", "age","pop")], key=age, value=pop)
## exposures by age 
E_age =colSums(E_x[,-1])
## get total exposures 
E = sum(rowSums(E_x[,-1]))


## reshape the forecasted baseline mortality as well 
pcbs_mx<-  master_forecast_dt[master_forecast_dt$region=="Palestine"&master_forecast_dt$year==2024&master_forecast_dt$sex%in%c("m", "f")&master_forecast_dt$source=="lc_pcbs_2019",]

pcbs_mx_mean <- pcbs_mx |> 
  select(year, sex, age, mx_noc) |>
  left_join(pcbs_exp |> select(year, sex, age, pop), by = c("year", "sex", "age")) |>
  mutate(Dx_noc = mx_noc*pop/1E5) |>
  group_by(sex, age) |>
  summarise(mean_Dx_noc = mean(Dx_noc))

D_x_pcbs= spread(pcbs_mx_mean[,c("sex", "age","mean_Dx_noc")], key=age, value=mean_Dx_noc)

### 2023 only: combatants
Dx_cmb <- readRDS("data/Dx_cmb.rds")
Dx_cmb_spread <- spread(Dx_cmb, key=age, value=Dx_cmb_mean)
#D_x_int = round(D_x_pcbs[,-1])

## age-sex specific mortality rates (for 2023 ONLY - add combatants)
#mu_x_pcbs <-  (D_x_pcbs[,-1] + Dx_cmb_spread[,-1])/E_x[,-1] 
#mu_age_pcbs <- colSums(D_x_pcbs[,-1] + Dx_cmb_spread[,-1])/E_age
# ## for 2024
mu_x_pcbs <-  (D_x_pcbs[,-1])/E_x[,-1] 
mu_age_pcbs <- colSums(D_x_pcbs[,-1])/E_age

### set the reported death toll (Palestine 2023: 22130, 2024: 24217)
### WB: 2023: 308, 2024: 494 
## Gaza Strip: 2023: 21822,  2024: 23719
R =24217
## total number of sexes 
S = nrow(mu_x_pcbs)
## total number of age groups 
X = ncol(mu_x_pcbs)
## age groups 
x <- as.numeric(colnames(mu_x_pcbs))

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
## setting up and running the Bayesian model 
##-------------------------------

## compile the model 
compiled_model <- stan_model(paste0(model_dir, "bmmr.stan"))

model_out <- sampling(compiled_model,
                      # include = TRUE,
                    sample_file=paste0(results_dir, 'moh24_samples.csv'), #writes the samples to CSV file
                      iter =2000,
                      warmup=1000, #BURN IN
                      chains =4,
                      seed = seed,
                      control = list(max_treedepth = 60,
                                     adapt_delta=0.85),
                      data = list(
                        mu_x_noc = mu_x_pcbs, ##  baseline mortality
                        mu_age_noc = mu_age_pcbs, # WPP age baseline 
                        D_baseline = round(D_x_pcbs[,-1]),
                        E_x = round(E_x[,-1]),
                        E_age = E_age,
                        pi_x_hat = pi_mu,
                        pi_sd = pi_sd, 
                        R = R,
                        S = S,
                        X= X,
                        rep_cat = rep_cat,
                        rep_ll = rep_ll[,-1],
                        rep_int = rep_int[,-1],
                        rep_cat_ind = rep_cat_ind))
## check for convergence
# 
# rstan::traceplot(model_out, pars=c("mu_age_total[1]", "pi_x[1,1]", "pi_x[1,3]", "pr"))
rstan::traceplot(model_out, pars=c("pr"))
plot(model_out, pars=c("pr"), show_density = TRUE, ci_level = 0.5, fill_color = "purple")
data.frame(summary(model_out, pars=c("pr"))$summary)

rstan::traceplot(model_out, pars=c("pi_x[2,1]", "pi_x[1,1]","mu_x_total[1,1]"))

pairs(model_out, pars=c("pi_x[2,1]", "pi_x[1,1]","mu_x_total[1,1]", "mu_age_total[1]", "lp__"))
other_pars <- data.frame(summary(model_out, pars=c("pi_x"))$summary)


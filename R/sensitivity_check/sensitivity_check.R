
rm(list=ls())
library(tidyverse)
library(haven)
library(rstan)
library(stringr)
seed = 823
options(mc.cores = parallel::detectCores(logical= FALSE))

## set the working directory
setwd("U:/Documents/repos/Life_expectancy_Palestine")
model_dir <- paste0(getwd(),"/R/model/")
results_dir <- paste0(getwd(),"/R/bmmr/samples/")


## read in age distributions (btselem data)
pi_x_selem <- readRDS("data_inter/pi_x_btselem_lim_5y.rds")

## reshape the age distributions 
pi_x= spread(pi_x_selem[,c("sex", "age", "pi_mean")], key=age, value=pi_mean)
pi_sds= spread(pi_x_selem[,c("sex", "age", "pi_sd")], key=age, value=pi_sd)
pi_ul = spread(pi_x_selem[,c("sex", "age", "pi_ul")], key=age, value=pi_ul)
pi_ll = spread(pi_x_selem[,c("sex", "age", "pi_ll")], key=age, value=pi_ll)

## delta method for E(log(theta)):
pi_mu = log(pi_x[,-1])
## delta method for sd(log(theta))
pi_sd = pi_sds[,-1]/pi_x[,-1]

## read in exposure data: 
wpp_5yr_22 <- readRDS("data_inter/wpp_populations_5y_ages.rds")

## get exposures for 2023 by sex 
wpp_pse <- wpp_5yr_22[wpp_5yr_22$code=="PSE"&wpp_5yr_22$year==2023&wpp_5yr_22$sex%in%c("m", "f"),]
## number of exposures by age
E_x = spread(wpp_pse[,c("sex", "age","pop")], key=age, value=pop)

## get total exposures 
# E = sum(rowSums(E_x_grp[,-1]))
E = sum(rowSums(E_x[,-1]))
## exposures by age 
E_age =colSums(E_x[,-1])

## read in baseline mortality data from WPP: 
wpp_deaths <-read.csv("R/bmmr/data/WPP_deaths_palestine_agegrp_2023.csv")

wpp_deaths_grp <- wpp_deaths |> 
  group_by(sex, age_grp) |> 
  summarise(deaths = sum(deaths))

D_x_wpp = spread(wpp_deaths_grp[,c("sex", "age_grp","deaths")], key=age_grp, value=deaths)

## get baseline mortality by age-sex
mu_x_wpp <-  (D_x_wpp[,-1])/E_x[,-1] 
## by age 
mu_age_wpp <- colSums(D_x_wpp[,-1])/E_age

### get reported cumulative death count (for 2024: this is OCHA day 452)
R =  21672
### multiply R by the age distribution to get R_x 
R_x = pi_x[,-1]*R

## round to the nearest integer (since R_x needs to be integer valued for modeling as a Poisson)
R_x = round(R_x)
## can't have 0s in the counts so replace them with 1 -> for 2024, the reported death count stays the same 
R_x[R_x==0] <- 1

S = nrow(R_x)
X = ncol(R_x)
## age groups 
x <- as.numeric(colnames(R_x))

## compile the model (uncomment these lines if you wish to run the model again)
compiled_model <- stan_model(paste0(model_dir, "bmmr_coverage_intervals.stan"))
# # ## run the model 

model_out <- sampling(compiled_model,
                      iter=2000,
                      warmup=1000, #BURN IN
                      chains =4,
                      seed = seed,
                      control = list(max_treedepth = 60,
                                     adapt_delta=0.85),
                      data = list(
                        mu_x_wpp = mu_x_wpp, ## WPP baseline mortality
                        mu_age_wpp = mu_age_wpp, ## WPP age baseline 
                        E_x = E_x[,-1],
                        E_age = E_age,
                        phi = pi_mu, ##means of the age distributions
                        sigma = pi_sd, ## sds of the age distributions 
                        R = R,
                        S = S,
                        X= X)
                      )

file_names <- c("bts_samples_uncertainty_0303_1", "bts_samples_uncertainty_0303_2", "bts_samples_uncertainty_0303_3", "bts_samples_uncertainty_0303_4")
model_out <- read_stan_csv(paste0(results_dir, file_names,".csv"))

### does the model recover the means and sds of the age distributions? 

## extract the means and 95% CrIs from the posterior samples 
pi_x_est = summary(model_out, pars=c("pi_x"), probs=c(0.025, 0.975))$summary
pi_x_est_df = data.frame(pi_x_est[,c(1, 4:5)])
colnames(pi_x_est_df)[2:3] = c("cr_ll", "cr_ul")
pi_x_est_df$age = pi_x_selem$age
pi_x_est_df$sex = pi_x_selem$sex

## add the empirical uncertainties 
pi_x_est_df$pi_mean <- pi_x_selem$pi_mean
pi_x_est_df$pi_ll <- pi_x_selem$pi_ll
pi_x_est_df$pi_ul <- pi_x_selem$pi_ul 

## plot the two uncertainties 
ggplot(pi_x_est_df) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age, y = mean), color="green", size=1.5)  +
  geom_ribbon(aes(x=age, y=mean, ymin = cr_ll, ymax =cr_ul, group=1),color="green", linetype="dashed", alpha = 0.3) + 
  geom_point(aes(x=age, y=pi_mean, group=1),  color="purple", size=1.5) + 
  geom_ribbon(aes(x=age, y=mean, ymin = pi_ll, ymax =pi_ul,group=1),  color="purple", alpha = 0.3) + 
  labs(x="Age Group", y="Age distribution",
       title="Age distribution prior check (historical estimates)",
       subtitle="Model estimated uncertainty (green) compated to the empirical uncertainty (purple)") + 
  theme_bw() +
  theme(plot.title=element_text(size=20, hjust=0.5),
        plot.subtitle = element_text(size=18, hjust=0.5),
        axis.text.x = element_text(size=12,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=12),
        axis.text.y = element_text(size=12)) 


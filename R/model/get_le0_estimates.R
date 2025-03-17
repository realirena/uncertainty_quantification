rm(list=ls())
library(tidyverse)
library(haven)
library(reshape2)
library(stringr)
library(rstan)
seed = 1234
## load the functions to calculate life expectancy 
source("R/0_setup.R")

## set the working directory
setwd("U:/Documents/repos/Life_expectancy_Palestine")
model_dir <- paste0(getwd(),"/R/bmmr/")
results_dir <- paste0(getwd(),"/R/bmmr/samples/2024/pcbs/2019/palestine/")

le0_noc <- readRDS("data_inter/ex0_noc.rds")
le0_noc_23 <- le0_noc[le0_noc$year==2023&le0_noc$source=="lc_pcbs_2019"&le0_noc$region=="Gaza Strip",]
## read in age distributions
pi_x_oct26 <- readRDS("data_inter/pi_x_moh_2023_gaza.rds")
pi_x_oct26 <- pi_x_oct26[pi_x_oct26$sex!="t",]

### estimated LE's (noc) by region and sex
le_noc_list <- list(
  c(78.22725, 74.95395, 76.67211), ## gaza 2023
  c(78.29336,	75.06242 , 76.77138), ## gaza 2024 , 
  c(79.68532, 76.32110, 77.98499), ## palestine 2023,
  c(79.78170, 76.44891, 78.09980) ## palestine 2024 
)


## read in samples 
file_names <- c("bts_samples_1", "bts_samples_2", "bts_samples_3", "bts_samples_4")
model_out <- read_stan_csv(paste0(results_dir, file_names,".csv"))

### extract the model-generated mortality distributions (incl WPP deaths)
mu_x_total = rstan::extract(model_out, pars=c("mu_x_total"))$mu_x_total

## mortality over sexes (only age specific )
mu_age_total =  rstan::extract(model_out, pars=c("mu_age_total"))$mu_age_total

## this is a 8000 (iterations) x 2 (sex) x 17 (age groups) array 
## we can get 8000 le0 estimates using the following code: 
lifetable_m  <- list()
lifetable_f  <- list()
lifetable_t <- list()
x <- pi_x_oct26$age[1:18]

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

## get life expectancy tables into a dataset 
all_lifetable_f <- Reduce(rbind,lifetable_f)
all_lifetable_m <- Reduce(rbind,lifetable_m)
all_lifetable_t <- Reduce(rbind,lifetable_t)

## grab the life expectancy (by sex) at age 0 
get_le0_dt <- function(lifetable, sex=NULL, year, scenario, le0){ 
  ## also use males for total LE computations (for now)
  lifetable_age0 <- lifetable[lifetable$x==0,]
  lifetable_age0$year <- year
  lifetable_age0$sex <- sex
  lifetable_age0$scenario <- scenario
  if(sex=="Females"){
    lifetable_age0$bmmr_lss <-	le0[1] -  lifetable_age0$ex   ## LSS for women
  } else if(sex=="Males"){
    lifetable_age0$bmmr_lss <-	le0[2] - lifetable_age0$ex     ## LSS for men 
  } else{
    lifetable_age0$bmmr_lss <- 	le0[3] - lifetable_age0$ex  ## total LSS
  }
  return(lifetable_age0)
}
## scenarios:  "GMoH report", "B'Tselem historical average", "UN-IGME pattern"

lifetable_f_age0 <- get_le0_dt(all_lifetable_f, "Females", 2024,  "B'Tselem historical average", le0= le_noc_list[[4]])
lifetable_m_age0 <- get_le0_dt(all_lifetable_m, "Males", 2024,  "B'Tselem historical average", le0= le_noc_list[[4]])
lifetable_t_age0 <- get_le0_dt(all_lifetable_t, "Total", 2024,"B'Tselem historical average", le0= le_noc_list[[4]])

## histograms of the estimated life expectancies after accounting for reporting rate error 
hist(lifetable_f_age0$ex)
hist(lifetable_m_age0$ex)
hist(lifetable_t_age0$ex)


g1 <- ggplot() + 
  geom_histogram(data=lifetable_f_age0, aes(x=ex),fill="#926ecc", alpha=0.5) +
  labs(title = "Histogram of estimated LE0 (B'tselem, female)") +
  theme(plot.title=element_text(size=20, hjust=0.5),
        plot.subtitle = element_text(size=18, hjust=0.5),
        axis.text.x = element_text(size=12,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=12),
        axis.text.y = element_text(size=12))

g2 <- ggplot() + 
  geom_histogram(data=lifetable_m_age0, aes(x=ex),fill="#378582", alpha=0.5) +
  labs(title = "Histogram of estimated LE0 (B'tselem, male)") +  
  theme(plot.title=element_text(size=20, hjust=0.5),
        plot.subtitle = element_text(size=18, hjust=0.5),
        axis.text.x = element_text(size=12,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=12),
        axis.text.y = element_text(size=12))

g3 <- ggplot(data=lifetable_t_age0, aes(x=ex)) + 
  geom_histogram(fill="#de5138", alpha=0.5) +
  labs(title = "Histogram of estimated LE0 (B'tselem, total)") + 
  theme(plot.title=element_text(size=20, hjust=0.5),
        plot.subtitle = element_text(size=18, hjust=0.5),
        axis.text.x = element_text(size=12,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=12),
        axis.text.y = element_text(size=12))


gridExtra::grid.arrange(g1, g2, g3, ncol=3)

write.csv(lifetable_m_age0, paste0(results_dir, "bts_lifetable_m_le0.csv"), row.names = FALSE)
write.csv(lifetable_f_age0, paste0(results_dir, "bts_lifetable_f_le0.csv"), row.names = FALSE)
write.csv(lifetable_t_age0, paste0(results_dir, "bts_lifetable_t_le0.csv"), row.names = FALSE)


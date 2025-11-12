rm(list=ls())
library(tidyverse)
library(haven)
library(reshape2)
library(stringr)
library(rstan)
seed = 1234

## set the working directory
setwd("U:/Documents/repos/uncertainty_quantification")
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/gaza/")
## load the functions to calculate life expectancy 
source("R/0_setup.R")
## get the age groups for the lifetable calculations 
pi_x <- readRDS("data/pi_x_moh_2024_2025_gaza_v2.rds")
pi_x <- pi_x[pi_x$sex!="t",]
pi_spread <- spread(pi_x[,c("sex", "age", "pi_x_mean")], key=age, value=pi_x_mean)

x <- as.numeric(colnames(pi_spread)[2:19])

le_noc <- readRDS("data/ex0_noc_2025_temp.rds")
# ### estimated LE's (noc) by region and sex
le_noc_list <- list(
  c(76.56860,73.23984, 74.77036), ## gaza 2023
  c(76.51045,	73.16800 ,74.68761), ## gaza 2024 ,
  c(78.35659, 75.16350, 76.86584), ## gaza 2025
  c(77.92489, 74.58173, 76.37233), ## palestine 2023,
  c(77.84302, 74.54344, 76.32383) ## palestine 2024
)

## for cumulative 2023-2024 numbers 
le_gaza_23_24 <- c(78.25961,  75.00566, 76.72000)
le_pst_23_24 <- c(79.73368,  76.38461, 78.04232)

## read in samples 
file_names <- c("moh25_samples_1","moh25_samples_2", "moh25_samples_3", "moh25_samples_4")
model_out <- read_stan_csv(paste0(results_dir, file_names,".csv"))

### extract the model-generated mortality distributions (incl WPP deaths)
mu_x_total = rstan::extract(model_out, pars=c("mu_x_total"))$mu_x_total

## mortality over sexes (only age specific )
mu_age_total =  rstan::extract(model_out, pars=c("mu_age_total"))$mu_age_total

num_iter <- dim(mu_x_total)[1] ## grab the number of MCMC iterations 
## alternative: if we don't want to use all of the samples, we can just use a random sample of iterations 
set.seed(243)

## extract 1,000 random draws of the model
ran_iter <- sample(c(1:num_iter), 1000, replace=FALSE)

## this should now be an array with dimension 1000 x 2 (sex) x 18 (age groups)
mu_x_subset <- mu_x_total[ran_iter,,]
mu_age_subset <- mu_age_total[ran_iter,]
## get estimated life expectancies from the life table calculations 

##  list to hold each lifetable calculation (by iteration)
lifetable_m  <- list()
lifetable_f  <- list()
lifetable_t <- list()

## we can get 1000 le0 estimates using the following code: 
for(i in 1:1000){
  lifetable_f[[i]] <- lifetable.mx(x, t(mu_x_subset[i, 1,]), sex="f")  # get life expectancy for women 
  lifetable_m[[i]] <- lifetable.mx(x, t(mu_x_subset[i,2,]))# get life expectancy for men 
  lifetable_t[[i]] <- lifetable.mx(x, t(mu_age_subset[i,]))
  
}

## get life expectancy tables into a dataset 
all_lifetable_f <- Reduce(rbind,lifetable_f)
all_lifetable_m <- Reduce(rbind,lifetable_m)
all_lifetable_t <- Reduce(rbind,lifetable_t)

## "B'Tselem historical average" "UN-IGME report" , "GMoH report"
lifetable_f_age0 <- get_le0_dt(all_lifetable_f, "Females", 2025, "GMoH report",le0=le_noc_list[[3]])
lifetable_m_age0 <- get_le0_dt(all_lifetable_m, "Males", 2025, "GMoH report", le0= le_noc_list[[3]])
lifetable_t_age0 <- get_le0_dt(all_lifetable_t, "Total", 2025, "GMoH report" , le0=le_noc_list[[3]])

## histograms of the estimated life expectancies after accounting for reporting rate error 
hist(lifetable_f_age0$ex)
hist(lifetable_m_age0$ex)
hist(lifetable_t_age0$ex)


write.csv(lifetable_m_age0, paste0(results_dir, "moh_25_lifetable_m_le0.csv"), row.names = FALSE)
write.csv(lifetable_f_age0, paste0(results_dir, "moh_25_lifetable_f_le0.csv"), row.names = FALSE)
write.csv(lifetable_t_age0, paste0(results_dir, "moh_25_lifetable_t_le0.csv"), row.names = FALSE)

all_lt <- rbind(lifetable_f_age0, lifetable_m_age0, lifetable_t_age0)

all_lt |>
  group_by(sex, year) |>
  summarise(mean_ex = mean(ex),
            ex_ll = quantile(ex, 0.025), 
            ex_ul = quantile(ex, 0.975), 
            mean_lss = mean(bmmr_lss),
            lss_ll = quantile(bmmr_lss, 0.025), 
            lss_ul = quantile(bmmr_lss, 0.975), )




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

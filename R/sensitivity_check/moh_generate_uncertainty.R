
rm(list=ls())
library(tidyverse)
library(haven)
library(rstan)
library(bayesplot)
library(stringr)
seed = 823
options(mc.cores = parallel::detectCores(logical= FALSE))

## set the working directory
setwd("U:/Documents/repos/Life_expectancy_Palestine")
model_dir <- "U:/Documents/repos/Life_expectancy_Palestine/R/bmmr/"
results_dir <- "U:/Documents/repos/Life_expectancy_Palestine/R/bmmr/samples/"


## read in exposure data: 
pi_x_moh_all_lists <- readRDS("data_inter/age_sex_distributions_moh_all_lists.rds")


### start with plots of the age distributions (by list)
ggplot(pi_x_moh_all_lists[pi_x_moh_all_lists$list%in%c(1,6),]) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age5, y = pi_x, color=list), size=3)  +
  geom_line(aes(x=age5, y = pi_x, color=list), size=1.25)  +
  labs(x="Age Group", y="Age Distribution", title="Estimated Age Distributions",
       subtitle="by sex and year") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        strip.text.x = element_text(size = 15),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))



### simulate death counts from a multinomial using the MoH lists: 
## read in exposure data: 

#moh_list6 <- pi_x_moh_all_lists[pi_x_moh_all_lists$list==6,]
moh_list1 <- pi_x_moh_all_lists[pi_x_moh_all_lists$list==1,]

pi_x_moh = spread(moh_list1[,c("sex", "age5", "pi_x")], key=age5, value=pi_x)

## 2023 numbers 
all_R_x_23 <- data.frame()
for(i in 1:10000){
  tmp <- data.frame(rbind(rmultinom(1, 21978, as.matrix(pi_x_moh[,-1]))))
  colnames(tmp)[1] <- "pop"
  tmp$sex <- c("Female", "Male")
  tmp$pi_x <-  tmp$pop/sum(tmp$pop)
  tmp$age5 <- rep(moh_list1$age5[1:17], each=2)
  
  all_R_x_23 <- rbind(all_R_x_23, tmp)
}

## 2024 numbers 
moh_list6 <- pi_x_moh_all_lists[pi_x_moh_all_lists$list==6,]
pi_x_24 = spread(moh_list6[,c("sex", "age5", "pi_x")], key=age5, value=pi_x)
all_R_x_24 <- data.frame()

## get 10k samples from a multinomial 
for(i in 1:10000){
  tmp <- data.frame(rbind(rmultinom(1,41870, as.matrix(pi_x_24[,-1]))))
  colnames(tmp)[1] <- "pop"
  tmp$sex <- c("Female","Male")
  tmp$pi_x <-  tmp$pop/sum(tmp$pop)
  tmp$age5 <- rep(moh_list6$age5[1:17], each=2)
  
  all_R_x_24 <- rbind(all_R_x_24, tmp)
}

plot_sims23 <- all_R_x_23 |> 
  group_by(sex, age5) |>
  summarise(pi_x_mean = mean(pi_x),
            pop_mean = mean(pop),
            pi_x_var = var(pi_x),
            pi_x_sd = sd(pi_x),
            pi_x_ll = quantile(pi_x, 0.025),
            pi_x_ul = quantile(pi_x, 0.975))


plot_sims23$pi_x_moh <- moh_list1$pi_x

#write.csv(plot_sims23, "U:/Documents/repos/Life_expectancy_Palestine/R/bmmr/data/oct_26_var_sims.csv", row.names = FALSE)
plot_sims24 <- all_R_x_24 |> 
  group_by(sex, age5) |>
  summarise(pi_x_mean = mean(pi_x),
            pi_x_var = var(pi_x),
            pi_x_sd = sd(pi_x),
            pi_x_ll = quantile(pi_x, 0.025),
            pi_x_ul = quantile(pi_x, 0.975))

plot_sims24$pi_x_moh <- moh_list6$pi_x
#write.csv(plot_sims24, "U:/Documents/repos/Life_expectancy_Palestine/R/bmmr/data/moh_2024_var_sims.csv", row.names = FALSE)

### start with plots of the age distributions (by list)
ggplot(plot_sims23) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age5, y = pi_x_mean), size=3)  +
  geom_line(aes(x=age5, y=pi_x_moh, group=1),  color="purple", linewidth=1.25) + 
  geom_ribbon(aes(x=age5, y=pi_x_mean, ymin = pi_x_ll, ymax =pi_x_ul,  group=sex),  size=1.25, alpha = 0.3) + 
 # geom_line(data=pi_x_moh_all_lists[pi_x_moh_all_lists$list==1,], aes(x=age5, y = pi_x), size=1.25) +
  labs(x="Age Group", y="Distribution", title="Uncertainty generated from multinomial distribution",
       subtitle="Based on GMoH Reporting from Oct. 7th - Dec.30th, 2023") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=30, hjust=0.5),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))

### start with plots of the age distributions (by list)### start with plots of the age distributions (by list)
ggplot(plot_sims24) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age5, y = pi_x_mean), size=3)  +
  geom_line(aes(x=age5, y=pi_x_moh, group=1),  color="purple", linewidth=1.25) + 
  geom_ribbon(aes(x=age5, y=pi_x_mean, ymin = pi_x_ll, ymax =pi_x_ul, group=sex),  size=1.25, alpha = 0.3) + 
  labs(x="Age Group", y="pi_x", title="Uncertainty generated from multinomial distribution",
       subtitle="2024 Numbers (Oct '23- Oct '24)") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))

pi_mu = spread(plot_sims23[,c("sex", "age5", "pi_x_mean")], key=age5, value=pi_x_mean)
pi_sd = spread(plot_sims23[,c("sex", "age5", "pi_x_sd")], key=age5, value=pi_x_sd)

log_pi_mu = as.matrix(log(pi_mu[,-1]))
log_pi_sd  = as.matrix(pi_sd[,-1]/pi_mu[,-1])


phi_f_vals <- list()
phi_m_vals <- list()
phi <- list()
for(i in 1:1000){
  f_tmp = exp(sapply(seq_along(log_pi_mu[1,]), function(i){rnorm(1, log_pi_mu[1,i],log_pi_sd[1,i])}))
  m_tmp =  exp(sapply(seq_along(log_pi_mu[2,]), function(i){rnorm(1, log_pi_mu[2,i],log_pi_sd[2,i])})) ## draw thetas from a normal distribution and then exponentiate them 
  phi_f_vals =f_tmp/sum(f_tmp, m_tmp) ## compute the proportions 
  phi_m_vals =m_tmp/sum(f_tmp, m_tmp) ## compute the proportions 
  phi[[i]] = rbind(phi_f_vals, phi_m_vals)
}


phi_dat = data.frame(Reduce(rbind, phi))
colnames(phi_dat) = colnames(pi_mu[,-1])
phi_dat$sex = c("f", "m")

plotPhi <- reshape2::melt(phi_dat, id.vars="sex", variable.name="age5", value.name="prior")

phi_summary <- plotPhi |>
  group_by(sex, age5) |>
  summarise(mean_pi = mean(prior),
            sd_pi = sd(prior),
            pi_ul = quantile(prior, 0.975), 
            pi_ll = quantile(prior, 0.025)) 

phi_summary$empirical_mean = plot_sims23$pi_x_mean
phi_summary$empirical_sd = plot_sims23$pi_x_sd
phi_summary$empirical_ul = plot_sims23$pi_x_ul
phi_summary$empirical_ll = plot_sims23$pi_x_ll

ggplot(data=phi_summary) + 
  geom_point(aes(x=age5, y=mean_pi), color="#378582", size=2) + 
  geom_ribbon(aes(x=age5, y=mean_pi, ymin = pi_ll, ymax =pi_ul,  group=sex),  color="#378582", linetype = "dashed", size=1.25, alpha = 0.2) + 
  facet_wrap(~sex) + 
  geom_ribbon(aes(x=age5, y=mean_pi, ymin = empirical_ll, ymax =empirical_ul, group=sex), color="purple", alpha=0.5) + 
  labs(title = "Prior Generated Uncertainty and Empirical Uncertainty",
       x="Age Group", y="Age Distribution", 
       subtitle="Based on GMoH Reporting from Oct. 7th - Dec.30th, 2023") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=30, hjust=0.5),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))



ggplot(plot_sims24) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age5, y = pi_x_mean), size=3)  +
  geom_line(aes(x=age5, y = pi_x_mean), size=1.25)  +
  geom_ribbon(aes(x=age5, y=pi_x_mean, ymin = pi_x_ll, ymax =pi_x_ul,  group=1),  size=1.25, alpha = 0.3) + 
  geom_point(aes(x=age5, y=pi_x_moh, group=1),  color="purple", size=3) + 
  geom_line(data=pi_x_moh_all_lists[pi_x_moh_all_lists$list!=6,], aes(x=age5, y = pi_x, color=list), size=1.25) +
  labs(x="Age Group", y="pi_x", title="Estimated Age Distributions from Multinomial (MoH List 6)",
       subtitle="(Total Reported Deaths: 45,541)") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))

### add in prior for the reporting?
## Lancet paper: mean is 59% correctly reported, with upper and lower intervals of 47% and 66% 

pr_prior = rbeta(1000, 2, 5)*0.19 + 0.47
plot(density(pr_prior))

## 2024 numbers 
all_R_x_24 <- data.frame()
pr_prior <- list()
for(i in 1:1000){
  pr =rbeta(1, 2,5)*0.19 + 0.47
  D = 41870*pr ## R = number from Ocha 10/7/23 - 10/6/24
  tmp <- data.frame(rbind(rmultinom(1,D, as.matrix(pi_x_moh[,-1]))))
  colnames(tmp)[1] <- "pop"
  tmp$sex <- c("Female","Male")
  tmp$pi_x <-  tmp$pop/sum(tmp$pop)
  tmp$age5 <- rep(moh_list6$age5[1:17], each=2)
  pr_prior[[i]] = pr
  
  all_R_x_24 <- rbind(all_R_x_24, tmp)
}

plot_sims_Rx <- all_R_x_24 |> 
  group_by(sex, age5) |>
  summarise(pi_x_mean = mean(pi_x),
            pi_x_ll = quantile(pi_x, 0.025),
            pi_x_ul = quantile(pi_x, 0.975))

plot_sims_Rx$pi_x_moh <- moh_list6$pi_x


### plot the results of simulations with reporting error added 
ggplot(plot_sims_Rx) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age5, y = pi_x_mean), size=3)  +
  geom_line(aes(x=age5, y = pi_x_mean), size=1.25)  +
  geom_ribbon(aes(x=age5, y=pi_x_mean, ymin = pi_x_ll, ymax =pi_x_ul,  group=1),  size=1.25, alpha = 0.3) + 
  geom_point(aes(x=age5, y=pi_x_moh, group=1),  color="purple", size=3) + 
  geom_line(data=pi_x_moh_all_lists[pi_x_moh_all_lists$list!=6,], aes(x=age5, y = pi_x, color=list), size=1.25) +
  labs(x="Age Group", y="pi_x", title="Estimated Age Distributions from Multinomial (MoH List 6)",
       subtitle="Total Reported Deaths: 45,541 (2023- 2024)") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))




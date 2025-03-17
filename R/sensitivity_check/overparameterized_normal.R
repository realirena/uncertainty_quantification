rm(list=ls())
library(tidyverse)
library(haven)
library(rstan)
library(bayesplot)
library(stringr)
library(reshape2)
seed = 823
options(mc.cores = parallel::detectCores(logical= FALSE))

## set the working directory
setwd("U:/Documents/repos/Life_expectancy_Palestine")
model_dir <- "U:/Documents/repos/Life_expectancy_Palestine/R/bmmr/"
results_dir <- "U:/Documents/repos/Life_expectancy_Palestine/R/bmmr/samples/"
## read in age distributions (oct 26 data)
pi_x_alt_dist_un <- readRDS("data_inter/pi_x_alt_dist_un.rds")
pi_x_un <- pi_x_alt_dist_un[pi_x_alt_dist_un$code=="PSE"&pi_x_alt_dist_un$year==2023&pi_x_alt_dist_un$sex%in%c("m", "f"),]


## read in exposure data: 
#wpp_5yr_22 <- readRDS("data_inter/wpp_populations_5y_ages.rds")
wpp_22 <-  readRDS("data_inter/wpp_populations_single_ages.rds")
## get palestine 2023 exposures by sex 
wpp_pse <- wpp_22[wpp_22$code=="PSE"&wpp_22$year==2023&wpp_22$sex%in%c("m", "f"),]


## number of exposures (by age group in 5s)
E_x = spread(wpp_pse[,c("sex", "age","pop")], key=age, value=pop)
## get total exposures 
E = sum(rowSums(E_x[,-1]))


## get the SDs of the age distributions (needed for the age distribution priors)
pi_x_un$upper_pi_sds = (abs(pi_x_un$pi_ul_x - pi_x_un$pi_x)*sqrt(1000))/1.64
pi_x_un$lower_pi_sds = (abs(pi_x_un$pi_ll_x - pi_x_un$pi_x)*sqrt(1000))/1.64

pi_x_un$pi_sds = ifelse(pi_x_un$lower_pi_sds>=pi_x_un$upper_pi_sds,pi_x_un$lower_pi_sds, pi_x_un$upper_pi_sds)

# ## get the SDs of the age distributions (needed for the age distribution priors)
# pi_x_un$upper_pi_sds = abs(pi_x_un$pi_ul_x - pi_x_un$pi_x)/1.64
# pi_x_un$lower_pi_sds = abs(pi_x_un$pi_ll_x - pi_x_un$pi_x)/1.64
# pi_x_un$pi_sds = ifelse(pi_x_un$lower_pi_sds>=pi_x_un$upper_pi_sds, pi_x_un$lower_pi_sds, pi_x_un$upper_pi_sds)



## number of exposures by each age 
pi_x= spread(pi_x_un[,c("sex", "age", "pi_x")], key=age, value=pi_x)
pi_sds = spread(pi_x_un[,c("sex", "age", "pi_sds")], key=age, value=pi_sds)
### ------------------------------
### calculations for the over-parameterized normal prior 
### ------------------------------
### exp(mu)
pi_mu = as.matrix(log(pi_x[,-1]))
### exp(sigma^2)
pi_sigma = as.matrix(pi_sds[,-1])

#### simulate from these prior values just for a sanity check: 
phi = list()
for(i in 1:1000){
  f_tmp = exp(sapply(seq_along(pi_mu[1,]), function(i){rnorm(1, pi_mu[1,i],pi_sigma[1,i])}))
  m_tmp =  exp(sapply(seq_along(pi_mu[2,]), function(i){rnorm(1, pi_mu[2,i], pi_sigma[1,i])})) ## draw thetas from a normal distribution and then exponentiate them 
  phi_f_vals =f_tmp/sum(f_tmp, m_tmp) ## compute the proportions 
  phi_m_vals =m_tmp/sum(f_tmp, m_tmp) ## compute the proportions 
  phi[[i]] = rbind(phi_f_vals, phi_m_vals)
}


phi_dat = data.frame(Reduce(rbind, phi))
#phi_dat2  = data.frame(Reduce(rbind, phi))
colnames(phi_dat) = colnames(pi_sigma)
phi_dat$sex = c("f", "m")

plotPhi <- reshape2::melt(phi_dat, id.vars="sex", variable.name="age", value.name="prior")

# ggplot(plotPhi[plotPhi$sex=="f",]) + 
#   geom_histogram(aes(x=prior), bins=35) + 
#   facet_wrap(~age) 
# 
# ggplot(plotPhi[plotPhi$sex=="m",]) + 
#   geom_histogram(aes(x=prior), bins=35) + 
#   facet_wrap(~age) 

phi_vals <- plotPhi |> 
  group_by(sex, age) |> 
  summarise(mean = mean(prior),
            sd= sd(prior),
            var=var(prior),
            prior_ll = quantile(prior, 0.025),
            prior_ul = quantile(prior, 0.975)
            )

phi_vals$empirical_mean <- pi_x_un$pi_x
phi_vals$empirical_ul = pi_x_un$pi_ul_x
phi_vals$empirical_ll = pi_x_un$pi_ll_x


ggplot(phi_vals) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age, y = mean), size=3)  +
  geom_line(aes(x=age, y = mean), size=1.25)  +
  geom_point(aes(x=age, y=empirical_mean, group=1),  color="purple", size=3) + 
 # geom_ribbon(aes(x=age, y=mean, ymin = prior_ll, ymax =prior_ul, group=1), color="blue", size=1.25, alpha = 0.3) + 
  #geom_ribbon(aes(x=age, y=mean, ymin = empirical_ll, ymax =empirical_ul, group=1),  color="green", size=1.25, alpha = 0.1) + 
  labs(x="Age", y="pi_x", title="Generated Uncertainty from Prior Distribution") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))

### plot of prior uncertainty
ggplot(phi_vals) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age, y = mean), size=0.5)  +
 # geom_line(aes(x=age, y = mean), size=1.25)  +
 # geom_point(aes(x=age, y=empirical_mean, group=1),  color="purple", size=3) + 
  geom_ribbon(aes(x=age, y=mean, ymin = prior_ll, ymax =prior_ul, group=1), color="blue", size=0.75, alpha = 0.8) + 
 # geom_ribbon(aes(x=age, y=mean, ymin = empirical_ll, ymax =empirical_ul, group=1),  color="green", size=1.25, alpha = 0.1) + 
  labs(x="Age", y="pi_x", title="Generated Uncertainty from Prior Distribution") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
     #   axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))

### plot of empirical uncertainty
ggplot(phi_vals) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age, y = mean), size=0.5)  +
 # geom_line(aes(x=age, y = mean), size=1.25)  +
#  geom_point(aes(x=age, y=empirical_mean, group=1),  color="purple", size=3) + 
  geom_ribbon(aes(x=age, y=mean, ymin = prior_ll, ymax =prior_ul, group=1), color="blue", size=0.75, alpha = 0.8) + 
 geom_ribbon(aes(x=age, y=mean, ymin = empirical_ll, ymax =empirical_ul, group=1),  color="green", size=0.75, alpha = 0.1) + 
  labs(x="Age", y="pi_x", title="Generated Uncertainty from Prior Distribution") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
       # axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))

ggplot(phi_vals) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age, y = mean), size=0.5)  +
  # geom_line(aes(x=age, y = mean), size=1.25)  +
  #  geom_point(aes(x=age, y=empirical_mean, group=1),  color="purple", size=3) + 
  geom_ribbon(aes(x=age, y=mean, ymin = prior_ll, ymax =prior_ul, group=1), color="blue", size=0.75, alpha = 0.8) + 
  geom_ribbon(aes(x=age, y=mean, ymin = empirical_ll, ymax =empirical_ul, group=1),  color="green", size=0.75, alpha = 0.1) + 
  labs(x="Age", y="pi_x", title="Generated Uncertainty from Prior Distribution") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))

ggplot(phi_vals) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age, y = empirical_mean), size=0.5)  +
  # geom_line(aes(x=age, y = mean), size=1.25)  +
  #  geom_point(aes(x=age, y=empirical_mean, group=1),  color="purple", size=3) + 
 # geom_ribbon(aes(x=age, y=mean, ymin = prior_ll, ymax =prior_ul, group=1), color="blue", size=0.75, alpha = 0.8) + 
  geom_ribbon(aes(x=age, y=mean, ymin = empirical_ll, ymax =empirical_ul, group=1),  color="green", size=0.75, alpha = 0.1) + 
  labs(x="Age", y="pi_x", title="Generated Uncertainty from Prior Distribution") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))




### play around with some prior distributions for the reporting rate: 
set.seed(1234)
pr_prior = data.frame(rbeta(10000, 2, 2)*0.14 + 0.52)
names(pr_prior) <- "x"
mean(pr_prior$x)

ggplot(data=pr_prior, aes(x=x)) + 
  geom_density(fill="#378582", alpha=0.5) +
  geom_vline(xintercept=mean(pr_prior$x), size=1.25, linetype="dashed",color="black") + 
  labs(title = "Reporting Rate Prior", subtitle="Mean: 0.59 (41% underreporting)",
       x="Probability") + 
  theme(plot.title=element_text(size=20, hjust=0.5),
        plot.subtitle = element_text(size=18, hjust=0.5),
        axis.text.x = element_text(size=12,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=12),
        axis.text.y = element_text(size=12))

y = pr_prior*0.4 + 0.68 ## shifting so there can be no more than 60% underreporting at most and 20% overreporting at most  
hist(y)
mean(y)

y2 = y + 0.8
hist(y2)


## read in age distributions (oct 26 data)
pi_x_bts <- readRDS("data_inter/pi_x_btselem_lim_5y.rds")

ggplot(pi_x_bts) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age, y = pi_mean), size=0.5)  +
  # geom_line(aes(x=age, y = mean), size=1.25)  +
  #  geom_point(aes(x=age, y=empirical_mean, group=1),  color="purple", size=3) + 
  # geom_ribbon(aes(x=age, y=mean, ymin = prior_ll, ymax =prior_ul, group=1), color="blue", size=0.75, alpha = 0.8) + 
  geom_ribbon(aes(x=age, y=pi_mean, ymin = pi_ll, ymax =pi_ul, group=1),  color="green", size=0.75, alpha = 0.1) + 
  labs(x="Age", y="pi_x", title="Empirical Distribution") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))

## number of exposures by each age 
pi_x= spread(pi_x_bts[,c("sex", "age", "pi_mean")], key=age, value=pi_mean)

## get the SDs of the age distributions (needed for the age distribution priors)
# pi_x_bts$upper_pi_sds = (abs(pi_x_bts$pi_ul- pi_x_bts$pi_mean)*sqrt(23))/1.64
# pi_x_bts$lower_pi_sds = (abs(pi_x_bts$pi_ll - pi_x_bts$pi_mean)*sqrt(23))/1.64
#pi_x_bts$pi_sds = ifelse(pi_x_bts$lower_pi_sds>=pi_x_bts$upper_pi_sds,pi_x_bts$lower_pi_sds, pi_x_bts$upper_pi_sds)


pi_sds = spread(pi_x_bts[,c("sex", "age", "pi_sd")], key=age, value=pi_sd)


pi_sigma = as.matrix(pi_sds[,-1])/as.matrix(pi_x[,-1])
pi_mu = as.matrix(log(pi_x[,-1]))# - 0.5*((as.matrix(pi_sds[,-1])^2)/(pi_x[,-1]^2)))
### exp(sigma^2)

#### simulate from these prior values just for a sanity check: 
phi_bts = list()
for(i in 1:5000){
  f_tmp = exp(sapply(seq_along(pi_mu[1,]), function(i){rnorm(1, pi_mu[1,i],log(pi_sigma[1,i]))}))
  m_tmp =  exp(sapply(seq_along(pi_mu[2,]), function(i){rnorm(1, pi_mu[2,i], log(pi_sigma[1,i]))})) ## draw thetas from a normal distribution and then exponentiate them 
  phi_f_vals =f_tmp/sum(f_tmp, m_tmp) ## compute the proportions 
  phi_m_vals =m_tmp/sum(f_tmp, m_tmp) ## compute the proportions 
  phi_bts[[i]] = rbind(phi_f_vals, phi_m_vals)
}

phi_dat_bts = data.frame(Reduce(rbind, phi_bts))
#phi_dat2  = data.frame(Reduce(rbind, phi))
colnames(phi_dat_bts) = colnames(pi_sigma)
phi_dat_bts$sex = c("f", "m")

plotPhi <- reshape2::melt(phi_dat_bts, id.vars="sex", variable.name="age", value.name="prior")


phi_vals <- plotPhi |> 
  group_by(sex, age) |> 
  summarise(mean = mean(prior),
            sd= sd(prior),
            var=var(prior),
            prior_ll = quantile(prior, 0.025),
            prior_ul = quantile(prior, 0.975)
  )

phi_vals$empirical_mean <- pi_x_bts$pi_mean
phi_vals$empirical_ul = pi_x_bts$pi_ul
phi_vals$empirical_ll = pi_x_bts$pi_ll
phi_vals$empirical_sd = pi_x_bts$pi_sd

ggplot(phi_vals) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age, y = mean), size=0.5)  +
  # geom_line(aes(x=age, y = mean), size=1.25)  +
  geom_point(aes(x=age, y=empirical_mean, group=1),  color="purple", size=0.5) + 
  geom_ribbon(aes(x=age, y=mean, ymin = prior_ll, ymax =prior_ul, group=1), color="blue", size=0.75, alpha = 0.8) + 
  geom_ribbon(aes(x=age, y=empirical_mean, ymin = empirical_ll, ymax =empirical_ul, group=1),  color="green", size=0.75, alpha = 0.1) + 
  labs(x="Age", y="pi_x", title="Empirical Distribution") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))


### simulate death counts from a multinomial using the MoH lists: 
## read in exposure data: 
pi_x_moh_all_lists <- readRDS("data_inter/age_sex_distributions_moh_all_lists.rds")

moh_list6 <- pi_x_moh_all_lists[pi_x_moh_all_lists$list==6,]


pi <- rbind(c(0.49, 0.1, 0.15), c(0.01, 0.1, 0.15))
rmultinom(1, 100, pi)

pi_x_moh = spread(moh_list6[,c("sex", "age5", "pi_x")], key=age5, value=pi_x)


## 2023 numbers 
all_R_x_23 <- data.frame()
for(i in 1:1000){
  tmp <- data.frame(rbind(rmultinom(1, 21978, as.matrix(pi_x_moh[,-1]))))
  colnames(tmp)[1] <- "pop"
  tmp$sex <- c("Female", "Male")
  tmp$pi_x <-  tmp$pop/sum(tmp$pop)
  tmp$age5 <- rep(moh_list6$age5[1:17], each=2)
  
  all_R_x_23 <- rbind(all_R_x_23, tmp)
}


## 2024 numbers 
all_R_x_24 <- data.frame()
for(i in 1:1000){
  tmp <- data.frame(rbind(rmultinom(1,41870, as.matrix(pi_x_moh[,-1]))))
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
            pi_x_ll = quantile(pi_x, 0.025),
            pi_x_ul = quantile(pi_x, 0.975))

plot_sims23$pi_x_moh <- moh_list6$pi_x

plot_sims24 <- all_R_x_24 |> 
  group_by(sex, age5) |>
  summarise(pi_x_mean = mean(pi_x),
            pi_x_ll = quantile(pi_x, 0.025),
            pi_x_ul = quantile(pi_x, 0.975))

plot_sims24$pi_x_moh <- moh_list6$pi_x

### start with plots of the age distributions (by list)
ggplot(plot_sims23) + 
  facet_wrap(~sex) + 
  geom_point(aes(x=age5, y = pi_x_mean), size=3)  +
  geom_line(aes(x=age5, y = pi_x_mean), size=1.25)  +
  geom_point(aes(x=age5, y=pi_x_moh, group=1),  color="purple", size=3) + 
  geom_ribbon(aes(x=age5, y=pi_x_mean, ymin = pi_x_ll, ymax =pi_x_ul,  color=sex, group=1),  size=1.25, alpha = 0.3) + 
  geom_line(data=pi_x_moh_all_lists[pi_x_moh_all_lists$list!=6,], aes(x=age5, y = pi_x, color=list), size=1.25) +
  labs(x="Age Group", y="pi_x", title="Estimated Age Distributuons from Multinomial (MoH List 6)",
       subtitle="2023 numbers (Ocha Day 85)") + 
  theme_bw() + 
  theme(plot.title=element_text(size=40, hjust=0.5),
        plot.subtitle = element_text(size=35, hjust=0.5),
        axis.text.x = element_text(size=30,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=30),
        legend.title=element_text(size=30),
        legend.text=element_text(size=30),
        axis.text.y = element_text(size=30))

### start with plots of the age distributions (by list)
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



rm(list=ls())
library(tidyverse)
library(haven)
library(rstan)
library(bayesplot)
library(stringr)
library(reshape2)
seed = 823
options(mc.cores = parallel::detectCores(logical= FALSE))


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
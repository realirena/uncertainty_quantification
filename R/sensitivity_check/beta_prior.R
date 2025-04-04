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
pr_prior = data.frame(rbeta(20000, 3,3)*0.48 + 0.76)
names(pr_prior) <- "x"
mean(pr_prior$x)

ggplot(data=pr_prior, aes(x=x)) + 
  geom_density(fill="#378582", alpha=0.5) +
  geom_vline(xintercept=mean(pr_prior$x), size=1.25, linetype="dashed",color="black") + 
  labs(title = "Reporting Rate Beta Prior", subtitle="Mean: 1.0 (lower and upper bounds: 0.76 - 1.24)",
       x="Probability") + 
  theme(plot.title=element_text(size=35, hjust=0.5),
        plot.subtitle = element_text(size=30, hjust=0.5),
        axis.text.x = element_text(size=20,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=20),
        axis.text.y = element_text(size=20))

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



# Plot --------------------------------------------------------------------

pr_prior_1 = data.frame("pr" = rbeta(100000, 2, 2)*0.14 + 0.52)
pr_prior_2 = data.frame("pr" = rbeta(100000, 3, 3)*0.48 + 0.76)
pr_prior <- cbind(serie = rep(c("Beta (2,2)*0.14 + 0.52", "Beta(3,3)*0.48 + 0.76"), each = 100000) , 
                  pr = rbind(pr_prior_1, pr_prior_2))
head(pr_prior)

ggplot() + 
  geom_density(data=pr_prior, aes(x=pr, fill=serie), alpha=0.5) +
  geom_vline(data=pr_prior %>% filter(serie == "Beta (2,2)*0.14 + 0.52"), 
                aes(xintercept=mean(pr)), size=1.25, linetype="dashed",color="black") + 
     geom_vline(data=pr_prior %>% filter(serie == "Beta(3,3)*0.48 + 0.76"), 
                aes(xintercept=mean(pr)), size=1.25, linetype="dashed",color="black") + 
     labs(title = "", 
          # subtitle="Mean: 1.0 (lower and upper bounds: 0.8 - 1.2)",
          x="Probability of under- or over-reporting", y = "Density") + 
  scale_fill_manual(values = c("#054fb9", "#b3c7f7"), name = "Prior") + 
  xlim(c(0.5,1.3)) +
  # scale_x_continuous(breaks = seq(0.5, 1.2, 0.1)) +
  theme_classic() + 
  theme(plot.title=element_text(size=14, hjust=0.5),
  plot.subtitle = element_text(size=18, hjust=0.5),
           axis.text.x = element_text(size=11,angle =45, vjust = 1, hjust = 1),
           axis.title=element_text(size=12),
           axis.text.y = element_blank(), 
           axis.ticks.y = element_blank(),
           legend.text = element_text(size=11),
           legend.title = element_text(size=12),
           legend.position = "bottom") +
  annotate("text", label = "Mean: 0.59", x = .62, y = 11, hjust = 0) +
  annotate("text", label = "Mean: 1.0", x = 1.05, y = 5, hjust = 0)

ggsave(file = "figures/pr_priors.pdf", height = 4, width = 6)

#### different reporting rates 
rep_rate_grp <- readRDS("data/pr_age.rds")

rep_rate_grp$int <- rep_rate_grp$pr_ll - rep_rate_grp$pr_ul
rep_rate_grp <- rep_rate_grp[rep_rate_grp$sex%in%c("Female", "Male"),]
rep_ll <-  spread(rep_rate_grp[,c("sex", "agegrp", "pr_ul")], key=agegrp, value=pr_ul)
rep_int <-  spread(rep_rate_grp[,c("sex", "agegrp", "int")], key=agegrp, value=int)
### 
rep_cat = 5 


file_names <- c("moh24_samples_1","moh24_samples_2", "moh24_samples_3", "moh24_samples_4")
model_out <- read_stan_csv(paste0(results_dir, file_names,".csv"))

pr_summary <- data.frame(summary(model_out, pars=c("pr"))$summary)
pr_summary$sex <- rep(c("Female", "Male"), each=5)
pr_summary$agegrp <- rep(c(0,15, 30, 45, 60), 2)

pr_summary <- merge(pr_summary[,c("sex", "agegrp", "mean", "X2.5.", "X97.5.")], rep_rate_grp, by=c("sex", "agegrp"))
### extract the model-generated mortality distributions (incl WPP deaths)
pr_samples = data.frame(extract(model_out, pars=c("pr"))$pr)
df2 <- data.frame(t(pr_samples)) 
df2$sex <- rep_rate_grp$sex
df2$agegrp <- rep_rate_grp$agegrp
df2$pr_mean <- rep_rate_grp$pr_mean

pr_plot <- melt(df2, id.vars=c("sex", "agegrp", "pr_mean"), variable.name="iteration", value.name="est")


pr_plot <- plot(model_out, pars=c("pr"), show_density = TRUE, ci_level = 0.8, fill_color = "purple") 

ggplot(data=pr_plot) + 
  geom_histogram(aes(x=est, group=sex, color=sex, fill=sex), alpha=0.75) +
  facet_wrap(~agegrp) + 
  geom_bar(aes(x=pr_mean, color=sex, fill=sex)) + 
  labs(title = "") +  
  theme(plot.title=element_text(size=20, hjust=0.5),
        plot.subtitle = element_text(size=18, hjust=0.5),
        axis.text.x = element_text(size=12,angle =45, vjust = 1, hjust = 1),
        axis.title=element_text(size=12),
        axis.text.y = element_text(size=12))







pr_plot

set.seed(1234)
pr_prior =rbeta(10000,2, 2)*0.3536502+0.2255704
mean(pr_prior)
hist(pr_prior)
rep_rate_grp[10,3]

pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[2,4]- rep_rate_grp[2,5])) + as.numeric(rep_rate_grp[2,5])
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[2,3]

pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[3,4]- rep_rate_grp[3,5]) + as.numeric(rep_rate_grp[3,5]))
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[3,3]


pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[4,4]- rep_rate_grp[4,5]) + as.numeric(rep_rate_grp[4,5]))
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[4,3]

pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[5,4]- rep_rate_grp[5,5]) + as.numeric(rep_rate_grp[5,5]))
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[5,3]


pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[6,4]- rep_rate_grp[6,5]) + as.numeric(rep_rate_grp[6,5]))
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[6,3]


pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[7,4]- rep_rate_grp[7,5]) + as.numeric(rep_rate_grp[7,5]))
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[7,3]


pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[8,4]- rep_rate_grp[8,5]) + as.numeric(rep_rate_grp[8,5]))
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[8,3]

pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[9,4]- rep_rate_grp[9,5]) + as.numeric(rep_rate_grp[9,5]))
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[9,3]

pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[10,4]- rep_rate_grp[10,5]) + as.numeric(rep_rate_grp[10,5]))
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[10,3]

pr_prior = (rbeta(10000,2,2)*as.numeric(rep_rate_grp[10,4]- rep_rate_grp[10,5]) + as.numeric(rep_rate_grp[10,5]))
hist(pr_prior)
mean(pr_prior)
rep_rate_grp[10,3]

### age group 0: female (beta(7,2))
## age group 0: male ( )
## age group 60 male: beta(4,2)

unique(rep_rate_grp$agegrp)
unique(pi_x_moh$age)

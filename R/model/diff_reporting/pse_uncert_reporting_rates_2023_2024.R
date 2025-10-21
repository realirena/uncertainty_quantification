rm(list=ls())
library(tidyverse)
library(haven)
library(reshape2)
library(stringr)
library(rstan)
seed = 1234

## load the functions to calculate life expectancy 
# source("R/0_setup.R")

load("R/model/West_Bank/mx_wb.RData")
load("R/model/West_Bank/mx_wb_total.RData")

# function for copy/paste in excel
copy_this <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file = paste0("clipboard-", object.size(x)),sep="\t",row.names=row.names,col.names=col.names,...)
}


# my clumsy way to transform the output matrix into tidy data
to_tidy <- function(mt = mu_x){
  out <- tibble()
  for(k in 1:18){
    
    tst1 <- 
      mt[,,k] %>% 
      as_tibble() %>% 
      mutate(age = case_when(k == 1 ~ 0,
                             k == 2 ~ 1,
                             k %in% 3:17 ~ (k-2)*5,
                             k == 18 ~ 80),
             iter = 1:n()) %>% 
      rename(f = V1, m = V2)
    
    out <- 
      out %>% 
      bind_rows(tst1)
  }
  
  out2 <- 
    out %>% 
    gather(m, f, key = sex, value = mx) %>% 
    arrange(iter, sex, age)
  
}

# function to extract the draws from the models
gime_rates <- function(rg, yr){
  ## read in samples 
  # list of samples (here "moh" refers to estimates with the age distribution of 
  # the gaza ministry of health. there are also for historic btselem historic
  # average, and for UN patterns)
  file_names <- paste0("moh", yr, "_samples_", 1:4)
  # file_names <- c("moh_""samples_1","moh_samples_2", "moh_samples_3", "moh_samples_4")
  
  # here we ca select the forecast by which expected (nonviolent) mortality was 
  # obtained, forecasting from 2019 or from 2022
  # we can also select the year, 2023 or 2024
  # and we can also select one of the three regions (palestine, gaza, or west_bank)
  model_out <- read_stan_csv(paste0("R/model/diff_reporting/samples/", rg, "/", file_names,".csv"))
  
  # there are 2 important values to distinguish:
  # all-cause mortality: mu_x_total
  # conflict mortality: mu_x
  # both groups of data have dimensions 4000 (draws) x 2 (sex, 1 for females, 2 
  # for males) x 18 (age groups)
  
  ### extract the model-generated mortality distributions (incl WPP deaths)
  # total mortality by iteration, sex, and age
  mu_x_total = rstan::extract(model_out, pars=c("mu_x_total"))$mu_x_total
  dim(mu_x_total)
  
  # for instance, all-cause mortality rates in iteration 1 for females by age (18 age groups)
  mu_x_total[1,1,]
  
  # conflict mortality by age (18 groups) and sex (2) x 4000 iterations
  mu_x = rstan::extract(model_out, pars=c("mu_x"))$mu_x
  dim(mu_x)
  
# to tidy
  # conf
  mx_cnf <- to_tidy(mu_x)
  
  # all-cause
  mx_tot <- to_tidy(mu_x_total)
  
  # both + no conf
  mxs <- 
    mx_tot %>% 
    rename(all = mx) %>% 
    left_join(mx_cnf %>% 
                rename(conflict = mx),
              by = join_by(age, iter, sex)) %>% 
    mutate(noconflict = all - conflict) %>% 
    gather(all, conflict, noconflict, key = cause, value = mx) %>% 
    mutate(region = rg, year = str_remove(yr, "_"))
  
  return(mxs)
}

# population ====
# ~~~~~~~~~~~~~~~
# extracting population exposures
pop <- readRDS("R/lc/data_plus_forecasts_v2.rds")
unique(pop$region)
unique(pop$year)
unique(pop$source)

# exposure for 23 and 24, from pcbs projections
pop2 <- 
  pop %>% 
  filter(year %in% 2023:2024,
         source == "pcbs") %>% 
  select(region, year, sex, age, pop) %>% 
  mutate(year = str_sub(year, 3,4) %>% as.character())

# exposure for october 23 - september 24 (rounding to the 23-24 average)
pop_av <- 
  pop2 %>% 
  reframe(pop = mean(pop),
          .by = c(region, sex, age)) %>% 
  mutate(year = "23_24")

# merging pops
pop3 <- 
  bind_rows(pop2, pop_av)


# mortality ====
# ~~~~~~~~~~~~~~
# extracting 4000 draws of all-cause, non conflict, and conflict mortality rates

# regions
rgs <- c("gaza", "palestine", "west_bank")
# for Gaza and Palestine. I did not find the draws for the West Bank(!)
rgs <- c("gaza", "palestine")

# periods
yrs <- c("23", "24", "_23_24")

# extracting estimates
all <- tibble()
for(r in rgs){
  for(y in yrs){
  all <- 
    all %>% 
    bind_rows(gime_rates(r, y))
  }
}

# TODO: pending to include the West Bank

all2 <- 
  all %>% 
  mutate(region = case_when(region == "gaza" ~ "Gaza Strip",
                            region == "west_bank" ~ "West Bank",
                            region == "palestine" ~ "Palestine",
                            TRUE ~ region)) %>% 
  left_join(pop3) %>% 
  mutate(dx = mx * pop)

all_s <- 
  all2 %>% 
  reframe(dx = sum(dx),
          pop = sum(pop),
          .by = c(iter, year, region, cause, age)) %>% 
  mutate(sex = "t",
         mx = dx/pop)

all3 <- 
  all2 %>% 
  bind_rows(all_s)

### Combatant mortality for 2023
Dx_cmb <- readRDS("data/Dx_cmb.rds") 

Dx_cmb2 <- Dx_cmb %>%
  group_by(age) %>%
  summarise(Dx_cmb_mean = sum(Dx_cmb_mean),
         sex = "t") %>%
  rbind(Dx_cmb)

## Age-sex specific mortality rates for COMBATANTS (2023 and 2023-2024 only)
mx_cmb <- pop3 %>% 
  filter(year != 24 & region != "West Bank") %>%
  left_join(Dx_cmb2, by = c("sex", "age"), relationship = "many-to-many") %>%
  mutate(mx_cmb = Dx_cmb_mean/pop) %>%
  select(-pop)

## Subtract combatant death to get true non-conflict mortality in 2023 and 2023-2024
all4 <- all3 %>%
  filter(cause == "noconflict") %>% 
  left_join(mx_cmb,  by = c("region", "year", "sex", "age"), relationship = "many-to-many") %>%
  rename(mx1 = mx) %>%
  mutate(mx_cmb = ifelse(is.na(mx_cmb), 0, mx_cmb),
         mx = mx1 - mx_cmb) %>%
  select(-mx1,-Dx_cmb_mean, -mx_cmb) %>%
  rbind(all3 %>% 
          filter(cause != "noconflict")) %>%
  select(-dx) %>%
  mutate(dx = mx * pop)


# central estimates ====
# ~~~~~~~~~~~~~~~~~~~~~~

# rates by sex and age
mxs_age_sex <- 
  all4 %>% 
  reframe(mx_m = mean(mx),
          mx_l = quantile(mx, .025),
          mx_u = quantile(mx, .975),
          .by = c(region, cause, year, sex, age)) %>%
  rbind(mx_wb %>% select(-Dx_m, -Dx_l, -Dx_u) %>%
          mutate(year = case_when(year == 2023 ~ "23",
                                  year == 2024 ~ "24", 
                                  year == 2023.5 ~ "23_24")))

# write.csv(mxs_age_sex, file = "R/mx_age_sex.csv")

# mxs_age_sex2 <- mxs_age_sex %>% 
#   filter(cause == "noconflict") %>% 
#   left_join(mx_cmb,  by = c("region", "year", "sex", "age")) %>%
#   rename(mx_mean = mx_m, mx_ll = mx_l, mx_ul = mx_u) %>% 
#   mutate(mx_cmb = ifelse(is.na(mx_cmb), 0, mx_cmb),
#          mx_m = mx_mean - mx_cmb,
#          mx_l = mx_ll - mx_cmb,
#          mx_u = mx_ul - mx_cmb) %>%
#   select(-mx_mean, -mx_ll, -mx_ul, -Dx_cmb_mean, -mx_cmb) %>%
#   rbind(mxs_age_sex %>% 
#           filter(cause != "noconflict"))

# relative changes by sex and age 
# non conflict mortality (the same forecasted or expected), here uncert = 0
bsns <-
  mxs_age_sex %>%
  filter(cause == "noconflict") %>%
  select(everything(), -mx_l, -mx_u, -cause, bsn = mx_m)

psc_age_sex <- 
  mxs_age_sex %>% 
  filter(cause == "all") %>% 
  left_join(bsns) %>% 
  mutate(psc_m = mx_m/bsn,
         psc_l = mx_l/bsn,
         psc_u = mx_u/bsn)

# plotting sex-age-specific mortality rates and relative increases
cols <- c("#ae2012", "#03071e", "#00a6fb")
mxs_age_sex %>% 
  filter(cause != "conflict") %>% 
  mutate(year = factor(year, levels = c("23", "24", "23_24"),
                       labels = c("2023", "2024", "2023-2024")),
         age_ad = ifelse(age == 80, 85, (age + lead(age))/2)) %>% 
  ggplot()+
  geom_ribbon(aes(age_ad, ymin = mx_l, ymax = mx_u, fill = sex, 
                  group = interaction(sex, cause)), 
              alpha = .3)+
  geom_line(aes(age_ad, mx_m, col = sex, group = interaction(sex, cause), 
                lty = cause))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 80, 10))+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  scale_linetype_discrete(breaks = c("noconflict", "all"),
                          labels = c("Expected", "Estimated"))+
  facet_grid(region~year)+
  labs(x = "Age", y = "Death rates", col = "Sex", fill = "Sex", 
       lty = "Mortality")+
  theme_bw()+
  theme(strip.background = element_blank())
ggsave("Figures/mx.pdf",
       w = 10, h = 7)
ggsave("Figures/mx.png",
       dpi = 400,
       w = 10, h = 5)


# plotting sex-age-specific mortality relative risks
psc_age_sex %>% 
  filter(cause != "conflict") %>% 
  mutate(year = factor(year, levels = c("23", "24", "23_24"),
                       labels = c("2023", "2024", "2023-2024")),
         age_ad = ifelse(age == 80, 85, (age + lead(age))/2)) %>% 
  ggplot()+
  geom_ribbon(aes(age_ad, ymin = psc_l, ymax = psc_u, fill = sex, 
                  group = interaction(sex)), 
              alpha = .3)+
  geom_line(aes(age_ad, psc_m, col = sex, group = interaction(sex)))+
  scale_y_log10()+
  scale_x_continuous(breaks = seq(0, 80, 10))+
  scale_color_manual(values = cols)+
  scale_fill_manual(values = cols)+
  scale_linetype_discrete(breaks = c("noconflict", "all"),
                          labels = c("Expected", "Estimated"))+
  facet_grid(region~year)+
  labs(x = "Age", y = "Relative Mortality\nIncrease (times)", 
       col = "Sex", fill = "Sex", lty = "Mortality")+
  theme_bw()+
  theme(strip.background = element_blank())
ggsave("Figures/pscs.pdf",
       w = 10, h = 7)
ggsave("figures/tsts_ka/pscs.png",
       dpi = 400,
       w = 10, h = 5)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# total by sex 
# ~~~~~~~~~~~~
mxs_sex <- 
  all4 %>% 
  reframe(dts = sum(dx),
          pop = sum(pop),
          .by = c(iter, year, region, cause, sex)) %>% 
  reframe(dts_m = mean(dts),
          dts_l = quantile(dts, 0.025),
          dts_u = quantile(dts, 0.975),
          pop = mean(pop),
          .by = c(year, region, cause, sex)) %>% 
  mutate(mx_m = dts_m/pop, 
         mx_l = dts_l/pop, 
         mx_u = dts_u/pop) %>%
  rbind(mx_wb_total %>%
          mutate(year = case_when(year == 2023 ~ "23",
                                  year == 2024 ~ "24", 
                                  year == 2023.5 ~ "23_24")))

write.csv(mxs_sex, file = "R/mx_sex.csv")

# baseline mortality by sex 
bsns_sex <- 
  mxs_sex %>% 
  filter(cause == "noconflict") %>% 
  select(year, region, sex, pop, bsn = mx_m)

# relative changes by sex 
psc_sex <- 
  mxs_sex %>% 
  filter(cause == "all") %>% 
  select(-cause) %>% 
  left_join(bsns_sex) %>% 
  mutate(psc_m = round(mx_m/bsn, 1),
         psc_l = round(mx_l/bsn, 1),
         psc_u = round(mx_u/bsn, 1))

# conflict deaths
cnf <- 
  mxs_sex %>% 
  filter(cause == "conflict") %>% 
  select(year, region, sex, 
         cnf_m = dts_m, cnf_l = dts_l, cnf_u = dts_u)

tab1 <- 
  psc_sex %>% 
  left_join(cnf) %>% 
  arrange(region, year) %>% 
  select(year, region, sex, pop, bsn, 
         mx_m, mx_l, mx_u, 
         dts_m, dts_l, dts_u, 
         cnf_m, cnf_l, cnf_u, 
         psc_m, psc_l, psc_u) %>% 
  mutate(bsn = bsn*1e5,
         mx_m = mx_m*1e5,
         mx_l = mx_l*1e5,
         mx_u = mx_u*1e5) 

copy_this(tab1)

library(scales)
library(knitr)

tab2 <- tab1 %>%
  mutate(year = factor(year, levels = c("23", "24", "23_24"),
                       labels = c("2023", "2024", "2023-2024")),
         dts_ci = sprintf("%.2f (%.2f, %.2f)", dts_m, dts_l, dts_u),
         cnf_ci = sprintf("%.2f (%.2f, %.2f)", cnf_m, cnf_l, cnf_u),
         psc_ci = sprintf("%.2f (%.2f, %.2f)", psc_m, psc_l, psc_u)) %>%
  select(region, year, sex, dts_ci, cnf_ci, psc_ci) %>%
  mutate(sex = case_when(sex == "f" ~ "Females",
                         sex == "m" ~ "Males",
                         sex == "t" ~ "Total")) %>%
  arrange(region, year, sex)

knitr::kable(tab2, format = "latex", booktabs = TRUE, caption = "Your Table Caption")

tab2 <- tab1 %>%
  mutate(year = factor(year, levels = c("23", "24", "23_24"),
                     labels = c("2023", "2024", "2023-2024")),
       dts_ci = sprintf("%.2f", dts_m),
       cnf_ci = sprintf("%.2f", cnf_m),
       psc_ci = sprintf("%.2f", psc_m)) %>%
  select(region, year, sex, dts_ci, cnf_ci, psc_ci) %>%
  mutate(sex = case_when(sex == "f" ~ "Females",
                         sex == "m" ~ "Males",
                         sex == "t" ~ "Total")) %>%
  rbind(tab1 %>%
          mutate(year = factor(year, levels = c("23", "24", "23_24"),
                               labels = c("2023", "2024", "2023-2024")),
                 dts_ci = sprintf("(%.2f, %.2f)", dts_l, dts_u),
                 cnf_ci = sprintf("(%.2f, %.2f)", cnf_l, cnf_u),
                 psc_ci = sprintf("(%.2f, %.2f)", psc_l, psc_u)) %>%
          select(region, year, sex, dts_ci, cnf_ci, psc_ci) %>%
          mutate(sex = case_when(sex == "f" ~ "Females",
                                 sex == "m" ~ "Males",
                                 sex == "t" ~ "Total"))) %>%
  arrange(region, year, sex)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# quality checks ====
# ~~~~~~~~~~~~~~~~~~~

# the change in the baseline between both years seems weird!!!
# it is the forcasted mortality, and also nonconflict mortality from the model
# testing consistency between both

# non conflict deaths: 
# ~~~~~~~~~~~~~~~~~~~

# from forecasts
fcs <- 
  pop %>% 
  filter(source == "lc_pcbs_2019", 
         year %in% 2023:2024,
         sex != "t",
         region != "West Bank") %>% 
  select(region, year, sex, age, mx_fcs = mx_noc) 

# from the model
mdl <- 
  all4 %>% 
  filter(cause == "noconflict",
         year != "23_24") %>% 
  reframe(mx_mdl = mean(mx)*1e5,
          .by = c(region, year, sex, age)) %>% 
  mutate(year = year %>% as.integer()+2000)

mdl <- 
  mxs_age_sex %>% 
  filter(cause == "noconflict",
         year != "23_24") %>% 
  mutate(mx_mdl = mx_m*1e5) %>% 
  mutate(year = year %>% as.integer()+2000)

tst <- 
  fcs %>% 
  left_join(mdl)

tst %>% 
  gather(mx_fcs, mx_mdl, key = source, value = mx) %>% 
  ggplot()+
  geom_line(aes(age, mx, col = source, group = source),
            alpha = 0.7)+
  scale_y_log10()+
  facet_grid(region~sex+year)+
  theme_bw()+
  theme(strip.background = element_blank())

ggsave("figures/tsts_ka/noconflict_comparison.png",
       dpi = 300)  



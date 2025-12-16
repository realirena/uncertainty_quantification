rm(list=ls())
library(tidyverse)
library(ungroup)
library(rstan)

# PCBS estimates
pop <- read_rds("R/lc/data_plus_forecasts_v2.rds")

# Guillot population estimates
pop_guill <- readRDS("R/data_cleaning/data_inter/pcbs_exposures_gaza_guillot.rds")

# Compare both, we see how the PCBS ones for 2024 were adjusted
pop_guill %>%
  filter(region == "Gaza Strip") %>%
  reframe(pop = sum(pop), .by = c(year, sex, age, source)) %>%
  rbind(pop %>% 
            filter(source == "pcbs",
                   region == "Gaza Strip") %>%
          reframe(pop = sum(pop), .by = c(year, sex, age, source))) %>%
  filter(year >= 2023) %>%
  ggplot() +
  geom_line(aes(x =  age, y = pop, col = source)) +
  facet_wrap(year~sex)

# Another plot
pop_guill %>%
  filter(year >= 2023) %>%
  ggplot() +
  geom_line(aes(x =  age, y = pop, col = as.factor(year))) +
  facet_wrap(~sex)


# calculating death counts from the rates and exposures
# (ideally, we obtain these directly from the samples)
# dxs <- 
#   mxs %>% 
#   filter(cause == "conflict",
#          year == "2023-2024") %>% 
#   mutate(sex = str_sub(sex, 1, 1) %>% str_to_lower()) %>% 
#   left_join(pop2) %>% 
#   mutate(dx = mx_m*pop) %>% 
#   select(year, sex, age, dx)

###### Extract 2023-2024 conflict deaths from the model
## read in samples 
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/gaza/")
file_names_y1 <- c("moh_23_24_samples_1","moh_23_24_samples_2", 
                "moh_23_24_samples_3", "moh_23_24_samples_4")
model_out_y1 <- read_stan_csv(paste0(results_dir, file_names_y1,".csv"))

### extract the model-generated mortality distributions (incl WPP deaths)
Dx_conf_all_y1 = rstan::extract(model_out_y1, pars=c("D_x_gen"))$D_x_gen

# my clumsy way to transform the output matrix into tidy data
to_tidy <- function(mt = dx){
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
    gather(m, f, key = sex, value = dx) %>% 
    arrange(iter, sex, age)
  
}

Dx_cnf_y1 <- to_tidy(Dx_conf_all_y1)

# Read combantant deaths
Dx_cmb <- readRDS("data/Dx_cmb.rds") 

# Add combatant deaths to conflict deaths for the first year
dxs_y1 <- Dx_cnf_y1 %>%
  left_join(Dx_cmb, by = c("sex", "age")) %>%
  mutate(dx = dx + Dx_cmb_mean) %>%
  reframe(dx = sum(dx),
          .by = c(iter, sex, age)) %>%
  reframe(dx = mean(dx), .by = c(sex, age)) %>%
  mutate(year = "2023-2024")

###### Extract 2024_2025 conflict deaths from the model
file_names_y2 <- c("moh_24_25_samples_1","moh_24_25_samples_2", 
                "moh_24_25_samples_3", "moh_24_25_samples_4")
model_out_y2 <- read_stan_csv(paste0(results_dir, file_names_y2,".csv"))

### extract the model-generated mortality distributions (incl WPP deaths)
Dx_conf_all_y2 = rstan::extract(model_out_y2, pars=c("D_x_gen"))$D_x_gen

Dx_cnf2_y2 <- to_tidy(Dx_conf_all_y2)

# Get mean of the samples
dxs_y2 <- Dx_cnf2_y2 %>%
  reframe(dx = mean(dx), .by = c(sex, age)) %>%
  mutate(year = "2024-2025")

# Combine conflict death estimats for both war years
dxs <- rbind(dxs_y1, dxs_y2)

# testing total death toll for each year
dxs %>% 
  summarise(dts = sum(dx),
            .by = c(year, sex))

# plotting age structure of deaths
dxs %>% 
  ggplot()+
  geom_line(aes(age, dx, col = as.factor(year)))+
  facet_grid(~sex)+
  theme_bw()

# age ungrouping function
ung_age_dts <- function(chunk){
  
  dt_in <-
    tibble(age = chunk$age,
           dx = chunk$dx,
           # this is just increasing the size in each group just for 
           # statistical power. we can then shrink it again after ungrouping
           dx_mt = dx*1e5) %>%
    mutate(dx_mt = ifelse(dx_mt == 0, 1, dx_mt))
  # last age inval
  nl <- 21
  
  # applying pclm
  dxs <- pclm(x = dt_in$age,
              y = dt_in$dx_mt,
              nlast = nl)$fitted
  fit <- tibble(age = 0:100, dx = dxs/1e5)
  
  # output dataframe
  out <-
    chunk %>%
    select(year, sex) %>%
    unique() %>%
    left_join(fit,
              by = character())
  return(out)
}

# ungrouping by sex
dxs_ungrp <- 
  dxs %>% 
  group_by(year, sex) %>% 
  # applying the ungrouping function
  do(ung_age_dts(chunk = .data)) %>% 
  ungroup()

# plotting single year age death counts
dxs_ungrp %>% 
  ggplot()+
  geom_line(aes(age, dx, col = as.factor(year)))+
  facet_grid(~sex)+
  theme_bw()

# testing consistency with the original data
dxs_gr <- 
  dxs_ungrp %>% 
  mutate(age = case_when(age == 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age >= 80 ~ 80,
                         TRUE ~ age - age%%5)) %>% 
  reframe(dx = sum(dx),
          .by = c(year, sex, age)) %>% 
  mutate(source = "ungrouped")

dxs_gr %>% 
  bind_rows(dxs %>% 
              mutate(source = "original")) %>% 
  ggplot()+
  geom_line(aes(age, dx, col = source), alpha = 0.5)+
  facet_grid(year~sex)+
  theme_bw()

# Perfect overlapping!!

# Adjustment to exposure for population in year 1
# population year 1, we estimate the population in April
pop_apr24 <- 
  pop_guill %>% 
  filter(year %in% 2023:2024,
         source == "pcbs_gaza",
         region == "Gaza Strip") %>% 
  select(year, sex, age, pop) %>% 
  spread(year, pop) %>% 
  mutate(pop = 3/12*`2023`+9/12*`2024`,
         year = "2023-2024") %>% 
  select(year, sex, age, pop)

pop_apr24 %>%
  rbind(pop_guill %>%
          filter(year >= 2023) %>%
          select(-source, -region)) %>%
  ggplot() +
  geom_line(aes(x =  age, y = pop, col = as.factor(year))) +
  facet_wrap(~sex)

# Remove 80% of the conflict deaths that occurred during the first war year from the population estimates (in April 2024)
### We remove 80% of the deaths because that is the percentage of the year deaths that occurred before April 2024
pop_y1 <- pop_apr24 %>%
  left_join(dxs %>%
              filter(year == "2023-2024"), 
            by = c("year", "sex", "age")) %>%
  mutate(pop_new = pop - 0.8*dx) %>% 
  select(year, sex, age, pop, pop_new)

# Plot to see the adjustment
pop_y1 %>% 
  ggplot() + 
  geom_line(aes(x = age, y = pop)) + 
  geom_line(aes(x = age, y = pop_new), linetype = 2) +
  facet_grid(~ sex)

# adjustment to exposure in year 2
pop_apr25 <- 
  pop_guill %>% 
  filter(year %in% 2024:2025,
         source == "pcbs_gaza",
         region == "Gaza Strip") %>% 
  select(year, sex, age, pop) %>% 
  spread(year, pop) %>% 
  mutate(pop = 3/12*`2024`+9/12*`2025`,
         year = "2024-2025") %>% 
  select(year, sex, age, pop)

# deaths from year 1 to be removed from population in year 2
# first, let's increase the conflict deaths age in 1, and then group them again
dx_cnf_y1_in_y2 <- 
  dxs_ungrp %>% 
  filter(year == "2023-2024") %>%
  mutate(age = age + 1) %>% 
  replace_na(list(dx = 0)) %>% 
  mutate(age = case_when(age == 0 ~ 0,
                         age %in% 1:4 ~ 1,
                         age >= 80 ~ 80,
                         TRUE ~ age - age%%5)) %>% 
  reframe(dx = sum(dx),
          .by = c(sex, age))

# We remove two things. First we remove the conflict deaths in 2023-2024 (war year 1). Second,
# we remove 35% of the conflict deaths that occurred between Oct 24 and Oct 25. 
pop_y2 <- 
  pop_apr25 %>% 
  left_join(dx_cnf_y1_in_y2) %>% 
  replace_na(list(dx = 0)) %>% 
  mutate(pop_new = pop - dx) %>% 
  rename(dx_y1 = dx) %>%
  left_join(dxs %>%
              filter(year == "2024-2025"), 
            by = c("year", "sex", "age")) %>%
  mutate(pop_new = pop_new - 0.35*dx) %>%  # 35% of the conflict deaths of the second year occurred before April 2025
  select(year, sex, age, pop, pop_new)

pop_y2 %>% 
  ggplot() + 
  geom_line(aes(x = age, y = pop)) + 
  geom_line(aes(x = age, y = pop_new), linetype = 2) +
  facet_grid(~ sex)

rbind(pop_y1, pop_y2) %>% 
  filter(sex != "t") %>%
  ggplot() + 
  geom_line(aes(x = age, y = pop)) + 
  geom_line(aes(x = age, y = pop_new), linetype = 2) +
  facet_grid(year ~ sex) 


# Testing the number of deaths removed for the second year
pop_y2 %>%
  reframe(pop = sum(pop),
          pop_new = sum(pop_new),
          .by = c(sex)) %>%
  mutate(deaths = pop - pop_new)

# Should be the same as all deaths from the first year plus 35% of the ones from the second year
dxs %>% 
  summarise(dts = sum(dx),
            .by = c(year, sex)) %>%
  spread(year, dts) %>%
  mutate(`2023-2024`+ 0.35*`2024-2025`)


pop_war_years <- rbind(pop_y1, pop_y2) %>% 
  filter(sex != "t") %>%
  select(-pop) %>%
  rename(pop = pop_new) %>%
  rbind(rbind(pop_y1, pop_y2) %>% 
          filter(sex != "t") %>%
          select(-pop) %>%
          rename(pop = pop_new) %>%
          reframe(pop = sum(pop), .by = c(year, age)) %>%
          mutate(sex = "t"))

# It seems the adjusted PCBS estimates estimated a sharper decline in population than out adjustment
pop_war_years %>%
  mutate(source = "Adjusted") %>%
  rbind(rbind(pop_apr24, pop_apr25) %>%
          mutate(source = "Original")) %>% 
  rbind(pop %>%
          filter(year >= 2023 & region == "Gaza Strip" & source == "pcbs") %>% 
          select(year, sex, age, pop, source) %>%
          mutate(year = ifelse(year == 2023, "2023-2024", "2024-2025"))) %>%
  ggplot() + 
  geom_line(aes(x = age, y = pop, col = source)) + 
  facet_grid(year ~ sex) 

# Get totals by source
pop_war_years %>%
  mutate(source = "Adjusted") %>%
  rbind(rbind(pop_apr24, pop_apr25) %>%
          mutate(source = "Original")) %>% 
  rbind(pop %>%
          filter(year >= 2023 & region == "Gaza Strip" & source == "pcbs") %>% 
          select(year, sex, age, pop, source) %>%
          mutate(year = ifelse(year == 2023, "2023-2024", "2024-2025"))) %>%
  reframe(pop = sum(pop), .by = c(year, source, sex)) %>%
  filter(sex == "t")

saveRDS(pop_war_years, file = "R/model/diff_reporting/samples/gaza/adjusted_exposures.rds")

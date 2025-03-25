
# ---------------------------------------------------------------------------- #
# Title:  Life expectancy estimates
#
# Code to create single file with life expectancy estimates
# ---------------------------------------------------------------------------- #

# Content:
#   0. Working directory, packages and functions
#   1. Estimate life expectancy
#   2. Save results
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())

## Counterfactual scenario of life expectancy with no conflict deaths for all regions
dt_ex <- 
  readRDS("data/ex0_noc.rds") %>% 
  filter(source=="lc_pcbs_2019") %>%
  mutate(scenario="Counterfactual with\n no conflict deaths", 
         lss = ex_noc - ex_cnf,
         sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))


### Gaza 2023
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2023/gaza_bu/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh_lifetable_t_le0.csv"))

oct_26_all23 <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts_lifetable_t_le0.csv"))

bts_all23 <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

###UN dist results 
un_geno_le_m_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_m_le0.csv"))
un_geno_le_f_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_f_le0.csv"))
un_geno_le_t_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_t_le0.csv"))

un_genocide_all23 <- rbind(un_geno_le_f_age0[,vars], un_geno_le_m_age0[,vars], un_geno_le_t_age0[,vars]) %>%
  mutate(pattern = "Genocide")

###UN dist results 
un_conflict_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_m_le0.csv"))
un_conflict_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_f_le0.csv"))
un_conflict_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_t_le0.csv"))

un_conflict_all23 <- rbind(un_conflict_le_f_age0[,vars], un_conflict_le_m_age0[,vars], un_conflict_le_t_age0[,vars]) %>%
  mutate(pattern = "Conflict")

###UN dist results 
un_earth_le_m_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_m_le0.csv"))
un_earth_le_f_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_f_le0.csv"))
un_earth_le_t_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_t_le0.csv"))

un_earth_all23 <- rbind(un_earth_le_f_age0[,vars], un_earth_le_m_age0[,vars], un_earth_le_t_age0[,vars]) %>%
  mutate(pattern = "Earthquake")

le_lss_all23_gaza <- rbind(oct_26_all23, bts_all23, un_genocide_all23, un_conflict_all23, un_earth_all23)

le_lss_all23_gaza$region="Gaza Strip"

### Gaza 
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2024/gaza_bu/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh_lifetable_t_le0.csv"))

oct_26_all24 <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts_lifetable_t_le0.csv"))

bts_all24 <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

###UN dist results 
un_geno_le_m_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_m_le0.csv"))
un_geno_le_f_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_f_le0.csv"))
un_geno_le_t_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_t_le0.csv"))

un_genocide_all24 <- rbind(un_geno_le_f_age0[,vars], un_geno_le_m_age0[,vars], un_geno_le_t_age0[,vars]) %>%
  mutate(pattern = "Genocide")

###UN dist results 
un_conflict_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_m_le0.csv"))
un_conflict_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_f_le0.csv"))
un_conflict_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_t_le0.csv"))

un_conflict_all24 <- rbind(un_conflict_le_f_age0[,vars], un_conflict_le_m_age0[,vars], un_conflict_le_t_age0[,vars]) %>%
  mutate(pattern = "Conflict")

###UN dist results 
un_earth_le_m_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_m_le0.csv"))
un_earth_le_f_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_f_le0.csv"))
un_earth_le_t_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_t_le0.csv"))

un_earth_all24 <- rbind(un_earth_le_f_age0[,vars], un_earth_le_m_age0[,vars], un_earth_le_t_age0[,vars]) %>%
  mutate(pattern = "Earthquake")

le_lss_all24_gaza <- rbind(oct_26_all24, bts_all24, un_genocide_all24, un_conflict_all24, un_earth_all24) 

le_lss_all24_gaza$region="Gaza Strip"


### Palestine 2023
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2023/palestine_bu/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh_lifetable_t_le0.csv"))

oct_26_all23 <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_t_le0.csv"))

bts_all23 <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

###UN dist results 
un_geno_le_m_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_m_le0.csv"))
un_geno_le_f_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_f_le0.csv"))
un_geno_le_t_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_t_le0.csv"))

un_genocide_all23 <- rbind(un_geno_le_f_age0[,vars], un_geno_le_m_age0[,vars], un_geno_le_t_age0[,vars])  %>%
  mutate(pattern = "Genocide")

###UN dist results 
un_conflict_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_m_le0.csv"))
un_conflict_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_f_le0.csv"))
un_conflict_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_t_le0.csv"))

un_conflict_all23 <- rbind(un_conflict_le_f_age0[,vars], un_conflict_le_m_age0[,vars], un_conflict_le_t_age0[,vars]) %>%
  mutate(pattern = "Conflict")

###UN dist results 
un_earth_le_m_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_m_le0.csv"))
un_earth_le_f_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_f_le0.csv"))
un_earth_le_t_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_t_le0.csv"))

un_earth_all23 <- rbind(un_earth_le_f_age0[,vars], un_earth_le_m_age0[,vars], un_earth_le_t_age0[,vars]) %>%
  mutate(pattern = "Earthquake")
 
le_lss_all23_pst <- rbind(oct_26_all23, bts_all23, un_genocide_all23, un_conflict_all23, un_earth_all23)

le_lss_all23_pst$region="Palestine"

### Gaza 
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2024/palestine_bu/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh_lifetable_t_le0.csv"))

oct_26_all24 <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts24_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts24_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts24_lifetable_t_le0.csv"))

bts_all24 <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

###UN dist results 
un_geno_le_m_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_m_le0.csv"))
un_geno_le_f_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_f_le0.csv"))
un_geno_le_t_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_t_le0.csv"))

un_genocide_all24 <- rbind(un_geno_le_f_age0[,vars], un_geno_le_m_age0[,vars], un_geno_le_t_age0[,vars]) %>%
  mutate(pattern = "Genocide")

###UN dist results 
un_conflict_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_m_le0.csv"))
un_conflict_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_f_le0.csv"))
un_conflict_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_t_le0.csv"))

un_conflict_all24 <- rbind(un_conflict_le_f_age0[,vars], un_conflict_le_m_age0[,vars], un_conflict_le_t_age0[,vars]) %>%
  mutate(pattern = "Conflict")

###UN dist results 
un_earth_le_m_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_m_le0.csv"))
un_earth_le_f_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_f_le0.csv"))
un_earth_le_t_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_t_le0.csv"))

un_earth_all24 <- rbind(un_earth_le_f_age0[,vars], un_earth_le_m_age0[,vars], un_earth_le_t_age0[,vars]) %>%
  mutate(pattern = "Earthquake")

le_lss_all24_pst <- rbind(oct_26_all24, bts_all24, un_genocide_all24, un_conflict_all24, un_earth_all24)

le_lss_all24_pst$region="Palestine"


## West Bank
wb_all23 <- read_rds("R/model/samples/pcbs_2019/2023/west_bank/lifetable_age0_wb_23_V2.rds") %>%
  mutate(pattern = NA)
wb_all23$region="West Bank"

wb_all24 <- read_rds("R/model/samples/pcbs_2019/2024/west_bank/lifetable_age0_wb_24_V2.rds") %>%
  mutate(pattern = NA)
wb_all24$region="West Bank"


ex <- rbind(le_lss_all23_gaza, le_lss_all24_gaza, le_lss_all23_pst, 
            le_lss_all24_pst, wb_all23, wb_all24) %>%
  group_by(year, sex, scenario, region, pattern) %>%
  mutate(ex_mean = mean(ex), 
         ex_ll = quantile(ex, 0.025), 
         ex_ul = quantile(ex, 0.975), 
         ex_sd = sd(ex), 
         lss_mean = mean(bmmr_lss), 
         lss_ll = quantile(bmmr_lss, 0.025), 
         lss_ul = quantile(bmmr_lss, 0.975), 
         lss_sd = sd(bmmr_lss)) %>%
  select(year, sex, scenario, pattern, region, ex_mean, ex_ll, ex_ul, ex_sd, 
         lss_mean, lss_ll, lss_ul, lss_sd) %>%
  unique() %>%
  # mutate(ex_noc = NA, ex_cnf = NA, lss = NA) %>%
  full_join(dt_ex %>% filter(year >= 2023) %>%
          select(ex_noc, ex_cnf, lss), by = c("sex", "region", "year")) %>%
  rbind(dt_ex %>% filter(year < 2023) %>%
          mutate(scenario = "Historic", ex_mean = ex_cnf, ex_ll = NA, ex_ul = NA, ex_sd = NA,
                 lss_mean = lss, lss_ll = NA, lss_ul = NA, lss_sd = NA)) %>%
  select(-ex_cnf, -lss) %>%
  mutate(lss_mean = if_else(lss_mean < 0, 0, lss_mean),
         lss_ll = if_else(lss_ll < 0, 0, lss_ll))

# ex %>%
#   filter(year >= 2023) %>%
#   ggplot() +
#   geom_point(aes(x = year, y = ex_mean, colour = region)) + 
#   facet_grid(sex ~ scenario)

write_csv(ex, "R/model/samples/pcbs_2019/LE_all.csv")

#### For sensitivity analysis of prior

### Gaza 2023
results_dir <- paste0(getwd(),"/R/sensitivity_check/samples/gaza_bu/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_t_le0.csv"))

oct_26_all23 <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_t_le0.csv"))

oct_26_all24 <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

oct_26_all_gaza <- rbind(oct_26_all23, oct_26_all24)

oct_26_all_gaza$region="Gaza Strip"

### Palestine 2023
results_dir <- paste0(getwd(),"/R/sensitivity_check/samples/palestine_bu/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_t_le0.csv"))

oct_26_all23 <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_t_le0.csv"))

oct_26_all24 <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

oct_26_all_pst <- rbind(oct_26_all23, oct_26_all24)

oct_26_all_pst$region="Palestine"

## West Bank
wb_all23 <- read_rds("R/model/samples/pcbs_2019/2023/west_bank/lifetable_age0_wb_23_V2.rds") %>%
  mutate(pattern = NA)
wb_all23$region="West Bank"

wb_all24 <- read_rds("R/model/samples/pcbs_2019/2024/west_bank/lifetable_age0_wb_24_V2.rds") %>%
  mutate(pattern = NA)
wb_all24$region="West Bank"

ex_prior <- rbind(oct_26_all_gaza, oct_26_all_pst, 
                  wb_all23, wb_all24) %>%
  group_by(year, sex, scenario, region) %>%
  mutate(ex_mean = mean(ex), 
         ex_ll = quantile(ex, 0.025), 
         ex_ul = quantile(ex, 0.975), 
         ex_sd = sd(ex), 
         lss_mean = mean(bmmr_lss), 
         lss_ll = quantile(bmmr_lss, 0.025), 
         lss_ul = quantile(bmmr_lss, 0.975), 
         lss_sd = sd(bmmr_lss)) %>%
  select(year, sex, scenario, region, ex_mean, ex_ll, ex_ul, ex_sd, 
         lss_mean, lss_ll, lss_ul, lss_sd) %>%
  unique() %>%
  # mutate(ex_noc = NA, ex_cnf = NA, lss = NA) %>%
  full_join(dt_ex %>% filter(year >= 2023) %>%
              select(ex_noc, ex_cnf, lss), by = c("sex", "region", "year")) %>%
  rbind(dt_ex %>% filter(year < 2023) %>%
          mutate(scenario = "Historic", ex_mean = ex_cnf, ex_ll = NA, ex_ul = NA, ex_sd = NA,
                 lss_mean = lss, lss_ll = NA, lss_ul = NA, lss_sd = NA)) %>%
  select(-ex_cnf, -lss) %>%
  mutate(lss_mean = if_else(lss_mean < 0, 0, lss_mean),
         lss_ll = if_else(lss_ll < 0, 0, lss_ll))

library(knitr)
library(kableExtra)

summary_ex <- ex_prior %>%
  filter(year <= 2019) %>%
  group_by(region, sex) %>%
  summarise(ex_mean = as.character(round(mean(ex_mean),1))) %>%
  left_join(ex_prior %>%
              filter(year >= 2023 & scenario != "Historic") %>%
              mutate(ex_mean2 = as.character(round(ex_mean,1))) %>%
              select(region, sex, year, ex_mean2) %>%
              spread("year", "ex_mean2") %>%
              ungroup() %>%
              select(region, sex, `2023`, `2024`),
            by = c("region", "sex")) %>%
  rbind(ex_prior %>%
          filter(year <= 2019) %>%
          group_by(region, sex) %>%
          summarise(ex_ll = round(quantile(ex_mean, 0.025),1), 
                    ex_ul = round(quantile(ex_mean, 0.975),1)) %>%
          mutate(ex_lim = paste0("(",round(ex_ll,1),"-",round(ex_ul,1),")")) %>%
          select(region, sex, ex_lim) %>%
          rename(ex_mean = ex_lim) %>%
          left_join(ex_prior %>%
                      filter(year >= 2023 & scenario != "Historic") %>%
                      mutate(ex_lim = paste0("(",round(ex_ll,1),"-",round(ex_ul,1),")")) %>%
                      select(region, sex, year, ex_lim) %>%
                      spread("year", "ex_lim") %>%
                      ungroup() %>%
                      select(region, sex, `2023`, `2024`),
                    by = c("region", "sex"))) %>%
  arrange(region, sex)


kable(summary_ex, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))

summary_lss <- ex_prior %>%
  filter(year <= 2019) %>%
  group_by(region, sex) %>%
  summarise(lss_mean = as.character(round(mean(lss_mean),1))) %>%
  left_join(ex_prior %>%
              filter(year >= 2023 & scenario != "Historic") %>%
              mutate(lss_mean2 = as.character(round(lss_mean,1))) %>%
              select(region, sex, year,lss_mean2) %>%
              spread("year", "lss_mean2") %>%
              ungroup() %>%
              select(region, sex, `2023`, `2024`),
            by = c("region", "sex")) %>%
  rbind(ex_prior %>%
          filter(year <= 2019) %>%
          group_by(region, sex) %>%
          summarise(lss_ll = round(quantile(lss_mean, 0.025),1), 
                    lss_ul = round(quantile(lss_mean, 0.975),1)) %>%
          mutate(lss_lim = paste0("(",round(lss_ll,1),"-",round(lss_ul,1),")")) %>%
          select(region, sex,lss_lim) %>%
          rename(lss_mean =lss_lim) %>%
          left_join(ex_prior %>%
                      filter(year >= 2023 & scenario != "Historic") %>%
                      mutate(lss_lim = paste0("(",round(lss_ll,1),"-",round(lss_ul,1),")")) %>%
                      select(region, sex, year,lss_lim) %>%
                      spread("year", "lss_lim") %>%
                      ungroup() %>%
                      select(region, sex, `2023`, `2024`),
                    by = c("region", "sex"))) %>%
  arrange(region, sex)

kable(summary_lss, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))

# 
# ## PCBS_2019 - 2023
# results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2023/")
# vars <- c("ex", "bmmr_lss","year", "sex", "scenario")
# 
# ## Palestine results 
# nat_le_m_age0 <- read.csv(paste0(results_dir, "palestine_bu/moh_lifetable_m_le0.csv"))
# nat_le_f_age0 <- read.csv(paste0(results_dir, "palestine_bu/moh_lifetable_f_le0.csv"))
# nat_le_t_age0 <- read.csv(paste0(results_dir, "palestine_bu/moh_lifetable_t_le0.csv"))
# 
# nat_all <- rbind(nat_le_f_age0[,vars], nat_le_m_age0[,vars], nat_le_t_age0[,vars])
# nat_all$region="Palestine"
# 
# ## Gaza results
# gaza_le_m_age0 <- read.csv(paste0(results_dir, "gaza_bu/moh_lifetable_m_le0.csv"))
# gaza_le_f_age0 <- read.csv(paste0(results_dir, "gaza_bu/moh_lifetable_f_le0.csv"))
# gaza_le_t_age0 <- read.csv(paste0(results_dir, "gaza_bu/moh_lifetable_t_le0.csv"))
# 
# gaza_all <- rbind(gaza_le_f_age0[,vars], gaza_le_m_age0[,vars], gaza_le_t_age0[,vars])
# gaza_all$region="Gaza Strip"
# 
# ## West Bank results
# wb_all <- read_rds(paste0(results_dir,"west_bank/lifetable_age0_wb_23_V2.rds"))
# 
# # wb_all <- wb_le_age0[wb_le_age0$year == 2023, ]
# 
# ## Combine all regions
# le_lss_all_23 <- rbind(nat_all, gaza_all, wb_all) %>%
#   mutate(source = "lc_pcbs_2019")
# 
# ## PCBS_2019 - 2024
# results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2024/")
# 
# ## Palestine
# nat_le_m_24 <- read.csv(paste0(results_dir, "palestine_bu/moh_lifetable_m_le0.csv"))
# nat_le_f_24 <- read.csv(paste0(results_dir, "palestine_bu/moh_lifetable_f_le0.csv"))
# nat_le_t_24 <- read.csv(paste0(results_dir, "palestine_bu/moh_lifetable_t_le0.csv"))
# 
# nat_24_all  <- rbind(nat_le_m_24[,vars], nat_le_f_24[,vars],nat_le_t_24[,vars])
# nat_24_all$region="Palestine"
# 
# ## Gaza
# gaza_le_m_24 <- read.csv(paste0(results_dir, "gaza_bu/moh_lifetable_m_le0.csv"))
# gaza_le_f_24 <- read.csv(paste0(results_dir, "gaza_bu/moh_lifetable_f_le0.csv"))
# gaza_le_t_24 <- read.csv(paste0(results_dir, "gaza_bu/moh_lifetable_t_le0.csv"))
# 
# gaza_24_all <- rbind(gaza_le_f_24[,vars], gaza_le_m_24[,vars], gaza_le_t_24[,vars])
# gaza_24_all$region="Gaza Strip"
# 
# ## West Bank
# wb_24_all <- read_rds(paste0(results_dir,"west_bank/lifetable_age0_wb_24_V2.rds"))
# 
# # wb_24_all <- wb_le_age0[wb_le_age0$year == 2024, ]
# 
# ## Combine all regions
# le_lss_all_24 <- rbind(nat_24_all, gaza_24_all, wb_24_all) %>%
#   mutate(source = "lc_pcbs_2019")
# 
# ## Guillot
# results_dir <- paste0(getwd(),"/R/sensitivity_check/guillot_samples/")
# 
# ## Gaza
# gaza_le_m_gll <- read.csv(paste0(results_dir, "moh_2024_lifetable_m_le0.csv"))
# gaza_le_f_gll <- read.csv(paste0(results_dir, "moh_2024_lifetable_f_le0.csv"))
# gaza_le_t_gll <- read.csv(paste0(results_dir, "moh_2024_lifetable_t_le0.csv"))
# 
# gaza_24_all_gll <- rbind(gaza_le_f_gll[,vars], gaza_le_m_gll[,vars], gaza_le_t_gll[,vars])
# gaza_24_all_gll$region="Gaza Strip"
# gaza_24_all_gll$case = "Guillot 1"
# 
# ## Guillot 2
# results_dir <- paste0(getwd(),"/R/sensitivity_check/guillot_samples/v2/")
# 
# gaza_le_m_gll2 <- read.csv(paste0(results_dir, "moh_2024_lifetable_m_le0.csv"))
# gaza_le_f_gll2 <- read.csv(paste0(results_dir, "moh_2024_lifetable_f_le0.csv"))
# gaza_le_t_gll2 <- read.csv(paste0(results_dir, "moh_2024_lifetable_t_le0.csv"))
# 
# gaza_24_all_gll2 <- rbind(gaza_le_f_gll2[,vars], gaza_le_m_gll2[,vars], gaza_le_t_gll2[,vars])
# gaza_24_all_gll2$region="Gaza Strip"
# gaza_24_all_gll2$case = "Guillot 2"


# ---------------------------------------------------------------------------- #
#     3. Save life expectancy estimates
# ---------------------------------------------------------------------------- #
ex <- rbind(le_lss_all23, le_lss_all24) %>%
  group_by(year, sex, scenario, region) %>%
  mutate(ex_mean = mean(ex), 
         ex_ll = quantile(ex, 0.025), 
         ex_ul = quantile(ex, 0.975), 
         ex_sd = sd(ex), 
         lss_mean = mean(bmmr_lss), 
         lss_ll = quantile(bmmr_lss, 0.025), 
         lss_ul = quantile(bmmr_lss, 0.975), 
         lss_sd = sd(bmmr_lss)) %>%
  select(year, sex, scenario, region, ex_mean, ex_ll, ex_ul, ex_sd, 
         lss_mean, lss_ll, lss_ul, lss_sd) %>%
  unique() %>%
  mutate(ex_noc = NA, ex_cnf = NA, lss = NA) %>%
  rbind(dt_ex %>%
          mutate(scenario = "Historic", ex_mean = NA, ex_ll = NA, ex_ul = NA, ex_sd = NA, 
                 lss_mean = NA, lss_ll = NA, lss_ul = NA, lss_sd = NA)) 

# write_csv(ex, "data_inter/life_expectanct_lc_pcbs_2019.csv")


library(knitr)
library(kableExtra)

summary_ex <- ex %>%
  filter(year <= 2019) %>%
  group_by(region, sex) %>%
  summarise(ex_mean = as.character(round(mean(ex_cnf),1))) %>%
  left_join(ex %>%
              filter(year >= 2023 & scenario != "Historic") %>%
              mutate(ex_mean2 = as.character(round(ex_mean,1))) %>%
              select(region, sex, year, ex_mean2) %>%
              spread("year", "ex_mean2") %>%
              ungroup() %>%
              select(region, sex, `2023`, `2024`),
            by = c("region", "sex")) %>%
  rbind(ex %>%
          filter(year <= 2019) %>%
          group_by(region, sex) %>%
          summarise(ex_ll = round(quantile(ex_cnf, 0.025),1), 
                    ex_ul = round(quantile(ex_cnf, 0.975),1)) %>%
          mutate(ex_lim = paste0("(",round(ex_ll,1),"-",round(ex_ul,1),")")) %>%
          select(region, sex, ex_lim) %>%
          rename(ex_mean = ex_lim) %>%
          left_join(ex %>%
                      filter(year >= 2023 & scenario != "Historic") %>%
                      mutate(ex_lim = paste0("(",round(ex_ll,1),"-",round(ex_ul,1),")")) %>%
                      select(region, sex, year, ex_lim) %>%
                      spread("year", "ex_lim") %>%
                      ungroup() %>%
                      select(region, sex, `2023`, `2024`),
                    by = c("region", "sex"))) %>%
  arrange(region, sex)


kable(summary_ex, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))


summary_lss <- ex %>%
  filter(year <= 2019) %>%
  group_by(region, sex) %>%
  summarise(lss_mean = as.character(round(mean(lss),1))) %>%
  left_join(ex %>%
              filter(year >= 2023 & scenario != "Historic") %>%
              mutate(lss_mean2 = as.character(round(lss_mean,1))) %>%
              select(region, sex, year,lss_mean2) %>%
              spread("year", "lss_mean2") %>%
              ungroup() %>%
              select(region, sex, `2023`, `2024`),
            by = c("region", "sex")) %>%
  rbind(ex %>%
          filter(year <= 2019) %>%
          group_by(region, sex) %>%
          summarise(lss_ll = round(quantile(lss, 0.025),1), 
                   lss_ul = round(quantile(lss, 0.975),1)) %>%
          mutate(lss_lim = paste0("(",round(lss_ll,1),"-",round(lss_ul,1),")")) %>%
          select(region, sex,lss_lim) %>%
          rename(lss_mean =lss_lim) %>%
          left_join(ex %>%
                      filter(year >= 2023 & scenario != "Historic") %>%
                      mutate(lss_lim = paste0("(",round(lss_ll,1),"-",round(lss_ul,1),")")) %>%
                      select(region, sex, year,lss_lim) %>%
                      spread("year", "lss_lim") %>%
                      ungroup() %>%
                      select(region, sex, `2023`, `2024`),
                    by = c("region", "sex"))) %>%
  arrange(region, sex)

kable(summary_lss, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))

### Guillot
ex_gll <- rbind(gaza_24_all_gll, gaza_24_all_gll2) %>%
  group_by(sex, case) %>%
  mutate(ex_mean = mean(ex), 
         ex_ll = quantile(ex, 0.025), 
         ex_ul = quantile(ex, 0.975), 
         ex_sd = sd(ex), 
         lss_mean = mean(bmmr_lss), 
         lss_ll = quantile(bmmr_lss, 0.025), 
         lss_ul = quantile(bmmr_lss, 0.975), 
         lss_sd = sd(bmmr_lss)) %>%
  select(sex, ex_mean, ex_ll, ex_ul, ex_sd, 
         lss_mean, lss_ll, lss_ul, lss_sd)%>%
  unique()


# ---------------------------------------------------------------------------- #
# Title:  Life expectancy estimates
#
# Code to create single file with life expectancy estimates and output tables
# with estimates for the manuscript
# ---------------------------------------------------------------------------- #
# Content:
#   0. Working directory, packages and functions
#   1. Results of main analysis
#   2. Results of sensitivity analysis of reporting rate prior
#   3. Results of comparison with other studies
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
library(knitr)
library(kableExtra)

# ---------------------------------------------------------------------------- #
# 1. Results of main analysis ------------------------------------------------
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#  Read data
# ---------------------------------------------------------------------------- #

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
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/gaza/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## MoH results (updated with age distribution uncertainty)
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_t_le0.csv"))

moh_all23 <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_t_le0.csv"))

bts_all23 <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## UN dist genocide results 
un_geno_le_m_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_m_le0.csv"))
un_geno_le_f_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_f_le0.csv"))
un_geno_le_t_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_t_le0.csv"))

un_genocide_all23 <- rbind(un_geno_le_f_age0[,vars], un_geno_le_m_age0[,vars], un_geno_le_t_age0[,vars]) %>%
  mutate(pattern = "Genocide")

## UN dist conflict results 
un_conflict_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_m_le0.csv"))
un_conflict_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_f_le0.csv"))
un_conflict_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_t_le0.csv"))

un_conflict_all23 <- rbind(un_conflict_le_f_age0[,vars], un_conflict_le_m_age0[,vars], un_conflict_le_t_age0[,vars]) %>%
  mutate(pattern = "Conflict")

## UN dist earthquake results 
un_earth_le_m_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_m_le0.csv"))
un_earth_le_f_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_f_le0.csv"))
un_earth_le_t_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_t_le0.csv"))

un_earth_all23 <- rbind(un_earth_le_f_age0[,vars], un_earth_le_m_age0[,vars], un_earth_le_t_age0[,vars]) %>%
  mutate(pattern = "Earthquake")

le_lss_all23_gaza <- rbind(moh_all23, bts_all23, un_genocide_all23, un_conflict_all23, un_earth_all23)

le_lss_all23_gaza$region="Gaza Strip"

#### Gaza 2024
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/gaza/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## MoH results (updated with age distribution uncertainty)
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_t_le0.csv"))

moh_all24 <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts24_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts24_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts24_lifetable_t_le0.csv"))

bts_all24 <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## UN dist genocide results 
un_geno_le_m_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_m_le0.csv"))
un_geno_le_f_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_f_le0.csv"))
un_geno_le_t_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_t_le0.csv"))

un_genocide_all24 <- rbind(un_geno_le_f_age0[,vars], un_geno_le_m_age0[,vars], un_geno_le_t_age0[,vars]) %>%
  mutate(pattern = "Genocide")

## UN dist conflict results 
un_conflict_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_m_le0.csv"))
un_conflict_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_f_le0.csv"))
un_conflict_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_t_le0.csv"))

un_conflict_all24 <- rbind(un_conflict_le_f_age0[,vars], un_conflict_le_m_age0[,vars], un_conflict_le_t_age0[,vars]) %>%
  mutate(pattern = "Conflict")

## UN dist earhquake results 
un_earth_le_m_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_m_le0.csv"))
un_earth_le_f_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_f_le0.csv"))
un_earth_le_t_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_t_le0.csv"))

un_earth_all24 <- rbind(un_earth_le_f_age0[,vars], un_earth_le_m_age0[,vars], un_earth_le_t_age0[,vars]) %>%
  mutate(pattern = "Earthquake")

le_lss_all24_gaza <- rbind(moh_all24, bts_all24, un_genocide_all24, un_conflict_all24, un_earth_all24) 

le_lss_all24_gaza$region="Gaza Strip"


#### Palestine 2023
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/palestine/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## MoHresults (updated with age distribution uncertainty)
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_t_le0.csv"))

moh_all23 <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_t_le0.csv"))

bts_all23 <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## UN dist genocide results 
un_geno_le_m_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_m_le0.csv"))
un_geno_le_f_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_f_le0.csv"))
un_geno_le_t_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_t_le0.csv"))

un_genocide_all23 <- rbind(un_geno_le_f_age0[,vars], un_geno_le_m_age0[,vars], un_geno_le_t_age0[,vars])  %>%
  mutate(pattern = "Genocide")

## UN dist conflict results 
un_conflict_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_m_le0.csv"))
un_conflict_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_f_le0.csv"))
un_conflict_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict23_lifetable_t_le0.csv"))

un_conflict_all23 <- rbind(un_conflict_le_f_age0[,vars], un_conflict_le_m_age0[,vars], un_conflict_le_t_age0[,vars]) %>%
  mutate(pattern = "Conflict")

## UN dist earthquake results 
un_earth_le_m_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_m_le0.csv"))
un_earth_le_f_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_f_le0.csv"))
un_earth_le_t_age0 <- read.csv(paste0(results_dir, "un_earth23_lifetable_t_le0.csv"))

un_earth_all23 <- rbind(un_earth_le_f_age0[,vars], un_earth_le_m_age0[,vars], un_earth_le_t_age0[,vars]) %>%
  mutate(pattern = "Earthquake")
 
le_lss_all23_pst <- rbind(moh_all23)
le_lss_all23_pst <- rbind(moh_all23, bts_all23, un_genocide_all23, un_conflict_all23, un_earth_all23)

le_lss_all23_pst$region="Palestine"

#### Palestine 2024 
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/palestine/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## MoH results (updated with age distribution uncertainty)
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_t_le0.csv"))

moh_all24 <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts24_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts24_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts24_lifetable_t_le0.csv"))

bts_all24 <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## UN dist genocide results 
un_geno_le_m_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_m_le0.csv"))
un_geno_le_f_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_f_le0.csv"))
un_geno_le_t_age0 <- read.csv(paste0(results_dir, "un_geno24_lifetable_t_le0.csv"))

un_genocide_all24 <- rbind(un_geno_le_f_age0[,vars], un_geno_le_m_age0[,vars], un_geno_le_t_age0[,vars]) %>%
  mutate(pattern = "Genocide")

## UN dist conflict results 
un_conflict_le_m_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_m_le0.csv"))
un_conflict_le_f_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_f_le0.csv"))
un_conflict_le_t_age0 <- read.csv(paste0(results_dir, "un_conflict24_lifetable_t_le0.csv"))

un_conflict_all24 <- rbind(un_conflict_le_f_age0[,vars], un_conflict_le_m_age0[,vars], un_conflict_le_t_age0[,vars]) %>%
  mutate(pattern = "Conflict")

## UN dist earthquake results 
un_earth_le_m_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_m_le0.csv"))
un_earth_le_f_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_f_le0.csv"))
un_earth_le_t_age0 <- read.csv(paste0(results_dir, "un_earth24_lifetable_t_le0.csv"))

un_earth_all24 <- rbind(un_earth_le_f_age0[,vars], un_earth_le_m_age0[,vars], un_earth_le_t_age0[,vars]) %>%
  mutate(pattern = "Earthquake")

le_lss_all24_pst <- rbind(moh_all24)
le_lss_all24_pst <- rbind(moh_all24, bts_all24, un_genocide_all24, un_conflict_all24, un_earth_all24)
le_lss_all24_pst$region="Palestine"


##### West Bank

## 2023
wb_all23 <- read_rds("R/model/samples/pcbs_2019/2023/west_bank/lifetable_age0_wb_23_V2.rds") %>%
  mutate(pattern = NA, scenario = "BTselem")
wb_all23$region="West Bank"

## 2024
wb_all24 <- read_rds("R/model/samples/pcbs_2019/2024/west_bank/lifetable_age0_wb_24_V2.rds") %>%
  mutate(pattern = NA, scenario = "BTselem")
wb_all24$region="West Bank"

#### Info from 0ct 23-oct 24

## Gaza 
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

gaza_le_m_age0 <- read.csv(paste0(results_dir, "gaza/moh_23_24_lifetable_m_le0.csv"))
gaza_le_f_age0 <- read.csv(paste0(results_dir, "gaza/moh_23_24_lifetable_f_le0.csv"))
gaza_le_t_age0 <- read.csv(paste0(results_dir, "gaza/moh_23_24_lifetable_t_le0.csv"))

gaza_all23_24 <- rbind(gaza_le_f_age0[,vars], gaza_le_m_age0[,vars], gaza_le_t_age0[,vars]) %>%
  mutate(pattern = NA, year = 2023.5)
gaza_all23_24$region="Gaza Strip"

## Palesine
pst_le_m_age0 <- read.csv(paste0(results_dir, "palestine/moh_23_24_lifetable_m_le0.csv"))
pst_le_f_age0 <- read.csv(paste0(results_dir, "palestine/moh_23_24_lifetable_f_le0.csv"))
pst_le_t_age0 <- read.csv(paste0(results_dir, "palestine/moh_23_24_lifetable_t_le0.csv"))

pst_all23_24 <- rbind(pst_le_f_age0[,vars], pst_le_m_age0[,vars], pst_le_t_age0[,vars]) %>%
  mutate(pattern = NA, year = 2023.5)
pst_all23_24$region="Palestine"

## West Bank
wb_all23_24 <- read_rds("R/sensitivity_check/samples_23_24/west_bank/lifetable_age0_wb_23_24.rds") %>%
  mutate(pattern = NA)
wb_all23_24$region="West Bank"
wb_all23_24$scenario="BTselem"


# ------------------------------------------------------------------------- #
# Summarize all estimates -------------------------------------------------
# ------------------------------------------------------------------------- #

#### Paste and summarize all ex estimates
ex <- rbind(le_lss_all23_gaza, le_lss_all24_gaza, le_lss_all23_pst, 
            le_lss_all24_pst, wb_all23, wb_all24, 
            gaza_all23_24, pst_all23_24, wb_all23_24) %>%
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

# Plot
ex %>%
  filter(year >= 2023) %>%
  ggplot() +
  geom_point(aes(x = year, y = ex_mean, colour = region)) +
  facet_grid(sex ~ scenario)

# ---------------------------------------------------------------------------- #
#     Save results
# ---------------------------------------------------------------------------- #

write_csv(ex, "R/model/diff_reporting/samples/LE_all_w_23_24.csv")

# ---------------------------------------------------------------------------- #
# Create tables for manuscript --------------------------------------------
# ---------------------------------------------------------------------------- #

# ex <- read.csv("R/model/diff_reporting/samples/LE_all_w_23_24.csv")

# Life expectancy
summary_ex <- ex %>%
  filter(year <= 2019) %>%
  group_by(region, sex) %>%
  summarise(ex_mean = as.character(round(mean(ex_mean),1))) %>%
  left_join(ex %>%
              filter(year >= 2023 & scenario %in% c("GMoH report", "BTselem")) %>%
              mutate(ex_mean2 = as.character(round(ex_mean,1))) %>%
              select(region, sex, year, ex_mean2) %>%
              spread("year", "ex_mean2") %>%
              ungroup() %>%
              select(region, sex, `2023`, `2024`, `2023.5`),
            by = c("region", "sex")) %>%
  rbind(ex %>%
          filter(year <= 2019) %>%
          group_by(region, sex) %>%
          # summarise(ex_ll = round(quantile(ex_mean, 0.025),1), 
          #           ex_ul = round(quantile(ex_mean, 0.975),1)) %>%
          summarise(ex_ll = round(min(ex_mean),1), 
                    ex_ul = round(max(ex_mean),1)) %>%
          mutate(ex_lim = paste0("[",round(ex_ll,1),"-",round(ex_ul,1),"]")) %>%
          select(region, sex, ex_lim) %>%
          rename(ex_mean = ex_lim) %>%
          left_join(ex %>%
                      filter(year >= 2023 & scenario %in% c("GMoH report", "BTselem")) %>%
                      mutate(ex_lim = paste0("(",round(ex_ll,1),"-",round(ex_ul,1),")")) %>%
                      select(region, sex, year, ex_lim) %>%
                      spread("year", "ex_lim") %>%
                      ungroup() %>%
                      select(region, sex, `2023`, `2024`, `2023.5`),
                    by = c("region", "sex"))) %>%
  arrange(region, sex)

# Print
kable(summary_ex, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))

# Life expectancy loss
summary_lss <- ex %>%
  filter(year <= 2019) %>%
  group_by(region, sex) %>%
  summarise(lss_mean = as.character(round(mean(lss_mean),1))) %>%
  left_join(ex %>%
              filter(year >= 2023 & scenario %in% c("GMoH report", "BTselem")) %>%
              mutate(lss_mean2 = as.character(round(lss_mean,1))) %>%
              select(region, sex, year,lss_mean2) %>%
              spread("year", "lss_mean2") %>%
              ungroup() %>%
              select(region, sex, `2023`, `2024`, `2023.5`),
            by = c("region", "sex")) %>%
  rbind(ex %>%
          filter(year <= 2019) %>%
          group_by(region, sex) %>%
          # summarise(lss_ll = round(quantile(lss_mean, 0.025),1), 
          #           lss_ul = round(quantile(lss_mean, 0.975),1)) %>%
          summarise(lss_ll = round(min(lss_mean),1), 
                    lss_ul = round(max(lss_mean),1)) %>%
          mutate(lss_lim = paste0("[",round(lss_ll,1),"-",round(lss_ul,1),"]")) %>%
          select(region, sex,lss_lim) %>%
          rename(lss_mean =lss_lim) %>%
          left_join(ex %>%
                      filter(year >= 2023 & scenario %in% c("GMoH report", "BTselem")) %>%
                      mutate(lss_lim = paste0("(",round(lss_ll,1),"-",round(lss_ul,1),")")) %>%
                      select(region, sex, year,lss_lim) %>%
                      spread("year", "lss_lim") %>%
                      ungroup() %>%
                      select(region, sex, `2023`, `2024`, `2023.5`),
                    by = c("region", "sex"))) %>%
  arrange(region, sex)

# Print
kable(summary_lss, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))


# ---------------------------------------------------------------------------- #
# 2. Results of sensitivity analysis of prior of reporting rate --------------
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#     Read data
# ---------------------------------------------------------------------------- #

#### Gaza
results_dir <- paste0(getwd(),"/R/sensitivity_check/samples/gaza_bu/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## 2023
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_t_le0.csv"))

moh_all23 <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## 2024
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_t_le0.csv"))

moh_all24 <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

moh_all_gaza <- rbind(moh_all23, moh_all24)

moh_all_gaza$region="Gaza Strip"

#### Palestine
results_dir <- paste0(getwd(),"/R/sensitivity_check/samples/palestine_bu/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## 2023
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_t_le0.csv"))

moh_all23 <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

## 2024
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh24_lifetable_t_le0.csv"))

moh_all24 <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars]) %>%
  mutate(pattern = NA)

moh_all_pst <- rbind(moh_all23, moh_all24)

moh_all_pst$region="Palestine"

#### West Bank

## 2023
wb_all23 <- read_rds("R/model/samples/pcbs_2019/2023/west_bank/lifetable_age0_wb_23_V2.rds") %>%
  mutate(pattern = NA)
wb_all23$region="West Bank"

## 2024
wb_all24 <- read_rds("R/model/samples/pcbs_2019/2024/west_bank/lifetable_age0_wb_24_V2.rds") %>%
  mutate(pattern = NA)
wb_all24$region="West Bank"

#### Paste and summarize all ex estimates
ex_prior <- rbind(moh_all_gaza, moh_all_pst, 
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

# ---------------------------------------------------------------------------- #
#     Create table for manuscript
# ---------------------------------------------------------------------------- #

# Life expectancy
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

# Print
kable(summary_ex, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))

# Life expectancy loss
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

# Print
kable(summary_lss, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))


# ---------------------------------------------------------------------------- #
# 3. Results of comparison to other studies ----------------------------------
# ---------------------------------------------------------------------------- #

## Guillot et al.
results_dir <- paste0(getwd(),"/R/sensitivity_check/guillot_samples/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## Gaza
gaza_le_m_gll <- read.csv(paste0(results_dir, "moh_2024_lifetable_m_le0.csv"))
gaza_le_f_gll <- read.csv(paste0(results_dir, "moh_2024_lifetable_f_le0.csv"))
gaza_le_t_gll <- read.csv(paste0(results_dir, "moh_2024_lifetable_t_le0.csv"))

gaza_all_gll <- rbind(gaza_le_f_gll[,vars], gaza_le_m_gll[,vars], gaza_le_t_gll[,vars])
gaza_all_gll$region = "Gaza Strip"

## Summarize estimates
## Life expectancy
ex_gll <- gaza_all_gll %>%
  group_by(sex) %>%
  mutate(ex_mean = mean(ex), 
         ex_ll = quantile(ex, 0.025), 
         ex_ul = quantile(ex, 0.975), 
         ex_sd = sd(ex)) %>%
  select(sex, ex_mean, ex_ll, ex_ul, ex_sd)%>%
  unique()

# Print
kable(ex_gll, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))

## Life expectancy loss
lss_gll <- gaza_all_gll %>%
  group_by(sex) %>%
  mutate(lss_mean = mean(bmmr_lss), 
         lss_ll = quantile(bmmr_lss, 0.025), 
         lss_ul = quantile(bmmr_lss, 0.975), 
         lss_sd = sd(bmmr_lss)) %>%
  select(sex, lss_mean, lss_ll, lss_ul, lss_sd)%>%
  unique()

# Print
kable(ss_gll, format = "latex", booktabs = TRUE, digits = 2) %>%
  kable_styling(latex_options = c("hold_position"))
# ---------------------------------------------------------------------------- #
# Title:  Plot of life expectancy by region (Figure 3)
#
# Code to create plot of life expectancy estimates from 2012-2024 by region
# (figure 3 in the main text)
# ---------------------------------------------------------------------------- #
# Content:
#   0. Working directory, packages and functions
#   1. Load data
#   2. Plot
#   3. Save plot
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
library(tidyverse)
library(ggdist)
library(bayesplot)

# ---------------------------------------------------------------------------- #
#     1. Load data
# ---------------------------------------------------------------------------- #

## Life expectancy estimates 2012-2019
dt_ex <- 
  readRDS("data/ex0_noc.rds") %>% 
  filter(source=="lc_pcbs_2019") %>%
  mutate(scenario="Counterfactual with\n no conflict deaths", 
         lss = ex_noc - ex_cnf,
         sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))

## Results directory for Gaza and Palestine estimates
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## Life expectancy estimates Palestine 2023 
nat_le_m_age0 <- read.csv(paste0(results_dir, "palestine/moh23_lifetable_m_le0.csv"))
nat_le_f_age0 <- read.csv(paste0(results_dir, "palestine/moh23_lifetable_f_le0.csv"))
nat_le_t_age0 <- read.csv(paste0(results_dir, "palestine/moh23_lifetable_t_le0.csv"))

nat_all <- rbind(nat_le_f_age0[,vars], nat_le_m_age0[,vars], nat_le_t_age0[,vars])
nat_all$region="Palestine"

## Life expectancy estimates Gaza 2023 
gaza_le_m_age0 <- read.csv(paste0(results_dir, "gaza/moh23_lifetable_m_le0.csv"))
gaza_le_f_age0 <- read.csv(paste0(results_dir, "gaza/moh23_lifetable_f_le0.csv"))
gaza_le_t_age0 <- read.csv(paste0(results_dir, "gaza/moh23_lifetable_t_le0.csv"))

gaza_all <- rbind(gaza_le_f_age0[,vars], gaza_le_m_age0[,vars], gaza_le_t_age0[,vars])
gaza_all$region="Gaza Strip"

## Life expectancy estimates West Bank 2023 
wb_all <- read_rds("R/model/samples/pcbs_2019/2023/west_bank/lifetable_age0_wb_23_v2.rds")
wb_all$region="West Bank"

# All estimates for 2023
le_lss_all <- rbind(nat_all, gaza_all, wb_all) 


## Life expectancy estimates Palestine 2024
nat_le_m_24 <- read.csv(paste0(results_dir, "palestine/moh24_lifetable_m_le0.csv"))
nat_le_f_24 <- read.csv(paste0(results_dir, "palestine/moh24_lifetable_f_le0.csv"))
nat_le_t_24 <- read.csv(paste0(results_dir, "palestine/moh24_lifetable_t_le0.csv"))

nat_24_all  <- rbind(nat_le_m_24[,vars], nat_le_f_24[,vars],nat_le_t_24[,vars])
nat_24_all$region="Palestine"

## Life expectancy estimates Gaza 2024
gaza_le_m_24 <- read.csv(paste0(results_dir, "gaza/moh24_lifetable_m_le0.csv"))
gaza_le_f_24 <- read.csv(paste0(results_dir, "gaza/moh24_lifetable_f_le0.csv"))
gaza_le_t_24 <- read.csv(paste0(results_dir, "gaza/moh24_lifetable_t_le0.csv"))

gaza_24_all <- rbind(gaza_le_f_24[,vars], gaza_le_m_24[,vars], gaza_le_t_24[,vars])
gaza_24_all$region="Gaza Strip"

## Life expectancy estimates West Bank 2024 
wb_24_all <- read_rds("R/model/samples/pcbs_2019/2024/west_bank/lifetable_age0_wb_24_v2.rds")
wb_all$region="West Bank"

# All estimates for 2024
le_lss_all24 <- rbind(nat_24_all, gaza_24_all, wb_24_all)


# ---------------------------------------------------------------------------- #
#     2. Plot
# ---------------------------------------------------------------------------- #

## Define panels
panels <- tibble(sex = rep(c("Females", "Males", "Total"), 2), 
                 type = c(rep("Life expectancy \nat birth", 3), 
                          rep("Life expectancy \nloss to conflict", 3)),
                 panel = c("A", "B", "C", "D", "E", "F"),
                 ypos = c(rep(76, 3), rep(21, 3)))

## Life expectancy at birth 2012-2023
# Here we filter only for males and females, but the sex filter can be removed
le0_plot <- ggplot() +   
  geom_point(data = dt_ex %>% filter(year <= 2022) %>%
               filter(sex != "Total"),
             aes(x = year, y = ex_cnf,color=region, pch =  region),  alpha = 1, size = 3) + 
  geom_line(data = dt_ex%>%
              filter(sex != "Total"), aes(year, ex_noc, color=region, group=region, 
                                          linetype = "Life Expectancy (no conflict)"),alpha=0.5,  linewidth=1) + 
  geom_point(data = le_lss_all  %>%
               filter(sex != "Total") %>% filter(year >= 2023) %>%
               group_by(region, year, sex, scenario) %>%
               mutate(ex_mean = mean(ex)),
             aes(x = year, y = ex_mean, color=region, pch =  region),  size = 3) + 
  geom_linerange(data = le_lss_all %>%
               filter(sex != "Total") %>% filter(year >= 2023) %>%
               group_by(region, year, sex, scenario) %>%
               mutate(ex_mean = mean(ex),
                      ex_min = quantile(ex, 0.025),
                      ex_max = quantile(ex, 0.975)),
             aes(x = year, y = ex_mean, ymin = ex_min, ymax = ex_max, color=region), linewidth = 0.75,position = "identity") + 
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) +
  scale_color_manual("",values = c("#ef476f", "#FFA500", "#118ab2"),
                     labels = c( 
                       "Gaza Strip",
                       "Palestine",
                       "West Bank"),guide = 'none') + 
  scale_fill_manual(values = c("#ef476f", "#FFA500", "#118ab2"), name  = "Region") +
  xlab("Year") + ylab("Life expectancy at birth") +
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="") +
  scale_shape_manual(values = c(15,16,17)) +
  guides(shape = "none") +
  theme_bw()+
  scale_x_continuous(breaks = seq(2012, 2024, 2)) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 13),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)) 

### Add 2024 to the plots
# Life expectancy 2012-2024
le0_23_24  <- le0_plot +  
  geom_linerange(data = le_lss_all24 %>%
                   filter(sex != "Total") %>% filter(year >= 2023) %>%
                   group_by(region, year, sex, scenario) %>%
                   mutate(ex_mean = mean(ex),
                          ex_min = quantile(ex, 0.025),
                          ex_max = quantile(ex, 0.975)),
                 aes(x = year, y = ex_mean, ymin = ex_min, ymax = ex_max, color=region), position = "identity", linewidth=0.75) + 
  geom_point(data = le_lss_all24  %>%
               filter(sex != "Total") %>% filter(year >= 2023) %>%
               group_by(region, year, sex, scenario) %>%
               mutate(ex_mean = mean(ex)),
             aes(x = year, y = ex_mean,color=region, pch =  region),  alpha = 1, size = 3) +
  
  guides(shape = guide_legend(title = "Region", override.aes = list(linetype = 0, 
                                                                    color = c("#ef476f", "#FFA500", "#118ab2"), size = 6)),
         linetype = guide_legend(order = 1), fill = "none")

le0_23_24

# ---------------------------------------------------------------------------- #
#     3. Save plot
# ---------------------------------------------------------------------------- #

# ggsave(le0_23_24, file = "figures/Fig3.pdf", height = 5.77, width = 8.9)

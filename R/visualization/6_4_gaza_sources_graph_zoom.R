
# ---------------------------------------------------------------------------- #
# Title:   Zoomed-in plot (Figure 4 main text)
#
# Code to create plot for sensitivity analysis of LE estimates for the 
# Gaza Strip from different age-sex distributions
# ---------------------------------------------------------------------------- #
# Content:
#   0. Working directory, packages and functions
#   1. Load data
#   2. Create plot
#   3. Save plot
# ---------------------------------------------------------------------------- #
#     0. Working directory, package and functions
# ---------------------------------------------------------------------------- #

rm(list = ls())
library(tidyverse)
library(ggdist)
library(bayesplot)
library(reshape2)

# ---------------------------------------------------------------------------- #
#     1. Load data
# ---------------------------------------------------------------------------- #

## Life expectancy estimates 2012-2019
dt_gaza <- 
  readRDS("data/ex0_noc.rds") %>% 
  filter(source=="lc_pcbs_2019"& region=="Gaza Strip") %>%
  mutate(lss = ex_noc - ex_cnf, 
         sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))


## Results directory for Gaza estimates
results_dir <- paste0(getwd(),"/R/model/diff_reporting/samples/gaza/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## MoH results 2023 
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh23_lifetable_t_le0.csv"))

moh_all <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars])

## B'tselem results 2023
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts23_lifetable_t_le0.csv"))

bts_all <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars])

## UN genocide dist results 2023
un_le_m_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_m_le0.csv"))
un_le_f_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_f_le0.csv"))
un_le_t_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_t_le0.csv"))

un_genocide23_all <- rbind(un_le_f_age0[,vars], un_le_m_age0[,vars], un_le_t_age0[,vars])

# All estimates for 2023
le_lss_all <- rbind(moh_all, bts_all, un_genocide23_all)

## MoH results 2024
moh_le_m_24 <- read.csv(paste0(results_dir, "moh24_lifetable_m_le0.csv"))
moh_le_f_24 <- read.csv(paste0(results_dir, "moh24_lifetable_f_le0.csv"))
moh_le_t_24 <- read.csv(paste0(results_dir, "moh24_lifetable_t_le0.csv"))

moh_24_all  <- rbind(moh_le_m_24[,vars], moh_le_f_24[,vars],moh_le_t_24[,vars])

## B'tselem results 2024
bts_le_m_24 <- read.csv(paste0(results_dir, "bts24_lifetable_m_le0.csv"))
bts_le_f_24 <- read.csv(paste0(results_dir, "bts24_lifetable_f_le0.csv"))
bts_le_t_24 <- read.csv(paste0(results_dir, "bts24_lifetable_t_le0.csv"))

bts24_all <- rbind(bts_le_f_24[,vars], bts_le_m_24[,vars], bts_le_t_24[,vars])

## UN genocide dist results 2024
un_le_m_genocide24 <- read.csv(paste0(results_dir, "un_geno24_lifetable_m_le0.csv"))
un_le_f_genocide24  <- read.csv(paste0(results_dir, "un_geno24_lifetable_f_le0.csv"))
un_le_t_genocide24  <- read.csv(paste0(results_dir, "un_geno24_lifetable_t_le0.csv"))

un_genocide24_all <- rbind(un_le_f_genocide24[,vars], un_le_m_genocide24[,vars],un_le_t_genocide24[,vars])

un_genocide_all <- rbind(un_genocide23_all, un_genocide24_all)
un_genocide_all$type <- "Genocide"

# All estimates for 2024
le_lss_all24 <- rbind(moh_24_all, bts24_all, un_genocide24_all)


# ---------------------------------------------------------------------------- #
#     2. Create plot
# ---------------------------------------------------------------------------- #

## Life expectancy at birth 2012-2023
le0_plot <- ggplot() +   
  stat_histinterval(data=le_lss_all, aes(x = year, 
                                         y=ex, group=scenario, 
                                         fill=scenario, 
                                         color=scenario),
                    size=2,
                    alpha=0.5) +
  geom_point(data = le_lss_all %>% filter(year >= 2023) %>%
               group_by(year, sex, scenario) %>%
               mutate(ex_mean = mean(ex)),
             aes(x = year, y = ex_mean, color=scenario, pch =  scenario),  alpha = 1, size = 3) + 
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                     guide='none') + 
  scale_fill_manual(name = "Scenario",values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) +
  xlab("Year") + ylab("Life expectancy at birth") + 
  scale_x_continuous(breaks = c(2023,2024)) +
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(fill = guide_legend(override.aes = list(linetype = 0, pch = NA, color = "black")),
         linetype = guide_legend(order = 1),  shape = "none")+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14), 
        panel.grid.minor.x = element_blank())


## Add 2024
le0_23_24  <- le0_plot +  stat_histinterval(data=le_lss_all24, aes(x = year, y=ex, group=scenario, 
                                                                   fill=scenario, color=scenario),
                                            size=2, alpha=0.5) +
  geom_point(data = le_lss_all24 %>% filter(year >= 2023) %>%
               group_by(year, sex, scenario) %>%
               mutate(ex_mean = mean(ex)),
             aes(x = year, y = ex_mean, color=scenario, pch =  scenario),  alpha = 1, size = 3) +
  guides(shape = guide_legend(title = "Scenario", override.aes = list(linetype = 0, 
                                                                    color = c("#07c8ba","#ef476f",  "#3aab07"), size = 6)),
         fill = "none")


le0_23_24

# ---------------------------------------------------------------------------- #
#     3. Save plot
# ---------------------------------------------------------------------------- #

# ggsave(le0_23_24, file = "figures/LE_sources_zoom.pdf", width = 8.75, height = 5.25)




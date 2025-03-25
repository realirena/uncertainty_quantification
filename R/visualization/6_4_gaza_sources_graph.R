rm(list = ls())
library(tidyverse)
library(ggdist)
library(bayesplot)
library(reshape2)
## set the working directory
setwd("U:/Documents/repos/Life_expectancy_Palestine")

dt <- 
  readRDS("data/ex0_noc.rds") %>% 
  mutate(sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))
dt_gaza <- 
  readRDS("data/ex0_noc.rds") %>% 
  filter(source=="lc_pcbs_2019"& region=="Gaza Strip") %>%
  mutate(lss = ex_noc - ex_cnf, 
         sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))

#gaza_plot <- melt(dt_gaza, id.vars=c("sex", "source", "region", "year"), variable.name="metric", value.name="le0")


dt_palestine <- 
  readRDS("data/ex0_noc.rds") %>% 
  filter(source=="lc_pcbs_2019"& region=="Palestine") %>%
  mutate(scenario="Counterfactual with\n no genocide deaths", 
         lss = ex_noc - ex_cnf,
         sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))

panels <- tibble(sex = rep(c("Females", "Males", "Total"), 2), 
                 type = c(rep("Life expectancy \nat birth", 3), 
                          rep("Life expectancy \nloss to genocide", 3)),
                 panel = c("A", "B", "C", "D", "E", "F"),
                 ypos = c(rep(76, 3), rep(21, 3)))



### add new graphing code to plot the uncertainty estimates: 
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2023/gaza_bu/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
oct26_le_m_age0 <- read.csv(paste0(results_dir, "moh_lifetable_m_le0.csv"))
oct26_le_f_age0 <- read.csv(paste0(results_dir, "moh_lifetable_f_le0.csv"))
oct26_le_t_age0 <- read.csv(paste0(results_dir, "moh_lifetable_t_le0.csv"))

oct_26_all <- rbind(oct26_le_f_age0[,vars], oct26_le_m_age0[,vars], oct26_le_t_age0[,vars])

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts_lifetable_t_le0.csv"))

bts_all <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars])

###UN dist results 
un_le_m_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_m_le0.csv"))
un_le_f_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_f_le0.csv"))
un_le_t_age0 <- read.csv(paste0(results_dir, "un_geno23_lifetable_t_le0.csv"))

un_genocide23_all <- rbind(un_le_f_age0[,vars], un_le_m_age0[,vars], un_le_t_age0[,vars])

le_lss_all <- rbind(oct_26_all, bts_all, un_genocide23_all)

le_lss_all |>
  group_by(sex, year) |>
  summarise(mean_ex = mean(ex),
            mean_lss = mean(bmmr_lss))

## plot of life expectancies at birth 
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
  # scale_color_manual(values = c("#de5138", "#5a9cee", "#E69F00")) + 
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                     guide='none') + 
  scale_fill_manual(name = "Scenario",values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) +
  xlab("Year") + ylab("Life expectancy at birth") + 
  scale_x_continuous(breaks = c(2023,2024)) +
  #scale_fill_manual(values=c("#fe9441","#85b5cd", "#DE9D0D")) + 
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

### add new graphing code to plot the 2024 uncertainty estimates: 
results_dir <- paste0(getwd(),"/R/model/samples/pcbs_2019/2024/gaza_bu/")
## MoH 2024 (oct 23 - oct 24) results
moh_le_m_24 <- read.csv(paste0(results_dir, "moh_lifetable_m_le0.csv"))
moh_le_f_24 <- read.csv(paste0(results_dir, "moh_lifetable_f_le0.csv"))
moh_le_t_24 <- read.csv(paste0(results_dir, "moh_lifetable_t_le0.csv"))

moh_24_all  <- rbind(moh_le_m_24[,vars], moh_le_f_24[,vars],moh_le_t_24[,vars])


bts_le_m_24 <- read.csv(paste0(results_dir, "bts_lifetable_m_le0.csv"))
bts_le_f_24 <- read.csv(paste0(results_dir, "bts_lifetable_f_le0.csv"))
bts_le_t_24 <- read.csv(paste0(results_dir, "bts_lifetable_t_le0.csv"))

bts24_all <- rbind(bts_le_f_24[,vars], bts_le_m_24[,vars], bts_le_t_24[,vars])

un_le_m_genocide24 <- read.csv(paste0(results_dir, "un_geno24_lifetable_m_le0.csv"))
un_le_f_genocide24  <- read.csv(paste0(results_dir, "un_geno24_lifetable_f_le0.csv"))
un_le_t_genocide24  <- read.csv(paste0(results_dir, "un_geno24_lifetable_t_le0.csv"))

un_genocide24_all <- rbind(un_le_f_genocide24[,vars], un_le_m_genocide24[,vars],un_le_t_genocide24[,vars])

un_genocide_all <- rbind(un_genocide23_all, un_genocide24_all)
un_genocide_all$type <- "Genocide"

le_lss_all24 <- rbind(moh_24_all, bts24_all, un_genocide24_all)

le0_23_24  <- le0_plot +  stat_histinterval(data=le_lss_all24, aes(x = year, y=ex, group=scenario, 
                                                                   fill=scenario, color=scenario),
                                            size=2,
                                            alpha=0.5) +
  geom_point(data = le_lss_all24 %>% filter(year >= 2023) %>%
               group_by(year, sex, scenario) %>%
               mutate(ex_mean = mean(ex)),
             aes(x = year, y = ex_mean, color=scenario, pch =  scenario),  alpha = 1, size = 3) 


# ggsave(le0_23_24, file = "figures/LE_sources_zoom.pdf", width = 8.75, height = 5.25)


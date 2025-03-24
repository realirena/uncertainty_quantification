rm(list = ls())
library(tidyverse)
library(ggdist)
library(bayesplot)
library(reshape2)
library(ggpubr)
## set the working directory
setwd("U:/Documents/repos/uncertainty_quantification/")

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
  mutate(scenario="Counterfactual with\n no conflict deaths", 
         lss = ex_noc - ex_cnf,
                  sex = case_when(sex == "m" ~ "Males",
                         sex == "f" ~ "Females",
                         sex == "t" ~ "Total"))
  
panels <- tibble(sex = rep(c("Females", "Males", "Total"), 2), 
                 type = c(rep("Life expectancy \nat birth", 3), 
                          rep("Life expectancy \nloss to conflict", 3)),
                 panel = c("A", "B", "C", "D", "E", "F"),
                 ypos = c(rep(76, 3), rep(21, 3)))



### add new graphing code to plot the uncertainty estimates: 
results_dir <- paste0(getwd(),"/R/sensitivity_check/samples/")
vars <- c("ex", "bmmr_lss","year", "sex", "scenario")

## oct 26th results (updated with age distribution uncertainty)
moh_le_m_age0 <- read.csv(paste0(results_dir, "moh_2024_lifetable_m_le0.csv"))
moh_le_f_age0 <- read.csv(paste0(results_dir, "moh_2024_lifetable_f_le0.csv"))
moh_le_t_age0 <- read.csv(paste0(results_dir, "moh_2024_lifetable_t_le0.csv"))

moh_all <- rbind(moh_le_f_age0[,vars], moh_le_m_age0[,vars], moh_le_t_age0[,vars])

## B'tselem results
bts_le_m_age0 <- read.csv(paste0(results_dir, "bts_2024_lifetable_m_le0.csv"))
bts_le_f_age0 <- read.csv(paste0(results_dir, "bts_2024_lifetable_f_le0.csv"))
bts_le_t_age0 <- read.csv(paste0(results_dir, "bts_2024_lifetable_t_le0.csv"))

bts_all <- rbind(bts_le_f_age0[,vars], bts_le_m_age0[,vars], bts_le_t_age0[,vars])


# UN genocide pattern -----------------------------------------------------

###UN dist results 
un_le_m_geno24 <- read.csv(paste0(results_dir, "un_geno24_lifetable_m_le0.csv"))
un_le_f_geno24 <- read.csv(paste0(results_dir, "un_geno24_lifetable_f_le0.csv"))
un_le_t_geno24 <- read.csv(paste0(results_dir, "un_geno24_lifetable_t_le0.csv"))

un_genocide24_all <- rbind(un_le_f_geno24[,vars], un_le_m_geno24[,vars], un_le_t_geno24[,vars])

le_lss_geno_all <- rbind(moh_all, bts_all, un_genocide24_all)

le_lss_geno_all |>
  group_by(sex, year, scenario) |>
  summarise(mean_ex = mean(ex),
    mean_lss = mean(bmmr_lss))

## plot of life expectancies at birth 
le0_plot <- ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = ex_cnf, color="Life Expectancy (observed)"), color="black", alpha = 1, size = 1.5) +
  geom_line(data =dt_gaza, aes(year, ex_noc, linetype = "Life Expectancy (no conflict)"),  alpha = 0.5,
            col = "black", linewidth=1) +
  # counter factual with no conflict deaths
  stat_histinterval(data=le_lss_geno_all, aes(x = year, 
                                         y=ex, group=scenario, 
                                         fill=scenario, 
                                         color=scenario),
                    size=4,
                    alpha=0.5) +
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) + 
  scale_fill_manual(values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                    guide='none') +
  xlab("") + ylab("Life expectancy at birth") + 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  facet_grid(~sex, scale = "free_y", space = "free_y", switch = "y") +
  scale_linetype_discrete(name="")+
  theme_bw()+
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1),  shape = "none")+ 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 15),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14))

## plot of life expectancy loss (subtract each estimated le0 from 76)
lss_plot <-  ggplot() +   
  geom_point(data = dt_gaza %>% filter(year <= 2022),
             aes(x = year, y = lss), color="black", alpha = 0.5, size = 1) +
  stat_histinterval(data=le_lss_geno_all, 
                    aes(x = year, y=bmmr_lss, group=scenario, fill=scenario, color=scenario), 
                    alpha=0.5, size = 4) +
  scale_color_manual("",values = c("#07c8ba","#ef476f",  "#3aab07"),
                     labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern")) + 
  scale_fill_manual(values = c("#07c8ba","#ef476f",  "#3aab07"),
                    labels=  c("B'Tselem historical average","GMoH report",  "UN-IGME pattern"),
                    guide='none') +
  facet_grid(~ sex, scale = "free_y", space = "free_y", switch = "y") +
  theme_bw()+ 
  scale_x_continuous(breaks = seq(2012,2024,2)) +
  xlab("Year") + ylab("Life expectancy loss") + 
  guides(color = guide_legend(override.aes = list(linetype = 0)),
         linetype = guide_legend(order = 1))+
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

# le0_lss <- gridExtra::grid.arrange(le0_plot, lss_plot)

le0_lss <- ggarrange(le0_plot, lss_plot, nrow = 2, common.legend = TRUE, legend = "bottom")
# 
ggsave("figures/gaza_prior_check_guillot_le0_lss_23_24.png", plot=le0_lss, w = 16, h = 8)

